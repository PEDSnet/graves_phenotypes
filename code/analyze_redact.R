#' Round to the nearest n
#' @param x input vector
#' @param round_to value to round to
#'
#' @return rounded vector
#' @export
#'
#' @examples
round_nearest_n <- function(x, round_to = 5L) {
  round(x / round_to) * round_to
}

#' Check if the table is remote
#' Some functions require a table to be local
#' @param df
#' @return Logical is table remote
#' @export
#'
#' @examples
is_remote_tbl <- function(df) {
  if (inherits(df, "tbl_lazy")) {
    TRUE
  } else {
    FALSE
  }
}

#' Replace NA with a value for a subset of columns
#'
#' @param df input dataframe
#' @param columns columns to replace
#' @param redaction_val default is NA but could be 0 to preserve integer type
#' or '[REDACTED]' for display
#'
#' @return
#' @export
#'
#' @examples
replace_na_subset <- function(df, columns, redaction_val) {
  df <- df %>% mutate(across(
    all_of(columns),
    ~ ifelse(is.na(.), redaction_val, as.character(.))
  ))
  df
}

#' Replace value below threshold with NA
#' And redacts next smallest value if the total redacted is less than the
#' threshold
#'
#' @param column Column to redact
#' @param threshold Redact values below this threshold
#'
#' @return Redacted column
#' @export
#'
#' @examples
redactor <- function(column,
                     threshold = 7L) {
  # Coerce to NA if not integers
  n <- as.integer(column)
  leq_threshold <- dplyr::between(n, 1L, threshold)
  n_sum <- sum(n, na.rm = TRUE)
  # redact if n is less than or equal to redaction threshold
  redact <- leq_threshold
  # also redact next smallest n if sum of redacted n is still less than or equal
  # to threshold
  if ((sum(n * leq_threshold, na.rm = TRUE) <= threshold) &&
        any(leq_threshold, na.rm = TRUE)) {
    redact[which.min(dplyr::if_else(leq_threshold, n_sum + 1L, n))] <- TRUE
  }
  n[redact] <- NA
  n
}

#' Redact values less then threshold then optionally round
#' Statistical disclosure control function to redact values less than a
#' threshold and then round. Redaction of small values by subgroup protects
#' against direct disclosure of information about groups less than the
#' threshold. Ensuring that the total redacted >= threshold protects against
#' subtracting known numbers from the total to deduce the redacted value.
#' Rounding protects against differencing multiple outputs with small number
#' differences over time e.g. adding an exclusion criteria that removes 1
#' person from the previous count.
#'
#' @param df unredacted dataframe
#' @param columns column(s) to redact
#' @param grouper apply redaction to each subgroup in this column
#' @param threshold redact counts at or below this threshold
#' @param redaction_val default is NA but could be 0 to preserve integer type
#' or '[REDACTED]' for display
#' @param round_to rounds to this value if not NA
#'
#' @return a redacted and (optionally) rounded dataframe
#' @export
#'
#' @examples
redact_and_round <-
  function(df,
           columns,
           groupers = NA,
           threshold = 10L,
           redaction_val = NA,
           round_to = 5L) {
    assertthat::assert_that(!is_remote_tbl(df),
      msg = "Cannot redact or round remote table, first collect the table"
    )
    # Redact each group individually
    # An NA in any subgroup should be NA in the result
    # Grouping by both variables at once would not redact the next smallest
    if (!all(is.na(groupers)) && (all(groupers %in% colnames(df)))) {
      redacted <- NA
      for (grouper in groupers) {
        group_redacted <- df %>%
          group_by(!!sym(grouper)) %>%
          mutate(across(
            all_of(columns),
            ~ redactor(.x, threshold = threshold)
          ))
        if (all(is.na(redacted))) {
          redacted <- group_redacted
        } else {
          redacted[is.na(redacted) | is.na(group_redacted)] <- NA
        }
      }
    } else {
      message("Redacting the whole column without grouping")
      redacted <- df %>%
        mutate(across(
          all_of(columns),
          ~ redactor(.x, threshold = threshold)
        ))
    }

    if (!is.na(round_to)) {
      redacted <- redacted %>%
        mutate(across(all_of(columns), ~ round_nearest_n(.x, round_to)))
    }

    if (!is.na(redaction_val)) {
      redacted <- redacted %>% replace_na_subset(columns, redaction_val)
    }

    redacted
  }
