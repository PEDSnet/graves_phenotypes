#' Parse text or symbol before the numeric in measurement source values
#' Sometimes symbols at the start of measurement source values cause them to
#' fail to parse. This is a problem, for example, if all the <0.02 values are
#' not parsed, because it would bias the results.
#'
#' NOTE: this logic assumes the table is collected locally
#'
#' @param measurement_tbl Locally collected measurement tbl to use
#' @param operator Start character(s) to remove
#' @param concept_id operator_concept_id to set
#'
#' @return Locally collected measurement tbl with fixed value_as_number,
#' operator_concept_id, and operator_concept_name for those value_source_values
#' with a NA value_as_number, but parseable value_source_value once the operator
#' has been removed
#' @export
#'
#' @examples
fix_inequality_symbol <- function(measurement_tbl,
                                  operator,
                                  concept_id,
                                  column = "value_as_number",
                                  prefix = "value",
                                  reparse = FALSE) {
  regex <- paste0("^", tolower(operator), "\\s*[0-9]*([.,])?[0-9]*$")
  operator_length <- str_length(operator) + 1L

  operator_name <- vocabulary_tbl("concept") %>%
    filter(concept_id == {{ concept_id }}) %>%
    pull(concept_name)

  source_value <- paste0(prefix, "_source_value")
  manual <- paste0(column, "_manual")
  operator_id <- if_else(column == "value_as_number", "operator_concept_id", paste0(column, "_operator_concept_id"))
  operator_id_edited <- paste0(operator_id, "_edited")
  operator_concept_name <- if_else(column == "value_as_number", "operator_concept_name", paste0(column, "_operator_concept_name"))
  operator_concept_name_edited <- paste0(operator_concept_name, "_edited")

  # NOTE: if_else does not appear to work on a local table, so we have to
  # split into wont_modify and to_update, and then union
  if(reparse){
    wont_modify <- measurement_tbl %>%
      filter(is.na(measurement_id))
  }else{
    wont_modify <- measurement_tbl %>%
      filter(!is.na(!!sym(column)) | is.na(!!sym(source_value)))
  }

  to_update <- measurement_tbl %>%
    anti_join(wont_modify, by = c("person_id", "measurement_id")) %>%
    mutate(
      numeric_part = if_else(
        str_detect(tolower(!!sym(source_value)), regex),
        str_sub(tolower(!!sym(source_value)), start = operator_length),
        NA
      ),
      !!sym(manual) := case_when(
        !is.na(numeric_part) &
          str_detect(numeric_part, "^\\s*0+,") ~
          suppressWarnings(as.numeric(str_replace(numeric_part, ",", "."))),
        !is.na(numeric_part) &
          str_detect(numeric_part, "^\\s*[1-9][0-9]*,") ~
          suppressWarnings(as.numeric(str_remove(numeric_part, ","))),
        !is.na(numeric_part) ~
          suppressWarnings(as.numeric(str_remove(numeric_part, ","))),
        TRUE ~ NA
      ),
      !!sym(operator_id_edited) := if_else(
        is.na(!!sym(manual)), NA, concept_id),
      !!sym(operator_concept_name_edited) := if_else(
        is.na(!!sym(manual)), NA, operator_name)
    ) %>% select(-numeric_part)

  modified <- to_update %>%
    filter(
      (is.na(!!sym(column)) & !is.na(!!sym(manual))) |
        (!!sym(column) != !!sym(manual))) %>%
    count(
      !!sym(source_value),
      !!sym(column),
      !!sym(manual),
      !!sym(operator_id),
      !!sym(operator_id_edited)
    ) %>%
    arrange(desc(n))
  message(operator, ": Fixed ", sum(modified[["n"]]), " source values")
  message(paste0(capture.output(modified), collapse = "\n"))

  # Then re-join
  updated <- to_update %>%
    mutate(
      !!sym(column) := if_else(is.na(!!sym(manual)),
                               !!sym(column),
                               !!sym(manual)),
      !!sym(operator_id) := if_else(is.na(!!sym(operator_id_edited)),
                                    !!sym(operator_id),
                                    !!sym(operator_id_edited)),
      !!sym(operator_concept_name) := if_else(is.na(!!sym(operator_concept_name_edited)),
                                              !!sym(operator_concept_name),
                                              !!sym(operator_concept_name_edited))
    ) %>%
    select(-!!sym(operator_id_edited), -!!sym(operator_concept_name_edited))

  combined <- dplyr::bind_rows(wont_modify, updated)
  combined
}

#' Pre-defined set of inequalities to parse in measurement source values
#'
#' @param measurement_tbl Locally collected measurement tbl to use
#'
#' @return Locally collected measurement tbl with 4 types inequalities fixed
#' @export
#'
#' @examples
fix_inequality_symbols <- function(measurement_tbl,
                                   column = "value_as_number",
                                   prefix = "value",
                                   reparse = TRUE) {
  measurement_tbl <-
    fix_inequality_symbol(
      measurement_tbl,
      "<",
      4171756L,
      column = column,
      prefix = prefix,
      reparse = reparse
    )
  measurement_tbl <-
    fix_inequality_symbol(
      measurement_tbl,
      "<=",
      4171754L,
      column = column,
      prefix = prefix,
      reparse = reparse
    )
  measurement_tbl <-
    fix_inequality_symbol(
      measurement_tbl,
      "less than",
      4171756L,
      column = column,
      prefix = prefix,
      reparse = reparse
    )
  measurement_tbl <-
    fix_inequality_symbol(
      measurement_tbl,
      ">",
      4172704L,
      column = column,
      prefix = prefix,
      reparse = reparse
    )
  measurement_tbl <-
    fix_inequality_symbol(
      measurement_tbl,
      ">=",
      4171755L,
      column = column,
      prefix = prefix,
      reparse = reparse
    )
  measurement_tbl <-
    fix_inequality_symbol(
      measurement_tbl,
      "greater than",
      4172704L,
      column = column,
      prefix = prefix,
      reparse = reparse
    )
  measurement_tbl %>% arrange(person_id)
}


#' Fix incorrect mappings of Free T3 to Total T3
fix_tt3_ft3_t3uptake <- function(measurement_tbl) {
  fixed <- measurement_tbl %>%
    mutate(
      measurement_concept_name = if_else(
        measurement_concept_id == 2212602L &
          unit_concept_id == 8845L &
          str_detect(measurement_source_value, fixed("TRIIODOTHYRONINE, FREE, SERUM")),
        "Triiodothyronine (T3) Free [Mass/volume] in Serum or Plasma",
        measurement_concept_name
      ),
      measurement_concept_id = if_else(
        measurement_concept_id == 2212602L &
          unit_concept_id == 8845L &
          str_detect(measurement_source_value, fixed("TRIIODOTHYRONINE, FREE, SERUM")),
        3026925L,
        measurement_concept_id
      ),
      measurement_concept_name = if_else(
        measurement_concept_id == 3010340L &
          (unit_concept_id == 8554L | range_high < 100) &
          str_detect(measurement_source_value, fixed("T3 UPTAKE, SERUM")),
        "Triiodothyronine resin uptake (T3RU) in Serum or Plasma",
        measurement_concept_name
      ),
      measurement_concept_id = if_else(
        measurement_concept_id == 3010340L &
          (unit_concept_id == 8554L | range_high < 100) &
          str_detect(measurement_source_value, fixed("T3 UPTAKE, SERUM")),
        3021717L,
        measurement_concept_id
      )
    )
  fixed
}

#' Fix incorrect mappings of TRAB to TSH
fix_trab_tsh <- function(measurement_tbl) {
  fixed <- measurement_tbl %>%
    mutate(
      measurement_concept_name = if_else(
        measurement_concept_id == 3009201L &
          str_detect(measurement_source_value, fixed("TSH RECEPTOR ANTIBODY")),
        "Thyrotropin receptor Ab [Units/volume] in Serum",
        measurement_concept_name
      ),
      measurement_concept_id = if_else(
        measurement_concept_id == 3009201L &
          str_detect(measurement_source_value, fixed("TSH RECEPTOR ANTIBODY")),
        3017044L,
        measurement_concept_id
      )
    )
  fixed
}

#' Fix incorrect mappings of Free T4 to Total T4
#'
#' Where the source values have LOINC or CPT codes for Free T4
#'
#' @param measurement_tbl Lazy db measurement tbl
#'
#' @return Lazy db measurement tbl with some
#' @export
#'
#' @examples
fix_tt4_ft4 <- function(measurement_tbl) {
  fixed <- measurement_tbl %>%
    mutate(
      measurement_concept_id = if_else(
        measurement_concept_id == 3016991L &&
          str_detect(measurement_source_value, fixed("3024-7")),
        3008598L,
        measurement_concept_id
      ),
      measurement_concept_name = if_else(
        measurement_concept_id == 3016991L &&
          str_detect(measurement_source_value, fixed("3024-7")),
        "Thyroxine (T4) free [Mass/volume] in Serum or Plasma",
        measurement_concept_name
      )
    )
  fixed <- fixed %>%
    mutate(
      measurement_concept_id = if_else(
        measurement_concept_id == 3016991L &&
          str_detect(measurement_source_value, fixed("6892-4")),
        3024675L,
        measurement_concept_id
      ),
      measurement_concept_name = if_else(
        measurement_concept_id == 3016991L &&
          str_detect(measurement_source_value, fixed("6892-4")),
        "Thyroxine (T4) free [Mass/volume] in Serum or Plasma by Dialysis",
        measurement_concept_name
      )
    )
  fixed <- fixed %>%
    mutate(
      measurement_concept_id = if_else(
        measurement_concept_id == 3016991L &&
          str_detect(measurement_source_value, fixed("84439")),
        4196969L,
        measurement_concept_id
      ),
      measurement_concept_name = if_else(
        measurement_concept_id == 3016991L &&
          str_detect(measurement_source_value, fixed("84439")),
        "T4 free measurement",
        measurement_concept_name
      )
    )
  fixed
}

#' Do not use Kelvin unit IDs
#'
#' @param measurement_tbl Lazy db measurement tbl
#'
#' @return Lazy db measurement tbl replacing 8792 with 8848 and 8903 with 8961
#' @export
#'
#' @examples
fix_kelvin <- function(measurement_tbl) {
  measurement_tbl <- measurement_tbl %>%
    mutate(
      unit_concept_id = if_else(
        unit_concept_id == 8792L,
        8848L,
        unit_concept_id
      ),
      unit_concept_name = if_else(
        unit_concept_id == 8792L,
        "thousand per microliter",
        unit_concept_name
      )
    ) %>%
    mutate(
      unit_concept_id = if_else(
        unit_concept_id == 8903L,
        8961L,
        unit_concept_id
      ),
      unit_concept_name = if_else(
        unit_concept_id == 8903L,
        "thousand per cubic millimeter",
        unit_concept_name
      )
    )
  measurement_tbl
}

fix_known_src_values <- function(measurement_tbl) {
  df <- readr::read_csv("src_mappings2.csv", show_col_types = FALSE)

  # iterate through patterns, build one big WHERE condition
  for (i in seq_len(nrow(df))) {
    row <- df[i, ]

    measurement_tbl <- measurement_tbl %>%
      mutate(
        measurement_concept_id = case_when(
          measurement_concept_id == 0L &
            str_detect(lower(measurement_source_value), row$pattern) ~ row$concept_id,
          TRUE ~ measurement_concept_id
        )
      )
  }

  measurement_tbl
}


fix_known_src_values <- function(measurement_tbl) {
  df <-
    readr::read_delim(
      "src_mappings2.csv",
      delim = ",",
      escape_backslash = TRUE,
      show_col_types = FALSE
    )
  for (i in rownames(df)) {
    row <- df[i, ]
    to_replace <- measurement_tbl %>%
      filter(
        measurement_concept_id == 0L,
        str_detect(lower(measurement_source_value), row[["pattern"]])
      ) %>%
      pull(measurement_id)
    message(
      "Replaced ",
      length(to_replace),
      " '",
      row[["pattern"]],
      "' with ",
      row[["concept_id"]]
    )
    measurement_tbl <- measurement_tbl %>% mutate(
      measurement_concept_id = if_else(
        measurement_id %in% to_replace,
        row[["concept_id"]],
        measurement_concept_id
      )
    )
  }
  measurement_tbl
}

#' Nullify biologically implausible values
#'
#' So that we can try to find a different closest measurement
#'
#' @param measurement_tbl
#'
#' @return
#' @export
#'
#' @examples
exclude_implausible <- function(measurement_tbl) {
  message("Excluding heart rate < 20, height < 50cm")
  measurement_tbl <- measurement_tbl %>%
    mutate(
      # Heart rate must be greater than 20
      value_as_number = if_else(
        (measurement_concept_id == 3023540L &&
           value_as_number < 20L),
        NA,
        value_as_number
      ),
      # Height must be greater than 50cm
      value_as_number = if_else(
        (measurement_concept_id == 3027018L &&
           value_as_number < 50L),
        NA,
        value_as_number
      )
    )
  measurement_tbl
}

#' Fix range values that are missing or just used the first digit
#'
#' @param measurement_tbl Lazy db measurement tbl
#' There are some range_high/range_low where the source value was NA and the
#' value was set to 0. Reset these to NA. There are others where only the first
#' digit has been used e.g. 150 -> 1 and we re-parse these to fix
#' For some sites, the range_high/range_low and source value will not match due
#' to unit translation e.g. Fahrenheit to Celsius, ounces to kg, inches to cm
#' If numbers have been rounded to the nearest digit, for some labs we need
#' more precision e.g. 0.4 should not be rounded to 0, as then all values would
#' be above the range_low
#'
#' @return Lazy db measurement tbl with fixed range_high, range_low
#' @export
#'
#' @examples
fix_range_high_low <- function(measurement_tbl) {
  message("Fixing range source values that were parsed incorrectly")
  range_cast <- measurement_tbl %>%
    mutate(
      range_high_is_numeric = str_detect(
        range_high_source_value,
        "^(\\d+(\\.\\d*)?|\\.\\d+)$"
      ),
      range_low_is_numeric = str_detect(
        range_low_source_value,
        "^(\\d+(\\.\\d*)?|\\.\\d+)$"
      ),
      range_high_as_numeric = if_else(
        range_high_is_numeric,
        as.numeric(range_high_source_value),
        NA
      ),
      range_low_as_numeric = if_else(
        range_low_is_numeric,
        as.numeric(range_low_source_value),
        NA
      ),
      # Check that the values do not match, that the values are not equal when
      # rounded with the round or floor function, and that the first digits
      # match
      range_high_fixed = if_else(
        !is.na(range_high_as_numeric) &
          (range_high != range_high_as_numeric) &
          ((as.numeric(str_sub(
            as.character(range_high_as_numeric),
            start = 1L,
            end = 1L
          )) == range_high) |
            (round(range_high, 0L) == round(range_high_as_numeric, 0L))),
        range_high_as_numeric,
        NA
      ),
      range_low_fixed = if_else(
        !is.na(range_low_as_numeric) &
          (range_low != range_low_as_numeric) &
          ((as.numeric(str_sub(
            as.character(range_low_as_numeric),
            start = 1L,
            end = 1L
          )) == range_low) |
            (round(range_low, 0L) == round(range_low_as_numeric, 0L))),
        range_low_as_numeric,
        NA
      )
    )

  # Notify the count of impacted values by site
  message(
    "Fixed range high: ",
    paste(range_cast %>%
            filter(!is.na(range_high_fixed)) %>%
            count(site) %>%
            collect(), collapse = "\n"))
  message(
    "Fixed range low: ",
    paste(range_cast %>%
            filter(!is.na(range_low_fixed)) %>%
            count(site) %>%
            collect(), collapse = "\n"))

  fixed <- range_cast %>%
    mutate(
      range_high = if_else(
        !is.na(range_high_fixed),
        range_high_fixed,
        range_high
      ),
      range_low = if_else(
        !is.na(range_low_fixed),
        range_low_fixed,
        range_low
      )
    )

  # If the range_high/range_low is 0 and the source value is NA, then set the
  # value to 0

  fixed <- fixed %>%
    mutate(range_high = if_else(((range_high == 0L) &
                                   is.na(range_high_source_value)
    ), NA, range_high),
    range_low = if_else(((range_low == 0L) &
                           is.na(range_low_source_value)
    ), NA, range_low))
}

#' Fix pipeline heights and BMI z-scores that depend on them
#'
#' The PEDSnet Pipeline for computing height from inches to cm seems to be
#' either dropping a leading 1 e.g. what should be 127 cm is 27 cm or skipping
#' the conversion
#' At this time we remove dependent values rather than fix them
#' Output a new table to the database that
#' 1. Removed the heights
#' 2. Removed BMIs that depend on them and BMIs of 0
#' 3. Removes BMI z-scores that depend on those BMIs
#'
#' @return
#' @export
#'
#' @examples
fix_height <- function() {
  measurement_tbl <- results_tbl("measurement_anthro")

  # Find BMIs of 0 (there was an NA height or weight)
  bmi_zero <- measurement_tbl %>%
    filter(measurement_concept_id == 3038553, value_as_number == 0) %>%
    distinct(person_id, measurement_id)

  # Find likely incorrect heights
  heights_to_remove <- measurement_tbl %>%
    filter(
      measurement_concept_id == 3023540 &
        unit_source_value == "cm (derived from inches in PEDSnet Pipeline)"
    ) %>%
    distinct(person_id, measurement_id)

  # Find BMIs that we want to remove
  bmis_to_remove <- heights_to_remove %>%
    mutate(source_value = paste0("ht: ", measurement_id)) %>%
    select(-measurement_id) %>%
    inner_join(measurement_tbl %>% filter(measurement_concept_id == 3038553),
               by = "person_id") %>%
    filter(str_detect(value_source_value, source_value)) %>%
    distinct(person_id, measurement_id) %>%
    union(bmi_zero)

  # And exclude z-scores based on those
  bmi_z_scores_to_remove <- bmis_to_remove %>%
    union(bmi_zero) %>%
    mutate(source_value = paste0("measurement: ", measurement_id)) %>%
    select(-measurement_id) %>%
    inner_join(measurement_tbl %>% filter(measurement_concept_id == 2000000043),
               by = join_by(person_id, source_value == value_source_value)) %>%
    distinct(person_id, measurement_id)

  measurement_tbl %>%
    anti_join(bmis_to_remove, by = join_by(person_id, measurement_id)) %>%
    anti_join(heights_to_remove, by = join_by(person_id, measurement_id)) %>%
    anti_join(bmi_z_scores_to_remove, by = join_by(person_id, measurement_id)) %>%
    output_tbl(name = "measurement_anthro_cleaned")

}

# Anti-tg
# 0 thyroglobulin a
# 0 thyroglobulin by lc
# 0 or 2212816 thyroglobulin.*thyroglobulin

# Anti-tpo
# 0 THYROID PEROXIDASE ANTIBODY|
# 0 or 2212816 peroxidase.*peroxidase

fix_anti_tpo_tg <- function(measurement_tbl) {
  measurement_tbl %>%
    mutate(
      measurement_concept_id = case_when(
        measurement_concept_id %in% c(0L, 2212816L) &
          str_detect(tolower(measurement_source_value), "peroxidase.*peroxidase") ~ 3027238L,
        measurement_concept_id %in% c(0L, 2212816L) &
          str_detect(tolower(measurement_source_value), "thyroglobulin.*thyroglobulin") ~ 3025547L,
        measurement_concept_id == 0L &
          measurement_source_value %in% c("THYROID PEROXIDASE ANTIBODY|", "THYROID PEROXIDASE ANTIBODY|3173") ~ 3027238L,
        measurement_concept_id == 0L &
          str_detect(tolower(measurement_source_value), "(thyroglobulin a|thyroglobulin by lc)") ~ 3025547L,
        .default = measurement_concept_id
      )
    )
}

#' Combine manual fixes into one function call
#'
#' That are going to be applied up-front in a given run; other fixes (such as
#' inequality operators) may be applied later
#'
#' @param measurement_tbl Lazy db measurement tbl
#'
#' @return Lazy db measurement tbl with manual fixes applied
#' @export
#'
#' @examples
make_manual_fixes <- function(measurement_tbl) {
  measurement_tbl <- fix_kelvin(measurement_tbl)
  measurement_tbl <- fix_tt4_ft4(measurement_tbl)
  measurement_tbl <- fix_tt3_ft3_t3uptake(measurement_tbl)
  measurement_tbl <- fix_trab_tsh(measurement_tbl)
  measurement_tbl <- fix_range_high_low(measurement_tbl)
  measurement_tbl <- fix_anti_tpo_tg(measurement_tbl)
  measurement_tbl <- exclude_implausible(measurement_tbl)
  measurement_tbl
}
