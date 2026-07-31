require(data.table)
require(tidyverse)
#' A collection of multi-purpose utility functions for the Graves study

#' Union together codesets
#'
#' @param codesets String name of codeset in database
#' Useful when you want to search for multiple codesets at once, such as
#' any vital (BMI, heart rate, systolic, diastolic) or any anti-thyroid drug
#' exposure
#'
#' @return A lazy db tbl of concept_id, codeset name
#' @export
#'
#' @examples
union_codesets <- function(codesets) {
  result <- NA
  for (codeset in codesets) {
    if (typeof(result) == "list") {
      result <- result %>%
        union(results_tbl(codeset) %>%
                mutate(codeset = {{ codeset }}) %>%
          select(concept_id, codeset))
    } else {
      result <- results_tbl(codeset) %>%
        mutate(codeset = {{ codeset }}) %>%
        select(concept_id, codeset)
    }
  }
  result
}

#' Remove diagnosis dates after treatment
#' Before we use the second date as a reference date, we need to make sure that
# 1) There was no total thyroidectomy before the graves dx
# 2) There was no anti-thyroid drug more than 'treatment_days' days before
#' graves dx
#'
#' @param cohort cohort_post_attrition created by driver.R that contains
#' dx_graves_date, thyroidectomy_date, and drug_exposure_date
#' @param max_days Treatment must have started at least this many days after
#' the graves diagnosis
#'
#' @return Lazy db tbl with post-treatment dx_graves_dates NA'd
#' @export
#'
#' @examples
filter_no_treatment <- function(cohort, treatment_window = 30L) {
  cohort <- cohort %>%
    mutate(dx_graves_date = if_else(
      !is.na(thyroidectomy_date) &&
        (dx_graves_date >= thyroidectomy_date),
      NA,
      dx_graves_date
    ))
  had_thyroidectomy <-
    (cohort %>% filter(is.na(dx_graves_date)) %>% count() %>% pull(n))
  message(
    "Removed ",
    had_thyroidectomy,
    " second dx that had total thyroidectomy"
  )
  cohort <- cohort %>%
    mutate(dx_graves_date = if_else(
      !is.na(earliest_at_drug_exposure_start_date) &&
        !is.na(dx_graves_date) &&
        (dx_graves_date - earliest_at_drug_exposure_start_date) > treatment_window,
      NA,
      dx_graves_date
    ))
  had_at <- cohort %>%
    filter(is.na(dx_graves_date)) %>%
    count() %>%
    pull(n) - had_thyroidectomy
  message(
    "Removed ",
    had_at,
    " second dx that had started an anti-thyroid drug"
  )
  cohort
}

#' Generate condition history for patients
#'
#' @param cohort cohort_post_attrition created by driver.R that contains
#' dx_graves_date, thyroidectomy_date, and drug_exposure_date
#' @param codesets list of condition codesets to check
#' @param min_days Condition can be this many days before the dx code
#' @param max_days Condition can be this many days after the dx code
#'
#' @return Lazy db tbl with one row per person, one column per condition with
#' 0/1 if the person ever had the condition
#' @export
#'
#' @examples
find_conditions <- function(cohort, codesets, min_days = -20L, max_days = 7L) {
  conditions <- list()
  for (i in seq_along(codesets)) {
    codeset <- codesets[i]
    closest <- find_closest_2_dates(
      results_tbl(codeset),
      cohort,
      results_tbl("condition_occurrence"),
      "condition_occurrence",
      order = "closest",
      min_days = min_days,
      max_days = max_days
    ) %>%
      mutate(
        value = as.numeric(if_else(is.na(condition_occurrence_id), 0L, 1L)),
        codeset = {{ codeset }}
      ) %>%
      select(person_id, codeset, value)

    conditions[[i]] <- closest
  }
  all_conditions <- dplyr::bind_rows(conditions) %>%
    pivot_wider(
      names_from = codeset,
      values_from = value
    )

  all_conditions
}

#' Get the name of the start date field per domain
#'
#' There is not a consistent pattern e.g. removing "occurrence" but keeping
#' "exposure", so we do this manually
#'
#' @param domain OMOP domain table name
#'
#' @return string name of the start date field
#' @export
#'
#' @examples
get_date_start_name <- function(domain) {
  if (domain == "measurement") {
    "measurement_date"
  } else if (domain == "condition_occurrence") {
    "condition_start_date"
  } else if (domain == "drug_exposure") {
    "drug_exposure_start_date"
  } else if (domain == "procedure_occurrence") {
    "procedure_date"
  } else if (domain == "visit_occurrence") {
    "visit_start_date"
  } else {
    stop("Domain start date not mapped: ", domain)
  }
}

#' Compute the time between an event domain's start date and another date
#'
#' Includes special handling for the measurement domain, as the measurement date
#' field is sometimes the order date and sometimes the result date, or neither.
#' Sometimes the diagnosis is the billing reason for labs, sometimes it is the
#' result of the labs. For measurements, compute the minimum difference.
#'
#' @param df A dataframe of events and at least one other date field
#' @param domain OMOP domain of the dataframe events
#' @param date_field Column name of the other date field
#'
#' @return
#' @export
#'
#' @examples
compute_diff <- function(df, domain, date_field) {
  if (domain == "measurement") {
    diff <- df %>%
      mutate(
        diff = (measurement_date - !!sym(date_field)),
        order_diff = (measurement_order_date - !!sym(date_field)),
        result_diff = (measurement_result_date - !!sym(date_field)))
  } else {
    domain_date_field <- get_date_start_name(domain)
    diff <- df %>%
      mutate(diff = (!!sym(domain_date_field) - !!sym(date_field)))
  }
  diff
}

#' Find the closest event to the dx code, within the range of days provided
#'
#' Option to find either the absolute closest event "closest" or the closest
#' event before the dx, and then look after the dx "closest_negative". For the
#' measurement domain, use a preference for numeric measurements.
#'
#' @param codeset Lazy db codeset tbl to check
#' @param cohort cohort_post_attrition created by driver.R that contains
#' dx_graves_date, thyroidectomy_date, and drug_exposure_date
#' @param domain_tbl Events table to use (may have local modifications)
#' @param domain OMOP domain of the events table
#' @param date_field Column name in the cohort to treat at the reference date
#' @param order "closest" or "closest_negative"
#' @param min_days Event can be this many days before the dx code
#' @param max_days Event can be this many days after the dx code
#'
#' @return
#' @export
#'
#' @examples
find_closest <-
  function(codeset,
           cohort,
           domain_tbl,
           domain,
           date_field = "dx_hyperthyroid_date",
           order = "closest_negative",
           min_days = -20L,
           max_days = 7L,
           fix_symbols = FALSE) {
    domain_concept_name <- paste0((domain %>%
                                     str_split_i("_", 1L)), "_concept_id")
    domain_time <- paste0(get_date_start_name(domain), "time")

    all_times <- (cohort %>%
                    select(person_id, !!sym(date_field))) %>%
      inner_join((
        domain_tbl %>%
          inner_join(codeset,
            by = join_by({{ domain_concept_name }} == "concept_id")
          )
      ), by = "person_id")

    times <- all_times %>%
      compute_diff(domain, date_field = date_field)

    # There is not an easy way to dynamically build the order_by criteria, so
    # we have to repeat the code
    # For measurement, there are extra criteria of numeric then categorical
    if (domain == "measurement") {
      times_filtered <- times %>%
        filter((diff >= min_days & diff <= max_days) |
                 (!is.na(order_diff) & order_diff >= min_days & order_diff <= max_days) |
                 (!is.na(result_diff) & result_diff >= min_days & result_diff <= max_days)) %>%
        collect()

      if (fix_symbols) {
        times_filtered <- fix_inequality_symbols(times_filtered, reparse = TRUE)
        message("Remaining value source values:")
        message(paste0(
          capture.output(
            times_filtered %>%
              filter(is.na(value_as_number)) %>%
              count(value_source_value) %>%
              arrange(desc(n))
          ),
          collapse = "\n"
        ))
      }

      times_filtered <- times_filtered %>%
        categorize_measurment_results()

      if (order == "closest") {
        closest <- times_filtered %>%
          group_by(person_id, !!sym(date_field)) %>%
          slice_min(
            # Choose closest numeric, then categorical
            # Preferring the collection time, then result time, then order time
            order_by = tibble(
              result_type != "numeric", result_type != "categorical",
              abs(diff), abs(result_diff), abs(order_diff), measurement_datetime
            ),
            n = 1L,
            with_ties = FALSE
          ) %>%
          ungroup()
      } else if (order == "closest_negative") {
        closest <- times_filtered %>%
          group_by(person_id, !!sym(date_field)) %>%
          slice_min(
            # Choose numeric with a preference for negative, then categorical
            # Preferring the collection time, then result time, then order time
            order_by = tibble(
              result_type != "numeric", result_type != "categorical",
              diff > -1L, abs(diff), result_diff > -1L,
              abs(result_diff), order_diff > -1L, abs(order_diff),
              measurement_datetime
            ),
            n = 1L,
            with_ties = FALSE
          ) %>%
          ungroup()
      }
    } else {
      times_filtered <- times %>%
        filter(diff >= min_days, diff <= max_days) %>%
        collect()

      if (order == "closest") {
        closest <- times_filtered %>%
          group_by(person_id, !!sym(date_field)) %>%
          slice_min(
            # Choose closest
            order_by = tibble(
              abs(diff), !!sym(domain_time)
            ),
            n = 1L,
            with_ties = FALSE
          ) %>%
          ungroup()
      } else if (order == "closest_negative") {
        closest <- times_filtered %>%
          group_by(person_id, !!sym(date_field)) %>%
          slice_min(
            # Choose closest with a preference for negative values
            order_by = tibble(
              diff > -1L, abs(diff), !!sym(domain_time)
            ),
            n = 1L,
            with_ties = FALSE
          ) %>%
          ungroup()
      }
    }
    closest
  }

#' Find the event closest to the first diagnosis date. If none is found,
#' try the first Graves-specific diagnosis date
#' The code is too slow executed entirely within the database, so we collect
#' the measurements
#'
#' @param codeset Lazy db codeset tbl to check
#' @param cohort cohort_post_attrition created by driver.R that contains
#' dx_graves_date, thyroidectomy_date, and drug_exposure_date
#' @param domain_tbl Events table to use (may have local modifications)
#' @param domain OMOP domain of the events table
#' @param min_days Event can be this many days before the dx code
#' @param max_days Event can be this many days after the dx code
#' @param fix_symbols Boolean whether to parse measurement source values for
#' inequalities (e.g. <0.02)
#'
#' @return Locally collected tbl with one row per-person
#' @export
#'
#' @examples
find_closest_2_dates <-
  function(codeset,
           cohort,
           domain_tbl,
           domain,
           order = "closest_negative",
           min_days = -20L,
           max_days = 7L,
           fix_symbols = FALSE) {
    domain_id <- paste0((domain %>%
                           str_split_i("_", 1L)), "_id")

    first_date <-
      find_closest(codeset,
        cohort,
        domain_tbl,
        domain,
        order = order,
        min_days = min_days,
        max_days = max_days,
        fix_symbols = fix_symbols
      ) %>%
      select(-any_of("dx_hyperthyroid_date")) %>%
      mutate(dx_source = "dx_hyperthyroid_date")

    missing_match_cohort <- cohort %>%
      anti_join(first_date, by = "person_id", copy = TRUE) %>%
      select(person_id, dx_graves_date)

    second_date <-
      find_closest(codeset,
        missing_match_cohort,
        domain_tbl,
        domain,
        date_field = "dx_graves_date",
        order = order,
        min_days = min_days,
        max_days = max_days,
        fix_symbols = fix_symbols
      ) %>%
      select(-any_of("dx_graves_date")) %>%
      mutate(dx_source = "dx_graves_date")

    missing_match_cohort2 <- missing_match_cohort %>%
      anti_join(second_date, by = "person_id", copy = TRUE) %>%
      select(-any_of(c("dx_hyperthyroid_date", "dx_graves_date"))) %>%
      mutate(dx_source = NA) %>%
      collect()

    combined <-
      dplyr::bind_rows(first_date, second_date, missing_match_cohort2)

    assertthat::assert_that((combined %>%
                               distinct_ct()) == (cohort %>% distinct_ct()))
    combined
  }


#' Compute z-score for systolic or diastolic column
#'
#' Using the CDC growth charts for infants and children ages 0 to 2 and children
#' ages 2 years and older and the formula in Appendix B: "Computation of Blood
#' Pressure Percentiles for Arbitrary Sex, Age, and Height" of "THE FOURTH
#' REPORT ON THE Diagnosis, Evaluation, and Treatment of High Blood Pressure in
#' Children and Adolescents"
#' https://www.nhlbi.nih.gov/sites/default/files/media/docs/hbp_ped.pdf
#'
#' Also as described on the Merck Manual calculator
#' https://www.merckmanuals.com/professional/pages-with-widgets/clinical-calculators
#'
#' @param df local tbl with one row per person and columns for at least
#' gender_concept_name, condition_start_age_in_months, and systolic and/or
#' diastolic
#' @param measurement either "systolic" or "diastolic"
#'
#' @return local tbl with one row per person and the resulting z_score
#' @export
#'
#' @examples
compute_bp_z_score <- function(df, measurement_tbl, measurement) {
  # Dataframe of coefficients
  df_coef <- data.frame(
    gender = c("MALE", "MALE", "FEMALE", "FEMALE"),
    measurement = c("systolic", "diastolic", "systolic", "diastolic"),
    avg = c(102.19768, 61.01217, 102.01027, 60.50510),
    age_factor_1 = c(1.82416, 0.68314, 1.94397, 1.01301),
    age_factor_2 = c(0.12776, 0.09835, 0.00598, 0.01157),
    age_factor_3 = c(0.00249, 0.01711, 0.00789, 0.00424),
    age_factor_4 = c(0.00135, 0.00045, 0.00059, 0.00137),
    height_factor_1 = c(2.73157, 1.46993, 2.03526, 1.16641),
    height_factor_2 = c(0.19618, 0.03144, 0.02534, 0.12795),
    height_factor_3 = c(0.04659, 0.03144, 0.01884, 0.03869),
    height_factor_4 = c(0.00947, 0.00967, 0.00121, 0.12795),
    measurement_z = c(10.7128, 11.6032, 10.4855, 10.9573),
    stringsAsFactors = TRUE
  )

  # find_closest expects remote db table
  df_remote <-
    copy_to_new(
      dest = config("db_src"),
      df = df,
      overwrite = TRUE,
      temporary = TRUE
    )
  # Find closest height measurement to the blood pressure reading
  height <- find_closest(
    results_tbl("mx_height"),
    df_remote,
    results_tbl("measurement_anthro_cleaned"),
    "measurement",
    date_field = "bp_measurement_date",
    order = "closest",
    min_days = -90L,
    max_days = 90L
  ) %>%
    collect()

  df <- df %>%
    left_join((height %>% select(person_id, height = value_as_number)),
      by = "person_id"
    )

  person_z_score <- df %>%
    # Find age in months to the nearest 0.5
    mutate(
      rounded = plyr::round_any(condition_start_age_in_months, 0.5),
      age_pt5 = if_else(rounded %% 1L == 0L, rounded + 0.5, rounded)
    ) %>%
    # Get age and gender specific parameters
    left_join(
      results_tbl("bp_percentiles"),
      by = join_by(
        "gender_concept_name" == "gender",
        "age_pt5" == "age_months"
      ),
      copy = TRUE
    ) %>%
    # If height is missing, assume average height for age/sex
    mutate(height_z = if_else(is.na(height), 0L, ((height / median_m)^power_l - 1L) /
                                (power_l * variation_s))) %>%
    mutate(age_factor = (condition_start_age_in_months / 12.0) - 10.0) %>%
    # Get gender and measurement specific coefficients
    left_join(df_coef %>% filter(measurement == {{ measurement }}),
      by = join_by("gender_concept_name" == "gender")
    ) %>%
    mutate(
      bp_avg = avg +
        (age_factor_1 * age_factor) +
        (age_factor_2 * age_factor^2L) +
        (age_factor_3 * age_factor^3L) -
        (age_factor_4 * age_factor^4L) +
        (height_factor_1 * height_z) -
        (height_factor_2 * height_z^2L) -
        (height_factor_3 * height_z^3L) +
        (height_factor_4 * height_z^4L)
    ) %>%
    mutate(bp_z = (!!sym(measurement) - bp_avg) / measurement_z) %>%
    select(person_id, bp_z)

  person_z_score
}

summarize_redact_and_round <- function(df,
                                       categorical,
                                       continuous,
                                       threshold,
                                       round_to,
                                       grouper = NULL) {
  if (!is.null(grouper)) {
    assertthat::assert_that(grouper %in% colnames(df),
                            msg = paste0(
                              {{grouper}},
                              " column must be in dataframe provided"))
  }

  df <- df %>% mutate(across(all_of(categorical), as.factor))
  result_df <- data.frame()

  for (column in colnames(df)) {
    vector <- df %>%
      select(any_of(c(column, grouper)))
    if (column %in% categorical) {
      row <- vector %>%
        group_by(across(any_of(c(column, grouper)))) %>%
        summarize(n = n()) %>%
        group_by(across(any_of(c(grouper)))) %>%
        mutate(total = sum(n, na.rm = TRUE)) %>%
        ungroup() %>%
        redact_and_round(
          columns = c("n", "total"),
          groupers = grouper,
          threshold = threshold,
          round_to = round_to
        ) %>%
        mutate(
          percentage = 100L * n / total
        ) %>%
        mutate(
          label = if_else(is.na(n),
            "[REDACTED]",
            paste0(n, " (", round(percentage, 1L), "%)")
          ),
          category = {{ column }}
        ) %>%
        rename(group = {{ column }}) %>%
        mutate(group = if_else(is.na(group), "missing", as.character(group)))

      if (is.null(grouper)) {
        row <- row %>%
          mutate(header = paste0(
            "(N=",
            coalesce(
              as.character(total),
              "[REDACTED]"
            ), ")"
          ))
      } else {
        row <- row %>%
          mutate(header = paste0(
            as.character(!!sym(grouper)),
            " (N=", coalesce(
              as.character(total),
              "[REDACTED]"
            ), ")"
          ))
      }

      result_df <- bind_rows(result_df, row)
    } else if (column %in% continuous) {
      counts <- vector %>%
        group_by(across(any_of(c(grouper)))) %>%
        summarize(
          nonmissing = sum(!is.na(!!sym(column))),
          missing = sum(is.na(!!sym(column))),
          total = n()
        ) %>%
        pivot_longer(
          cols = c("nonmissing", "missing"),
          names_to = "group",
          values_to = "n"
        )

      # Follow normal redaction policies for reporting the counts
      display_counts <- counts %>%
        redact_and_round(
          columns = c("n", "total"),
          groupers = grouper,
          threshold = threshold,
          round_to = round_to
        ) %>%
        mutate(percentage = 100L * n / total) %>%
        mutate(
          label = if_else(is.na(n),
            "[REDACTED]",
            paste0(n, " (", round(percentage, 1L), "%)")
          ),
          category = {{ column }}
        )

      # We can show the mean(sd) if missing is redacted but nonmissing is not
      # So now look at each individual row, rather than by group
      mean_sd_counts <- counts %>%
        redact_and_round(
          columns = "n",
          threshold = threshold,
          round_to = round_to
        ) %>%
        group_by(across(any_of(c(grouper)))) %>%
        mutate(n = if_else(group == "missing" &
                             # any(is.na(n[group == "nonmissing"])), NA, n)) %>%
                             anyNA(n[group == "nonmissing"]), NA, n)) %>%
        ungroup() %>%
        # We need something to join on for when grouper is NULL
        mutate(join_dummy = 1L)

      # Compute the mean (sd) and redact any rows that should be redacted
      mean_sd <- vector %>%
        group_by(across(any_of(c(grouper)))) %>%
        summarize(
          mean = mean(!!sym(column), na.rm = TRUE),
          sd = sd(!!sym(column), na.rm = TRUE)
        ) %>%
        mutate(join_dummy = 1L) %>%
        left_join(mean_sd_counts %>% filter(group == "nonmissing"),
          by = c(grouper, "join_dummy")
        ) %>%
        mutate(
          mean = if_else(is.na(n), NA, mean),
          sd = if_else(is.na(n), NA, sd)
        ) %>%
        mutate(mean_sd = if_else(is.na(mean),
          "[REDACTED]",
          paste0(
            round(mean, 1L),
            " (",
            round(sd, 1L), ")"
          )
        )) %>%
        select(!c(n, total, join_dummy))

      row <- display_counts %>%
        left_join(mean_sd, by = c("group", grouper)) %>%
        mutate(label = if_else(!is.na(mean_sd), mean_sd, label)) %>%
        select(-mean_sd)

      if (is.null(grouper)) {
        row <- row %>%
          mutate(header = paste0(
            "(N=",
            coalesce(
              as.character(total),
              "[REDACTED]"
            ), ")"
          ))
      } else {
        row <- row %>%
          mutate(header = paste0(
            as.character(!!sym(grouper)),
            " (N=", coalesce(
              as.character(total),
              "[REDACTED]"
            ), ")"
          ))
      }
      result_df <- bind_rows(result_df, row)
    }
  }
  result_df
}
