# Vector of additional packages to load before executing the request
config_append('extra_packages', c())

#' Execute the request
#'
#' This function presumes the environment has been set up, and executes the
#' steps of the request.
#'
#' In addition to performing queries and analyses, the execution path in this
#' function should include periodic progress messages to the user, and logging
#' of intermediate totals and timing data through [append_sum()].
#'
#' @return The return value is dependent on the content of the request, but is
#'   typically a structure pointing to some or all of the retrieved data or
#'   analysis results.  The value is not used by the framework itself.
#' @md
.run  <- function() {

    setup_pkgs() # Load runtime packages as specified above

    message('Starting execution with framework version ',
          config('framework_version'))

  # Set up the step log with as many attrition columns as you need.
  # For example, this call sets up the log with a `persons` count that will be
  # required at each step.
  init_sum(cohort = 'Start', persons = 0)

  # By convention, accumulate execution results in a list rather than as
  # independent variables, in order to make returning the entire set easier
  rslt <- list()
  
  #############################################
  # Start with the entire PEDSnet cohort
  #############################################
  rslt$person <- cdm_tbl("person")
  init_sum(
    cohort = "Persons in PEDSnet",
    persons = rslt$person %>% distinct_ct(),
    graves = NA,
    hyperthyroid_tsi = NA,
  )

  message("Find all patients with recorded Graves dx")
  
  sensitive_icd_codeset <- vocabulary_tbl("concept") %>%
    filter((str_detect(concept_code, "^E05") & vocabulary_id == "ICD10CM") |
             (str_detect(concept_code, "^242") & vocabulary_id == "ICD9CM"))
  
  from_snomed <- cdm_tbl("condition_occurrence") %>%
    semi_join(results_tbl("dx_graves"),
              by = join_by(condition_concept_id == concept_id)) %>%
    mutate(source = "snomed")
  
  from_icd <- cdm_tbl("condition_occurrence") %>%
    semi_join(sensitive_icd_codeset,
              by = join_by(condition_source_concept_id == concept_id)
    ) %>%
    mutate(source = "icd")
  
  all_codes <- from_snomed %>%
    union(from_icd) %>%
    left_join(
      vocabulary_tbl("concept") %>%
        select(concept_id, concept_code, vocabulary_id),
      by = join_by(condition_source_concept_id == concept_id)
    ) %>%
    group_by(condition_occurrence_id) %>%
    mutate(
      source = if_else(n() > 1, "multiple", source)
    ) %>%
    distinct() %>%
    ungroup()
  
  all_codes %>%
    # Exclude family history
    filter(!str_detect(tolower(condition_source_value),
                       "(mother|mom|maternal history)")) %>%
    output_tbl("graves_snomed_hyperthyroidism_icd")
  
  
  rslt$personal_tbls <- personal_tables() %>% filter(output_name %in% c(
    "condition_occurrence",
    "drug_exposure",
    "drug_era",
    "measurement_vitals",
    "measurement_anthro",
    "measurement_labs",
    "observation",
    "visit_occurrence",
    "person",
    "procedure_occurrence",
    "death"
  ))
  
  rslt$impersonal_tbls <- impersonal_tables() %>% filter(output_name %in% c(
    "care_site",
    "provider",
    "visit_payer",
    "location"
  ))
  
  
  message("Build mini-cdm")
  rslt$mini_cdm <- build_mini_cdm(
    cohort = results_tbl("graves_snomed_hyperthyroidism_icd") %>%
      distinct(person_id),
    clean = FALSE,
    personal = rslt$personal_tbls,
    impersonal = rslt$impersonal_tbls,
    materialize = TRUE,
    # Do not prefix tables with 'cdm_'
    .fix_names = function(x) x
  )
  
  # Manually create a joined measurement table for when we want to query for
  # any of bmi, wbc, labs etc
  results_tbl("measurement_anthro") %>%
    union(results_tbl("measurement_labs")) %>%
    union(results_tbl("measurement_vitals")) %>%
    output_tbl(
      name = "measurement",
      indexes = list(
        "person_id",
        "visit_occurrence_id",
        "measurement_concept_id"
      )
    )
  
  # Create a table of anthro z-scores with biologically implausible removed
  results_tbl("measurement_anthro") %>%
    filter(measurement_concept_id %in% c(2000000041, 2000000042, 2000000043)) %>%
    filter(abs(value_as_number) < 7) %>%
    output_tbl(name = "anthro_z_scores_lt7")
  
  # Attrition criteria
  ##################################################################
  # Criteria 1: Graves diagnosis OR hyperthyroid and positive TSI
  ##################################################################
  
  # Clean and save the tsi labs because we will use them twice
  measurement_tbl <- make_manual_fixes(results_tbl("measurement"))
  tsi <- measurement_tbl %>%
    semi_join(results_tbl("mx_tsi"),
              by = join_by(measurement_concept_id == concept_id)) %>%
    filter(!is.na(value_as_number)) %>%
    collect()
  
  tsi_source_values <- measurement_tbl %>%
    filter(
      measurement_concept_id %in% c(0L, 44814650L),
      measurement_source_value %in% c(
        "THYROID STIMULATING IMMUNOGLOBULIN|",
        "THYROID STIMULATING IG, SERUM | Thyroid Stim Immunoglobulin | Thyroid Stim Immunoglobulin",
        "THYROID STIMULATING IMMUNOGLOBULIN | Thyroid Stim Immunoglobulin | Thyroid Stim Immunoglobulin",
        "TSI|769"
      )
    ) %>%
    filter(!is.na(value_as_number)) %>%
    # So we do not have to search through source values later, and just use the
    # codeset
    mutate(measurement_concept_id = 4020112) %>%
    collect()
  
  tsi <- tsi %>% union(tsi_source_values)
  
  tsi_fixed <- fix_inequality_symbols(tsi, reparse = TRUE)
  tsi_fixed <- fix_inequality_symbols(tsi_fixed,
                                      column = "range_low",
                                      prefix = "range_low",
                                      reparse = TRUE)
  tsi_fixed <- fix_inequality_symbols(tsi_fixed,
                                      column = "range_high",
                                      prefix = "range_high",
                                      reparse = TRUE)
  
  # Map '% baseline', '%Baselin', '% baseli" to 8688 (% baseline)
  # Map 8554 (%) to 8688 (% baseline)
  # Map 'IU/L', '[IU]/L, 'international unit per liter' to 8923
  # Map 'international unit per milliliter' to 8985 b/c source value
  # Map TSI index to 8529
  # 8587 unclear whether there has been a transformation, set to NA
  tsi_cleaned <- tsi_fixed %>%
    mutate(
      unit_concept_id =
        case_when(
          unit_concept_id == 8587L & unit_source_value == "uIU/ml" ~ NA,
          unit_concept_id == 8524L & unit_source_value == "." ~ NA,
          # % to % baseline
          unit_concept_id == 8554L ~ 8688L,
          # IU/L
          # IU/mL 8985 have source IU/L, correct range_low/range_high, and
          # values in expected range
          (
            is.na(unit_concept_id) |
              unit_concept_id %in% c(0L, 44814650L, 8985L, 8645L)
          ) & (
            tolower(unit_source_value) %in% c(
              "u/l",
              "iu/l",
              "[iu]/l",
              "international unit per liter",
              "intl units/l",
              "inti units/l",
              "inti units/ l",
              "intlunit/l",
              "intlunits/l",
              "units/l"
            ) |
              range_high == 0.55
          ) ~ 8923L,
          # % baseline to % baseline
          # range_low or range_high of 139 or 140 enough to assign % baseline
          (is.na(unit_concept_id) |
             unit_concept_id %in% c(0L, 44814650L)) &
            (
              tolower(unit_source_value) %in% c(
                "% baseline",
                "% basline",
                "% baseli",
                "%baseline",
                "%baselin",
                "% of baseline",
                "% of baselent",
                '%=""',
                "& baseline",
                "<140 % baseline"
              ) |
                range_high %in% c(139L, 140L) |
                range_low %in% c(140L)
            ) ~ 8688L,
          (
            is.na(unit_concept_id) |
              unit_concept_id %in% c(0L, 44814650L, 8779L)
          ) &
            ((
              tolower(unit_source_value) %in% c("tsi index", "index")
            ) |
              range_high == 1.3 |
              range_low == 1.3) ~ 8529L,
          (is.na(unit_concept_id) |
             unit_concept_id %in% c(0L, 44814650L)) &
            value_as_number > 70 ~ 8688L,
          .default = unit_concept_id
        )
    )
  
  # Less definitive categorization
  # The most common source value at one site is "TSI index"; there are many
  # values with NI and the range 1 to 10
  tsi_cleaned <- tsi_cleaned %>%
    mutate(
      unit_concept_id = if_else(
        unit_concept_id == 44814650L &
          value_as_number >= 1 & value_as_number <= 15,
        8529L,
        unit_concept_id
      )
    )
    
  
  # Check for outliers/biological feasibility
  tsi_cleaned %>%
    group_by(unit_concept_id) %>%
    summarize(
      n = n(),
      mean = mean(value_as_number, na.rm = TRUE),
      median = median(value_as_number, na.rm = TRUE),
      min = min(value_as_number, na.rm = TRUE),
      max = max(value_as_number, na.rm = TRUE)
    )
  
  output_tbl(tsi_cleaned, name = "all_tsi_cleaned_manually")
  
  
  # Find people who have Hyperthyroid dx
  icd_cohort <- results_tbl("graves_snomed_hyperthyroidism_icd") %>%
    filter(source %in% c("icd"))
  
  snomed_cohort <- results_tbl("graves_snomed_hyperthyroidism_icd") %>%
    filter(source %in% c("snomed", "multiple"))

  # A person may have multiple hyperthyroid diagnoses, but we want to find if
  # any of them have an elevated tsi
  tsi_elevated <- find_closest(
    results_tbl("mx_tsi"),
    icd_cohort %>% distinct(person_id, condition_start_date),
    results_tbl("all_tsi_cleaned_manually"),
    "measurement",
    date_field = "condition_start_date",
    order = "closest_negative",
    min_days = -20L,
    max_days = 90L
  ) %>%
    mutate(elevated = case_when(!is.na(range_high) & (value_as_number >= range_high) ~ TRUE,
                                !is.na(range_low) & is.na(range_high) & (value_as_number > range_low) ~ TRUE,
                                (is.na(range_high) & is.na(range_low) & (unit_concept_id == 8529) & (value_as_number >= 1.3)) ~ TRUE,
                                (is.na(range_high) & is.na(range_low) & (unit_concept_id == 8923L) & (value_as_number >= 0.55)) ~ TRUE,
                                (is.na(range_high) & is.na(range_low) & (unit_concept_id == 8688) & (value_as_number >= 140)) ~ TRUE,
                                .default = FALSE)) %>%
    filter(elevated == TRUE)
  
  icd_cohort_elevated <- icd_cohort %>%
    inner_join(
      tsi_elevated %>% select(
        person_id,
        condition_start_date,
        elevated_tsi_value = value_as_number,
        elevated_tsi_unit_concept_id = unit_concept_id
      ),
      by = join_by(person_id, condition_start_date),
      copy = TRUE
    )
  
  rslt$any_graves <- snomed_cohort %>%
    mutate(inclusion = "graves") %>%
    union(icd_cohort_elevated %>% mutate(inclusion = "hyperthyroid_tsi")) %>%
    # Check source values for family history
    # (family history excluded from concept set)
    filter(!str_detect(tolower(condition_source_value), "(mom|mother)")) %>%
    group_by(person_id) %>%
    # There could be multiple condition diagnoses on the same day
    # Choose one that has a visit_occurrence_id (not problem list)
    # With a preference for graves
    slice_min(
      order_by = tibble(condition_start_date, is.na(visit_occurrence_id), inclusion != "graves"),
      n = 1L,
      with_ties = FALSE
    ) %>%
    ungroup() %>%
    # Exclude if the earliest dx is Neonatal Graves disease
    mutate(dx_graves_date = condition_start_date, dx_graves_id = condition_occurrence_id) %>%
    select(
      site,
      person_id,
      dx_graves_date,
      dx_graves_id,
      inclusion,
      elevated_tsi_value,
      elevated_tsi_unit_concept_id
    )
  
  append_sum(
    cohort = "Any Graves",
    persons = rslt$any_graves %>% distinct_ct(),
    graves = rslt$any_graves %>% filter(inclusion == "graves") %>% distinct_ct(),
    hyperthyroid_tsi = rslt$any_graves %>% filter(inclusion == "hyperthyroid_tsi") %>% distinct_ct()
  )

  # Now in this cohort, find the earliest graves or hyperthyroidism dx
  rslt$earliest_graves <- rslt$any_graves %>%
    left_join(results_tbl("graves_snomed_hyperthyroidism_icd") %>% select(-site), by = "person_id") %>%
    group_by(person_id) %>%
    # There could be multiple condition diagnoses on the same day
    slice_min(
      order_by = tibble(condition_start_date, is.na(visit_occurrence_id)),
      n = 1L,
      with_ties = FALSE
    ) %>%
    ungroup() %>%
    mutate(dx_hyperthyroid_date = condition_start_date,
           dx_hyperthyroid_id = condition_occurrence_id) %>%
    select(
      site,
      person_id,
      dx_graves_date,
      dx_graves_id,
      inclusion,
      elevated_tsi_value,
      elevated_tsi_unit_concept_id,
      dx_hyperthyroid_date,
      dx_hyperthyroid_id,
      visit_occurrence_id,
      condition_start_age_in_months
    )
  
  rslt$earliest_graves %>%
    mutate(diff = as.integer(dx_graves_date - dx_hyperthyroid_date)) %>%
    filter(diff <= 90) %>%
    select(-diff) %>%
    output_tbl("cohort_any_graves", indexes = list("person_id"))
  
  append_sum(
    cohort = "<= 90 days between earliest hyperthyroid dx and graves dx",
    persons = results_tbl("cohort_any_graves") %>% distinct_ct(),
    graves = results_tbl("cohort_any_graves") %>% filter(inclusion == "graves") %>% distinct_ct(),
    hyperthyroid_tsi = results_tbl("cohort_any_graves") %>% filter(inclusion == "hyperthyroid_tsi") %>% distinct_ct()
  )

  # Attrition criteria
  ##########################################################
  # Criteria 1: First dx between 2013-01-01 and 2024-12-31
  ##########################################################
  rslt$date_range <- results_tbl("cohort_any_graves") %>%
    filter(dx_hyperthyroid_date >= "2013-01-01") %>%
    filter(dx_hyperthyroid_date <= "2024-12-31")
  
  append_sum(
    cohort = "First recorded diagnosis between Jan 1, 2013 and Dec 31, 2024",
    persons = distinct_ct(rslt$date_range),
    graves = rslt$date_range %>% filter(inclusion == "graves") %>% distinct_ct(),
    hyperthyroid_tsi = rslt$date_range %>% filter(inclusion == "hyperthyroid_tsi") %>% distinct_ct()
  )

  ##########################################################
  # Criteria 2: Age <= 18 at first dx
  ##########################################################
  rslt$eighteen_under <- rslt$date_range %>%
    filter(condition_start_age_in_months <= 216L)

  append_sum(
    cohort = "Aged <= 18 at first recorded dx",
    persons = distinct_ct(rslt$eighteen_under),
    graves = rslt$eighteen_under %>% filter(inclusion == "graves") %>% distinct_ct(),
    hyperthyroid_tsi = rslt$eighteen_under %>% filter(inclusion == "hyperthyroid_tsi") %>% distinct_ct()
  )

  ##########################################################
  # Criteria 3: Age >= 1 at first dx
  ##########################################################
  rslt$one_over <- rslt$eighteen_under %>%
    filter(condition_start_age_in_months >= 12L)

  append_sum(
    cohort = "Aged >=1 at first recorded dx",
    persons = distinct_ct(rslt$one_over),
    graves = rslt$one_over %>% filter(inclusion == "graves") %>% distinct_ct(),
    hyperthyroid_tsi = rslt$one_over %>% filter(inclusion == "hyperthyroid_tsi") %>% distinct_ct()
  )

  ##############################################################
  # Criteria 4: No definitive treatment prior to first Graves dx
  ##############################################################

  thyroid_procedures <- results_tbl("procedure_occurrence") %>%
    inner_join(
      results_tbl("tx_total_thyroidectomy"),
      by = join_by("procedure_concept_id" == "concept_id")
    ) %>%
    select(person_id, thyroidectomy_date = procedure_date) %>%
    mutate(source = "procedure")
  
  thyroid_procedures_source <- results_tbl("procedure_occurrence") %>%
    filter(procedure_concept_id == 0, str_detect(procedure_source_value, "60240")) %>%
    select(person_id, thyroidectomy_date = procedure_date) %>%
    mutate(source = "procedure")
  
  thyroid_conditions <- results_tbl("condition_occurrence") %>%
    filter(str_detect(tolower(condition_source_value), "thyroidectomy"),
           str_detect(tolower(condition_source_value), "(total|complete)")) %>%
    select(person_id, thyroidectomy_date = condition_start_date) %>%
    mutate(source = "condition")

  thyroidectomies <- thyroid_procedures %>%
    union(thyroid_procedures_source) %>%
    union(thyroid_conditions) %>%
    group_by(person_id) %>%
    slice_min(
      order_by = thyroidectomy_date,
      n = 1L,
      with_ties = FALSE
    ) %>%
    ungroup()
  
  rslt$no_thyroidectomy <- rslt$one_over %>%
    left_join(thyroidectomies, by = "person_id") %>%
    filter(
      is.na(thyroidectomy_date) |
        (thyroidectomy_date - dx_hyperthyroid_date) > 0L
    )
  
  # Ensure there is one row per patient
  assertthat::are_equal(
    pull(count(rslt$no_thyroidectomy)),
    distinct_ct(rslt$no_thyroidectomy)
  )
  
  # A9509 = diagnostic 2615330
  # A9516 = diagnostic 2615337
  # A9528 = diagnostic 2615349
  # A9529 = diagnostic 2615350
  # A9517 = theraputic 35854142 --> 2615338
  # A9530 = theraputic 19069873 --> 2615351
  # 79005 = theraputic 36713073 --> 2212066
  # CW7GGZZ = theraputic 2791016
  
  rai_codes <- c(35854142, 2615338, 19069873, 2615351, 36713073, 2212066, 2791016)
  # In this cohort, those with a drug exposure also have a procedure order, so
  # we skip the drug exposures
  
  all_rai <-
    results_tbl("procedure_occurrence") %>%
    filter(procedure_concept_id %in% rai_codes |
             procedure_source_concept_id %in% rai_codes) %>%
    select(person_id, rai_date = procedure_date) %>%
    distinct()
  
  # NOTE: ablation could have been for cancer, but we err on the side of
  # excluding
  # Includes historical/post ablation, thyroidectomy or generic procedural
  # hypothyroidism
  hx_definitive <- results_tbl("condition_occurrence") %>%
    filter(
      condition_concept_id %in% c(132583L, 137820L, 4231548L) |
        condition_source_concept_id == 35207108L |
        str_detect(
          tolower(condition_source_value),
          "(E89\\.0|244\\.1|244\\.0)"
        )
    ) %>%
    filter(!str_detect(tolower(condition_source_value), "partial")) %>%
    select(
      person_id,
      hx_definitive_visit_occurrence_id = visit_occurrence_id,
      hx_definitive_date = condition_start_date
    ) %>%
    group_by(person_id) %>%
    slice_min(order_by = hx_definitive_date,
              with_ties = FALSE,
              n = 1) %>%
    ungroup()
  
  # If there are more than 3, then just computing the lag with the prior row
  # may not work
  assertthat::assert_that(
    all_rai %>%
      group_by(person_id) %>%
      summarize(n = n()) %>%
      ungroup() %>%
      summarize(m = max(n)) %>%
      pull() <= 3
  )
  
  all_rai_wide <- all_rai %>%
    group_by(person_id) %>%
    arrange(person_id, rai_date) %>%
    mutate(lag = rai_date - lag(rai_date)) %>%
    filter(is.na(lag) | lag >= 180L) %>%
    mutate(n = row_number()) %>%
    ungroup() %>%
    collect() %>%
    select(-lag) %>%
    pivot_wider(names_from = n,
                names_prefix = "rai_date_",
                values_from = rai_date)
  
  all_rai_wide_hx <- all_rai_wide %>%
    full_join(hx_definitive, by = "person_id", copy = TRUE)

  rslt$no_rai <- rslt$no_thyroidectomy %>%
    left_join(all_rai_wide_hx, by = "person_id", copy = TRUE) %>%
    filter((is.na(rai_date_1) |
              (rai_date_1 - dx_hyperthyroid_date) >= 0L),
           # Exclude historical records on the same day or recorded in the same
           # visit
           (is.na(hx_definitive_date) |
              (hx_definitive_date - dx_hyperthyroid_date) > 0) |
             visit_occurrence_id == hx_definitive_visit_occurrence_id)
  
  # Ensure there is one row per patient
  assertthat::are_equal(
    pull(count(rslt$no_rai)),
    distinct_ct(rslt$no_rai)
  )

  append_sum(
    cohort = "No previous definitive therapy",
    persons = distinct_ct(rslt$no_rai),
    graves = rslt$no_rai %>% filter(inclusion == "graves") %>% distinct_ct(),
    hyperthyroid_tsi = rslt$no_rai %>% filter(inclusion == "hyperthyroid_tsi") %>% distinct_ct()
  )

  ################################################
  # Criteria 5: Diagnosis has visit occurrence id
  ################################################
  rslt$has_visit <- rslt$no_rai %>%
    filter(!is.na(visit_occurrence_id))

  append_sum(
    cohort = "Has visit associated with diagnosis",
    persons = distinct_ct(rslt$has_visit),
    graves = rslt$has_visit %>% filter(inclusion == "graves") %>% distinct_ct(),
    hyperthyroid_tsi = rslt$has_visit %>% filter(inclusion == "hyperthyroid_tsi") %>% distinct_ct()
  )

  ############################################################################
  # Criteria 6: At least 2 outpatient visits
  ############################################################################
  # Number of outpatient on distinct days
  visit_count <- rslt$has_visit %>%
    inner_join(results_tbl("visit_occurrence"), by = "person_id") %>%
    filter(visit_concept_id == 9202L) %>%
    filter(
      visit_start_date < dx_hyperthyroid_date
    ) %>%
    group_by(person_id) %>%
    summarize(visit_count = n_distinct(visit_start_date),
              prev_visit = max(visit_start_date))

  rslt$has_2_op_visits <- rslt$has_visit %>%
    left_join(visit_count, by = "person_id") %>%
    filter(visit_count >= 2L)

  # Ensure there is one row per patient
  assertthat::are_equal(
    pull(count(rslt$has_2_op_visits)),
    distinct_ct(rslt$has_2_op_visits)
  )

  append_sum(
    cohort = "At least two outpatient visits on distinct days prior to first diagnosis",
    persons = distinct_ct(rslt$has_2_op_visits),
    graves = rslt$has_2_op_visits %>% filter(inclusion == "graves") %>% distinct_ct(),
    hyperthyroid_tsi = rslt$has_2_op_visits %>% filter(inclusion == "hyperthyroid_tsi") %>% distinct_ct()
  )

  #############################################################
  # Criteria 7: >= 1 BMI, BP, or HR within -20/+7 days of visit
  #############################################################
  # NOTE: written as a filter, because dbplyr does not allow
  # (dx_hyperthyroid_date - measurement_date) <= n as a join condition
  # https://dplyr.tidyverse.org/reference/join_by.html

  all_codes <-
    union_codesets(c("mx_bmi", "mx_diastolic", "mx_systolic", "mx_heart_rate"))

  measurements_all <- results_tbl("measurement") %>%
    inner_join(all_codes,
      by = join_by("measurement_concept_id" == "concept_id")
    )

  measurement_counts <-
    rslt$has_2_op_visits %>%
    inner_join(
      measurements_all %>%
        select(
          person_id,
          measurement_concept_id,
          measurement_date
        ),
      by = "person_id"
    ) %>%
    # Since we are looking at vitals, rather than labs, we do not need to
    # check the order or result date
    mutate(diff = dx_hyperthyroid_date - measurement_date) %>%
    filter(diff >= -20L, diff <= 7L) %>%
    count(person_id, name = "measurement_count")

  rslt$has_measurement <- rslt$has_2_op_visits %>%
    left_join(measurement_counts, by = "person_id") %>%
    filter(measurement_count > 0L)

  assertthat::are_equal(
    pull(count(rslt$has_measurement)),
    distinct_ct(rslt$has_measurement)
  )

  append_sum(
    cohort = "Has a vital measurement within -20 to +7 days of first diagnosis",
    persons = distinct_ct(rslt$has_measurement),
    graves = rslt$has_measurement %>% filter(inclusion == "graves") %>% distinct_ct(),
    hyperthyroid_tsi = rslt$has_measurement %>% filter(inclusion == "hyperthyroid_tsi") %>% distinct_ct()
  )

  #############################################################
  # Criteria 8: No anti-thyroid medication 30 days before dx
  #############################################################
  at_drugs <- union_codesets(c("rx_methimazole", "rx_propylthiouracil", "rx_carbimazole"))

  earliest_anti_thyroid <- results_tbl("drug_exposure") %>%
    inner_join(at_drugs,
      by = join_by("drug_concept_id" == "concept_id")
    ) %>%
    group_by(person_id) %>%
    slice_min(
      order_by = drug_exposure_start_date,
      n = 1L,
      with_ties = FALSE
    ) %>%
    ungroup() %>%
    select(
      person_id,
      earliest_at_rx_id = drug_concept_id,
      earliest_at_rx_route_source_value = route_source_value,
      earliest_at_rx_router_concept_id = route_concept_id,
      earliest_at_drug_exposure_start_date = drug_exposure_start_date
    )

  rslt$no_prev_at_rx <- rslt$has_measurement %>%
    left_join(earliest_anti_thyroid, by = "person_id") %>%
    filter(
      is.na(earliest_at_drug_exposure_start_date) |
        (earliest_at_drug_exposure_start_date - dx_hyperthyroid_date) > -30L
    )

  # Ensure there is one row per patient
  assertthat::are_equal(
    pull(count(rslt$no_prev_at_rx)),
    distinct_ct(rslt$no_prev_at_rx)
  )

  append_sum(
    cohort = "No anti-thyroid medication earlier than 30 days ago",
    persons = distinct_ct(rslt$no_prev_at_rx),
    graves = rslt$no_prev_at_rx %>% filter(inclusion == "graves") %>% distinct_ct(),
    hyperthyroid_tsi = rslt$no_prev_at_rx %>% filter(inclusion == "hyperthyroid_tsi") %>% distinct_ct()
  )
  
  #########################################################
  # Criteria 9: First Graves dx does not include remission
  #########################################################
  has_remission <- rslt$no_prev_at_rx %>%
    inner_join(results_tbl("condition_occurrence") %>%
                filter(condition_concept_id == 37108817),
              by = join_by(person_id, dx_graves_date == condition_start_date)) %>%
    distinct(person_id)
  
  rslt$no_remission <- rslt$no_prev_at_rx %>%
    anti_join(has_remission)
  
  append_sum(
    cohort = "First recorded diagnosis does not include remission",
    persons = distinct_ct(rslt$no_remission),
    graves = rslt$no_remission %>% filter(inclusion == "graves") %>% distinct_ct(),
    hyperthyroid_tsi = rslt$no_remission %>% filter(inclusion == "hyperthyroid_tsi") %>% distinct_ct()
  )

  #############################################################
  # Criteria 10: No measurements before birth (unclear whether
  # pediatric record or mother's record- there was at least one
  # example of heights from two people in the record over time)
  #############################################################
  pre_birth_mx <- results_tbl("measurement") %>%
    filter(measurement_age_in_months < 0) %>%
    select(person_id) %>%
    distinct()

  rslt$no_pre_birth_mx <- rslt$no_remission %>%
    anti_join(pre_birth_mx, by = "person_id")

  append_sum(
    cohort = "No measurements before birth",
    persons = distinct_ct(rslt$no_pre_birth_mx),
    graves = rslt$no_pre_birth_mx %>% filter(inclusion == "graves") %>% distinct_ct(),
    hyperthyroid_tsi = rslt$no_pre_birth_mx %>% filter(inclusion == "hyperthyroid_tsi") %>% distinct_ct()
  )

  # Write step summary log to CSV and/or database,
  # as determined by configuration
  output_sum()
  
  # Since our cdm is pre-attrition, output the cohort after attrition
  results_tbl("person") %>%
    inner_join(rslt$no_pre_birth_mx %>% select(-site), by = "person_id") %>%
    output_tbl(name = "cohort_post_attrition2")
  
  #############################################################
  # Sensitivity Checks
  #############################################################
  # For sensitivity checks, include first and closest hypothyroid date
  # This will include hypothyroidism from partial thyroidectomy
  first_hypothyroid <- results_tbl("condition_occurrence") %>%
    semi_join(
      results_tbl("dx_hypothyroidism") %>%
        # Exclude congenital because it could be transient as a newborn
        filter(!str_detect(tolower(concept_name), "congenital")),
      by = join_by(condition_concept_id == concept_id)
    ) %>%
    select(person_id,
           condition_start_date,
           first_hypothyroid_concept_name = condition_concept_name) %>%
    inner_join(
      results_tbl("cohort_post_attrition") %>% select(person_id, dx_hyperthyroid_date),
      by = join_by(person_id)
    ) %>%
    group_by(person_id) %>%
    slice_min(order_by = condition_start_date,
              n = 1,
              with_ties = FALSE) %>%
    ungroup() %>%
    mutate(time_to_first_hypothyroid = as.integer(condition_start_date - dx_hyperthyroid_date)) %>%
    select(person_id,
           time_to_first_hypothyroid,
           first_hypothyroid_concept_name)
  
  closest_hypothyroid <- results_tbl("condition_occurrence") %>%
    semi_join(
      results_tbl("dx_hypothyroidism") %>%
        # Exclude congenital because it could be transient as a newborn
        filter(!str_detect(tolower(concept_name), "congenital")),
      by = join_by(condition_concept_id == concept_id)
    ) %>%
    select(person_id, condition_start_date, closest_hypothyroid_concept_name = condition_concept_name) %>%
    inner_join(results_tbl("cohort_post_attrition") %>% select(person_id, dx_hyperthyroid_date),
               by = join_by(person_id)) %>%
    mutate(time_to_closest_hypothyroid = as.integer(condition_start_date - dx_hyperthyroid_date)) %>%
    group_by(person_id) %>%
    slice_min(
      order_by = abs(time_to_closest_hypothyroid),
      n = 1,
      with_ties = FALSE
    ) %>%
    ungroup() %>%
    select(person_id,
           time_to_closest_hypothyroid,
           closest_hypothyroid_concept_name)
  
  # For sensitivity checks, include first and closest levothyroxine date
  first_levothyroxine <- results_tbl("drug_exposure") %>%
    semi_join(
      read_codeset("rx_levothyroxine", col_types = "icicccccDD"),
      by = join_by(drug_concept_id == concept_id),
      copy = TRUE
    ) %>%
    select(person_id, drug_exposure_start_date) %>%
    inner_join(
      results_tbl("cohort_post_attrition") %>% select(person_id, dx_hyperthyroid_date),
      by = join_by(person_id)
    ) %>%
    group_by(person_id) %>%
    slice_min(order_by = drug_exposure_start_date,
              n = 1,
              with_ties = FALSE) %>%
    ungroup() %>%
    mutate(time_to_first_levothyroxine = as.integer(drug_exposure_start_date - dx_hyperthyroid_date)) %>%
    select(
      person_id,
      time_to_first_levothyroxine
    )
  
  closest_levothyroxine <- results_tbl("drug_exposure") %>%
    semi_join(
      read_codeset("rx_levothyroxine", col_types = "icicccccDD"),
      by = join_by(drug_concept_id == concept_id),
      copy = TRUE
    ) %>%
    select(person_id, drug_exposure_start_date) %>%
    inner_join(
      results_tbl("cohort_post_attrition") %>% select(person_id, dx_hyperthyroid_date),
      by = join_by(person_id)
    ) %>%
    mutate(time_to_closest_levothyroxine = as.integer(drug_exposure_start_date - dx_hyperthyroid_date)) %>%
    group_by(person_id) %>%
    slice_min(
      order_by = abs(time_to_closest_levothyroxine),
      n = 1,
      with_ties = FALSE
    ) %>%
    ungroup() %>%
    select(person_id, time_to_closest_levothyroxine)
  
  first_hashimoto <- results_tbl("condition_occurrence") %>%
    filter(condition_concept_id %in% c(135215L, 4130018L, 4100629L)) %>%
    select(person_id, condition_start_date, hashimoto_concept_name = condition_concept_name) %>%
    inner_join(results_tbl("cohort_post_attrition") %>% select(person_id, dx_hyperthyroid_date),
               by = join_by(person_id)) %>%
    group_by(person_id) %>%
    slice_min(
      order_by = condition_start_date,
      n = 1,
      with_ties = FALSE
    ) %>%
    ungroup() %>%
    mutate(time_to_first_hashimoto = as.integer(condition_start_date - dx_hyperthyroid_date)) %>%
    select(person_id,
           time_to_first_hashimoto,
           hashimoto_concept_name)
  
  results_tbl("cohort_post_attrition") %>%
    left_join(first_hypothyroid, by = "person_id") %>%
    left_join(closest_hypothyroid, by = "person_id") %>%
    left_join(first_levothyroxine, by = "person_id") %>%
    left_join(closest_levothyroxine, by = "person_id") %>%
    left_join(first_hashimoto, by = "person_id") %>%
    output_tbl(name = "cohort_post_attrition2")

  message("Done.")

  invisible(rslt)

}
