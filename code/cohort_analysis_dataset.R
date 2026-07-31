# Create a 1-row per person dataset with just the variables of interest
create_analysis_cohort <- function() {
  cohort <- results_tbl("cohort_post_attrition2")
  cohort <- filter_no_treatment(cohort, treatment_window = 30L)
  measurement_tbl <- make_manual_fixes(results_tbl("measurement"))
  # Already included in the person table
  # 1. person_id
  # 2. site
  # 3. dx_hyperthyroid_date
  # 4. sex
  # 5. race
  # 6. ethnicity
  # 7. age at diagnosis (days)

  baseline_table <- cohort %>%
    mutate(presenting_visit_year = year(dx_hyperthyroid_date)) %>%
    select(
      person_id,
      site,
      presenting_visit_year,
      condition_start_age_in_months,
      gender_concept_name,
      race_concept_name,
      ethnicity_concept_name
    )

  # To compute
  # 8. payer
  all_visits <- (cohort %>%
    select(person_id, dx_hyperthyroid_date, site)) %>%
    inner_join((
      results_tbl("visit_occurrence") %>%
        select(person_id, visit_occurrence_id, visit_start_date)
    ), by = "person_id") %>%
    mutate(time_to_payer = visit_start_date - dx_hyperthyroid_date)

  payer_count <- all_visits %>%
    inner_join(results_tbl("visit_payer"), by = "visit_occurrence_id") %>%
    group_by(person_id) %>%
    slice_min(order_by = tibble(abs(time_to_payer)), with_ties = TRUE) %>%
    ungroup() %>%
    group_by(person_id) %>%
    summarize(
      plan_count = n_distinct(plan_class),
      plan = min(plan_class),
      time_to_payer = min(time_to_payer)
    )

  add_payer <- baseline_table %>%
    left_join(payer_count, by = "person_id") %>%
    mutate(plan = if_else(plan_count > 1L, "Multiple", plan)) %>%
    select(-plan_count, -time_to_payer)

  # 9. trisomy 21
  # 10. other autoimmune (T1DM, vitiligo, addison's, or celiac)
  # 11. vitamin d deficiency
  cohort_conditions <-
    find_conditions(
      cohort,
      c(
        "dx_t1dm",
        "dx_vitiligo",
        "dx_addisons",
        "dx_celiac",
        "dx_trisomy_21",
        "mx_dx_vitamin_d"
      ),
      min_days = -365L,
      max_days = 0L
    ) %>%
    mutate(
      dx_other_autoimmune =
        as.numeric(dx_t1dm | dx_vitiligo | dx_addisons | dx_celiac)
    ) %>%
    select(person_id,
           dx_trisomy_21,
           dx_other_autoimmune,
           dx_vitamin_d = mx_dx_vitamin_d)

  add_conditions <-
    add_payer %>% left_join(cohort_conditions, by = "person_id", copy = TRUE)

  # Measurements
  # NOTE: these have all been collected locally
  # 12. bmi z score
  bmi <-
    find_closest_2_dates(
      results_tbl("mx_bmi"),
      cohort,
      results_tbl("measurement_anthro_cleaned"),
      "measurement",
      order = "closest"
    ) %>%
    select(person_id, bmi_z_score = value_as_number)
  # 13. diastolic
  diastolic <-
    find_closest_2_dates(
      results_tbl("mx_diastolic"),
      cohort,
      measurement_tbl,
      "measurement"
    ) %>%
    left_join(
      cohort %>% select(
        person_id,
        gender_concept_name,
        condition_start_age_in_months
      ),
      by = "person_id",
      copy = TRUE
    ) %>%
    select(
      person_id,
      bp_measurement_date = measurement_date,
      gender_concept_name,
      condition_start_age_in_months,
      diastolic = value_as_number
    )
  diastolic_z_score <- compute_bp_z_score(
    diastolic,
    measurement_tbl,
    measurement = "diastolic"
  ) %>%
    select(person_id, diastolic_z_score = bp_z)
  # 14. systolic
  systolic <-
    find_closest_2_dates(
      results_tbl("mx_systolic"),
      cohort,
      measurement_tbl,
      "measurement"
    ) %>%
    left_join(
      cohort %>% select(
        person_id,
        gender_concept_name,
        condition_start_age_in_months
      ),
      by = "person_id",
      copy = TRUE
    ) %>%
    select(
      person_id,
      bp_measurement_date = measurement_date,
      gender_concept_name,
      condition_start_age_in_months,
      systolic = value_as_number
    )
  systolic_z_score <- compute_bp_z_score(systolic,
    measurement_tbl,
    measurement = "systolic"
  ) %>%
    rename(systolic_z_score = bp_z)
  # 15. heart rate
  heart_rate <- find_closest_2_dates(
    results_tbl("mx_heart_rate"),
    cohort,
    measurement_tbl,
    "measurement"
  ) %>%
    select(person_id, hr = value_as_number)
  # 16. tsh
  tsh <- find_closest_2_dates(
    results_tbl("mx_tsh"),
    cohort,
    measurement_tbl,
    "measurement",
    fix_symbols = TRUE
  ) %>%
    select(person_id,
      tsh = value_as_number,
      tsh_operator = operator_concept_name
    )
  # 17. tsi
  tsi <-
    find_closest_2_dates(
      results_tbl("mx_tsi"),
      cohort,
      results_tbl("all_tsi_cleaned_manually"),
      "measurement",
      max_days = 90L
    )

  # Split TSI into % baseline and IU/L
  # % baseline unit_concept_id c(8529, 8688)
  tsi_percent <- tsi %>%
    filter(unit_concept_id %in% c(8529L, 8688L)) %>%
    mutate(
      value_as_number = case_when(unit_concept_id == 8529 ~ value_as_number * 100, .default = value_as_number)
    ) %>%
    select(person_id,
           tsi_percent = value_as_number,
           tsi_percent_operator = operator_concept_name)


  # IU/L 8923
  tsi_iul <- tsi %>%
    filter(unit_concept_id == 8923L) %>%
    select(person_id, tsi_iul = value_as_number, tsi_iul_operator = operator_concept_name)

  # There should be no leftover tsi measurements
  leftover <- tsi %>%
    filter(!is.na(value_as_number)) %>%
    anti_join(tsi_percent, by = "person_id") %>%
    anti_join(tsi_iul, by = "person_id")

  stopifnot(assertthat::are_equal(pull(count(leftover)), 0L))

  # 18. free T4
  free_t4 <-
    find_closest_2_dates(
      results_tbl("mx_free_t4"),
      cohort,
      measurement_tbl,
      "measurement",
      fix_symbols = TRUE
    ) %>%
    select(person_id,
      ft4 = value_as_number,
      ft4_operator = operator_concept_name
    )
  # 19. total T3
  total_t3 <-
    find_closest_2_dates(results_tbl("mx_total_t3"),
                         cohort,
                         measurement_tbl,
                         "measurement",
                         fix_symbols = TRUE) %>%
    # Nanogram per milliliter to Nanogram per deciliter
    mutate(
      value_as_number =
        case_when(
          unit_concept_id == 8842 ~ 100L *  value_as_number,
          unit_concept_id == 0 &
            tolower(unit_source_value) == "ng/ml" ~ 100L * value_as_number,
          unit_concept_id == 8817 &
            range_high < 3 &
            value_as_number < 5 ~ 100 * value_as_number,
          .default = value_as_number
        )
    ) %>%
    select(person_id,
           tt3 = value_as_number,
           tt3_operator = operator_concept_name,
           tt3_diff = diff)
  # 20. anti-tpo
  anti_tpo_values <-
    find_closest_2_dates(
      results_tbl("mx_anti_tpo"),
      cohort,
      measurement_tbl,
      "measurement",
      max_days = 90L,
      fix_symbols = TRUE
    )

  # Just fix range_high
  anti_tpo_fixed <- fix_inequality_symbols(anti_tpo_values,
                                      column = "range_high",
                                      prefix = "range_high",
                                      reparse = TRUE)
  anti_tpo <- anti_tpo_fixed %>%
    mutate(positive_anti_tpo = as.integer(
      (!is.na(range_high) & value_as_number > range_high) |
        (is.na(range_high) & value_as_number > 9L)
    )) %>%
    select(
      person_id,
      anti_tpo = value_as_number,
      positive_anti_tpo,
      anti_tpo_operator = operator_concept_name,
      anti_tpo_range_low = range_low,
      anti_tpo_range_high = range_high
    )

  # 21. anti-tg
  anti_tg_values <-
    find_closest_2_dates(
      results_tbl("mx_anti_tg"),
      cohort,
      measurement_tbl,
      "measurement",
      max_days = 90L,
      fix_symbols = TRUE
    )

  anti_tg_fixed <- fix_inequality_symbols(anti_tg_values,
                                           column = "range_high",
                                           prefix = "range_high",
                                           reparse = TRUE)
  anti_tg <- anti_tg_fixed %>%
    mutate(positive_anti_tg = as.integer(
      (!is.na(range_high) & value_as_number > range_high) |
        (is.na(range_high) & value_as_number > 4L)
    )) %>%
    select(
      person_id,
      anti_tg = value_as_number,
      positive_anti_tg,
      anti_tg_operator = operator_concept_name,
      anti_tg_range_low = range_low,
      anti_tg_range_high = range_high
    )

  # 22. beta-blocker
  beta_blocker <-
    find_closest_2_dates(results_tbl("rx_beta_blockers"),
      cohort,
      results_tbl("drug_exposure"),
      "drug_exposure",
      min_days = -20L,
      max_days = 30L
    ) %>%
    mutate(rx_beta_blocker = as.integer(!is.na(drug_exposure_id))) %>%
    select(person_id, rx_beta_blocker)

  # 23. anti-thyroid drug
  anti_thyroid <-
    find_closest_2_dates(
      union_codesets(c(
        "rx_methimazole", "rx_propylthiouracil", "rx_carbimazole"
      )),
      cohort,
      results_tbl("drug_exposure"),
      "drug_exposure",
      min_days = -20L,
      max_days = 30L
    ) %>%
    mutate(rx_anti_thyroid = as.integer(!is.na(drug_exposure_id))) %>%
    select(person_id, rx_anti_thyroid)

  all_measurements <- bmi %>%
    left_join(diastolic_z_score, by = "person_id") %>%
    left_join(systolic_z_score, by = "person_id") %>%
    left_join(heart_rate, by = "person_id") %>%
    left_join(tsh, by = "person_id") %>%
    left_join(tsi_percent, by = "person_id") %>%
    left_join(tsi_iul, by = "person_id") %>%
    left_join(free_t4, by = "person_id") %>%
    left_join(total_t3, by = "person_id") %>%
    left_join(anti_tpo, by = "person_id") %>%
    left_join(anti_tg, by = "person_id") %>%
    left_join(beta_blocker, by = "person_id") %>%
    left_join(anti_thyroid, by = "person_id")

  summary_tbl <- add_conditions %>%
    left_join(all_measurements, by = "person_id", copy = TRUE)

  # Now do the crosswalks
  summary_tbl %>%
    distinct(person_id) %>%
    gen_xwalk("person_id") %>%
    output_tbl(name = "xwalk_person")

  summary_tbl %>%
    distinct(site) %>%
    gen_xwalk("site") %>%
    output_tbl(name = "xwalk_site")

  summary_tbl %>%
    new_id(
      id_col = "person_id",
      xwalk = results_tbl("xwalk_person"),
      replace = TRUE
    ) %>%
    new_id(
      id_col = "site",
      xwalk = results_tbl("xwalk_site"),
      replace = TRUE
    ) %>%
    output_tbl(name = "graves_analysis_dataset")
}
