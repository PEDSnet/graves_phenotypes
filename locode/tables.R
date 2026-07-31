categorical <- c(
  "gender_concept_name",
  "race_concept_name",
  "ethnicity_concept_name",
  "plan",
  "positive_anti_tpo",
  "positive_anti_tg",
  "dx_trisomy_21",
  "dx_other_autoimmune",
  "dx_vitamin_d",
  "rx_beta_blocker",
  "rx_anti_thyroid"
)
continuous <- c(
  "age_at_dx",
  "bmi_z_score",
  "hr",
  "systolic_z_score",
  "diastolic_z_score",
  "tsh",
  "tsi_percent",
  "tsi_iul",
  "ft4",
  "tt3"
)

# Plot categorical
plot_cat <- function(df, column) {
  df %>%
    select(year, !!sym(column)) %>%
    group_by(year) %>%
    summarize(
      n = n(),
      positive = sum(!!sym(column), na.rm = TRUE),
      non_missing = sum(!is.na(!!sym(column)))
    ) %>%
    mutate(
      pct_non_missing = positive / non_missing,
      pct_total = positive / n
    ) %>%
    pivot_longer(c(pct_total, pct_non_missing),
                 names_to = "group",
                 values_to = "pct") %>%
    ggplot(aes(x = year, y = pct, color = group)) +
    geom_line() +
    labs(title = column)
}


# Plot continuous
plot_cont <- function(df, column) {
  df %>%
    select(year, !!sym(column)) %>%
    group_by(year) %>%
    summarize(
      n = n(),
      mean = mean(!!sym(column), na.rm = TRUE),
      median = median(!!sym(column), na.rm = TRUE),
      non_missing = sum(!is.na(!!sym(column)))
    ) %>%
    pivot_longer(c(median, mean), names_to = "group", values_to = "value") %>%
    ggplot(aes(x = year, y = value, color = group)) +
    geom_line() +
    labs(title = column)
}

table1 <- function(df) {
  df %>%
    select(category, group, label, header) %>%
    pivot_wider(
      names_from = header,
      values_from = label,
      values_fill = "0 (0%)"
    )
}

# Convert all dates to the first of the year for nice plotting
df <- results_tbl("graves_analysis_dataset") %>%
  collect() %>%
  mutate(age_at_dx = condition_start_age_in_months / 12L)

df_stanford <- df %>%
  semi_join(
    results_tbl("xwalk_site") %>%
      filter(site == "stanford") %>%
      collect(),
    by = join_by("site" == "seq_id")
  )

# Overall Stanford
summarize_redact_and_round(
  df_stanford,
  categorical = categorical,
  continuous = continuous,
  threshold = NA,
  round_to = NA
) %>%
  table1() %>%
  output_tbl(name = "stanford_overall_unredacted", file = TRUE)

# Stanford by year
summarize_redact_and_round(
  df_stanford,
  categorical = categorical,
  continuous = continuous,
  threshold = NA,
  round_to = NA,
  grouper = "presenting_visit_year"
) %>%
  table1() %>%
  output_tbl(name = "stanford_year_unredacted", file = TRUE)

# All sites overall
summarize_redact_and_round(
  df,
  categorical = categorical,
  continuous = continuous,
  threshold = 10L,
  round_to = NA,
) %>%
  table1() %>%
  output_tbl(name = "overall", file = TRUE)

# All sites by year
summarize_redact_and_round(
  df,
  categorical = categorical,
  continuous = continuous,
  threshold = 10L,
  round_to = NA,
  grouper = "presenting_visit_year"
) %>%
  table1() %>%
  output_tbl(name = "overall_year", file = TRUE)

# Overall by site
summarize_redact_and_round(
  df,
  categorical = categorical,
  continuous = continuous,
  threshold = 10L,
  round_to = NA,
  grouper = "site"
) %>%
  table1() %>%
  output_tbl(name = "overall_site", file = TRUE)
