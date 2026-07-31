#' Sort table by max person count
#' Optionally sort within a subgroup
#'
#' @param df Dataframe with at least one person count column
#' @param group_column Optionally group by this column then sort
#'
#' @return A dataframe sorted by max person count
#' @export
#'
#' @examples
order_by_person_count <- function(df, group_column = NA) {
  df <- df %>%
    group_by(!!sym(group_column)) %>%
    rowwise() %>%
    mutate(max_col = max(c_across(contains("pc")), na.rm = TRUE)) %>%
    ungroup() %>%
    arrange(!!sym(group_column), desc(max_col)) %>%
    select(-any_of(c("max_col", "NA")))
  df
}

#' Count records and people with each code in a codeset
#' With the option of looking across multiple domains
#'
#' @param cohort Contains the person_id of the cohort of interest
#' @param codeset Codeset of interest
#' @param domains The tables e.g. condition_occurrence, procedure_occurrence
#'
#' @return a tbl with 2 columns (person count, record count) for each domain
#' @export
#'
#' @examples
count_codes_cohort <-
  function(cohort,
           codeset,
           domains = list(),
           include_suffix = TRUE) {
    result <- list()
    for (domain in domains) {
      domain_concept_name <- paste0((domain %>%
        str_split_i("_", 1L)), "_concept_id")
      cohort_domain <- (cohort %>%
        select(person_id)) %>%
        inner_join(
          (results_tbl({{ domain }}) %>%
            select(person_id, {{ domain_concept_name }})),
          by = "person_id"
        )

      res <- codeset %>%
        left_join(cohort_domain,
          by = join_by(concept_id == {{ domain_concept_name }})
        ) %>%
        group_by(concept_id) %>%
        summarize(pc = count(distinct(person_id)), rc = count(person_id))

      if (length(result) == 0L) {
        res <- (vocabulary_tbl("concept") %>%
          select(concept_name, concept_id)) %>%
          inner_join(res, by = "concept_id") %>%
          collect()
      }

      result <- append(result, list(res))
    }

    merged <-
      Reduce(
        function(...) {
          merge(
            ...,
            all = TRUE,
            by = "concept_id",
            suffixes = lapply(domains, function(x) {
              paste0(".", x)
            })
          )
        },
        result
      )

    # Do we want to include the domain as a suffix even if there is only one
    # domain? May want this for the csv output
    if ((length(domains) == 1L) && include_suffix) {
      merged <- merged %>%
        rename_with(~ paste(., domain, sep = "."),
          .cols = !c("concept_name", "concept_id")
        )
    }

    merged
  }

#' Display code count table
#'
#' @param tbl A table produced by count_codes_cohort
#' @param domains Header title for each domain
#'
#' @return Displays a kable table
#' @export
#'
#' @examples
display_code_count_kable <- function(tbl, domains) {
  cohort_column_names <- colnames(tbl)
  cleaned_names <- sub("\\..*$", "", cohort_column_names)
  colnames(tbl) <- cleaned_names
  headers <- c(" " = 2L, setNames(rep(2L, length(domains)), unlist(domains)))
  tbl <- tbl %>% order_by_person_count()
  kable(tbl) %>% add_header_above(headers)
}

#' Create CSV of cohort code counts
#'
#' @return NA
#' @export
#'
#' @examples
code_counts_to_csv <- function(redact = TRUE) {
  df_list <- list()
  my_dict <- list(
    "mx_bmi" = list("measurement"),
    "mx_systolic" = list("measurement"),
    "mx_diastolic" = list("measurement"),
    "mx_heart_rate" = list("measurement"),
    "mx_tsi" = list("measurement"),
    "mx_trab" = list("measurement"),
    "mx_tsh" = list("measurement"),
    "mx_total_t3" = list("measurement"),
    "mx_free_t4" = list("measurement"),
    "mx_anti_tpo" = list("measurement"),
    "mx_anti_tg" = list("measurement"),
    "mx_covid" = list("measurement"),
    "rx_methimazole" = list("drug_exposure"),
    "rx_carbimazole" = list("drug_exposure"),
    "rx_propylthiouracil" = list("drug_exposure"),
    "rx_beta_blockers" = list("drug_exposure"),
    "mx_dx_vitamin_d" = list("condition_occurrence", "measurement"),
    "tx_total_thyroidectomy" =
      list("procedure_occurrence", "condition_occurrence"),
    "dx_covid" = list("condition_occurrence"),
    "dx_addisons" = list("condition_occurrence"),
    "dx_t1dm" = list("condition_occurrence"),
    "dx_celiac" = list("condition_occurrence"),
    "dx_vitiligo" = list("condition_occurrence"),
    "dx_trisomy_21" = list("condition_occurrence")
  )
  i <- 1L
  for (codeset in names(my_dict)) {
    res <- count_codes_cohort(
      results_tbl("cohort_post_attrition"),
      results_tbl(codeset),
      domains = my_dict[[codeset]]
    )
    res[["codeset"]] <- codeset
    df_list[[i]] <- data.frame(res)
    i <- i + 1L
  }
  combined_df <- bind_rows(df_list)
  # Put concept name first
  combined_df <- combined_df %>%
    select(codeset, concept_id, concept_name, everything())

  combined_df <- combined_df %>%
    order_by_person_count(group_column = "codeset")

  # Redact
  if (redact) {
    cols <- combined_df %>%
      select(starts_with("rc.") | starts_with("pc.")) %>%
      colnames()
    combined_df <- combined_df %>%
      redact_and_round(columns = cols, grouper = "codeset")
  }

  combined_df %>%
    output_tbl(name = "code_counts", file = TRUE)
}

#' Categorize measurement results as numeric, category, symbol, or neither
#'
#' @param measurements 
#'
#' @return
#' @export
#'
#' @examples
categorize_measurment_results <- function(measurements) {
  res <- measurements %>%
    mutate(result_type = if_else((!is.na(value_as_number) &
                               (value_as_number != 0)),
                            "numeric",
                            if_else((!is.na(value_as_concept_id) &
                                       (value_as_concept_id != 0)),
                                    "category",
                                    if_else(
                                      (value_source_value %like% "<%") |
                                      (value_source_value %like% ">%")
                                    , "symbol", "neither")
                            )
    ))
  res
}

#' For each codeset provided, compute the proportion of the cohort
#' that has that domain sometime between the given day and max_days.
#' For measurements, categorize by whether they have a value_as_number,
#' value_as_concept_id, a symbol that may be usable e.g. <0.01, or no usable
#' information (missing, cancelled etc).
#'
#' @param codesets Codesets to plot (string name of codeset in database or
#' vector of manually provided codes)
#' @param domain OMOP domain of the events table
#' @param lookback If true, count from max to min; if false from min to max
#' @param max_days Latest day after presenting visit to look for codes
#' @param min_days Earliest day before presenting visit to look for codes
#'
#' @return tbl with columns for result_type, count, day, and codeset
#' @export
#'
#' @examples
proportion_over_time <-
  function(codesets,
           domain,
           lookback = TRUE,
           max_days = 14L,
           min_days = -60L) {
  rows <- c()
  all_codes <- union_codesets(codesets)
  domain_concept_name <- paste0((domain %>%
                                   str_split_i("_", 1L)), "_concept_id")
  domain_date <- paste0(get_date_start_name(domain))

  times <- results_tbl("cohort_post_attrition") %>%
    inner_join(
      (results_tbl(domain) %>%
        inner_join(all_codes,
                   by = join_by({{ domain_concept_name }} == "concept_id")
        )),
      by = "person_id"
    ) %>%
    mutate(diff = (!!sym(domain_date) - dx_hyperthyroid_date)) %>%
    filter(diff <= max_days, diff >= min_days)
  
  if(domain == "measurement"){
    # Add result_type column to classify what type of result the measurement has
    times <- times %>%
      categorize_measurment_results() %>%
      collect()
  } else {
    times <- times %>%
      collect() %>%
      mutate(result_type = NA)
  }

  for (codeset in codesets) {
    domain_codeset <- times %>%
      filter(codeset == {{ codeset }})
    total_days <- max_days - (min_days) + 1
    for (i in 1L:total_days) {
      day <- min_days + (i - 1L)
      if(lookback){
        filtered <- domain_codeset %>%
          filter(diff >= day)
      } else{
        filtered <- domain_codeset %>%
          filter(diff <= day)
      }
      count <- filtered %>%
        group_by(result_type) %>%
        summarize(count = n_distinct(person_id)) %>%
        mutate(days = day, codeset = codeset)
      total_row <- tibble(
        result_type = "total",
        count = filtered %>% distinct_ct(),
        days = day,
        codeset = codeset
      )
      count <- bind_rows(count, total_row)
      rows <- rbind(rows, count)
    }
  }
  results_df <- data.frame(rows)
}

#' Plot proportion of cohort with each codeset around presenting visit
#'
#' @param days_df tbl created by proportion_over_time
#'
#' @return A ggplot
#' @export
#'
#' @examples
plot_all_codesets <- function(days_df){
  cohort_size <- results_tbl("cohort_post_attrition") %>% distinct_ct()
  pal = colorRampPalette(RColorBrewer::brewer.pal(13, "Set2"))
  days_df %>%
    filter(result_type == "total") %>%
    redact_and_round(columns = "count", grouper = "codeset") %>%
    mutate(count = round(100L * count / cohort_size, 0L)) %>%
    ggplot(aes(x = days, y = count, color = codeset)) +
    geom_line() +
    scale_fill_manual(values = pal) +
    ggtitle("Proportion of cohort with an event relative to presenting visit") +
    xlab("Days since presenting visit") +
    ylab("Propotion of cohort")
}

#' For each codeset plot each measurement result type over time
#'
#' @param days_df tbl created by proportion_over_time
#'
#' @return A ggplot
#' @export
#'
#' @examples
plot_result_type_panel <- function(days_df){
  cohort_size <- results_tbl("cohort_post_attrition") %>% distinct_ct()
  redacted_rounded <- days_df %>%
    redact_and_round(columns = "count", grouper = c("codeset", "result_type")) %>%
    mutate(count = round(100L * count / cohort_size, 0L))
  
  p <- redacted_rounded %>%
    ggplot(aes(x = days, y = count, color = result_type)) +
    geom_line() +
    scale_color_brewer(palette = "Paired")
  
  p + facet_wrap(~ codeset, ncol = 3, scales = "free_y") +
    ggtitle("Proportion of cohort with a measurement by result type") +
    xlab("Days since presenting visit") +
    ylab("Propotion of cohort")
}
