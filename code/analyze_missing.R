#' Create a dataframe with the proportion missing by codeset by site
#'
#' @param codesets list of codeset strings to check
#' @param cohort cohort_post_attrition created by driver.R that contains
#' dx_graves_date, thyroidectomy_date, and drug_exposure_date
#' @param measurement_tbl Subset of measurements to use
#' @param min_days Measurement can be this many days before the dx code
#' @param max_days Measurement can be this many days after the dx code
#' @param redact Boolean whether to redact and round small counts
#'
#' @return Locally collected tbl with one row per site, one column per codeset
#' @export
#'
#' @examples
describe_missing_by_site <-
  function(codesets,
           cohort,
           measurement_tbl,
           min_days = -14L,
           max_days = 14L,
           redact = FALSE) {
    rows <- c()
    second_dx_rows <- c()

    site_totals <- cohort %>%
      count(site, name = "total") %>%
      collect()
    overall_total <- cohort %>%
      count(name = "total") %>%
      mutate(site = "overall") %>%
      collect()
    totals <- site_totals %>% rbind(overall_total)

    for (codeset_str in codesets) {
      codeset <- results_tbl(codeset_str)
      closest_measurement <-
        find_closest_2_dates(codeset,
          cohort,
          measurement_tbl,
          "measurement",
          min_days = min_days,
          max_days = max_days
        )
      # n is the count with a measurement
      row <- closest_measurement %>%
        filter(!is.na(site)) %>%
        group_by(site) %>%
        summarize(n = n())
      # Add the overall count
      # Because find_closest_2_dates does not return the whole cohort, if there
      # are 0 measurements, the site will be missing, so we do a left join here
      row <- totals %>%
        left_join(
          row %>%
            add_row(
              site = "overall",
              n = row %>%
                summarize(overall = sum(n)) %>%
                pull()
            ),
          by = "site"
        ) %>%
        mutate(n = coalesce(n, 0L), codeset = codeset_str)

      from_second_dx <- closest_measurement %>%
        count(dx_source) %>%
        filter(!is.na(dx_source))

      if (redact) {
        from_second_dx <- from_second_dx %>%
          redact_and_round(columns = "n")
      }

      from_second_dx <- from_second_dx %>%
        mutate(from_second_dx = paste0(round(100L * (n / sum(n)), 1L), "%")) %>%
        filter(dx_source == "dx_graves_date") %>%
        select(from_second_dx) %>%
        mutate(codeset = codeset_str)
      rows <- rbind(rows, row)
      second_dx_rows <- rbind(second_dx_rows, from_second_dx)
    }

    results_df <- rows %>%
      mutate(missing = total - n)

    if (redact) {
      results_df <- results_df %>%
        redact_and_round(
          columns = c("n", "missing", "total"),
          groupers = "codeset"
        )
    }

    missing_df <- results_df %>%
      mutate(
        pct = round(100L * (missing / total), 1L),
        label = paste0(missing, " (", pct, "%)"),
        site = paste0(site, " n=(", total, ")")
      ) %>%
      select(codeset, site, label) %>%
      pivot_wider(names_from = "site", values_from = "label")

    missing_df %>% left_join(second_dx_rows, by = "codeset")
  }
