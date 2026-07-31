#' Find the units associated with the numeric measurements
#' We leave the result as one row per measurement so a box plot of
#' value_as_number can be created and a column 'name_pct' to use as the plot
#' title. Units with less than 1% of the measurements have been removed.
#'
#' @param codeset codeset table to check
#'
#' @return a dataframe with one row per measurement
#' @export
#'
#' @examples
get_numeric_unit_data <- function(codeset, cohort, measurement_tbl) {
  closest_measurement <-
    find_closest_2_dates(codeset, cohort, measurement_tbl, "measurement")
  numeric <- closest_measurement %>% filter(result_type == "numeric")

  # Do a right join so we copy the smaller table to the database
  unit_counts <- (vocabulary_tbl("concept") %>%
                    select(concept_name, concept_id)) %>%
    right_join(
      (numeric %>%
         count(unit_concept_id)),
      by = join_by("concept_id" == "unit_concept_id"), copy = TRUE
    ) %>%
    mutate(
      unit_concept_name = concept_name,
      unit_concept_id = concept_id
    ) %>%
    collect()

  unit_values_ordered <- unit_counts %>%
    mutate(pct = round(100L * (n / sum(n)), 0L)) %>%
    mutate(name_pct =
             paste0(concept_name, " (", pct, "%) ", unit_concept_id)) %>%
    arrange(desc(n))

  order_list <- unit_values_ordered[["name_pct"]]
  numeric_with_pct <- numeric %>%
    inner_join(unit_values_ordered, by = "unit_concept_id")
  numeric_with_pct[["name_pct"]] <- factor(numeric_with_pct[["name_pct"]],
    levels = order_list
  )
  numeric_with_pct
}

#' Create a box plot of measurement values by unit
#'
#' @param df dataframe created by get_numeric_unit_data
#'
#' @return ggplot
#' @export
#'
#' @examples
plot_unit_histogram <- function(df, redact = FALSE) {
  # Add text labels with measurement concept names with each unit
  category_summary <- df %>%
    group_by(name_pct, unit_concept_id) %>%
    summarise(categories = paste(unique(measurement_concept_name),
                                 collapse = "\n"),
              total = n(),
              source_units = paste(unique(tolower(unit_source_value)), collapse = "\n")
              ) %>%
    ungroup()

  if (redact) {
    category_summary <- category_summary %>%
      filter(total > 10L)
    df <- df %>% semi_join(category_summary, by = "unit_concept_id")
    p_outliers <- ggplot(df, aes(y = value_as_number)) +
      geom_boxplot(outlier.shape = NA, coef = 0L)
  } else {
    p_outliers <- ggplot(df, aes(y = value_as_number)) +
      geom_boxplot(outliers = TRUE)
  }

  p_outliers + facet_wrap(~name_pct, ncol = 4L, scales = "free_y") +
    theme(strip.text = element_text(
      size = 8L)) +
    geom_text(
      data = category_summary, aes(x = -0.5, y = Inf, label = paste0("Concepts:\n", categories)),
      vjust = 1.0, hjust = 0L, size = 2.5, color = "black",
      position = position_nudge(y = 0L)
    ) +
    geom_text(
      data = category_summary, aes(x = -0.5, y = 0, label = paste0("Source Units:\n", source_units)),
      vjust = 0, hjust = 0L, size = 2.3, color = "red",
      position = position_nudge(y = 0L)
    ) +
    theme(
      aspect.ratio = 1  # Ensure equal aspect ratio for all subplots
    )
}
