dx_hyperthyroid_date <- as.Date("2023-02-01")
dx_graves_date <- as.Date("2024-02-01")

test_cohort <- data.frame(
  person_id = 1L,
  site = "site_x",
  dx_hyperthyroid_date = dx_hyperthyroid_date,
  dx_graves_date = dx_graves_date,
  stringsAsFactors = FALSE
)
test_cohort <-
  copy_to_new(
    dest = config("db_src"),
    df = test_cohort,
    overwrite = TRUE,
    temporary = TRUE
  )

test_that("Choose measurement 3 days ago over 5 days ago (both numeric)", {
  test_df <- data.frame(
    person_id = c(1L, 1L),
    measurement_id = c(1L, 2L),
    measurement_concept_id = c(2000000043L, 2000000043L),
    measurement_date = c(dx_hyperthyroid_date - 3L, dx_hyperthyroid_date - 5L),
    measurement_datetime = as.POSIXct(c(
      dx_hyperthyroid_date - 3L, dx_hyperthyroid_date - 5L
    )),
    measurement_order_date =
      c(dx_hyperthyroid_date - 3L, dx_hyperthyroid_date - 5L),
    measurement_result_date =
      c(dx_hyperthyroid_date - 3L, dx_hyperthyroid_date - 5L),
    value_as_number = c(0.02, 0.01),
    value_as_concept_id = c(0L, 0L),
    value_source_value = c("0.02", "0.01"),
    site = c("site_x", "site_x"),
    stringsAsFactors = FALSE
  )
  test_df <-
    copy_to_new(
      dest = config("db_src"),
      df = test_df,
      overwrite = TRUE,
      temporary = TRUE
    )
  result <-
    find_closest(results_tbl("mx_bmi"), test_cohort, test_df, "measurement") %>%
    collect()
  expect_identical(result[["measurement_id"]], 1L)
  expect_identical(result[["diff"]], -3L)
})

test_that("Choose measurement 1 days ago over 0 days ago (both numeric)", {
  test_df <- data.frame(
    person_id = c(1L, 1L),
    measurement_id = c(1L, 2L),
    measurement_concept_id = c(2000000043L, 2000000043L),
    measurement_date = c(dx_hyperthyroid_date - 1L, dx_hyperthyroid_date - 0L),
    measurement_datetime = as.POSIXct(c(
      dx_hyperthyroid_date - 1L, dx_hyperthyroid_date - 0L
    )),
    measurement_order_date =
      c(dx_hyperthyroid_date - 1L, dx_hyperthyroid_date - 0L),
    measurement_result_date =
      c(dx_hyperthyroid_date - 1L, dx_hyperthyroid_date - 0L),
    value_as_number = c(0.02, 0.01),
    value_as_concept_id = c(0L, 0L),
    value_source_value = c("0.02", "0.01"),
    site = c("site_x", "site_x"),
    stringsAsFactors = FALSE
  )
  test_df <-
    copy_to_new(
      dest = config("db_src"),
      df = test_df,
      overwrite = TRUE,
      temporary = TRUE
    )
  result <-
    find_closest(results_tbl("mx_bmi"), test_cohort, test_df, "measurement") %>%
    collect()
  expect_identical(result[["measurement_id"]], 1L)
  expect_identical(result[["diff"]], -1L)
})

test_that("Choose measurement 5 days ago over 3 days after", {
  test_df <- data.frame(
    person_id = c(1L, 1L),
    measurement_id = c(1L, 2L),
    measurement_concept_id = c(2000000043L, 2000000043L),
    measurement_date = c(dx_hyperthyroid_date + 3L, dx_hyperthyroid_date - 5L),
    measurement_datetime = as.POSIXct(c(
      dx_hyperthyroid_date + 3L, dx_hyperthyroid_date - 5L
    )),
    measurement_order_date =
      c(dx_hyperthyroid_date + 3L, dx_hyperthyroid_date - 5L),
    measurement_result_date =
      c(dx_hyperthyroid_date + 3L, dx_hyperthyroid_date - 5L),
    value_as_number = c(0.02, 0.01),
    value_as_concept_id = c(0L, 0L),
    value_source_value = c("0.02", "0.01"),
    site = c("site_x", "site_x"),
    stringsAsFactors = FALSE
  )
  test_df <-
    copy_to_new(
      dest = config("db_src"),
      df = test_df,
      overwrite = TRUE,
      temporary = TRUE
    )
  result <-
    find_closest(results_tbl("mx_bmi"), test_cohort, test_df, "measurement") %>%
    collect()
  expect_identical(result[["measurement_id"]], 2L)
  expect_identical(result[["diff"]], -5L)
})

test_that("Choose numeric 5 days ago over categorical 3 days ago", {
  test_df <- data.frame(
    person_id = c(1L, 1L),
    measurement_id = c(1L, 2L),
    measurement_concept_id = c(2000000043L, 2000000043L),
    measurement_date = c(dx_hyperthyroid_date - 3L, dx_hyperthyroid_date - 5L),
    measurement_datetime = as.POSIXct(c(
      dx_hyperthyroid_date - 3L, dx_hyperthyroid_date - 5L
    )),
    measurement_order_date =
      c(dx_hyperthyroid_date - 3L, dx_hyperthyroid_date - 5L),
    measurement_result_date =
      c(dx_hyperthyroid_date - 3L, dx_hyperthyroid_date - 5L),
    value_as_number = c(NA, 0.01),
    value_as_concept_id = c(9191L, 0L),
    value_source_value = c("Positive", "0.01"),
    site = c("site_x", "site_x"),
    stringsAsFactors = FALSE
  )
  test_df <-
    copy_to_new(
      dest = config("db_src"),
      df = test_df,
      overwrite = TRUE,
      temporary = TRUE
    )
  result <-
    find_closest(results_tbl("mx_bmi"), test_cohort, test_df, "measurement") %>%
    collect()
  expect_identical(result[["measurement_id"]], 2L)
  expect_identical(result[["diff"]], -5L)
})

test_that("Return none if no measurement within the window", {
  test_df <- data.frame(
    person_id = c(1L, 1L),
    measurement_id = c(1L, 2L),
    measurement_concept_id = c(2000000043L, 2000000043L),
    measurement_date =
      c(dx_hyperthyroid_date + 90L, dx_hyperthyroid_date + 91L),
    measurement_datetime = as.POSIXct(c(
      dx_hyperthyroid_date + 90L, dx_hyperthyroid_date + 91L
    )),
    measurement_order_date =
      c(dx_hyperthyroid_date + 90L, dx_hyperthyroid_date + 91L),
    measurement_result_date =
      c(dx_hyperthyroid_date + 90L, dx_hyperthyroid_date + 91L),
    value_as_number = c(0.02, 0.01),
    value_as_concept_id = c(0L, 0L),
    value_source_value = c("0.02", "0.01"),
    site = c("site_x", "site_x"),
    stringsAsFactors = FALSE
  )
  test_df <-
    copy_to_new(
      dest = config("db_src"),
      df = test_df,
      overwrite = TRUE,
      temporary = TRUE
    )
  result <-
    find_closest(results_tbl("mx_bmi"), test_cohort, test_df, "measurement") %>%
    collect()
  expect_identical(nrow(result), 0L)
})

test_that("Find closest condition within 180 days, using absolute value", {
  test_df <- data.frame(
    person_id = c(1L, 1L),
    condition_occurrence_id = c(1L, 2L),
    condition_concept_id = c(4148897L, 4148897L),
    condition_start_date =
      c(dx_hyperthyroid_date + -170L, dx_hyperthyroid_date + 150L),
    condition_start_datetime = as.POSIXct(c(
      dx_hyperthyroid_date -170L, dx_hyperthyroid_date + 150L
    )),
    site = c("site_x", "site_x"),
    stringsAsFactors = FALSE
  )
  test_df <-
    copy_to_new(
      dest = config("db_src"),
      df = test_df,
      overwrite = TRUE,
      temporary = TRUE
    )
  result <-
    find_closest(results_tbl("dx_trisomy_21"), test_cohort, test_df, "condition_occurrence", order = "closest", min_days = -180L, max_days = 180L) %>%
    collect()
  expect_identical(result[["condition_occurrence_id"]], 2L)
  expect_identical(result[["diff"]], 150L)
})

test_that("If there is no match on the first date, find match on second", {
  test_df <- data.frame(
    person_id = c(1L, 1L),
    measurement_id = c(1L, 2L),
    measurement_concept_id = c(2000000043L, 2000000043L),
    measurement_date = c(dx_hyperthyroid_date + 50L, dx_graves_date - 30L),
    measurement_datetime = as.POSIXct(c(
      dx_hyperthyroid_date + 50L, dx_graves_date - 30L
    )),
    measurement_order_date =
      c(dx_hyperthyroid_date + 50L, dx_graves_date - 30L),
    measurement_result_date =
      c(dx_hyperthyroid_date + 50L, dx_graves_date + 1L),
    value_as_number = c(0.02, 0.01),
    value_as_concept_id = c(0L, 0L),
    value_source_value = c("0.02", "0.01"),
    site = c("site_x", "site_x"),
    stringsAsFactors = FALSE
  )
  test_df <-
    copy_to_new(
      dest = config("db_src"),
      df = test_df,
      overwrite = TRUE,
      temporary = TRUE
    )
  result <-
    find_closest_2_dates(
      results_tbl("mx_bmi"),
      test_cohort,
      test_df,
      "measurement",
      fix_symbols = FALSE
    )
  expect_identical(result[["dx_source"]], "dx_graves_date")
  expect_identical(result[["measurement_id"]], 2L)
  expect_identical(result[["diff"]], -30L)
})
