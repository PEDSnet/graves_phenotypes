# No thyroidectomy or drug exposure, retain graves date
test_that("no treatment keeps graves date", {
  df <- data.frame(
    person_id = 1L,
    dx_hyperthyroid_date = as.Date("2023-01-01"),
    dx_graves_date = as.Date("2023-02-01"),
    thyroidectomy_date = NA,
    earliest_at_drug_exposure_start_date = NA
  )
  result <- filter_no_treatment(df)
  expect_identical(result[["dx_graves_date"]], df[["dx_graves_date"]])
})

# Thyroidectomy after graves dx, retain graves date
test_that("later thyroiectomy keeps graves date", {
  df <- data.frame(
    person_id = 1L,
    dx_hyperthyroid_date = as.Date("2023-01-01"),
    dx_graves_date = as.Date("2023-02-01"),
    thyroidectomy_date = as.Date("2024-02-01"),
    earliest_at_drug_exposure_start_date = NA
  )
  result <- filter_no_treatment(df)
  expect_identical(result[["dx_graves_date"]], df[["dx_graves_date"]])
})

# Thyroidectomy between the dxs, remove the graves date
test_that("thyroidectomy, remove graves date", {
  df <- data.frame(
    person_id = 1L,
    dx_hyperthyroid_date = as.Date("2023-01-01"),
    dx_graves_date = as.Date("2023-02-01"),
    thyroidectomy_date = as.Date("2023-01-10"),
    earliest_at_drug_exposure_start_date = NA
  )
  result <- filter_no_treatment(df)
  expect_true(is.na(result[["dx_graves_date"]]), TRUE)
})

# Anti-thyroid drug 10 days before graves when no more than 7 allowed, remove
test_that("treatment before graves dx, remove graves date", {
  df <- data.frame(
    person_id = 1L,
    dx_hyperthyroid_date = as.Date("2023-01-01"),
    dx_graves_date = as.Date("2023-02-01"),
    thyroidectomy_date = NA,
    earliest_at_drug_exposure_start_date = as.Date("2023-01-22")
  )
  result <- filter_no_treatment(df, treatment_window = 7L)
  expect_true(is.na(result[["dx_graves_date"]]), TRUE)
})

# Anti-thyroid drug 10 days before graves when no more than 14 allowed, retain
test_that("treatment close to graves dx, but allowed", {
  df <- data.frame(
    person_id = 1L,
    dx_hyperthyroid_date = as.Date("2023-01-01"),
    dx_graves_date = as.Date("2023-02-01"),
    thyroidectomy_date = NA,
    earliest_at_drug_exposure_start_date = as.Date("2023-01-22")
  )
  result <- filter_no_treatment(df, treatment_window = 14L)
  expect_identical(result[["dx_graves_date"]], df[["dx_graves_date"]])
})
