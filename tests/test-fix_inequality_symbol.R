test_that("Do not change source value with no symbol", {
  df <- data.frame(
    person_id = 1L,
    measurement_id = 2L,
    value_as_number = NA,
    value_source_value = "0.02",
    operator_concept_id = 4172703L,
    operator_concept_name = "=",
    stringsAsFactors = FALSE
  )
  result <- fix_inequality_symbol(df, "<", 4171756L)
  expect_true(is.na(result[["value_as_number"]]))
})

test_that("Do not change a non-null value", {
  df <- data.frame(
    person_id = 1L,
    measurement_id = 2L,
    value_as_number = 0.02,
    value_source_value = "<0.02",
    operator_concept_id = 4172703L,
    operator_concept_name = "=",
    stringsAsFactors = FALSE
  )
  val_before <- df[["value_as_number"]]
  result <- fix_inequality_symbol(df, "<", 4171756L)
  expect_identical(result[["value_as_number"]], val_before)
})

test_that("Parse <0.02", {
  df <- data.frame(
    person_id = 1L,
    measurement_id = 2L,
    value_as_number = NA,
    value_source_value = "<0.02",
    operator_concept_id = 4172703L,
    operator_concept_name = "=",
    stringsAsFactors = FALSE
  )
  result <- fix_inequality_symbol(df, "<", 4171756L)
  expect_equal(result[["value_as_number"]], 0.02)
  expect_identical(result[["operator_concept_id"]], 4171756L)
  expect_identical(result[["operator_concept_name"]], "<")
})

test_that("Parse < 0.02", {
  df <- data.frame(
    person_id = 1L,
    measurement_id = 2L,
    value_as_number = NA,
    value_source_value = "< 0.02",
    operator_concept_id = 4172703L,
    operator_concept_name = "=",
    stringsAsFactors = FALSE
  )
  result <- fix_inequality_symbol(df, "<", 4171756L)
  expect_equal(result[["value_as_number"]], 0.02)
  expect_identical(result[["operator_concept_id"]], 4171756L)
  expect_identical(result[["operator_concept_name"]], "<")
})

test_that("Parse LESS THAN 0.02", {
  df <- data.frame(
    person_id = 1L,
    measurement_id = 2L,
    value_as_number = NA,
    value_source_value = "LESS THAN 0.02",
    operator_concept_id = 4172703L,
    operator_concept_name = "=",
    stringsAsFactors = FALSE
  )
  result <- fix_inequality_symbol(df, "LESS THAN", 4171756L)
  expect_equal(result[["value_as_number"]], 0.02)
  expect_identical(result[["operator_concept_id"]], 4171756L)
  expect_identical(result[["operator_concept_name"]], "<")
})

test_that("Parse > 5.", {
  df <- data.frame(
    person_id = 1L,
    measurement_id = 2L,
    value_as_number = NA,
    value_source_value = "> 5.",
    operator_concept_id = 4172703L,
    operator_concept_name = "=",
    stringsAsFactors = FALSE
  )
  result <- fix_inequality_symbol(df, ">", 4172704L)
  expect_identical(result[["value_as_number"]], 5.0)
  expect_identical(result[["operator_concept_id"]], 4172704L)
  expect_identical(result[["operator_concept_name"]], ">")
})

test_that("Parse > .2", {
  df <- data.frame(
    person_id = 1L,
    measurement_id = 2L,
    value_as_number = NA,
    value_source_value = "> .2",
    operator_concept_id = 4172703L,
    operator_concept_name = "=",
    stringsAsFactors = FALSE
  )
  result <- fix_inequality_symbol(df, ">", 4172704L)
  expect_equal(result[["value_as_number"]], 0.2)
  expect_identical(result[["operator_concept_id"]], 4172704L)
  expect_identical(result[["operator_concept_name"]], ">")
})

test_that("Fix symbols with multiple rows", {
  df <- data.frame(
    person_id = c(1L, 2L, 3L, 4L),
    measurement_id = c(2L, 3L, 4L, 5L),
    value_as_number = c(NA, NA, NA, 0.01),
    value_source_value = c("<0.02", "LESS THAN 5", "> 0.7", "0.010 <"),
    operator_concept_id = c(4172703L, NA, NA, 4172703L),
    operator_concept_name = c("=", NA, NA, "="),
    stringsAsFactors = FALSE
  )
  result <- fix_inequality_symbols(df)
  expect_equal(result[["value_as_number"]], c(0.02, 5.0, 0.7, 0.01))
  expect_identical(result[["operator_concept_id"]], c(4171756L, 4171756L, 4172704L, 4172703L))
  expect_identical(result[["operator_concept_name"]], c("<", "<", ">", "="))
})
