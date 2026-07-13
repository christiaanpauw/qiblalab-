make_valid_raw <- function() {
  # Minimal synthetic raw data frame that passes all checks
  tibble::tibble(
    row_id                  = 1L,
    "Gibson Classification" = "Petra",
    "Year CE"               = "705",
    "Year AH"               = 86,
    "age_group"             = "Umayyad",
    "City"                  = "Damascus",
    "Country"               = "Syria",
    "Mosque Name"           = "Test Mosque",
    "Rebuilt"               = "never",
    "Latitude"              = 33.5,
    "Longitude"             = 36.3,
    "dir"                   = 167.0,
    "Website Link"          = "https://example.com"
  )
}

expand_raw <- function(base, n = 160) {
  # Replicate a single-row frame n times so row count check passes
  base[rep(1L, n), ]
}

test_that("validate_qibla_data() returns a qibla_validation object", {
  raw    <- expand_raw(make_valid_raw())
  result <- validate_qibla_data(raw)
  expect_s3_class(result, "qibla_validation")
  expect_named(result, c("issues", "n_rows", "n_cols"))
})

test_that("validate_qibla_data() finds no issues on clean synthetic data", {
  raw    <- expand_raw(make_valid_raw())
  result <- validate_qibla_data(raw)
  expect_equal(length(result$issues), 0L)
})

test_that("validate_qibla_data() flags a missing column as an error", {
  raw <- expand_raw(make_valid_raw())
  raw$dir <- NULL
  result  <- validate_qibla_data(raw)
  errs    <- validation_errors(result)
  expect_true("dir" %in% errs$field)
})

test_that("validate_qibla_data() warns on wrong row count", {
  raw    <- make_valid_raw()   # only 1 row
  result <- validate_qibla_data(raw)
  warns  <- validation_warnings(result)
  expect_true(any(grepl("rows", warns$field)))
})

test_that("validate_qibla_data() errors on out-of-range latitude", {
  raw <- expand_raw(make_valid_raw())
  raw$Latitude[1L] <- 200
  result <- validate_qibla_data(raw)
  errs   <- validation_errors(result)
  expect_true("Latitude" %in% errs$field)
})

test_that("validate_qibla_data() errors on out-of-range longitude", {
  raw <- expand_raw(make_valid_raw())
  raw$Longitude[1L] <- -200
  result <- validate_qibla_data(raw)
  errs   <- validation_errors(result)
  expect_true("Longitude" %in% errs$field)
})

test_that("validate_qibla_data() warns on azimuth >= 360", {
  raw <- expand_raw(make_valid_raw())
  raw$dir[1L] <- 360
  result <- validate_qibla_data(raw)
  warns  <- validation_warnings(result)
  expect_true("dir" %in% warns$field)
})

test_that("validate_qibla_data() warns on unknown Gibson Classification value", {
  raw <- expand_raw(make_valid_raw())
  raw[["Gibson Classification"]][1L] <- "Medina"
  result <- validate_qibla_data(raw)
  warns  <- validation_warnings(result)
  expect_true(any(grepl("Gibson", warns$field)))
})

test_that("validation_errors() returns empty tibble when no errors", {
  raw    <- expand_raw(make_valid_raw())
  result <- validate_qibla_data(raw)
  errs   <- validation_errors(result)
  expect_s3_class(errs, "tbl_df")
  expect_equal(nrow(errs), 0L)
})

test_that("print.qibla_validation() runs without error", {
  raw    <- expand_raw(make_valid_raw())
  result <- validate_qibla_data(raw)
  expect_no_error(print(result))
})
