local_xlsx_path <- function() {
  testthat::test_path("../../data-raw/Mosques_Jan2021.xlsx")
}

# ── parse_year_ce (internal) ──────────────────────────────────────────────────

test_that("parse_year_ce handles exact integer years", {
  result <- qiblalab:::parse_year_ce(c("622", "705", "800"))
  expect_equal(result$min, c(622L, 705L, 800L))
  expect_equal(result$max, c(622L, 705L, 800L))
})

test_that("parse_year_ce handles century ranges", {
  result <- qiblalab:::parse_year_ce("700-799")
  expect_equal(result$min, 700L)
  expect_equal(result$max, 799L)
})

test_that("parse_year_ce returns NA for 'unknown'", {
  result <- qiblalab:::parse_year_ce(c("unknown", "Unknown", NA_character_))
  expect_true(all(is.na(result$min)))
  expect_true(all(is.na(result$max)))
})

test_that("parse_year_ce gives min <= max for all valid inputs", {
  inputs <- c("622", "700-799", "300-399", "800", "1195", "600-699")
  result <- qiblalab:::parse_year_ce(inputs)
  ok     <- !is.na(result$min)
  expect_true(all(result$min[ok] <= result$max[ok]))
})

# ── prepare_qibla_data() ──────────────────────────────────────────────────────

test_that("prepare_qibla_data() output matches shipped gibson_qibla", {
  skip_if_not_installed("readxl")
  path <- local_xlsx_path()
  skip_if_not(file.exists(path), "data-raw XLSX not found")

  raw     <- import_qibla_data(path = path)
  cleaned <- prepare_qibla_data(raw)
  data(gibson_qibla, package = "qiblalab", envir = environment())

  # Same dimensions and column names
  expect_equal(dim(cleaned), dim(gibson_qibla))
  expect_equal(names(cleaned), names(gibson_qibla))

  # Key columns identical (excluding timestamp-carrying provenance)
  cols <- c("row_id", "mosque_name", "latitude", "longitude",
            "azimuth", "gibson_classification", "year_ce_min", "year_ce_max",
            "country", "age_group")
  for (col in cols) {
    expect_equal(cleaned[[col]], gibson_qibla[[col]], label = col)
  }
})

test_that("prepare_qibla_data() renames 'dir' to 'azimuth'", {
  skip_if_not_installed("readxl")
  path <- local_xlsx_path()
  skip_if_not(file.exists(path), "data-raw XLSX not found")

  raw     <- import_qibla_data(path = path)
  cleaned <- prepare_qibla_data(raw)
  expect_true("azimuth" %in% names(cleaned))
  expect_false("dir"    %in% names(cleaned))
})

test_that("prepare_qibla_data() standardises 'unknown' -> 'Unknown' in classification", {
  skip_if_not_installed("readxl")
  path <- local_xlsx_path()
  skip_if_not(file.exists(path), "data-raw XLSX not found")

  raw     <- import_qibla_data(path = path)
  cleaned <- prepare_qibla_data(raw)
  expect_false("unknown" %in% cleaned$gibson_classification)
  expect_true("Unknown"  %in% cleaned$gibson_classification)
})

test_that("prepare_qibla_data() corrects country spelling errors", {
  skip_if_not_installed("readxl")
  path <- local_xlsx_path()
  skip_if_not(file.exists(path), "data-raw XLSX not found")

  raw     <- import_qibla_data(path = path)
  cleaned <- prepare_qibla_data(raw)
  expect_false("iran"        %in% cleaned$country)
  expect_false("Somolia"     %in% cleaned$country)
  expect_false("Uzbeckistan" %in% cleaned$country)
  expect_false("Lybia"       %in% cleaned$country)
  # Source values preserved
  expect_true("Somolia"  %in% cleaned$country_source)
  expect_true("Lybia"    %in% cleaned$country_source)
})

test_that("prepare_qibla_data() attaches a non-empty audit log", {
  skip_if_not_installed("readxl")
  path <- local_xlsx_path()
  skip_if_not(file.exists(path), "data-raw XLSX not found")

  raw     <- import_qibla_data(path = path)
  cleaned <- prepare_qibla_data(raw)
  log     <- attr(cleaned, "audit_log")
  expect_s3_class(log, "data.frame")
  expect_gt(nrow(log), 0L)
  expect_true(all(c("column", "original", "corrected", "n_records", "reason") %in% names(log)))
})

test_that("prepare_qibla_data() errors when required columns are missing", {
  bad <- data.frame(x = 1)
  expect_error(prepare_qibla_data(bad), class = "rlang_error")
})
