local_xlsx_path <- function() {
  testthat::test_path("../../data-raw/Mosques_Jan2021.xlsx")
}

test_that("import_qibla_data() returns a data frame with the expected shape", {
  skip_if_not_installed("readxl")
  path <- local_xlsx_path()
  skip_if_not(file.exists(path), "data-raw XLSX not found")

  raw <- import_qibla_data(path = path)
  expect_s3_class(raw, "data.frame")
  expect_equal(nrow(raw), 160L)
  # 12 original columns + row_id
  expect_equal(ncol(raw), 13L)
})

test_that("import_qibla_data() adds row_id as first column with values 1:160", {
  skip_if_not_installed("readxl")
  path <- local_xlsx_path()
  skip_if_not(file.exists(path), "data-raw XLSX not found")

  raw <- import_qibla_data(path = path)
  expect_equal(names(raw)[1L], "row_id")
  expect_equal(raw$row_id, seq_len(160L))
})

test_that("import_qibla_data() preserves all original Gibson column names", {
  skip_if_not_installed("readxl")
  path <- local_xlsx_path()
  skip_if_not(file.exists(path), "data-raw XLSX not found")

  raw <- import_qibla_data(path = path)
  expect_true(all(c(
    "Gibson Classification", "Year CE", "Year AH", "age_group",
    "City", "Country", "Mosque Name", "Rebuilt",
    "Latitude", "Longitude", "dir", "Website Link"
  ) %in% names(raw)))
})

test_that("import_qibla_data() attaches provenance attribute with required fields", {
  skip_if_not_installed("readxl")
  path <- local_xlsx_path()
  skip_if_not(file.exists(path), "data-raw XLSX not found")

  raw  <- import_qibla_data(path = path)
  prov <- attr(raw, "provenance")
  expect_type(prov, "list")
  expect_equal(prov$doi,     "10.6084/m9.figshare.13570655.v2")
  expect_equal(prov$licence, "CC BY 4.0")
  expect_equal(prov$md5,     "af8a9b535d3930d989b395744a85e4df")
  expect_true("citation"    %in% names(prov))
  expect_true("imported_at" %in% names(prov))
})

test_that("import_qibla_data() errors on a non-existent path", {
  skip_if_not_installed("readxl")
  expect_error(import_qibla_data(path = "/no/such/file.xlsx"))
})
