test_that("download_qibla_data() returns path to existing cached file without re-downloading", {
  # Use the data-raw copy so no network needed
  src <- testthat::test_path("../../data-raw/Mosques_Jan2021.xlsx")
  skip_if_not(file.exists(src), "data-raw XLSX not found")

  tmp <- withr::local_tempdir()
  file.copy(src, file.path(tmp, "Mosques_Jan2021.xlsx"))

  result <- download_qibla_data(dir = tmp, force = FALSE)
  expect_equal(result, file.path(tmp, "Mosques_Jan2021.xlsx"))
  expect_true(file.exists(result))
})

test_that("download_qibla_data() detects a corrupt cached file", {
  tmp <- withr::local_tempdir()
  writeLines("corrupt", file.path(tmp, "Mosques_Jan2021.xlsx"))

  skip_if_offline()
  # Corrupt file triggers re-download; if network unavailable it may error —
  # either outcome is acceptable, but no silent pass-through of bad data.
  result <- tryCatch(
    download_qibla_data(dir = tmp, force = FALSE),
    error = function(e) e
  )
  if (inherits(result, "error")) {
    expect_match(conditionMessage(result), "MD5|download", ignore.case = TRUE)
  } else {
    # Re-download succeeded — verify checksum
    expect_equal(
      as.character(tools::md5sum(result)),
      "af8a9b535d3930d989b395744a85e4df"
    )
  }
})

test_that("download_qibla_data() rejects invalid dir argument", {
  expect_error(download_qibla_data(dir = 123))
  expect_error(download_qibla_data(force = "yes"))
})
