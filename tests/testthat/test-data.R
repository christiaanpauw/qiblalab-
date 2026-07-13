test_that("gibson_qibla loads and has expected structure", {
  data(gibson_qibla, package = "qiblalab")
  expect_equal(nrow(gibson_qibla), 160)
  expect_true(all(c("row_id", "mosque_name", "latitude", "longitude",
                     "azimuth", "gibson_classification",
                     "year_ce_min", "year_ce_max") %in% names(gibson_qibla)))
  expect_equal(gibson_qibla$row_id, seq_len(160))
})

test_that("gibson_qibla provenance attribute is complete", {
  data(gibson_qibla, package = "qiblalab")
  prov <- attr(gibson_qibla, "provenance")
  expect_equal(prov$doi, "10.6084/m9.figshare.13570655.v2")
  expect_equal(prov$md5, "af8a9b535d3930d989b395744a85e4df")
  expect_equal(prov$licence, "CC BY 4.0")
})

test_that("gibson_qibla audit log records cleaning actions", {
  data(gibson_qibla, package = "qiblalab")
  log <- attr(gibson_qibla, "audit_log")
  expect_s3_class(log, "data.frame")
  expect_true(nrow(log) > 0)
  expect_true(all(c("column", "original", "corrected", "n_records", "reason") %in% names(log)))
})

test_that("year_ce_min <= year_ce_max for all non-NA rows", {
  data(gibson_qibla, package = "qiblalab")
  ok <- gibson_qibla[!is.na(gibson_qibla$year_ce_min), ]
  expect_true(all(ok$year_ce_min <= ok$year_ce_max))
})

test_that("azimuth values are in [0, 360) where present", {
  data(gibson_qibla, package = "qiblalab")
  az <- gibson_qibla$azimuth[!is.na(gibson_qibla$azimuth)]
  expect_true(all(az >= 0 & az < 360))
})

test_that("gibson_classification contains only expected values", {
  data(gibson_qibla, package = "qiblalab")
  valid <- c("Petra", "Between", "Mecca", "Parallel", "Jerusalem", "Unknown")
  expect_true(all(gibson_qibla$gibson_classification %in% valid))
})
