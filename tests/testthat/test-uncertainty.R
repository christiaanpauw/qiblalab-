## Tests for define_uncertainty(), sample_qibla_dataset(), run_sensitivity_analysis()

data(gibson_qibla, package = "qiblalab")

# ── define_uncertainty() ──────────────────────────────────────────────────────

test_that("define_uncertainty returns qibla_uncertainty", {
  u <- define_uncertainty()
  expect_s3_class(u, "qibla_uncertainty")
})

test_that("define_uncertainty stores fields correctly", {
  u <- define_uncertainty(coord_sigma = 0.01, date_sigma = 10, azimuth_sigma = 2)
  expect_equal(u$coord_sigma,   0.01)
  expect_equal(u$date_sigma,    10)
  expect_equal(u$azimuth_sigma, 2)
})

test_that("define_uncertainty errors on negative sigma", {
  expect_error(define_uncertainty(coord_sigma   = -1))
  expect_error(define_uncertainty(date_sigma    = -0.1))
  expect_error(define_uncertainty(azimuth_sigma = -5))
})

test_that("define_uncertainty: print runs without error", {
  u <- define_uncertainty(coord_sigma = 0.1)
  expect_no_error(print(u))
})

# ── sample_qibla_dataset() ────────────────────────────────────────────────────

test_that("sample_qibla_dataset returns tibble with same dimensions", {
  u <- define_uncertainty()
  s <- sample_qibla_dataset(gibson_qibla, u, seed = 1)
  expect_equal(nrow(s), nrow(gibson_qibla))
  expect_equal(ncol(s), ncol(gibson_qibla))
})

test_that("sample_qibla_dataset: zero sigmas leave data unchanged", {
  u <- define_uncertainty(coord_sigma = 0, date_sigma = 0, azimuth_sigma = 0)
  s <- sample_qibla_dataset(gibson_qibla, u, seed = 1)
  expect_equal(s$latitude,  gibson_qibla$latitude)
  expect_equal(s$longitude, gibson_qibla$longitude)
  expect_equal(s$azimuth,   gibson_qibla$azimuth)
})

test_that("sample_qibla_dataset: azimuth_sigma>0 changes azimuth values", {
  u <- define_uncertainty(azimuth_sigma = 5)
  s <- sample_qibla_dataset(gibson_qibla, u, seed = 1)
  has_az <- !is.na(gibson_qibla$azimuth)
  expect_false(identical(s$azimuth[has_az], gibson_qibla$azimuth[has_az]))
})

test_that("sample_qibla_dataset: azimuths stay in [0, 360)", {
  u   <- define_uncertainty(azimuth_sigma = 30)
  s   <- sample_qibla_dataset(gibson_qibla, u, seed = 42)
  ok  <- !is.na(s$azimuth)
  expect_true(all(s$azimuth[ok] >= 0 & s$azimuth[ok] < 360))
})

test_that("sample_qibla_dataset: coord_sigma>0 changes coordinates", {
  u <- define_uncertainty(coord_sigma = 0.5)
  s <- sample_qibla_dataset(gibson_qibla, u, seed = 1)
  expect_false(identical(s$latitude,  gibson_qibla$latitude))
  expect_false(identical(s$longitude, gibson_qibla$longitude))
})

test_that("sample_qibla_dataset: latitude stays in [-90, 90]", {
  u <- define_uncertainty(coord_sigma = 5)
  s <- sample_qibla_dataset(gibson_qibla, u, seed = 1)
  expect_true(all(s$latitude >= -90 & s$latitude <= 90))
})

test_that("sample_qibla_dataset: longitude stays in (-180, 180]", {
  u <- define_uncertainty(coord_sigma = 5)
  s <- sample_qibla_dataset(gibson_qibla, u, seed = 1)
  expect_true(all(s$longitude >= -180 & s$longitude <= 180))
})

test_that("sample_qibla_dataset: NA azimuths remain NA", {
  u      <- define_uncertainty(azimuth_sigma = 10)
  s      <- sample_qibla_dataset(gibson_qibla, u, seed = 1)
  na_pos <- is.na(gibson_qibla$azimuth)
  expect_true(all(is.na(s$azimuth[na_pos])))
})

test_that("sample_qibla_dataset: year_ce character column unchanged", {
  u <- define_uncertainty(date_sigma = 20)
  s <- sample_qibla_dataset(gibson_qibla, u, seed = 1)
  expect_equal(s$year_ce, gibson_qibla$year_ce)
})

test_that("sample_qibla_dataset: date perturbation sets min==max", {
  u <- define_uncertainty(date_sigma = 10)
  s <- sample_qibla_dataset(gibson_qibla, u, seed = 1)
  has <- !is.na(s$year_ce_min) & !is.na(s$year_ce_max)
  expect_true(all(s$year_ce_min[has] == s$year_ce_max[has]))
})

test_that("sample_qibla_dataset: seed gives reproducible results", {
  u  <- define_uncertainty(coord_sigma = 0.1, azimuth_sigma = 3)
  s1 <- sample_qibla_dataset(gibson_qibla, u, seed = 99)
  s2 <- sample_qibla_dataset(gibson_qibla, u, seed = 99)
  expect_equal(s1$azimuth,  s2$azimuth)
  expect_equal(s1$latitude, s2$latitude)
})

test_that("sample_qibla_dataset: different seeds give different results", {
  u  <- define_uncertainty(azimuth_sigma = 3)
  s1 <- sample_qibla_dataset(gibson_qibla, u, seed = 1)
  s2 <- sample_qibla_dataset(gibson_qibla, u, seed = 2)
  has_az <- !is.na(gibson_qibla$azimuth)
  expect_false(identical(s1$azimuth[has_az], s2$azimuth[has_az]))
})

# ── run_sensitivity_analysis() ────────────────────────────────────────────────

test_that("run_sensitivity_analysis: tolerance mode returns tibble with expected columns", {
  h   <- qibla_hypothesis("Test", candidate = "petra", tolerance = 5)
  out <- run_sensitivity_analysis(gibson_qibla, h)
  expect_s3_class(out, "tbl_df")
  expect_named(out, c("tolerance", "n_consistent", "proportion", "binom_p"))
})

test_that("run_sensitivity_analysis: default tolerances are c(2, 5, 10)", {
  h   <- qibla_hypothesis("Test", candidate = "petra", tolerance = 5)
  out <- run_sensitivity_analysis(gibson_qibla, h)
  expect_equal(out$tolerance, c(2, 5, 10))
})

test_that("run_sensitivity_analysis: tolerances are sorted", {
  h   <- qibla_hypothesis("Test", candidate = "petra", tolerance = 5)
  out <- run_sensitivity_analysis(gibson_qibla, h, tolerances = c(10, 2, 5))
  expect_equal(out$tolerance, c(2, 5, 10))
})

test_that("run_sensitivity_analysis: proportions non-decreasing with tolerance", {
  h   <- qibla_hypothesis("Test", candidate = "mecca", tolerance = 10)
  out <- run_sensitivity_analysis(gibson_qibla, h, tolerances = c(2, 5, 10, 20))
  expect_true(all(diff(out$proportion) >= 0))
})

test_that("run_sensitivity_analysis: proportion in [0, 1]", {
  h   <- qibla_hypothesis("Test", candidate = "petra", tolerance = 5)
  out <- run_sensitivity_analysis(gibson_qibla, h)
  expect_true(all(out$proportion >= 0 & out$proportion <= 1))
})

test_that("run_sensitivity_analysis: probabilistic mode returns sd columns", {
  h   <- qibla_hypothesis("Test", candidate = "petra", tolerance = 5)
  u   <- define_uncertainty(azimuth_sigma = 3)
  out <- run_sensitivity_analysis(gibson_qibla, h, mode = "probabilistic",
                                  uncertainty = u, n_samples = 10L, seed = 1)
  expect_s3_class(out, "tbl_df")
  expect_true(all(c("sd_proportion", "sd_binom_p", "n_samples") %in% names(out)))
})

test_that("run_sensitivity_analysis: probabilistic mode errors without uncertainty", {
  h <- qibla_hypothesis("Test", candidate = "petra", tolerance = 5)
  expect_error(
    run_sensitivity_analysis(gibson_qibla, h, mode = "probabilistic"),
    class = "error"
  )
})

test_that("run_sensitivity_analysis: probabilistic proportions in [0, 1]", {
  h   <- qibla_hypothesis("Test", candidate = "mecca", tolerance = 5)
  u   <- define_uncertainty(azimuth_sigma = 5)
  out <- run_sensitivity_analysis(gibson_qibla, h, mode = "probabilistic",
                                  uncertainty = u, n_samples = 10L, seed = 2)
  expect_true(all(out$proportion >= 0 & out$proportion <= 1))
})
