## Tests for circular statistics functions

# ── circular_mean() ───────────────────────────────────────────────────────────

test_that("circular_mean: wrap-around near 0/360", {
  expect_equal(circular_mean(c(1, 359)), 0, tolerance = 0.1)
  expect_equal(circular_mean(c(10, 350)), 0, tolerance = 0.5)
})

test_that("circular_mean: cardinal directions", {
  expect_equal(circular_mean(c(0, 0, 0)), 0, tolerance = 1e-10)
  expect_equal(circular_mean(c(90, 90)), 90, tolerance = 1e-10)
  expect_equal(circular_mean(c(180, 180)), 180, tolerance = 1e-10)
  expect_equal(circular_mean(c(270, 270)), 270, tolerance = 1e-10)
})

test_that("circular_mean: result in [0, 360)", {
  x <- runif(50, 0, 360)
  m <- circular_mean(x)
  expect_true(m >= 0 && m < 360)
})

test_that("circular_mean: NA handling", {
  expect_true(is.na(circular_mean(c(NA_real_, NA_real_))))
  expect_equal(circular_mean(c(90, NA_real_), na.rm = TRUE), 90, tolerance = 1e-6)
  expect_true(is.na(circular_mean(c(90, NA_real_), na.rm = FALSE)))
})

test_that("circular_mean: empty vector returns NA", {
  expect_true(is.na(circular_mean(numeric(0))))
})

# ── circular_resultant() ──────────────────────────────────────────────────────

test_that("circular_resultant: perfect concentration = 1", {
  expect_equal(circular_resultant(rep(45, 10)), 1, tolerance = 1e-10)
})

test_that("circular_resultant: antipodal pair = 0", {
  expect_equal(circular_resultant(c(0, 180)), 0, tolerance = 1e-10)
})

test_that("circular_resultant: near-uniform four directions", {
  expect_lt(circular_resultant(c(0, 90, 180, 270)), 0.01)
})

test_that("circular_resultant: result in [0, 1]", {
  x <- runif(100, 0, 360)
  r <- circular_resultant(x)
  expect_true(r >= 0 && r <= 1)
})

test_that("circular_resultant: NA handling", {
  expect_true(is.na(circular_resultant(c(NA_real_, NA_real_))))
  r <- circular_resultant(c(90, NA_real_), na.rm = TRUE)
  expect_equal(r, 1, tolerance = 1e-10)
})

# ── circular_var() ────────────────────────────────────────────────────────────

test_that("circular_var: 1 - rbar identity", {
  x <- runif(30, 0, 360)
  expect_equal(circular_var(x), 1 - circular_resultant(x), tolerance = 1e-12)
})

test_that("circular_var: concentrated data near 0", {
  expect_lt(circular_var(rnorm(50, 180, 2)), 0.01)
})

# ── circular_sd() ─────────────────────────────────────────────────────────────

test_that("circular_sd: perfect concentration = 0", {
  expect_equal(circular_sd(rep(90, 20)), 0, tolerance = 1e-10)
})

test_that("circular_sd: non-negative", {
  expect_gte(circular_sd(runif(50, 0, 360)), 0)
})

test_that("circular_sd: increases with spread", {
  tight <- circular_sd(rnorm(100, 180, 3))
  loose <- circular_sd(runif(100, 0, 360))
  expect_lt(tight, loose)
})

# ── circular_kappa() ─────────────────────────────────────────────────────────

test_that("circular_kappa: monotone in rbar", {
  rbars <- seq(0.1, 0.95, by = 0.05)
  kappas <- vapply(rbars, circular_kappa, numeric(1))
  expect_true(all(diff(kappas) > 0))
})

test_that("circular_kappa: low rbar gives low kappa", {
  expect_lt(circular_kappa(0.1), 1)
})

test_that("circular_kappa: high rbar gives high kappa", {
  expect_gt(circular_kappa(0.9), 5)
})

# ── rayleigh_test() ───────────────────────────────────────────────────────────

test_that("rayleigh_test: concentrated data rejects uniformity", {
  set.seed(1)
  x <- rnorm(100, mean = 180, sd = 5)
  rt <- rayleigh_test(x)
  expect_lt(rt$p.value, 0.001)
})

test_that("rayleigh_test: uniform data does not reject (large n, stable)", {
  # With 2000 draws the expected Rayleigh Z ~ 1, p >> 0.05 with overwhelming probability
  set.seed(42)
  x  <- runif(2000, 0, 360)
  rt <- rayleigh_test(x)
  expect_gt(rt$p.value, 0.01)
})

test_that("rayleigh_test: returns correct structure", {
  rt <- rayleigh_test(c(10, 20, 15, 12, 18))
  expect_named(rt, c("statistic", "rbar", "p.value", "n", "method"))
  expect_true(rt$p.value >= 0 && rt$p.value <= 1)
  expect_equal(rt$n, 5L)
})

test_that("rayleigh_test: statistic = n * rbar^2", {
  x <- c(100, 110, 95, 105)
  rt <- rayleigh_test(x)
  rbar <- circular_resultant(x)
  expect_equal(rt$statistic, length(x) * rbar^2, tolerance = 1e-10)
})

test_that("rayleigh_test: na.rm works", {
  x <- c(100, 110, NA, 95)
  expect_no_error(rayleigh_test(x, na.rm = TRUE))
})

# ── circular_summary() ────────────────────────────────────────────────────────

test_that("circular_summary: returns qibla_circular_summary", {
  s <- circular_summary(c(10, 20, 15), na.rm = TRUE)
  expect_s3_class(s, "qibla_circular_summary")
})

test_that("circular_summary: fields are consistent", {
  x <- c(170, 180, 190, 175, 185)
  s <- circular_summary(x)
  expect_equal(s$n, 5L)
  expect_equal(s$rbar, circular_resultant(x), tolerance = 1e-12)
  expect_equal(s$variance, circular_var(x), tolerance = 1e-12)
  expect_equal(s$mean, circular_mean(x), tolerance = 1e-10)
})

test_that("circular_summary: print runs without error", {
  s <- circular_summary(c(10, 20, 30))
  expect_no_error(print(s))
})

test_that("circular_summary: real azimuth data", {
  data(gibson_qibla, package = "qiblalab")
  s <- circular_summary(gibson_qibla$azimuth, na.rm = TRUE)
  expect_equal(s$n, sum(!is.na(gibson_qibla$azimuth)))
  expect_true(s$rbar > 0 && s$rbar < 1)
})
