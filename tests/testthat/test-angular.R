## Tests for angular_difference(), absolute_angular_error(), signed_angular_error()

# ── angular_difference() ──────────────────────────────────────────────────────

test_that("angular_difference: basic cases", {
  expect_equal(angular_difference(10,  350),  20)
  expect_equal(angular_difference(350, 10),  -20)
  expect_equal(angular_difference(90,   0),   90)
  expect_equal(angular_difference(0,   90),  -90)
})

test_that("angular_difference: result is in [-180, 180]", {
  a <- seq(0, 359, by = 13)
  b <- seq(5, 364, by = 13)
  d <- angular_difference(a, b)
  expect_true(all(d >= -180 & d <= 180))
})

test_that("angular_difference: wrap at 0/360 boundary", {
  expect_equal(angular_difference(1, 359), 2)
  expect_equal(angular_difference(359, 1), -2)
})

test_that("angular_difference: opposite bearings give +/-180", {
  d <- angular_difference(180, 0)
  expect_true(abs(d) == 180)
})

test_that("angular_difference: NA propagation", {
  expect_true(is.na(angular_difference(NA_real_, 0)))
  expect_true(is.na(angular_difference(0, NA_real_)))
})

test_that("angular_difference: vectorised", {
  a <- c(10, 350, 90)
  b <- c(350, 10, 270)
  d <- angular_difference(a, b)
  expect_equal(d, c(20, -20, -180))
})

test_that("angular_difference: handles values outside [0, 360)", {
  # 370 is equivalent to 10
  expect_equal(angular_difference(370, 350), angular_difference(10, 350))
  expect_equal(angular_difference(-10, 10),  angular_difference(350, 10))
})

# ── absolute_angular_error() ──────────────────────────────────────────────────

test_that("absolute_angular_error: always non-negative", {
  a <- seq(0, 350, by = 10)
  b <- seq(5, 355, by = 10)
  expect_true(all(absolute_angular_error(a, b) >= 0))
})

test_that("absolute_angular_error: always <= 180", {
  a <- seq(0, 350, by = 7)
  b <- seq(0, 350, by = 11)
  expect_true(all(absolute_angular_error(a, b) <= 180))
})

test_that("absolute_angular_error: symmetric", {
  a <- c(10, 175, 350, 45)
  b <- c(350, 5, 10, 315)
  expect_equal(absolute_angular_error(a, b), absolute_angular_error(b, a))
})

test_that("absolute_angular_error: known values", {
  expect_equal(absolute_angular_error(175, 180), 5)
  expect_equal(absolute_angular_error(5,   355), 10)
  expect_equal(absolute_angular_error(0,   180), 180)
  expect_equal(absolute_angular_error(1,   359), 2)
})

test_that("absolute_angular_error: NA propagation", {
  expect_true(is.na(absolute_angular_error(NA_real_, 0)))
})

# ── signed_angular_error() ────────────────────────────────────────────────────

test_that("signed_angular_error: positive = clockwise overshoot", {
  expect_gt(signed_angular_error(185, 180), 0)
})

test_that("signed_angular_error: negative = anticlockwise undershoot", {
  expect_lt(signed_angular_error(175, 180), 0)
})

test_that("signed_angular_error: magnitude matches absolute_angular_error", {
  obs  <- c(5, 355, 100, 260)
  theo <- c(355, 5, 90, 270)
  expect_equal(abs(signed_angular_error(obs, theo)),
               absolute_angular_error(obs, theo))
})

test_that("signed_angular_error: result in [-180, 180]", {
  obs  <- seq(0, 350, by = 10)
  theo <- seq(5, 355, by = 10)
  d    <- signed_angular_error(obs, theo)
  expect_true(all(d >= -180 & d <= 180))
})
