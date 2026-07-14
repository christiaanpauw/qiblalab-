## Tests for ggplot2 plot functions in R/plots.R

skip_if_not_installed("ggplot2")

data(gibson_qibla, package = "qiblalab")

# ── plot_rose() ───────────────────────────────────────────────────────────────

test_that("plot_rose: returns a ggplot", {
  p <- plot_rose(gibson_qibla)
  expect_s3_class(p, "ggplot")
})

test_that("plot_rose: builds without error", {
  p <- plot_rose(gibson_qibla)
  expect_no_error(ggplot2::ggplot_build(p))
})

test_that("plot_rose: custom bins accepted", {
  p <- plot_rose(gibson_qibla, bins = 18L)
  expect_s3_class(p, "ggplot")
})

test_that("plot_rose: custom azimuth_col", {
  df <- gibson_qibla
  df$az2 <- df$azimuth
  p <- plot_rose(df, azimuth_col = "az2")
  expect_s3_class(p, "ggplot")
})

test_that("plot_rose: errors when azimuth_col not found", {
  expect_error(plot_rose(gibson_qibla, azimuth_col = "bad_col"), "not found")
})

test_that("plot_rose: title argument applied", {
  p <- plot_rose(gibson_qibla, title = "My title")
  expect_equal(p$labels$title, "My title")
})

# ── plot_residuals() ──────────────────────────────────────────────────────────

test_that("plot_residuals: accepts numeric vector", {
  errs <- rnorm(50, 0, 20)
  p    <- plot_residuals(errs)
  expect_s3_class(p, "ggplot")
  expect_no_error(ggplot2::ggplot_build(p))
})

test_that("plot_residuals: accepts qibla_test_result", {
  h   <- qibla_hypothesis("M", candidate = "mecca", tolerance = 5)
  res <- test_qibla_hypothesis(h, gibson_qibla)
  p   <- plot_residuals(res)
  expect_s3_class(p, "ggplot")
})

test_that("plot_residuals: signed = FALSE uses absolute errors from test result", {
  h   <- qibla_hypothesis("M", candidate = "mecca", tolerance = 5)
  res <- test_qibla_hypothesis(h, gibson_qibla)
  p   <- plot_residuals(res, signed = FALSE)
  expect_s3_class(p, "ggplot")
  expect_no_error(ggplot2::ggplot_build(p))
})

test_that("plot_residuals: NA values dropped silently", {
  errs <- c(1, NA, 3, NA, 5)
  expect_no_error(ggplot2::ggplot_build(plot_residuals(errs)))
})

# ── plot_orientation_time() ───────────────────────────────────────────────────

test_that("plot_orientation_time: returns a ggplot", {
  p <- plot_orientation_time(gibson_qibla)
  expect_s3_class(p, "ggplot")
})

test_that("plot_orientation_time: builds without error", {
  p <- plot_orientation_time(gibson_qibla)
  expect_no_error(ggplot2::ggplot_build(p))
})

test_that("plot_orientation_time: colour_col applied", {
  p <- plot_orientation_time(gibson_qibla, colour_col = "age_group")
  expect_s3_class(p, "ggplot")
  expect_no_error(ggplot2::ggplot_build(p))
})

test_that("plot_orientation_time: errors on missing time_col", {
  expect_error(plot_orientation_time(gibson_qibla, time_col = "bad"), "not found")
})

test_that("plot_orientation_time: errors on missing colour_col", {
  expect_error(
    plot_orientation_time(gibson_qibla, colour_col = "no_such"),
    "not found"
  )
})

# ── plot_sensitivity() ────────────────────────────────────────────────────────

test_that("plot_sensitivity: accepts sensitivity tibble", {
  tbl <- tibble::tibble(tolerance = c(2, 5, 10),
                        proportion = c(0.2, 0.5, 0.8))
  p   <- plot_sensitivity(tbl)
  expect_s3_class(p, "ggplot")
  expect_no_error(ggplot2::ggplot_build(p))
})

test_that("plot_sensitivity: accepts qibla_test_result", {
  h   <- qibla_hypothesis("M", candidate = "mecca", tolerance = 5)
  res <- test_qibla_hypothesis(h, gibson_qibla)
  p   <- plot_sensitivity(res)
  expect_s3_class(p, "ggplot")
  expect_no_error(ggplot2::ggplot_build(p))
})

test_that("plot_sensitivity: sd_proportion ribbon rendered", {
  tbl <- tibble::tibble(tolerance    = c(2, 5, 10),
                        proportion   = c(0.2, 0.5, 0.8),
                        sd_proportion = c(0.05, 0.04, 0.03))
  p   <- plot_sensitivity(tbl)
  expect_s3_class(p, "ggplot")
  expect_no_error(ggplot2::ggplot_build(p))
})

test_that("plot_sensitivity: errors when required columns absent", {
  expect_error(plot_sensitivity(data.frame(x = 1)), "tolerance")
})

# ── plot_candidate_comparison() ───────────────────────────────────────────────

test_that("plot_candidate_comparison: returns a ggplot", {
  cmp <- compare_qibla_hypotheses(gibson_qibla)
  p   <- plot_candidate_comparison(cmp)
  expect_s3_class(p, "ggplot")
  expect_no_error(ggplot2::ggplot_build(p))
})

test_that("plot_candidate_comparison: exclude_ambiguous works", {
  cmp <- compare_qibla_hypotheses(gibson_qibla)
  p   <- plot_candidate_comparison(cmp, exclude_ambiguous = TRUE)
  expect_s3_class(p, "ggplot")
})

test_that("plot_candidate_comparison: errors when nearest_id absent", {
  expect_error(plot_candidate_comparison(gibson_qibla), "nearest_id")
})

# ── plot_cluster_profile() ────────────────────────────────────────────────────

test_that("plot_cluster_profile: returns a ggplot", {
  res <- cluster_qiblas(gibson_qibla, k = 3L, seed = 1)
  p   <- plot_cluster_profile(res)
  expect_s3_class(p, "ggplot")
  expect_no_error(ggplot2::ggplot_build(p))
})

test_that("plot_cluster_profile: custom bins accepted", {
  res <- cluster_qiblas(gibson_qibla, k = 2L, seed = 1)
  p   <- plot_cluster_profile(res, bins = 12L)
  expect_s3_class(p, "ggplot")
})

test_that("plot_cluster_profile: errors on wrong class", {
  expect_error(plot_cluster_profile(list()), "qibla_cluster_result")
})
