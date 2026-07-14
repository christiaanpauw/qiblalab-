## Tests for map functions in R/maps.R

skip_if_not_installed("ggplot2")

data(gibson_qibla, package = "qiblalab")

# ── map_mosques() ─────────────────────────────────────────────────────────────

test_that("map_mosques: returns a ggplot", {
  p <- map_mosques(gibson_qibla)
  expect_s3_class(p, "ggplot")
})

test_that("map_mosques: builds without error", {
  p <- map_mosques(gibson_qibla)
  expect_no_error(ggplot2::ggplot_build(p))
})

test_that("map_mosques: colour_col applied", {
  p <- map_mosques(gibson_qibla, colour_col = "age_group")
  expect_s3_class(p, "ggplot")
  expect_no_error(ggplot2::ggplot_build(p))
})

test_that("map_mosques: errors when lat_col not found", {
  expect_error(map_mosques(gibson_qibla, lat_col = "bad"), "not found")
})

test_that("map_mosques: errors when lon_col not found", {
  expect_error(map_mosques(gibson_qibla, lon_col = "bad"), "not found")
})

test_that("map_mosques: errors when colour_col not found", {
  expect_error(map_mosques(gibson_qibla, colour_col = "bad"), "not found")
})

test_that("map_mosques: title argument applied", {
  p <- map_mosques(gibson_qibla, title = "My map")
  expect_equal(p$labels$title, "My map")
})

# ── map_clusters() ────────────────────────────────────────────────────────────

test_that("map_clusters: returns a ggplot", {
  res <- cluster_qiblas(gibson_qibla, k = 3L, seed = 1)
  p   <- map_clusters(res)
  expect_s3_class(p, "ggplot")
})

test_that("map_clusters: builds without error", {
  res <- cluster_qiblas(gibson_qibla, k = 3L, seed = 1)
  p   <- map_clusters(res)
  expect_no_error(ggplot2::ggplot_build(p))
})

test_that("map_clusters: errors on wrong class", {
  expect_error(map_clusters(list()), "qibla_cluster_result")
})

test_that("map_clusters: errors when lat_col missing from data", {
  res <- cluster_qiblas(gibson_qibla, k = 2L, seed = 1)
  expect_error(map_clusters(res, lat_col = "bad"), "not found")
})
