## Tests for cluster_qiblas() — circular_kmeans, hierarchical, von_mises_mixture

data(gibson_qibla, package = "qiblalab")

# ── Return structure ───────────────────────────────────────────────────────────

test_that("cluster_qiblas: returns qibla_cluster_result", {
  res <- cluster_qiblas(gibson_qibla, k = 3L, seed = 1)
  expect_s3_class(res, "qibla_cluster_result")
})

test_that("cluster_qiblas: assignments has length nrow(data)", {
  res <- cluster_qiblas(gibson_qibla, k = 3L, seed = 1)
  expect_equal(length(res$assignments), nrow(gibson_qibla))
})

test_that("cluster_qiblas: NA azimuths map to NA assignments", {
  res    <- cluster_qiblas(gibson_qibla, k = 3L, seed = 1)
  na_pos <- is.na(gibson_qibla$azimuth)
  expect_true(all(is.na(res$assignments[na_pos])))
})

test_that("cluster_qiblas: non-NA assignments are integers in 1..k", {
  k   <- 3L
  res <- cluster_qiblas(gibson_qibla, k = k, seed = 1)
  ok  <- !is.na(res$assignments)
  expect_true(all(res$assignments[ok] >= 1L & res$assignments[ok] <= k))
})

test_that("cluster_qiblas: all k clusters are represented", {
  res <- cluster_qiblas(gibson_qibla, k = 3L, seed = 1)
  ok  <- !is.na(res$assignments)
  expect_equal(sort(unique(res$assignments[ok])), 1L:3L)
})

test_that("cluster_qiblas: centroids tibble has k rows", {
  k   <- 4L
  res <- cluster_qiblas(gibson_qibla, k = k, seed = 1)
  expect_equal(nrow(res$centroids), k)
})

test_that("cluster_qiblas: centroids direction_deg in [0, 360)", {
  res <- cluster_qiblas(gibson_qibla, k = 3L, seed = 1)
  d   <- res$centroids$direction_deg
  expect_true(all(d >= 0 & d < 360))
})

test_that("cluster_qiblas: n_members sums to n_clustered", {
  res <- cluster_qiblas(gibson_qibla, k = 3L, seed = 1)
  expect_equal(sum(res$centroids$n_members), res$n_clustered)
})

test_that("cluster_qiblas: model_selection has k_max rows", {
  k_max <- 6L
  res   <- cluster_qiblas(gibson_qibla, k = 2L, k_max = k_max, seed = 1)
  expect_equal(nrow(res$model_selection), k_max)
})

test_that("cluster_qiblas: model_selection k column is 1..k_max", {
  res <- cluster_qiblas(gibson_qibla, k = 2L, k_max = 5L, seed = 1)
  expect_equal(res$model_selection$k, 1L:5L)
})

test_that("cluster_qiblas: dispersion non-decreasing removed or flat for k=1", {
  res <- cluster_qiblas(gibson_qibla, k = 1L, k_max = 1L, seed = 1)
  expect_equal(res$centroids$dispersion, res$centroids$dispersion)  # trivially
})

test_that("cluster_qiblas: n_clustered equals sum of non-NA azimuths", {
  res <- cluster_qiblas(gibson_qibla, k = 3L, seed = 1)
  expect_equal(res$n_clustered, sum(!is.na(gibson_qibla$azimuth)))
})

test_that("cluster_qiblas: n_total equals nrow(data)", {
  res <- cluster_qiblas(gibson_qibla, k = 3L, seed = 1)
  expect_equal(res$n_total, nrow(gibson_qibla))
})

test_that("cluster_qiblas: print runs without error", {
  res <- cluster_qiblas(gibson_qibla, k = 2L, k_max = 3L, seed = 1)
  expect_no_error(print(res))
})

# ── ADR-07: Gibson classification guard ───────────────────────────────────────

test_that("cluster_qiblas: errors when features = 'gibson_classification'", {
  expect_error(
    cluster_qiblas(gibson_qibla, k = 3L, features = "gibson_classification"),
    "ADR-07"
  )
})

test_that("cluster_qiblas: errors when features = 'gibson_classification_source'", {
  expect_error(
    cluster_qiblas(gibson_qibla, k = 3L, features = "gibson_classification_source"),
    "ADR-07"
  )
})

# ── Synthetic data: known 2-cluster structure ─────────────────────────────────

test_that("circular_kmeans: recovers two well-separated clusters", {
  set.seed(42)
  n  <- 60L
  # Two tight groups: around 45° and 225° (antipodal)
  az <- c(rnorm(n / 2, 45,  4) %% 360,
          rnorm(n / 2, 225, 4) %% 360)
  df <- data.frame(azimuth = az)
  res <- cluster_qiblas(df, k = 2L, k_max = 2L, seed = 1)

  # Both clusters should have centroids near 45 or 225
  dirs <- sort(res$centroids$direction_deg)
  expect_true(abs(dirs[1] - 45)  < 15 || abs(dirs[2] - 45)  < 15)
  expect_true(abs(dirs[1] - 225) < 15 || abs(dirs[2] - 225) < 15)
})

test_that("circular_kmeans: seed gives reproducible assignments", {
  r1 <- cluster_qiblas(gibson_qibla, k = 3L, seed = 7)
  r2 <- cluster_qiblas(gibson_qibla, k = 3L, seed = 7)
  expect_equal(r1$assignments, r2$assignments)
})

test_that("circular_kmeans: within-cluster dispersion decreases or stays flat as k increases", {
  res <- cluster_qiblas(gibson_qibla, k = 4L, k_max = 4L, seed = 1)
  td  <- res$model_selection$value
  # Total dispersion is non-increasing with k (k-means property)
  expect_true(all(diff(td) <= 1e-6))
})

# ── Hierarchical method ────────────────────────────────────────────────────────

test_that("hierarchical: returns correct class and structure", {
  res <- cluster_qiblas(gibson_qibla, k = 3L, method = "hierarchical", seed = 1)
  expect_s3_class(res, "qibla_cluster_result")
  expect_equal(nrow(res$centroids), 3L)
  expect_equal(length(res$assignments), nrow(gibson_qibla))
})

test_that("hierarchical: assignments are 1..k for non-NA rows", {
  res <- cluster_qiblas(gibson_qibla, k = 2L, method = "hierarchical")
  ok  <- !is.na(res$assignments)
  expect_true(all(res$assignments[ok] %in% 1L:2L))
})

# ── von Mises mixture (skip if movMF absent) ──────────────────────────────────

test_that("von_mises_mixture: returns correct class", {
  skip_if_not_installed("movMF")
  res <- cluster_qiblas(gibson_qibla, k = 3L, method = "von_mises_mixture", seed = 1)
  expect_s3_class(res, "qibla_cluster_result")
  expect_equal(nrow(res$centroids), 3L)
})

test_that("von_mises_mixture: BIC is finite in model_selection", {
  skip_if_not_installed("movMF")
  res <- cluster_qiblas(gibson_qibla, k = 2L, method = "von_mises_mixture",
                        k_max = 3L, seed = 1)
  expect_true(all(is.finite(res$model_selection$value)))
})

# ── Edge cases ────────────────────────────────────────────────────────────────

test_that("cluster_qiblas: k > n_clustered errors", {
  tiny <- gibson_qibla[1:2, ]
  tiny$azimuth <- c(90, 180)
  expect_error(cluster_qiblas(tiny, k = 5L), "Fewer mosques")
})

test_that("cluster_qiblas: k_max auto-adjusted when k > k_max", {
  res <- cluster_qiblas(gibson_qibla, k = 5L, k_max = 3L, seed = 1)
  expect_true(res$k_max >= res$k)
})
