## Tests for .ari(), .nmi(), .purity() internals and validate_clusters()

data(gibson_qibla, package = "qiblalab")

# ── .ari() ────────────────────────────────────────────────────────────────────

test_that(".ari: identical clusterings give ARI = 1", {
  a <- c(1L, 1L, 2L, 2L, 3L, 3L)
  expect_equal(qiblalab:::.ari(a, a), 1)
})

test_that(".ari: perfectly anti-correlated labels still score 1 (label-invariant)", {
  a <- c(1L, 1L, 2L, 2L)
  b <- c(2L, 2L, 1L, 1L)
  expect_equal(qiblalab:::.ari(a, b), 1)
})

test_that(".ari: random assignments give ARI near 0 in expectation", {
  set.seed(1)
  a <- sample(1:3, 200, replace = TRUE)
  b <- sample(1:3, 200, replace = TRUE)
  expect_lt(abs(qiblalab:::.ari(a, b)), 0.1)
})

test_that(".ari: all same label gives ARI 0 (degenerate collapser)", {
  a <- rep(1L, 10)
  b <- c(1L, 1L, 2L, 2L, 3L, 3L, 3L, 2L, 1L, 1L)
  # One clustering is trivial; ARI should be 0
  expect_equal(qiblalab:::.ari(a, b), 0)
})

test_that(".ari: NA values excluded pairwise", {
  a <- c(1L, 1L, 2L, 2L, NA)
  b <- c(1L, 1L, 2L, 2L, 2L)
  # Same as ARI on first 4 elements
  expect_equal(qiblalab:::.ari(a, b), 1)
})

test_that(".ari: result in [-0.5, 1]", {
  a <- c(1L, 1L, 1L, 2L, 2L, 2L)
  b <- c(1L, 2L, 1L, 2L, 1L, 2L)
  ari <- qiblalab:::.ari(a, b)
  expect_gte(ari, -0.5)
  expect_lte(ari, 1)
})

# ── .nmi() ────────────────────────────────────────────────────────────────────

test_that(".nmi: identical clusterings give NMI = 1", {
  a <- c(1L, 1L, 2L, 2L)
  expect_equal(qiblalab:::.nmi(a, a), 1)
})

test_that(".nmi: independent clusterings give NMI near 0", {
  set.seed(2)
  a <- sample(1:3, 300, replace = TRUE)
  b <- sample(1:5, 300, replace = TRUE)
  expect_lt(qiblalab:::.nmi(a, b), 0.1)
})

test_that(".nmi: result in [0, 1]", {
  set.seed(3)
  a <- sample(1:4, 50, replace = TRUE)
  b <- sample(1:3, 50, replace = TRUE)
  v <- qiblalab:::.nmi(a, b)
  expect_gte(v, 0)
  expect_lte(v, 1)
})

# ── .purity() ─────────────────────────────────────────────────────────────────

test_that(".purity: perfect match gives 1", {
  a <- c(1L, 1L, 2L, 2L)
  b <- c(1L, 1L, 2L, 2L)
  expect_equal(qiblalab:::.purity(a, b), 1)
})

test_that(".purity: all same label gives at most 1", {
  a <- c(1L, 1L, 1L, 1L)
  b <- c(1L, 2L, 3L, 4L)
  expect_lte(qiblalab:::.purity(a, b), 1)
})

test_that(".purity: result in [0, 1]", {
  set.seed(4)
  a <- sample(1:3, 100, replace = TRUE)
  b <- sample(1:4, 100, replace = TRUE)
  v <- qiblalab:::.purity(a, b)
  expect_gte(v, 0)
  expect_lte(v, 1)
})

test_that(".purity: NA excluded", {
  a <- c(1L, 1L, 2L, 2L, NA)
  b <- c(1L, 1L, 2L, NA, 2L)
  # Only rows 1, 2, 3 used (both non-NA)
  expect_equal(qiblalab:::.purity(a, b), 1)
})

# ── validate_clusters() ───────────────────────────────────────────────────────

res3 <- cluster_qiblas(gibson_qibla, k = 3L, seed = 1)

test_that("validate_clusters: returns qibla_cluster_validation", {
  val <- validate_clusters(res3, bootstrap_n = 10L, seed = 1)
  expect_s3_class(val, "qibla_cluster_validation")
})

test_that("validate_clusters: internal section has expected fields", {
  val <- validate_clusters(res3, bootstrap_n = 10L, seed = 1)
  int <- val$internal
  expect_true(all(c("within_dispersion", "mean_within_dispersion",
                    "min_centroid_separation_deg", "separation_ok",
                    "pct_ambiguous") %in% names(int)))
})

test_that("validate_clusters: mean_within_dispersion in [0, 1]", {
  val <- validate_clusters(res3, bootstrap_n = 10L, seed = 1)
  d   <- val$internal$mean_within_dispersion
  expect_true(is.na(d) || (d >= 0 && d <= 1))
})

test_that("validate_clusters: within_dispersion vector has length k", {
  val <- validate_clusters(res3, bootstrap_n = 10L, seed = 1)
  expect_equal(length(val$internal$within_dispersion), res3$k)
})

test_that("validate_clusters: min centroid separation is non-negative", {
  val <- validate_clusters(res3, bootstrap_n = 10L, seed = 1)
  sep <- val$internal$min_centroid_separation_deg
  expect_true(is.na(sep) || sep >= 0)
})

test_that("validate_clusters: stability section has expected fields", {
  val <- validate_clusters(res3, bootstrap_n = 10L, seed = 1)
  st  <- val$stability
  expect_true(all(c("boot_ari_values", "mean_boot_ari", "sd_boot_ari",
                    "n_bootstrap", "stability_ok") %in% names(st)))
})

test_that("validate_clusters: n_bootstrap matches request (minus failures)", {
  val <- validate_clusters(res3, bootstrap_n = 15L, seed = 1)
  expect_lte(val$stability$n_bootstrap, 15L)
  expect_gte(val$stability$n_bootstrap, 1L)
})

test_that("validate_clusters: mean_boot_ari in [-0.5, 1]", {
  val <- validate_clusters(res3, bootstrap_n = 15L, seed = 1)
  m   <- val$stability$mean_boot_ari
  expect_true(is.na(m) || (m >= -0.5 && m <= 1))
})

test_that("validate_clusters: boot_ari_values each in [-0.5, 1]", {
  val <- validate_clusters(res3, bootstrap_n = 15L, seed = 1)
  v   <- val$stability$boot_ari_values
  expect_true(all(v >= -0.5 & v <= 1))
})

test_that("validate_clusters: external validation with age_group", {
  val <- validate_clusters(res3, external = "age_group",
                           bootstrap_n = 10L, seed = 1)
  ev  <- val$external[["age_group"]]
  expect_false(is.null(ev))
  expect_true(all(c("ari", "nmi", "purity") %in% names(ev)))
})

test_that("validate_clusters: external ARI in [-0.5, 1]", {
  val <- validate_clusters(res3, external = "age_group",
                           bootstrap_n = 10L, seed = 1)
  ari <- val$external[["age_group"]]$ari
  expect_true(is.na(ari) || (ari >= -0.5 && ari <= 1))
})

test_that("validate_clusters: external validation with gibson_classification (post-hoc)", {
  val <- validate_clusters(res3,
                           external = "gibson_classification",
                           bootstrap_n = 10L, seed = 1)
  ev  <- val$external[["gibson_classification"]]
  expect_false(is.null(ev))
  expect_named(ev, c("column", "ari", "nmi", "purity"))
})

test_that("validate_clusters: missing external column skipped with warning", {
  expect_warning(
    validate_clusters(res3, external = "nonexistent_col",
                      bootstrap_n = 10L, seed = 1),
    "not found"
  )
})

test_that("validate_clusters: predictive section present when n >= 60", {
  val <- validate_clusters(res3, bootstrap_n = 10L,
                           predictive_splits = 5L, seed = 1)
  expect_false(is.null(val$predictive))
  expect_true(all(c("mean_angular_error", "sd_angular_error",
                    "n_splits") %in% names(val$predictive)))
})

test_that("validate_clusters: predictive mean error in [0, 180]", {
  val <- validate_clusters(res3, bootstrap_n = 10L,
                           predictive_splits = 5L, seed = 1)
  err <- val$predictive$mean_angular_error
  expect_true(is.na(err) || (err >= 0 && err <= 180))
})

test_that("validate_clusters: predictive NULL when n < 60", {
  small_df  <- gibson_qibla[!is.na(gibson_qibla$azimuth), ][1:30, ]
  res_small <- cluster_qiblas(small_df, k = 2L, seed = 1)
  val_small <- validate_clusters(res_small, bootstrap_n = 5L, seed = 1)
  expect_null(val_small$predictive)
})

test_that("validate_clusters: print runs without error", {
  val <- validate_clusters(res3,
                           external = c("age_group", "gibson_classification"),
                           bootstrap_n = 10L, predictive_splits = 5L, seed = 1)
  expect_no_error(print(val))
})

test_that("validate_clusters: seed gives reproducible mean_boot_ari", {
  v1 <- validate_clusters(res3, bootstrap_n = 10L, seed = 99)
  v2 <- validate_clusters(res3, bootstrap_n = 10L, seed = 99)
  expect_equal(v1$stability$mean_boot_ari, v2$stability$mean_boot_ari)
})
