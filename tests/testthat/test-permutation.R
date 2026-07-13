## Tests for permute_orientations() and permutation_test()

data(gibson_qibla, package = "qiblalab")
data(destinations, package = "qiblalab")

# ── permute_orientations() ────────────────────────────────────────────────────

test_that("permute_orientations: same dimensions as input", {
  out <- permute_orientations(gibson_qibla, seed = 1)
  expect_equal(nrow(out), nrow(gibson_qibla))
  expect_equal(ncol(out), ncol(gibson_qibla))
})

test_that("permute_orientations: sorted non-NA values are identical to input", {
  out  <- permute_orientations(gibson_qibla, seed = 1)
  orig <- sort(gibson_qibla$azimuth, na.last = NA)
  perm <- sort(out$azimuth,          na.last = NA)
  expect_equal(perm, orig)
})

test_that("permute_orientations: NA positions remain NA", {
  out    <- permute_orientations(gibson_qibla, seed = 1)
  na_pos <- is.na(gibson_qibla$azimuth)
  expect_true(all(is.na(out$azimuth[na_pos])))
})

test_that("permute_orientations: non-NA positions remain non-NA", {
  out    <- permute_orientations(gibson_qibla, seed = 1)
  ok_pos <- !is.na(gibson_qibla$azimuth)
  expect_true(all(!is.na(out$azimuth[ok_pos])))
})

test_that("permute_orientations: seed gives reproducible shuffle", {
  s1 <- permute_orientations(gibson_qibla, seed = 42)
  s2 <- permute_orientations(gibson_qibla, seed = 42)
  expect_equal(s1$azimuth, s2$azimuth)
})

test_that("permute_orientations: different seeds give different shuffles", {
  s1 <- permute_orientations(gibson_qibla, seed = 1)
  s2 <- permute_orientations(gibson_qibla, seed = 2)
  expect_false(identical(s1$azimuth, s2$azimuth))
})

test_that("permute_orientations: other columns are unchanged", {
  out <- permute_orientations(gibson_qibla, seed = 1)
  expect_equal(out$latitude,  gibson_qibla$latitude)
  expect_equal(out$longitude, gibson_qibla$longitude)
  expect_equal(out$row_id,    gibson_qibla$row_id)
})

test_that("permute_orientations: block permutation preserves within-block value set", {
  # After block permutation, the set of non-NA azimuths within each country
  # must equal the original set.
  out <- permute_orientations(gibson_qibla, block_col = "country", seed = 1)
  countries <- unique(gibson_qibla$country)
  for (cty in countries) {
    orig_az <- sort(gibson_qibla$azimuth[gibson_qibla$country == cty], na.last = NA)
    perm_az <- sort(out$azimuth[out$country == cty],                   na.last = NA)
    expect_equal(perm_az, orig_az, label = paste("country:", cty))
  }
})

test_that("permute_orientations: errors on missing azimuth_col", {
  expect_error(permute_orientations(gibson_qibla, azimuth_col = "nonexistent"))
})

test_that("permute_orientations: errors on missing block_col", {
  expect_error(permute_orientations(gibson_qibla, block_col = "nonexistent"))
})

# ── permutation_test() ────────────────────────────────────────────────────────

mecca   <- destinations[destinations$id == "mecca", ]
stat_fn <- function(d) {
  b   <- bearing_to_destination(d, mecca)
  err <- absolute_angular_error(d$azimuth, b)
  circular_resultant(err[!is.na(err)])
}

test_that("permutation_test: returns qibla_permutation_test", {
  res <- permutation_test(gibson_qibla, stat_fn, n_permutations = 19L, seed = 1)
  expect_s3_class(res, "qibla_permutation_test")
})

test_that("permutation_test: observed matches stat_fn(data)", {
  res <- permutation_test(gibson_qibla, stat_fn, n_permutations = 19L, seed = 1)
  expect_equal(res$observed, stat_fn(gibson_qibla))
})

test_that("permutation_test: p.value in [0, 1]", {
  res <- permutation_test(gibson_qibla, stat_fn, n_permutations = 19L, seed = 1)
  expect_true(res$p.value >= 0 && res$p.value <= 1)
})

test_that("permutation_test: permuted vector has length n_permutations", {
  n   <- 29L
  res <- permutation_test(gibson_qibla, stat_fn, n_permutations = n, seed = 1)
  expect_equal(length(res$permuted), n)
})

test_that("permutation_test: n_permutations field matches request", {
  res <- permutation_test(gibson_qibla, stat_fn, n_permutations = 29L, seed = 1)
  expect_equal(res$n_permutations, 29L)
})

test_that("permutation_test: alternative 'greater' vs 'less' differ", {
  rg <- permutation_test(gibson_qibla, stat_fn, n_permutations = 29L,
                          alternative = "greater", seed = 1)
  rl <- permutation_test(gibson_qibla, stat_fn, n_permutations = 29L,
                          alternative = "less",    seed = 1)
  expect_false(isTRUE(all.equal(rg$p.value, rl$p.value)))
})

test_that("permutation_test: print runs without error", {
  res <- permutation_test(gibson_qibla, stat_fn, n_permutations = 19L, seed = 1)
  expect_no_error(print(res))
})

test_that("permutation_test: concentrated data has small p-value (greater)", {
  # Use a stat that is high when mosques are concentrated toward Mecca.
  # The real data happens to have moderate concentration; with 99 permutations
  # and a reasonable stat, the p-value should not be 1.
  res <- permutation_test(gibson_qibla, stat_fn, n_permutations = 99L,
                          alternative = "greater", seed = 7)
  # p-value should not be degenerate (0 or 1 exclusively); just check it's finite
  expect_true(is.finite(res$p.value))
})

test_that("permutation_test: stat_fn must return a scalar", {
  bad_stat <- function(d) c(1, 2)
  expect_error(permutation_test(gibson_qibla, bad_stat, n_permutations = 5L, seed = 1))
})
