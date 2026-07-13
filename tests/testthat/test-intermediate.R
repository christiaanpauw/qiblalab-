## Tests for intermediate_bearing() and infer_latent_destination()

data(gibson_qibla, package = "qiblalab")
data(destinations, package = "qiblalab")
mecca <- destinations[destinations$id == "mecca", ]
petra <- destinations[destinations$id == "petra", ]

# ── intermediate_bearing: circular_midpoint ───────────────────────────────────

test_that("circular_midpoint returns one value per mosque", {
  out <- intermediate_bearing(gibson_qibla, dest1 = mecca, dest2 = petra)
  expect_length(out, nrow(gibson_qibla))
})

test_that("circular_midpoint is in [0, 360)", {
  out <- intermediate_bearing(gibson_qibla, dest1 = mecca, dest2 = petra)
  ok  <- !is.na(out)
  expect_true(all(out[ok] >= 0 & out[ok] < 360))
})

test_that("circular_midpoint equals bearing to dest1 when both dests identical", {
  out_mid <- intermediate_bearing(gibson_qibla, dest1 = mecca, dest2 = mecca)
  out_ref <- bearing_to_destination(gibson_qibla, mecca)
  expect_equal(out_mid, out_ref, tolerance = 1e-8)
})

test_that("circular_midpoint weight=0.5 equals weighted method weight=0.5", {
  mid <- intermediate_bearing(gibson_qibla, method = "circular_midpoint",
                               dest1 = mecca, dest2 = petra)
  wt  <- intermediate_bearing(gibson_qibla, method = "weighted",
                               dest1 = mecca, dest2 = petra, weight = 0.5)
  expect_equal(mid, wt, tolerance = 1e-10)
})

test_that("circular_midpoint errors when dest1 or dest2 missing", {
  expect_error(intermediate_bearing(gibson_qibla, dest1 = mecca))
  expect_error(intermediate_bearing(gibson_qibla, dest2 = petra))
})

# ── intermediate_bearing: weighted ────────────────────────────────────────────

test_that("weighted weight=1 returns bearing to dest1", {
  out_wt  <- intermediate_bearing(gibson_qibla, method = "weighted",
                                   dest1 = mecca, dest2 = petra, weight = 1)
  out_ref <- bearing_to_destination(gibson_qibla, mecca)
  expect_equal(out_wt, out_ref, tolerance = 1e-8)
})

test_that("weighted weight=0 returns bearing to dest2", {
  out_wt  <- intermediate_bearing(gibson_qibla, method = "weighted",
                                   dest1 = mecca, dest2 = petra, weight = 0)
  out_ref <- bearing_to_destination(gibson_qibla, petra)
  expect_equal(out_wt, out_ref, tolerance = 1e-8)
})

test_that("weighted result is in [0, 360)", {
  out <- intermediate_bearing(gibson_qibla, method = "weighted",
                               dest1 = mecca, dest2 = petra, weight = 0.3)
  ok  <- !is.na(out)
  expect_true(all(out[ok] >= 0 & out[ok] < 360))
})

# ── intermediate_bearing: common_convention ───────────────────────────────────

test_that("common_convention due_south returns 180 for all", {
  out <- intermediate_bearing(gibson_qibla, method = "common_convention",
                               convention = "due_south")
  expect_equal(out, rep(180, nrow(gibson_qibla)))
})

test_that("common_convention due_north returns 0 for all", {
  out <- intermediate_bearing(gibson_qibla, method = "common_convention",
                               convention = "due_north")
  expect_equal(out, rep(0, nrow(gibson_qibla)))
})

test_that("common_convention solar_east returns 90 for all", {
  out <- intermediate_bearing(gibson_qibla, method = "common_convention",
                               convention = "solar_east")
  expect_equal(out, rep(90, nrow(gibson_qibla)))
})

test_that("common_convention unknown name errors", {
  expect_error(
    intermediate_bearing(gibson_qibla, method = "common_convention",
                          convention = "facing_mars"),
    "Unknown convention"
  )
})

test_that("common_convention requires convention argument", {
  expect_error(
    intermediate_bearing(gibson_qibla, method = "common_convention")
  )
})

# ── intermediate_bearing: latent_cluster ──────────────────────────────────────

test_that("latent_cluster returns same value for every mosque", {
  out <- intermediate_bearing(gibson_qibla, method = "latent_cluster")
  ok  <- !is.na(out)
  expect_true(length(unique(out[ok])) == 1L)
})

test_that("latent_cluster result equals circular mean of all azimuths", {
  out      <- intermediate_bearing(gibson_qibla, method = "latent_cluster")
  expected <- circular_mean(gibson_qibla$azimuth, na.rm = TRUE)
  expect_equal(out[1L], expected, tolerance = 1e-10)
})

test_that("latent_cluster result is in [0, 360)", {
  out <- intermediate_bearing(gibson_qibla, method = "latent_cluster")
  expect_true(out[1L] >= 0 && out[1L] < 360)
})

# ── intermediate_bearing: free_latent ─────────────────────────────────────────

test_that("free_latent returns one value per mosque", {
  out <- intermediate_bearing(gibson_qibla, method = "free_latent")
  expect_length(out, nrow(gibson_qibla))
})

test_that("free_latent result is in [0, 360)", {
  out <- intermediate_bearing(gibson_qibla, method = "free_latent")
  ok  <- !is.na(out)
  expect_true(all(out[ok] >= 0 & out[ok] < 360))
})

# ── infer_latent_destination() ────────────────────────────────────────────────

test_that("infer_latent_destination returns list with expected names", {
  res <- infer_latent_destination(gibson_qibla)
  expect_named(res, c("latitude", "longitude", "mean_absolute_error", "n_used", "convergence"))
})

test_that("infer_latent_destination: latitude in [-90, 90]", {
  res <- infer_latent_destination(gibson_qibla)
  expect_true(res$latitude >= -90 && res$latitude <= 90)
})

test_that("infer_latent_destination: longitude in [-180, 180]", {
  res <- infer_latent_destination(gibson_qibla)
  expect_true(res$longitude >= -180 && res$longitude <= 180)
})

test_that("infer_latent_destination: mean_absolute_error is non-negative and < 180", {
  res <- infer_latent_destination(gibson_qibla)
  expect_true(res$mean_absolute_error >= 0 && res$mean_absolute_error < 180)
})

test_that("infer_latent_destination: n_used equals mosques with azimuths", {
  res <- infer_latent_destination(gibson_qibla)
  expect_equal(res$n_used, sum(!is.na(gibson_qibla$azimuth)))
})

test_that("infer_latent_destination converges on real data", {
  res <- infer_latent_destination(gibson_qibla)
  expect_equal(res$convergence, 0L)
})

test_that("infer_latent_destination: optimum beats naive mean-of-bearings estimate", {
  # The inferred latent destination should beat a fixed naive point (0, 0)
  res <- infer_latent_destination(gibson_qibla)
  has_az <- !is.na(gibson_qibla$azimuth)
  sub    <- gibson_qibla[has_az, ]
  naive_b <- bearing_haversine(sub$latitude, sub$longitude, 0, 0)
  naive_e <- mean(absolute_angular_error(sub$azimuth, naive_b), na.rm = TRUE)
  expect_lte(res$mean_absolute_error, naive_e)
})

test_that("infer_latent_destination errors with fewer than 3 azimuths", {
  two_az <- gibson_qibla[1:2, ]
  two_az$azimuth[1:2] <- c(180, 90)
  expect_error(infer_latent_destination(two_az), "3 mosques")

  one_az <- gibson_qibla[1:2, ]
  one_az$azimuth[2] <- NA
  expect_error(infer_latent_destination(one_az), "3 mosques")
})
