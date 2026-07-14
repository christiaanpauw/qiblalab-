## Tests for bearing_haversine(), bearing_rhumb(), bearing_to_destination(),
## bearing_to_candidates(). Reference values computed analytically.

# ── bearing_haversine() ───────────────────────────────────────────────────────

test_that("bearing_haversine: cardinal directions from equator/prime meridian", {
  expect_equal(bearing_haversine(0, 0, 1,  0), 0,   tolerance = 0.01)  # N
  expect_equal(bearing_haversine(0, 0, 0,  1), 90,  tolerance = 0.01)  # E
  expect_equal(bearing_haversine(1, 0, 0,  0), 180, tolerance = 0.01)  # S
  expect_equal(bearing_haversine(0, 1, 0,  0), 270, tolerance = 0.01)  # W
})

test_that("bearing_haversine: returns values in [0, 360)", {
  b <- bearing_haversine(
    c(0, 30, -10, 51.5),
    c(0, 10,  20, -0.1),
    c(1, 21,   5, 40.7),
    c(1, 39, -10, -74)
  )
  expect_true(all(is.na(b) | (b >= 0 & b < 360)))
})

test_that("bearing_haversine: same point returns NA", {
  expect_true(is.na(bearing_haversine(30, 35, 30, 35)))
})

test_that("bearing_haversine: antipodal points return NA", {
  expect_true(is.na(bearing_haversine(45, 0, -45, 180)))
})

test_that("bearing_haversine: Petra to Mecca", {
  # Petra: 30.3285N 35.4444E -> Mecca: 21.4225N 39.8262E
  # Expected: approx 155 deg (SE)
  b <- bearing_haversine(30.3285, 35.4444, 21.4225, 39.8262)
  expect_equal(b, 155, tolerance = 1)
})

test_that("bearing_haversine: Mecca to Petra", {
  # Reverse: approx 337 deg (NW)
  b <- bearing_haversine(21.4225, 39.8262, 30.3285, 35.4444)
  expect_equal(b, 337, tolerance = 1)
})

test_that("bearing_haversine: Petra to Jerusalem", {
  # Petra: 30.3285N 35.4444E -> Jerusalem: 31.7781N 35.2354E
  # Jerusalem is almost directly north of Petra (slight NNW)
  b <- bearing_haversine(30.3285, 35.4444, 31.7781, 35.2354)
  expect_equal(b, 352, tolerance = 2)
})

test_that("bearing_haversine: wrap-around near 0/360 boundary", {
  # North-northwest bearing should be close to 360 (not negative or >360)
  b <- bearing_haversine(30, 36, 32, 35)
  expect_true(b > 315 && b < 360)
})

test_that("bearing_haversine: is vectorised", {
  b <- bearing_haversine(
    c(0, 30),  c(0, 35),
    c(1, 21),  c(0, 40)
  )
  expect_length(b, 2L)
  expect_equal(b[1], bearing_haversine(0, 0, 1, 0), tolerance = 1e-10)
})

test_that("bearing_haversine: rejects out-of-range inputs", {
  expect_error(bearing_haversine(91, 0, 0, 0))
  expect_error(bearing_haversine(0, 181, 0, 0))
})

test_that("bearing_haversine: NA input propagates to NA output", {
  expect_true(is.na(bearing_haversine(NA_real_, 0, 0, 0)))
})

# ── bearing_rhumb() ───────────────────────────────────────────────────────────

test_that("bearing_rhumb: due east on equator matches haversine", {
  expect_equal(bearing_rhumb(0, 0, 0, 10),  90,  tolerance = 0.1)
  expect_equal(bearing_rhumb(0, 0, 0, -10), 270, tolerance = 0.1)
})

test_that("bearing_rhumb: due north matches haversine", {
  expect_equal(bearing_rhumb(0, 0, 10, 0), 0, tolerance = 0.1)
})

test_that("bearing_rhumb: returns NA for same point", {
  expect_true(is.na(bearing_rhumb(30, 35, 30, 35)))
})

test_that("bearing_rhumb: Petra to Mecca differs from haversine by < 5 deg", {
  bh <- bearing_haversine(30.3285, 35.4444, 21.4225, 39.8262)
  br <- bearing_rhumb(30.3285, 35.4444, 21.4225, 39.8262)
  expect_lt(absolute_angular_error(bh, br), 5)
})

# ── bearing_to_destination() ──────────────────────────────────────────────────

test_that("bearing_to_destination: works with destinations data frame", {
  data(gibson_qibla, package = "qiblalab")
  data(destinations, package = "qiblalab")
  mecca <- destinations[destinations$id == "mecca", ]
  b <- bearing_to_destination(gibson_qibla, mecca)
  expect_length(b, nrow(gibson_qibla))
  expect_true(all(is.na(b) | (b >= 0 & b < 360)))
})

test_that("bearing_to_destination: errors if destination has > 1 row", {
  data(gibson_qibla, package = "qiblalab")
  data(destinations, package = "qiblalab")
  expect_error(bearing_to_destination(gibson_qibla, destinations), "one row")
})

test_that("bearing_to_destination: respects model argument", {
  data(gibson_qibla, package = "qiblalab")
  data(destinations, package = "qiblalab")
  petra <- destinations[destinations$id == "petra", ]
  bh <- bearing_to_destination(gibson_qibla, petra, model = bearing_haversine)
  br <- bearing_to_destination(gibson_qibla, petra, model = bearing_rhumb)
  # Results should differ (rhumb != great-circle) but median difference is small
  diffs <- absolute_angular_error(bh, br)
  expect_true(any(diffs > 0, na.rm = TRUE))
  expect_lt(median(diffs, na.rm = TRUE), 5)
})

# ── bearing_to_candidates() ───────────────────────────────────────────────────

test_that("bearing_to_candidates: returns long tibble with correct columns", {
  data(gibson_qibla, package = "qiblalab")
  data(destinations, package = "qiblalab")
  result <- bearing_to_candidates(gibson_qibla)
  expect_s3_class(result, "tbl_df")
  expect_true(all(c("row_id", "destination_id", "destination_name", "bearing")
                  %in% names(result)))
  expect_equal(nrow(result), nrow(gibson_qibla) * nrow(destinations))
})

test_that("bearing_to_candidates: bearings in [0, 360)", {
  data(gibson_qibla, package = "qiblalab")
  data(destinations, package = "qiblalab")
  result <- bearing_to_candidates(gibson_qibla)
  b <- result$bearing[!is.na(result$bearing)]
  expect_true(all(b >= 0 & b < 360))
})

test_that("bearing_to_candidates: destination_id values match candidates$id", {
  data(gibson_qibla, package = "qiblalab")
  data(destinations, package = "qiblalab")
  result <- bearing_to_candidates(gibson_qibla)
  expect_setequal(unique(result$destination_id), destinations$id)
})
