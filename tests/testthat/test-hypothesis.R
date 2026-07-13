## Tests for qibla_hypothesis(), test_qibla_hypothesis(), compare_qibla_hypotheses()

# ── qibla_hypothesis() ────────────────────────────────────────────────────────

test_that("qibla_hypothesis: returns correct class", {
  h <- qibla_hypothesis("Test", candidate = "petra", tolerance = 5)
  expect_s3_class(h, "qibla_hypothesis")
})

test_that("qibla_hypothesis: stores fields correctly", {
  h <- qibla_hypothesis(
    name           = "My hypothesis",
    population     = year_ce_max <= 700,
    candidate      = "petra",
    tolerance      = 5,
    expected_share = 0.6,
    alternative    = "greater",
    description    = "A test"
  )
  expect_equal(h$name, "My hypothesis")
  expect_equal(h$tolerance, 5)
  expect_equal(h$expected_share, 0.6)
  expect_equal(h$alternative, "greater")
  expect_equal(h$description, "A test")
})

test_that("qibla_hypothesis: population stored as quosure", {
  h <- qibla_hypothesis("h", population = year_ce_max <= 700,
                         candidate = "mecca", tolerance = 5)
  expect_true(rlang::is_quosure(h$population))
})

test_that("qibla_hypothesis: errors on non-numeric tolerance", {
  expect_error(qibla_hypothesis("h", candidate = "petra", tolerance = "5"))
})

test_that("qibla_hypothesis: errors on tolerance outside (0, 180]", {
  expect_error(qibla_hypothesis("h", candidate = "petra", tolerance = -1))
  expect_error(qibla_hypothesis("h", candidate = "petra", tolerance = 181))
})

test_that("qibla_hypothesis: prints without error", {
  h <- qibla_hypothesis("Test", candidate = "petra", tolerance = 5)
  expect_no_error(print(h))
})

# ── test_qibla_hypothesis() ───────────────────────────────────────────────────

test_that("test_qibla_hypothesis: returns qibla_test_result", {
  data(gibson_qibla, package = "qiblalab")
  h <- qibla_hypothesis("All mosques", candidate = "petra", tolerance = 10)
  result <- test_qibla_hypothesis(h, gibson_qibla)
  expect_s3_class(result, "qibla_test_result")
})

test_that("test_qibla_hypothesis: n_analysed equals mosques with azimuths in population", {
  data(gibson_qibla, package = "qiblalab")
  h <- qibla_hypothesis("All", candidate = "petra", tolerance = 10)
  result <- test_qibla_hypothesis(h, gibson_qibla)
  expect_equal(result$n_analysed, sum(!is.na(gibson_qibla$azimuth)))
})

test_that("test_qibla_hypothesis: population filter reduces sample", {
  data(gibson_qibla, package = "qiblalab")
  h_all   <- qibla_hypothesis("All",   candidate = "petra", tolerance = 10)
  h_early <- qibla_hypothesis("Early", candidate = "petra", tolerance = 10,
                               population = year_ce_max <= 700)
  r_all   <- test_qibla_hypothesis(h_all,   gibson_qibla)
  r_early <- test_qibla_hypothesis(h_early, gibson_qibla)
  expect_lt(r_early$n_population, r_all$n_population)
})

test_that("test_qibla_hypothesis: proportion_observed in [0, 1]", {
  data(gibson_qibla, package = "qiblalab")
  h <- qibla_hypothesis("All", candidate = "mecca", tolerance = 5)
  result <- test_qibla_hypothesis(h, gibson_qibla)
  expect_true(result$proportion_observed >= 0 && result$proportion_observed <= 1)
})

test_that("test_qibla_hypothesis: n_consistent matches manual count", {
  data(gibson_qibla, package = "qiblalab")
  data(destinations, package = "qiblalab")
  h <- qibla_hypothesis("All", candidate = "petra", tolerance = 5)
  result <- test_qibla_hypothesis(h, gibson_qibla)

  petra  <- destinations[destinations$id == "petra", ]
  az     <- gibson_qibla$azimuth[!is.na(gibson_qibla$azimuth)]
  lat    <- gibson_qibla$latitude[!is.na(gibson_qibla$azimuth)]
  lon    <- gibson_qibla$longitude[!is.na(gibson_qibla$azimuth)]
  b      <- bearing_haversine(lat, lon, petra$latitude, petra$longitude)
  manual <- sum(absolute_angular_error(az, b) <= 5, na.rm = TRUE)

  expect_equal(result$n_consistent, manual)
})

test_that("test_qibla_hypothesis: sensitivity table always includes 2, 5, 10", {
  data(gibson_qibla, package = "qiblalab")
  h <- qibla_hypothesis("All", candidate = "petra", tolerance = 7)
  result <- test_qibla_hypothesis(h, gibson_qibla)
  expect_true(all(c(2, 5, 7, 10) %in% result$sensitivity$tolerance))
})

test_that("test_qibla_hypothesis: sensitivity proportions increase with tolerance", {
  data(gibson_qibla, package = "qiblalab")
  h <- qibla_hypothesis("All", candidate = "petra", tolerance = 10)
  result <- test_qibla_hypothesis(h, gibson_qibla)
  props <- result$sensitivity$proportion
  expect_true(all(diff(props) >= 0))
})

test_that("test_qibla_hypothesis: binom_test p-value in [0, 1]", {
  data(gibson_qibla, package = "qiblalab")
  h <- qibla_hypothesis("All", candidate = "petra", tolerance = 5)
  result <- test_qibla_hypothesis(h, gibson_qibla)
  expect_true(result$binom_test$p.value >= 0 && result$binom_test$p.value <= 1)
})

test_that("test_qibla_hypothesis: prints without error", {
  data(gibson_qibla, package = "qiblalab")
  h <- qibla_hypothesis("All", candidate = "petra", tolerance = 5)
  result <- test_qibla_hypothesis(h, gibson_qibla)
  expect_no_error(print(result))
})

test_that("test_qibla_hypothesis: errors when population matches 0 rows", {
  data(gibson_qibla, package = "qiblalab")
  h <- qibla_hypothesis("None", candidate = "petra", tolerance = 5,
                         population = year_ce_max < 0)
  expect_error(test_qibla_hypothesis(h, gibson_qibla), "0 rows")
})

test_that("test_qibla_hypothesis: alternative = 'less' gives different p-value", {
  data(gibson_qibla, package = "qiblalab")
  h_g <- qibla_hypothesis("G", candidate = "petra", tolerance = 5,
                           alternative = "greater")
  h_l <- qibla_hypothesis("L", candidate = "petra", tolerance = 5,
                           alternative = "less")
  rg <- test_qibla_hypothesis(h_g, gibson_qibla)
  rl <- test_qibla_hypothesis(h_l, gibson_qibla)
  expect_false(isTRUE(all.equal(rg$binom_test$p.value, rl$binom_test$p.value)))
})

# ── compare_qibla_hypotheses() ────────────────────────────────────────────────

test_that("compare_qibla_hypotheses: returns a tibble with expected columns", {
  data(gibson_qibla, package = "qiblalab")
  cmp <- compare_qibla_hypotheses(gibson_qibla)
  expect_s3_class(cmp, "tbl_df")
  expect_true(all(c("nearest_id", "nearest_error", "second_nearest_id",
                    "margin", "ambiguous") %in% names(cmp)))
})

test_that("compare_qibla_hypotheses: one row per mosque", {
  data(gibson_qibla, package = "qiblalab")
  cmp <- compare_qibla_hypotheses(gibson_qibla)
  expect_equal(nrow(cmp), nrow(gibson_qibla))
})

test_that("compare_qibla_hypotheses: nearest_error <= second_nearest_error", {
  data(gibson_qibla, package = "qiblalab")
  cmp <- compare_qibla_hypotheses(gibson_qibla)
  ok  <- !is.na(cmp$nearest_error) & !is.na(cmp$second_nearest_error)
  expect_true(all(cmp$nearest_error[ok] <= cmp$second_nearest_error[ok]))
})

test_that("compare_qibla_hypotheses: margin = second - nearest error", {
  data(gibson_qibla, package = "qiblalab")
  cmp <- compare_qibla_hypotheses(gibson_qibla)
  ok  <- !is.na(cmp$margin)
  expect_equal(cmp$margin[ok],
               cmp$second_nearest_error[ok] - cmp$nearest_error[ok],
               tolerance = 1e-10)
})

test_that("compare_qibla_hypotheses: NA azimuth rows have NA results", {
  data(gibson_qibla, package = "qiblalab")
  cmp   <- compare_qibla_hypotheses(gibson_qibla)
  no_az <- is.na(gibson_qibla$azimuth)
  expect_true(all(is.na(cmp$nearest_id[no_az])))
  expect_true(all(is.na(cmp$margin[no_az])))
})

test_that("compare_qibla_hypotheses: ambiguity flag reflects threshold", {
  data(gibson_qibla, package = "qiblalab")
  cmp <- compare_qibla_hypotheses(gibson_qibla, ambiguity_threshold = 5)
  ok  <- !is.na(cmp$margin)
  expect_true(all(cmp$ambiguous[ok] == (cmp$margin[ok] < 5)))
})

test_that("compare_qibla_hypotheses: nearest_id is a valid destination id", {
  data(gibson_qibla, package = "qiblalab")
  data(destinations, package = "qiblalab")
  cmp <- compare_qibla_hypotheses(gibson_qibla)
  valid_ids <- c(destinations$id, NA_character_)
  expect_true(all(cmp$nearest_id %in% valid_ids))
})

test_that("compare_qibla_hypotheses: bearing and error columns present for each candidate", {
  data(gibson_qibla, package = "qiblalab")
  data(destinations, package = "qiblalab")
  cmp <- compare_qibla_hypotheses(gibson_qibla)
  for (id in destinations$id) {
    expect_true(paste0("bearing_", id) %in% names(cmp))
    expect_true(paste0("error_",   id) %in% names(cmp))
  }
})
