## Uncertainty specification, Monte Carlo sampling, and sensitivity analysis.
## ADR-04: three modes — deterministic, sensitivity, probabilistic.
## ADR-08: tolerance policy (report across a range, no silent default).

# ── define_uncertainty() ──────────────────────────────────────────────────────

#' Define an uncertainty specification for Monte Carlo sampling
#'
#' Records assumed uncertainties for mosque coordinates, construction dates,
#' and azimuth measurements. Pass the result to [sample_qibla_dataset()] or
#' [run_sensitivity_analysis()] with `mode = "probabilistic"`.
#'
#' @param coord_sigma Numeric >= 0. 1-sigma positional uncertainty in decimal
#'   degrees (Gaussian, applied independently to latitude and longitude).
#'   Default `0` (no positional perturbation).
#' @param date_sigma Numeric >= 0. 1-sigma date uncertainty in years
#'   (Gaussian). Applied to records where `year_ce_min == year_ce_max`
#'   (nominally exact dates). Interval-dated records are handled by uniform
#'   sampling inside [sample_qibla_dataset()]. Default `5`.
#' @param azimuth_sigma Numeric >= 0. 1-sigma azimuth measurement uncertainty
#'   in degrees (Gaussian, wrapped to \[0, 360)). Default `0`.
#'
#' @return A `qibla_uncertainty` object.
#'
#' @seealso [sample_qibla_dataset()], [run_sensitivity_analysis()]
#' @export
#' @examples
#' u <- define_uncertainty(coord_sigma = 0.01, date_sigma = 10, azimuth_sigma = 2)
#' u
define_uncertainty <- function(coord_sigma   = 0,
                                date_sigma    = 5,
                                azimuth_sigma = 0) {
  checkmate::assert_number(coord_sigma,   lower = 0)
  checkmate::assert_number(date_sigma,    lower = 0)
  checkmate::assert_number(azimuth_sigma, lower = 0)
  structure(
    list(coord_sigma   = coord_sigma,
         date_sigma    = date_sigma,
         azimuth_sigma = azimuth_sigma),
    class = "qibla_uncertainty"
  )
}

#' @export
print.qibla_uncertainty <- function(x, ...) {
  cli::cli_h3("Qibla uncertainty specification")
  cli::cli_bullets(c(
    "*" = "Coordinate sigma: {x$coord_sigma} deg",
    "*" = "Date sigma:       {x$date_sigma} yr  (Gaussian for exact dates)",
    "*" = "Azimuth sigma:    {x$azimuth_sigma} deg"
  ))
  invisible(x)
}


# ── sample_qibla_dataset() ────────────────────────────────────────────────────

#' Draw one Monte Carlo sample from an uncertainty specification
#'
#' Returns a copy of `data` with coordinates, dates, and azimuths perturbed
#' according to a [define_uncertainty()] specification. Call this repeatedly
#' inside a Monte Carlo loop. NA values are never moved: mosques without
#' measured azimuths remain without azimuths after sampling; only the numeric
#' values are perturbed.
#'
#' Date handling (ADR-04):
#' \itemize{
#'   \item Exact dates (`year_ce_min == year_ce_max`): drawn from
#'     Normal(year_ce_min, date_sigma), rounded to the nearest year.
#'   \item Interval dates (`year_ce_min < year_ce_max`): drawn uniformly from
#'     \[year_ce_min, year_ce_max\], rounded to the nearest year.
#'   \item Unknown dates (NA): remain NA.
#' }
#' The original `year_ce` character column is not modified.
#'
#' @param data A mosque data frame (typically [gibson_qibla]).
#' @param uncertainty A `qibla_uncertainty` object from [define_uncertainty()].
#' @param seed Optional integer seed for reproducibility of a single draw.
#'
#' @return A tibble with the same columns as `data`, with selected values
#'   perturbed.
#'
#' @seealso [define_uncertainty()], [run_sensitivity_analysis()]
#' @export
#' @examples
#' data(gibson_qibla)
#' u <- define_uncertainty(coord_sigma = 0.01, azimuth_sigma = 2)
#' s <- sample_qibla_dataset(gibson_qibla, u, seed = 1)
#' head(s[, c("latitude", "longitude", "azimuth")])
sample_qibla_dataset <- function(data, uncertainty, seed = NULL) {
  checkmate::assert_data_frame(data)
  checkmate::assert_class(uncertainty, "qibla_uncertainty")
  if (!is.null(seed)) checkmate::assert_int(seed)

  if (!is.null(seed)) set.seed(seed)

  out <- data
  n   <- nrow(data)

  # Perturb coordinates
  if (uncertainty$coord_sigma > 0) {
    if ("latitude" %in% names(out)) {
      out$latitude <- pmax(-90, pmin(90,
        out$latitude + stats::rnorm(n, 0, uncertainty$coord_sigma)))
    }
    if ("longitude" %in% names(out)) {
      out$longitude <- ((out$longitude +
        stats::rnorm(n, 0, uncertainty$coord_sigma) + 180) %% 360) - 180
    }
  }

  # Perturb dates
  if (uncertainty$date_sigma > 0 &&
        all(c("year_ce_min", "year_ce_max") %in% names(out))) {
    has_dates <- !is.na(out$year_ce_min) & !is.na(out$year_ce_max)
    exact     <- has_dates & (out$year_ce_min == out$year_ce_max)
    interval  <- has_dates & (out$year_ce_min <  out$year_ce_max)

    n_exact <- sum(exact)
    if (n_exact > 0L) {
      sampled_yr <- round(
        out$year_ce_min[exact] + stats::rnorm(n_exact, 0, uncertainty$date_sigma)
      )
      out$year_ce_min[exact] <- sampled_yr
      out$year_ce_max[exact] <- sampled_yr
    }
    for (i in which(interval)) {
      yr <- round(stats::runif(1L, out$year_ce_min[i], out$year_ce_max[i]))
      out$year_ce_min[i] <- yr
      out$year_ce_max[i] <- yr
    }
  }

  # Perturb azimuths
  if (uncertainty$azimuth_sigma > 0 && "azimuth" %in% names(out)) {
    has_az <- !is.na(out$azimuth)
    n_az   <- sum(has_az)
    if (n_az > 0L) {
      noise              <- stats::rnorm(n_az, 0, uncertainty$azimuth_sigma)
      out$azimuth[has_az] <- ((out$azimuth[has_az] + noise) %% 360 + 360) %% 360
    }
  }

  tibble::as_tibble(out)
}


# ── run_sensitivity_analysis() ────────────────────────────────────────────────

#' Sensitivity analysis for a qibla hypothesis
#'
#' Evaluates a [qibla_hypothesis()] across a range of tolerances
#' (`mode = "tolerance"`) and optionally across Monte Carlo datasets drawn
#' from an uncertainty specification (`mode = "probabilistic"`).
#'
#' @param data A mosque data frame.
#' @param hypothesis A `qibla_hypothesis` object.
#' @param mode `"tolerance"` or `"probabilistic"` (see Details).
#' @param tolerances Numeric vector of tolerance values in degrees. Default
#'   `c(2, 5, 10)`.
#' @param uncertainty A `qibla_uncertainty` object. Required for
#'   `mode = "probabilistic"`.
#' @param n_samples Integer. Monte Carlo draw count. Default `200`.
#' @param model Bearing model function. Default [bearing_haversine()].
#' @param seed Integer seed for the Monte Carlo draws.
#' @param lat_col,lon_col,azimuth_col Column names in `data`.
#'
#' @return A tibble. For `mode = "tolerance"`: columns `tolerance`,
#'   `n_consistent`, `proportion`, `binom_p`. For `mode = "probabilistic"`:
#'   `tolerance`, `proportion`, `sd_proportion`, `binom_p`, `sd_binom_p`,
#'   `n_samples`.
#'
#' @seealso [define_uncertainty()], [sample_qibla_dataset()],
#'   [test_qibla_hypothesis()]
#' @export
#' @examples
#' data(gibson_qibla)
#' h <- qibla_hypothesis("Test", candidate = "petra", tolerance = 5)
#' run_sensitivity_analysis(gibson_qibla, h)
run_sensitivity_analysis <- function(data,
                                      hypothesis,
                                      mode        = c("tolerance", "probabilistic"),
                                      tolerances  = c(2, 5, 10),
                                      uncertainty = NULL,
                                      n_samples   = 200L,
                                      model       = bearing_haversine,
                                      seed        = NULL,
                                      lat_col     = "latitude",
                                      lon_col     = "longitude",
                                      azimuth_col = "azimuth") {
  mode <- match.arg(mode)
  checkmate::assert_data_frame(data)
  checkmate::assert_class(hypothesis, "qibla_hypothesis")
  checkmate::assert_numeric(tolerances, lower = 0, upper = 180, min.len = 1L)
  checkmate::assert_count(n_samples, positive = TRUE)
  checkmate::assert_function(model)
  if (!is.null(seed)) checkmate::assert_int(seed)

  tolerances <- sort(unique(tolerances))

  if (mode == "probabilistic") {
    checkmate::assert_class(uncertainty, "qibla_uncertainty",
                             .var.name = "uncertainty (required for mode='probabilistic')")
    return(.sensitivity_probabilistic(data, hypothesis, tolerances, uncertainty,
                                      n_samples, model, seed, lat_col, lon_col, azimuth_col))
  }

  .sensitivity_tolerance(data, hypothesis, tolerances, model, lat_col, lon_col, azimuth_col)
}

.sensitivity_tolerance <- function(data, hypothesis, tolerances, model,
                                   lat_col, lon_col, azimuth_col) {
  dest_table <- get("destinations", envir = asNamespace("qiblalab"))
  dest       <- resolve_candidate(hypothesis$candidate, dest_table)
  filtered   <- dplyr::filter(data, !!hypothesis$population)
  has_az     <- !is.na(filtered[[azimuth_col]])
  analysed   <- filtered[has_az, ]
  n_an       <- nrow(analysed)
  if (n_an == 0L) cli::cli_abort("No mosques with azimuths in the population.")

  b  <- bearing_to_destination(analysed, dest, model, lat_col, lon_col)
  er <- absolute_angular_error(analysed[[azimuth_col]], b)

  nc  <- vapply(tolerances, function(t) sum(er <= t, na.rm = TRUE), integer(1L))
  pr  <- nc / n_an
  bp  <- vapply(seq_along(tolerances), function(j) {
    stats::binom.test(nc[j], n_an, p = hypothesis$expected_share,
                      alternative = hypothesis$alternative)$p.value
  }, numeric(1L))

  tibble::tibble(
    tolerance    = tolerances,
    n_consistent = nc,
    proportion   = pr,
    binom_p      = bp
  )
}

.sensitivity_probabilistic <- function(data, hypothesis, tolerances, uncertainty,
                                        n_samples, model, seed, lat_col, lon_col, azimuth_col) {
  if (!is.null(seed)) set.seed(seed)

  dest_table <- get("destinations", envir = asNamespace("qiblalab"))
  dest       <- resolve_candidate(hypothesis$candidate, dest_table)

  ntol       <- length(tolerances)
  prop_mat   <- matrix(NA_real_, nrow = n_samples, ncol = ntol)
  binom_mat  <- matrix(NA_real_, nrow = n_samples, ncol = ntol)

  for (s in seq_len(n_samples)) {
    sampled  <- sample_qibla_dataset(data, uncertainty)
    filtered <- dplyr::filter(sampled, !!hypothesis$population)
    has_az   <- !is.na(filtered[[azimuth_col]])
    analysed <- filtered[has_az, ]
    n_an     <- nrow(analysed)
    if (n_an == 0L) next

    b  <- bearing_to_destination(analysed, dest, model, lat_col, lon_col)
    er <- absolute_angular_error(analysed[[azimuth_col]], b)

    for (j in seq_len(ntol)) {
      nc           <- sum(er <= tolerances[j], na.rm = TRUE)
      prop_mat[s, j]  <- nc / n_an
      binom_mat[s, j] <- stats::binom.test(
        nc, n_an, p = hypothesis$expected_share,
        alternative = hypothesis$alternative
      )$p.value
    }
  }

  tibble::tibble(
    tolerance     = tolerances,
    proportion    = colMeans(prop_mat,  na.rm = TRUE),
    sd_proportion = apply(prop_mat,  2L, stats::sd, na.rm = TRUE),
    binom_p       = colMeans(binom_mat, na.rm = TRUE),
    sd_binom_p    = apply(binom_mat, 2L, stats::sd, na.rm = TRUE),
    n_samples     = n_samples
  )
}
