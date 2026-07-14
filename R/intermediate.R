## Intermediate qibla bearing methods and latent destination inference.
## Design doc §8. ADR-01: directional (0-360) throughout.

# ── intermediate_bearing() ────────────────────────────────────────────────────

#' Compute an implied intermediate qibla bearing
#'
#' Returns one implied qibla bearing per mosque using one of five methods.
#' The "intermediate" concept acknowledges that early mosques may not have been
#' oriented toward any single modern reference point.
#'
#' Methods:
#' \describe{
#'   \item{`circular_midpoint`}{Circular mean of the bearings from each mosque
#'     to `dest1` and `dest2`. Requires both `dest1` and `dest2`.}
#'   \item{`weighted`}{Weighted circular mean: `weight` on the bearing to
#'     `dest1`, `1 - weight` on the bearing to `dest2`. At `weight = 0.5`
#'     this equals `circular_midpoint`. Requires both `dest1` and `dest2`.}
#'   \item{`free_latent`}{Calls [infer_latent_destination()] to find the
#'     geographic point that minimises mean absolute angular error across all
#'     mosques with measured azimuths, then returns the bearing from each
#'     mosque to that point. Requires `azimuth_col` to be present.}
#'   \item{`common_convention`}{Returns a fixed bearing based on a named
#'     historical convention. Requires `convention`. Currently supported:
#'     `"due_south"` (180), `"due_north"` (0), `"solar_east"` (90),
#'     `"solar_west"` (270).}
#'   \item{`latent_cluster`}{Returns the circular mean of all observed
#'     azimuths — the global consensus direction implied by the dataset.
#'     This is a one-cluster approximation; proper clustering is in Phase 5.}
#' }
#'
#' @param data A mosque data frame (e.g. [gibson_qibla]).
#' @param method One of the five method strings (see Details).
#' @param dest1,dest2 Destination: a character id from [destinations] or a
#'   single-row data frame with `latitude` and `longitude`. Required for
#'   `"circular_midpoint"` and `"weighted"`.
#' @param weight Numeric in \[0, 1\]. Weight on the bearing to `dest1` for the
#'   `"weighted"` method. Default `0.5`.
#' @param convention Character scalar. Named convention for
#'   `"common_convention"`. One of `"due_south"`, `"due_north"`,
#'   `"solar_east"`, `"solar_west"`.
#' @param model Bearing model function. Default [bearing_haversine()].
#' @param lat_col,lon_col,azimuth_col Column names in `data`.
#' @param dest_table Destinations table used to resolve character ids. Defaults
#'   to the shipped [destinations].
#'
#' @return Numeric vector in \[0, 360), one value per row of `data`. `NA` where
#'   a required bearing or azimuth is `NA`.
#'
#' @seealso [infer_latent_destination()], [bearing_to_destination()]
#' @export
#' @examples
#' data(gibson_qibla)
#' data(destinations)
#' mecca <- destinations[destinations$id == "mecca", ]
#' petra <- destinations[destinations$id == "petra", ]
#' mid   <- intermediate_bearing(gibson_qibla, dest1 = mecca, dest2 = petra)
#' head(mid)
#'
#' wt  <- intermediate_bearing(gibson_qibla, method = "weighted",
#'                             dest1 = "mecca", dest2 = "petra", weight = 0.7)
#' head(wt)
intermediate_bearing <- function(data,
                                  method      = c("circular_midpoint", "weighted",
                                                  "free_latent", "common_convention",
                                                  "latent_cluster"),
                                  dest1       = NULL,
                                  dest2       = NULL,
                                  weight      = 0.5,
                                  convention  = NULL,
                                  model       = bearing_haversine,
                                  lat_col     = "latitude",
                                  lon_col     = "longitude",
                                  azimuth_col = "azimuth",
                                  dest_table  = NULL) {
  method <- match.arg(method)
  checkmate::assert_data_frame(data)
  checkmate::assert_function(model)
  checkmate::assert_string(lat_col)
  checkmate::assert_string(lon_col)
  checkmate::assert_string(azimuth_col)
  checkmate::assert_number(weight, lower = 0, upper = 1)

  if (is.null(dest_table)) {
    dest_table <- get("destinations", envir = asNamespace("qiblalab"))
  }

  nr <- nrow(data)

  switch(method,
    circular_midpoint = {
      .check_dest_args(dest1, dest2, method)
      d1 <- resolve_candidate(dest1, dest_table)
      d2 <- resolve_candidate(dest2, dest_table)
      b1 <- bearing_to_destination(data, d1, model, lat_col, lon_col)
      b2 <- bearing_to_destination(data, d2, model, lat_col, lon_col)
      z  <- exp(1i * b1 * pi / 180) + exp(1i * b2 * pi / 180)
      result <- (Arg(z) * 180 / pi + 360) %% 360
      result[is.na(b1) | is.na(b2)] <- NA_real_
      result
    },
    weighted = {
      .check_dest_args(dest1, dest2, method)
      checkmate::assert_number(weight, lower = 0, upper = 1)
      d1 <- resolve_candidate(dest1, dest_table)
      d2 <- resolve_candidate(dest2, dest_table)
      b1 <- bearing_to_destination(data, d1, model, lat_col, lon_col)
      b2 <- bearing_to_destination(data, d2, model, lat_col, lon_col)
      z  <- weight * exp(1i * b1 * pi / 180) + (1 - weight) * exp(1i * b2 * pi / 180)
      result <- (Arg(z) * 180 / pi + 360) %% 360
      result[is.na(b1) | is.na(b2)] <- NA_real_
      result
    },
    free_latent = {
      checkmate::assert_true(azimuth_col %in% names(data),
                             .var.name = paste0("'", azimuth_col, "' in names(data)"))
      lat_lon <- infer_latent_destination(data, model, lat_col, lon_col, azimuth_col)
      implied <- data.frame(latitude = lat_lon$latitude, longitude = lat_lon$longitude)
      bearing_to_destination(data, implied, model, lat_col, lon_col)
    },
    common_convention = {
      checkmate::assert_string(convention)
      .apply_convention(convention, nr)
    },
    latent_cluster = {
      checkmate::assert_true(azimuth_col %in% names(data),
                             .var.name = paste0("'", azimuth_col, "' in names(data)"))
      az    <- data[[azimuth_col]]
      cmean <- circular_mean(az, na.rm = TRUE)
      if (is.na(cmean)) {
        cli::cli_abort(
          "No non-NA azimuths in {.arg data}; cannot compute {.val latent_cluster} bearing."
        )
      }
      rep(cmean, nr)
    }
  )
}

.check_dest_args <- function(dest1, dest2, method) {
  if (is.null(dest1) || is.null(dest2)) {
    cli::cli_abort(
      "{.arg dest1} and {.arg dest2} must both be supplied for method {.val {method}}."
    )
  }
}

.apply_convention <- function(convention, n) {
  known <- c(due_south = 180, due_north = 0, solar_east = 90, solar_west = 270)
  if (!convention %in% names(known)) {
    avail <- paste(names(known), collapse = ", ")
    cli::cli_abort(c(
      "Unknown convention {.val {convention}}.",
      "i" = "Available: {avail}"
    ))
  }
  rep(known[[convention]], n)
}


# ── infer_latent_destination() ────────────────────────────────────────────────

#' Infer the implied latent destination from mosque orientations
#'
#' Uses numerical optimisation (L-BFGS-B) to find the latitude and longitude
#' of the point that minimises the mean absolute angular error between observed
#' mosque azimuths and the theoretical bearing from each mosque to that point.
#' This is a data-driven alternative to specifying a destination a priori.
#'
#' The result is a statistical estimate, not a historical identification. It
#' should be reported as an analytical tool and not interpreted as evidence for
#' any particular destination (package neutrality principle).
#'
#' @param data A mosque data frame with azimuth measurements.
#' @param model Bearing model function. Default [bearing_haversine()].
#' @param lat_col,lon_col,azimuth_col Column names in `data`.
#' @param init_lat,init_lon Starting coordinates for the optimisation (decimal
#'   degrees). Default: 25 N, 40 E (central Middle East).
#'
#' @return A named list:
#' \describe{
#'   \item{`latitude`, `longitude`}{Inferred destination (decimal degrees).}
#'   \item{`mean_absolute_error`}{Mean absolute angular error at the solution (degrees).}
#'   \item{`n_used`}{Number of mosques used (those with non-`NA` azimuths).}
#'   \item{`convergence`}{Integer code from [stats::optim()]. 0 = converged.}
#' }
#'
#' @seealso [intermediate_bearing()]
#' @export
#' @examples
#' data(gibson_qibla)
#' res <- infer_latent_destination(gibson_qibla)
#' cat("Inferred lat:", round(res$latitude, 2), "\n")
#' cat("Inferred lon:", round(res$longitude, 2), "\n")
#' cat("Mean error:  ", round(res$mean_absolute_error, 2), "deg\n")
infer_latent_destination <- function(data,
                                      model       = bearing_haversine,
                                      lat_col     = "latitude",
                                      lon_col     = "longitude",
                                      azimuth_col = "azimuth",
                                      init_lat    = 25,
                                      init_lon    = 40) {
  checkmate::assert_data_frame(data)
  checkmate::assert_function(model)
  checkmate::assert_string(lat_col)
  checkmate::assert_string(lon_col)
  checkmate::assert_string(azimuth_col)
  checkmate::assert_number(init_lat, lower = -90, upper = 90)
  checkmate::assert_number(init_lon, lower = -180, upper = 180)
  checkmate::assert_true(azimuth_col %in% names(data),
                         .var.name = paste0("'", azimuth_col, "' in names(data)"))

  has_az   <- !is.na(data[[azimuth_col]])
  sub      <- data[has_az, ]
  n_used   <- nrow(sub)

  if (n_used < 3L) {
    cli::cli_abort("At least 3 mosques with azimuths are required; got {n_used}.")
  }
  if (n_used < 10L) {
    cli::cli_warn(
      "Only {n_used} mosques with azimuths. Latent-destination inference is ",
      "unreliable with small samples.",
      .frequency = "once", .frequency_id = "latent_small_n"
    )
  }

  obs_az   <- sub[[azimuth_col]]
  from_lat <- sub[[lat_col]]
  from_lon <- sub[[lon_col]]

  objective <- function(params) {
    b <- model(from_lat, from_lon, params[1L], params[2L])
    mean(absolute_angular_error(obs_az, b), na.rm = TRUE)
  }

  fit <- stats::optim(
    par    = c(init_lat, init_lon),
    fn     = objective,
    method = "L-BFGS-B",
    lower  = c(-89.9, -180),
    upper  = c( 89.9,  180)
  )

  if (fit$convergence != 0L) {
    cli::cli_warn(
      "Optimisation did not converge (code {fit$convergence}). ",
      "Try different starting values via {.arg init_lat}/{.arg init_lon}.",
      .frequency = "once", .frequency_id = "latent_converge"
    )
  }

  list(
    latitude            = fit$par[1L],
    longitude           = fit$par[2L],
    mean_absolute_error = fit$value,
    n_used              = n_used,
    convergence         = fit$convergence
  )
}
