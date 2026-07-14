## Bearing model functions and high-level wrappers.
## ADR-02: default geodesic model is great-circle on a sphere (Haversine).
## ADR-01: bearings are directional (0-360), not axial.

# Mean Earth radius (IAU, metres) - used only for reference; not needed for bearing.
.EARTH_RADIUS_M <- 6371000L

# Degrees to radians and back
.d2r <- function(x) x * pi / 180
.r2d <- function(x) x * 180 / pi

# Wrap to [0, 360)
.wrap360 <- function(x) ((x %% 360) + 360) %% 360


# ── Low-level bearing model functions ─────────────────────────────────────────

#' Great-circle initial bearing using the Haversine formula (spherical Earth)
#'
#' Computes the initial bearing from each (from_lat, from_lon) to each
#' (to_lat, to_lon) along the great-circle path on a spherical Earth.
#' Earth radius assumed: 6 371 000 m (IAU mean).
#'
#' Returns `NA` when origin and destination are identical or antipodal
#' (bearing is undefined in both cases).
#'
#' @param from_lat,from_lon Numeric vectors. Origin latitude/longitude, decimal
#'   degrees. Latitude in \[-90, 90\]; longitude in \[-180, 180\].
#' @param to_lat,to_lon Numeric vectors. Destination latitude/longitude,
#'   decimal degrees.
#'
#' @return Numeric vector of initial bearings in degrees clockwise from north,
#'   in \[0, 360).
#'
#' @seealso [bearing_rhumb()], [bearing_vincenty()], [bearing_to_destination()]
#' @export
#' @examples
#' # Due north
#' bearing_haversine(0, 0, 1, 0)
#' # Due east (on the equator)
#' bearing_haversine(0, 0, 0, 1)
#' # Petra to Mecca
#' bearing_haversine(30.3285, 35.4444, 21.4225, 39.8262)
bearing_haversine <- function(from_lat, from_lon, to_lat, to_lon) {
  checkmate::assert_numeric(from_lat, lower = -90,  upper = 90)
  checkmate::assert_numeric(from_lon, lower = -180, upper = 180)
  checkmate::assert_numeric(to_lat,   lower = -90,  upper = 90)
  checkmate::assert_numeric(to_lon,   lower = -180, upper = 180)

  phi1 <- .d2r(from_lat)
  phi2 <- .d2r(to_lat)
  dl   <- .d2r(to_lon - from_lon)

  y <- sin(dl) * cos(phi2)
  x <- cos(phi1) * sin(phi2) - sin(phi1) * cos(phi2) * cos(dl)

  bearing <- .wrap360(.r2d(atan2(y, x)))

  # Same point: return NA
  same <- (abs(from_lat - to_lat) < 1e-10) & (abs(from_lon - to_lon) < 1e-10)
  # Antipodal: |phi1 + phi2| < eps and |dl - pi| < eps
  antipodal <- (abs(phi1 + phi2) < 1e-10) & (abs(abs(dl %% (2 * pi)) - pi) < 1e-4)

  bearing[same | antipodal] <- NA_real_
  bearing
}


#' Rhumb-line (loxodromic) bearing
#'
#' Computes the constant-compass bearing from each (from_lat, from_lon) to
#' each (to_lat, to_lon). A rhumb line maintains a constant bearing relative to
#' meridians. Returns `NA` for identical points; at the poles the formula is
#' undefined and also returns `NA`.
#'
#' Rhumb bearings differ from great-circle bearings, especially for long or
#' high-latitude paths. Some historians argue early Islamic builders used
#' rhumb-line methods; see ADR-02.
#'
#' @inheritParams bearing_haversine
#' @return Numeric vector of rhumb bearings in degrees clockwise from north,
#'   in \[0, 360).
#'
#' @seealso [bearing_haversine()], [bearing_to_destination()]
#' @export
#' @examples
#' # Compare great-circle vs rhumb for a long path
#' bearing_haversine(51.5, -0.1, 40.7, -74.0)
#' bearing_rhumb(51.5, -0.1, 40.7, -74.0)
bearing_rhumb <- function(from_lat, from_lon, to_lat, to_lon) {
  checkmate::assert_numeric(from_lat, lower = -90,  upper = 90)
  checkmate::assert_numeric(from_lon, lower = -180, upper = 180)
  checkmate::assert_numeric(to_lat,   lower = -90,  upper = 90)
  checkmate::assert_numeric(to_lon,   lower = -180, upper = 180)

  phi1 <- .d2r(from_lat)
  phi2 <- .d2r(to_lat)
  dl   <- .d2r(to_lon - from_lon)

  # Adjust dl for anti-meridian crossing
  dl <- ifelse(abs(dl) > pi, ifelse(dl > 0, dl - 2 * pi, dl + 2 * pi), dl)

  # Mercator projection difference
  dpsi <- log(tan(pi / 4 + phi2 / 2) / tan(pi / 4 + phi1 / 2))

  # Use dl/dphi ratio where latitudes are equal (E/W rhumb)
  dphi <- phi2 - phi1
  q    <- ifelse(abs(dpsi) > 1e-10, dphi / dpsi, cos(phi1))

  bearing <- .wrap360(.r2d(atan2(dl, dpsi)))

  same <- (abs(from_lat - to_lat) < 1e-10) & (abs(from_lon - to_lon) < 1e-10)
  bearing[same] <- NA_real_
  bearing
}


#' Geodesic initial bearing on the WGS84 ellipsoid (Vincenty formula)
#'
#' Wraps `geosphere::bearing()` to compute the geodesic initial bearing on
#' the WGS84 ellipsoid. Requires the `geosphere` package (in `Suggests`).
#'
#' @inheritParams bearing_haversine
#' @return Numeric vector of initial bearings in degrees clockwise from north,
#'   in \[0, 360).
#'
#' @seealso [bearing_haversine()], [bearing_to_destination()]
#' @export
#' @examples
#' \dontrun{
#' bearing_vincenty(30.3285, 35.4444, 21.4225, 39.8262)
#' }
bearing_vincenty <- function(from_lat, from_lon, to_lat, to_lon) {
  rlang::check_installed("geosphere",
                         reason = "for WGS84 ellipsoidal bearings via bearing_vincenty()")
  checkmate::assert_numeric(from_lat, lower = -90,  upper = 90)
  checkmate::assert_numeric(from_lon, lower = -180, upper = 180)
  checkmate::assert_numeric(to_lat,   lower = -90,  upper = 90)
  checkmate::assert_numeric(to_lon,   lower = -180, upper = 180)

  p1 <- cbind(from_lon, from_lat)
  p2 <- cbind(to_lon,   to_lat)
  .wrap360(geosphere::bearing(p1, p2))
}


# ── High-level wrappers ────────────────────────────────────────────────────────

#' Compute the bearing from each mosque to a single destination
#'
#' Adds a bearing (in degrees clockwise from north) from every row in `data`
#' to a specified destination point. The bearing model is replaceable; see
#' [bearing_haversine()], [bearing_rhumb()], [bearing_vincenty()].
#'
#' @param data A data frame with columns `lat_col` and `lon_col` (typically
#'   [gibson_qibla] or a subset of it).
#' @param destination A single-row data frame, or a named list/vector, with
#'   fields `latitude` and `longitude` (or the names supplied to `lat_col`
#'   and `lon_col`). Typically one row of [destinations].
#' @param model A bearing model function with signature
#'   `f(from_lat, from_lon, to_lat, to_lon)`. Default: [bearing_haversine()].
#' @param lat_col,lon_col Column names for latitude and longitude in `data`
#'   (and `destination` if it is a data frame). Defaults: `"latitude"`,
#'   `"longitude"`.
#'
#' @return Numeric vector, one value per row of `data`, in \[0, 360).
#'
#' @seealso [bearing_to_candidates()], [angular_difference()],
#'   [signed_angular_error()]
#' @export
#' @examples
#' data(gibson_qibla)
#' data(destinations)
#' mecca <- destinations[destinations$id == "mecca", ]
#' head(bearing_to_destination(gibson_qibla, mecca))
bearing_to_destination <- function(data,
                                   destination,
                                   model   = bearing_haversine,
                                   lat_col = "latitude",
                                   lon_col = "longitude") {
  checkmate::assert_data_frame(data)
  checkmate::assert_string(lat_col)
  checkmate::assert_string(lon_col)
  checkmate::assert_function(model)
  checkmate::assert_true(lat_col %in% names(data),
                         .var.name = paste0("'", lat_col, "' in names(data)"))
  checkmate::assert_true(lon_col %in% names(data),
                         .var.name = paste0("'", lon_col, "' in names(data)"))

  # Accept data frame row or named list/vector
  if (is.data.frame(destination)) {
    if (nrow(destination) != 1L) {
      cli::cli_abort(c(
        "{.arg destination} must have exactly one row.",
        "i" = "Got {nrow(destination)} rows. Subset to a single row first."
      ))
    }
    to_lat <- destination[[lat_col]]
    to_lon <- destination[[lon_col]]
  } else {
    checkmate::assert_numeric(destination[lat_col], lower = -90, upper = 90,
                              .var.name = paste0("destination[\"", lat_col, "\"]"))
    to_lat <- destination[[lat_col]]
    to_lon <- destination[[lon_col]]
  }

  model(data[[lat_col]], data[[lon_col]], to_lat, to_lon)
}


#' Compute bearings from each mosque to all candidate destinations
#'
#' For each row in `data`, computes the bearing to every row in `candidates`
#' and returns a long-format tibble. This is the primary entry point for
#' multi-destination comparison.
#'
#' @inheritParams bearing_to_destination
#' @param candidates A data frame of candidate destinations with at minimum
#'   an `id` column and the columns named by `lat_col` and `lon_col`.
#'   Defaults to the shipped [destinations] table.
#' @param data_id_col Column in `data` used to identify each site in the
#'   output. Defaults to `"row_id"`.
#'
#' @return A tibble with columns: the `data_id_col` from `data`, `destination_id`
#'   (from `candidates$id`), `destination_name` (from `candidates$name` if
#'   present), and `bearing`.
#'
#' @seealso [bearing_to_destination()], [angular_difference()]
#' @export
#' @examples
#' data(gibson_qibla)
#' data(destinations)
#' bearings <- bearing_to_candidates(gibson_qibla)
#' head(bearings)
bearing_to_candidates <- function(data,
                                  candidates   = NULL,
                                  model        = bearing_haversine,
                                  lat_col      = "latitude",
                                  lon_col      = "longitude",
                                  data_id_col  = "row_id") {
  if (is.null(candidates)) {
    candidates <- get("destinations", envir = asNamespace("qiblalab"))
  }
  checkmate::assert_data_frame(data)
  checkmate::assert_data_frame(candidates)
  checkmate::assert_function(model)
  checkmate::assert_string(lat_col)
  checkmate::assert_string(lon_col)
  checkmate::assert_string(data_id_col)
  checkmate::assert_true("id" %in% names(candidates),
                         .var.name = "'id' in names(candidates)")

  has_name <- "name" %in% names(candidates)

  rows <- lapply(seq_len(nrow(candidates)), function(i) {
    dest    <- candidates[i, ]
    bearing <- bearing_to_destination(data, dest, model, lat_col, lon_col)
    out <- tibble::tibble(
      .data_id         = data[[data_id_col]],
      destination_id   = dest$id,
      bearing          = bearing
    )
    if (has_name) out$destination_name <- dest$name
    out
  })

  result <- dplyr::bind_rows(rows)
  names(result)[names(result) == ".data_id"] <- data_id_col
  if (has_name) {
    result <- result[, c(data_id_col, "destination_id", "destination_name", "bearing")]
  }
  result
}
