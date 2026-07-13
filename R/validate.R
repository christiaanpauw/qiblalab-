# Expected schema for the Gibson 2021 dataset
.EXPECTED_COLS <- c(
  "Gibson Classification", "Year CE", "Year AH", "age_group",
  "City", "Country", "Mosque Name", "Rebuilt",
  "Latitude", "Longitude", "dir", "Website Link"
)

.VALID_CLASSIFICATIONS <- c(
  "Petra", "Between", "Mecca", "Parallel", "Jerusalem", "Unknown", "unknown"
)

#' Validate raw Qibla data against the expected schema
#'
#' Checks column presence, types, value ranges, and MD5 against the pinned
#' Figshare version. Returns a validation report object; prints a summary with
#' pass/warn/error indicators. Does not stop on the first issue — all problems
#' are collected and reported together.
#'
#' @param raw A data frame returned by [import_qibla_data()].
#'
#' @return A `qibla_validation` object (list with `$issues`, `$n_rows`,
#'   `$n_cols`). Print it for a human-readable summary. Use
#'   `validation_errors(x)` and `validation_warnings(x)` to extract issues
#'   programmatically.
#'
#' @seealso [import_qibla_data()], [prepare_qibla_data()]
#' @export
#' @examples
#' \dontrun{
#' raw <- import_qibla_data()
#' validate_qibla_data(raw)
#' }
validate_qibla_data <- function(raw) {
  checkmate::assert_data_frame(raw)

  issues <- list()
  add_issue <- function(severity, field, msg) {
    issues[[length(issues) + 1L]] <<- list(severity = severity, field = field, message = msg)
  }

  # ── Column presence ────────────────────────────────────────────────────────
  missing_cols <- setdiff(.EXPECTED_COLS, names(raw))
  for (col in missing_cols) {
    add_issue("error", col, paste0("Required column '", col, "' is absent"))
  }

  # ── Row count ──────────────────────────────────────────────────────────────
  if (nrow(raw) != 160L) {
    add_issue("warning", "(rows)",
              paste0("Expected 160 rows; found ", nrow(raw)))
  }

  # ── Latitude / Longitude ranges ────────────────────────────────────────────
  if ("Latitude" %in% names(raw)) {
    bad_lat <- sum(!is.na(raw$Latitude) & (raw$Latitude < -90 | raw$Latitude > 90))
    if (bad_lat > 0L) add_issue("error", "Latitude",
                                paste0(bad_lat, " value(s) outside [-90, 90]"))
  }
  if ("Longitude" %in% names(raw)) {
    bad_lon <- sum(!is.na(raw$Longitude) & (raw$Longitude < -180 | raw$Longitude > 180))
    if (bad_lon > 0L) add_issue("error", "Longitude",
                                paste0(bad_lon, " value(s) outside [-180, 180]"))
  }

  # ── Azimuth range ─────────────────────────────────────────────────────────
  if ("dir" %in% names(raw)) {
    bad_az <- sum(!is.na(raw$dir) & (raw$dir < 0 | raw$dir >= 360))
    if (bad_az > 0L) add_issue("warning", "dir",
                               paste0(bad_az, " azimuth value(s) outside [0, 360)"))
  }

  # ── Gibson Classification values ───────────────────────────────────────────
  if ("Gibson Classification" %in% names(raw)) {
    unknown_vals <- setdiff(
      unique(raw$`Gibson Classification`),
      .VALID_CLASSIFICATIONS
    )
    if (length(unknown_vals) > 0L) {
      add_issue("warning", "Gibson Classification",
                paste0("Unexpected value(s): ",
                       paste(unknown_vals, collapse = ", ")))
    }
  }

  # ── MD5 against pinned version ─────────────────────────────────────────────
  prov <- attr(raw, "provenance")
  if (!is.null(prov$md5) && prov$md5 != .FIGSHARE_MD5) {
    add_issue("warning", "(file)",
              paste0(
                "MD5 does not match the pinned version (", .FIGSHARE_MD5, "). ",
                "This may be a newer Figshare release. ",
                "Review the data dictionary before calling prepare_qibla_data()."
              ))
  }

  structure(
    list(issues = issues, n_rows = nrow(raw), n_cols = ncol(raw)),
    class = "qibla_validation"
  )
}

#' @export
print.qibla_validation <- function(x, ...) {
  errors   <- Filter(function(i) i$severity == "error",   x$issues)
  warnings <- Filter(function(i) i$severity == "warning", x$issues)

  cli::cli_inform(
    "Validated {x$n_rows} row{?s} x {x$n_cols} column{?s}."
  )

  if (length(x$issues) == 0L) {
    cli::cli_alert_success("No issues found.")
    return(invisible(x))
  }

  if (length(errors) > 0L) {
    cli::cli_alert_danger("{length(errors)} error{?s}:")
    for (e in errors) {
      cli::cli_bullets(c("x" = "{e$field}: {e$message}"))
    }
  }
  if (length(warnings) > 0L) {
    cli::cli_alert_warning("{length(warnings)} warning{?s}:")
    for (w in warnings) {
      cli::cli_bullets(c("!" = "{w$field}: {w$message}"))
    }
  }
  invisible(x)
}

#' Extract errors from a validation report
#' @param x A `qibla_validation` object.
#' @return A data frame of error-level issues.
#' @export
validation_errors <- function(x) {
  checkmate::assert_class(x, "qibla_validation")
  rows <- Filter(function(i) i$severity == "error", x$issues)
  if (length(rows) == 0L) return(tibble::tibble(field = character(), message = character()))
  tibble::tibble(
    field   = vapply(rows, `[[`, character(1L), "field"),
    message = vapply(rows, `[[`, character(1L), "message")
  )
}

#' Extract warnings from a validation report
#' @param x A `qibla_validation` object.
#' @return A data frame of warning-level issues.
#' @export
validation_warnings <- function(x) {
  checkmate::assert_class(x, "qibla_validation")
  rows <- Filter(function(i) i$severity == "warning", x$issues)
  if (length(rows) == 0L) return(tibble::tibble(field = character(), message = character()))
  tibble::tibble(
    field   = vapply(rows, `[[`, character(1L), "field"),
    message = vapply(rows, `[[`, character(1L), "message")
  )
}
