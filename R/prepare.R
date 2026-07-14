#' Prepare raw Qibla data for analysis
#'
#' Applies the documented cleaning pipeline to data returned by
#' [import_qibla_data()], producing the same form as the shipped
#' [gibson_qibla] dataset. Every action is recorded in an audit log attached
#' to the result as `attr(., "audit_log")`.
#'
#' Permitted operations (see ADR-06):
#' - Column renaming to snake_case (original names retained in source columns)
#' - Case standardisation of categorical values
#' - Spelling corrections in `Country` (originals retained in `country_source`)
#' - Parsing of `Year CE` strings into `year_ce_min` / `year_ce_max` integers
#'
#' Not done here (deliberate — see ADRs):
#' - Dropping or imputing records
#' - Parsing `Rebuilt` free text
#' - Any bearing or orientation calculation
#'
#' @param raw A data frame returned by [import_qibla_data()].
#'
#' @return A tibble with 17 columns (see [gibson_qibla] for full documentation).
#'   Carries `provenance`, `audit_log`, and `missing_fields` attributes.
#'
#' @seealso [import_qibla_data()], [validate_qibla_data()], [gibson_qibla]
#' @export
#' @examples
#' \dontrun{
#' raw     <- import_qibla_data()
#' cleaned <- prepare_qibla_data(raw)
#' attr(cleaned, "audit_log")
#' }
prepare_qibla_data <- function(raw) {
  checkmate::assert_data_frame(raw)
  missing_cols <- setdiff(.EXPECTED_COLS, names(raw))
  if (length(missing_cols) > 0L) {
    cli::cli_abort(c(
      "Raw data is missing required column(s).",
      "x" = paste(missing_cols, collapse = ", "),
      "i" = "Did you pass the output of {.fn import_qibla_data}?"
    ))
  }

  audit  <- list()
  log_action <- function(column, original, corrected, n, reason) {
    audit[[length(audit) + 1L]] <<- list(
      column    = column,
      original  = original,
      corrected = corrected,
      n_records = n,
      reason    = reason
    )
  }

  df <- raw

  # ── 1. Rename to snake_case ────────────────────────────────────────────────
  old_names <- names(df)
  rename_map <- c(
    "Gibson Classification" = "gibson_classification",
    "Year CE"               = "year_ce",
    "Year AH"               = "year_ah",
    "age_group"             = "age_group",
    "City"                  = "city",
    "Country"               = "country",
    "Mosque Name"           = "mosque_name",
    "Rebuilt"               = "rebuilt",
    "Latitude"              = "latitude",
    "Longitude"             = "longitude",
    "dir"                   = "azimuth",
    "Website Link"          = "website",
    "row_id"                = "row_id"
  )
  names(df) <- dplyr::recode(names(df), !!!rename_map)
  log_action("(all)", paste(old_names, collapse = ", "),
             paste(names(df), collapse = ", "), nrow(df),
             "Column names converted to snake_case; 'dir' renamed to 'azimuth'")

  # ── 2. Retain pre-correction source values ─────────────────────────────────
  df$gibson_classification_source <- df$gibson_classification
  df$country_source               <- df$country

  # ── 3. Standardise Gibson Classification case ──────────────────────────────
  n_lower <- sum(df$gibson_classification == "unknown", na.rm = TRUE)
  if (n_lower > 0L) {
    df$gibson_classification[df$gibson_classification == "unknown"] <- "Unknown"
    log_action("gibson_classification", "unknown", "Unknown", n_lower,
               "Case inconsistency: 'unknown' and 'Unknown' are the same category")
  }

  # ── 4. Fix country name typos ──────────────────────────────────────────────
  country_fixes <- list(
    list(from = "iran",        to = "Iran",       reason = "Lowercase country name"),
    list(from = "Somolia",     to = "Somalia",    reason = "Spelling error"),
    list(from = "Uzbeckistan", to = "Uzbekistan", reason = "Spelling error"),
    list(from = "Lybia",       to = "Libya",      reason = "Spelling error")
  )
  for (fix in country_fixes) {
    n <- sum(df$country == fix$from, na.rm = TRUE)
    if (n > 0L) {
      df$country[df$country == fix$from] <- fix$to
      log_action("country", fix$from, fix$to, n, fix$reason)
    }
  }

  # ── 5. Parse Year CE into numeric min/max bounds ───────────────────────────
  parsed <- parse_year_ce(df$year_ce)
  df$year_ce_min <- parsed$min
  df$year_ce_max <- parsed$max
  log_action(
    "year_ce", "character (mixed)", "year_ce_min + year_ce_max (integer)", nrow(df),
    paste0(
      "Derived integer bounds. Ranges like '700-799' use stated bounds. ",
      "'unknown'/'Unknown' yields NA. Exact years give min == max."
    )
  )

  # ── 6. Select and order columns ────────────────────────────────────────────
  df <- df[, c(
    "row_id", "mosque_name", "city", "country", "country_source",
    "latitude", "longitude",
    "year_ce", "year_ce_min", "year_ce_max", "year_ah",
    "age_group", "gibson_classification", "gibson_classification_source",
    "azimuth", "rebuilt", "website"
  )]

  # ── Attach metadata ────────────────────────────────────────────────────────
  audit_df <- do.call(
    rbind,
    lapply(audit, function(x) as.data.frame(x, stringsAsFactors = FALSE))
  )

  src_prov <- attr(raw, "provenance")
  attr(df, "provenance")     <- src_prov
  attr(df, "audit_log")      <- audit_df
  attr(df, "missing_fields") <- c(
    "azimuth_uncertainty", "coordinate_uncertainty", "date_confidence",
    "measurement_method", "orientation_source", "faction", "dynasty",
    "structure_status", "prayer_wall_identification", "bibliography",
    "data_quality_flags"
  )

  tibble::as_tibble(df)
}

# Vectorised Year CE parser — returns a data.frame with columns `min` and `max`
parse_year_ce <- function(x) {
  result <- data.frame(
    min = rep(NA_integer_, length(x)),
    max = rep(NA_integer_, length(x))
  )
  for (i in seq_along(x)) {
    s <- trimws(x[[i]])
    if (is.na(s) || tolower(s) == "unknown") next
    if (grepl("^\\d{3,4}-\\d{3,4}$", s)) {
      parts        <- as.integer(strsplit(s, "-")[[1L]])
      result$min[i] <- parts[[1L]]
      result$max[i] <- parts[[2L]]
    } else if (grepl("^\\d{3,4}$", s)) {
      yr            <- as.integer(s)
      result$min[i] <- yr
      result$max[i] <- yr
    }
  }
  result
}
