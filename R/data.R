#' Early Islamic Qibla Database (Gibson 2021)
#'
#' @description
#' A cleaned version of Dan Gibson's Early Islamic Qibla Database, covering
#' 160 mosques and prayer sites from the first three centuries of Islam.
#' All original columns are preserved. See `attr(gibson_qibla, "audit_log")`
#' for every cleaning action applied and `attr(gibson_qibla, "provenance")`
#' for full source metadata.
#'
#' @format A data frame with 160 rows and 17 columns:
#' \describe{
#'   \item{row_id}{Integer. Immutable 1-based index into the original spreadsheet row.}
#'   \item{mosque_name}{Character. Primary name of the mosque or prayer site.}
#'   \item{city}{Character. City or site name.}
#'   \item{country}{Character. Country (spelling errors corrected; originals in `country_source`).}
#'   \item{country_source}{Character. Country name as it appears in the source file.}
#'   \item{latitude}{Double. Decimal degrees, WGS84 assumed.}
#'   \item{longitude}{Double. Decimal degrees, WGS84 assumed.}
#'   \item{year_ce}{Character. Construction date in CE as recorded by Gibson. May be an integer
#'     year, a century range (e.g. `"700-799"`), or `"Unknown"`.}
#'   \item{year_ce_min}{Integer. Lower bound of `year_ce` parsed to an integer.
#'     Equals `year_ce_max` for exact dates. `NA` when date is unknown.}
#'   \item{year_ce_max}{Integer. Upper bound of `year_ce` parsed to an integer.
#'     Equals `year_ce_min` for exact dates. `NA` when date is unknown.}
#'   \item{year_ah}{Double. Construction date in AH. `NA` for 54 records.}
#'   \item{age_group}{Character. Gibson's historical-period label (e.g. `"Umayyad"`, `"Abbasid"`).}
#'   \item{gibson_classification}{Character. Gibson's destination hypothesis:
#'     `"Petra"`, `"Between"`, `"Mecca"`, `"Parallel"`, `"Jerusalem"`, or `"Unknown"`.
#'     This is one analyst's interpretation, not ground truth. See ADR-07.}
#'   \item{gibson_classification_source}{Character. Classification as it appears in the source file.}
#'   \item{azimuth}{Double. Observed building or qibla azimuth, degrees clockwise from north.
#'     Treated as directional (0-360). `NA` for 28 sites with no measured orientation. See ADR-01.}
#'   \item{rebuilt}{Character. Free-text rebuild date(s) in mixed formats (CE, AH, "never").
#'     Not machine-parsed; retain as-is. See ADR-05.}
#'   \item{website}{Character. URL to Gibson's nabataea.net entry for the site.}
#' }
#'
#' @section Provenance:
#' `attr(gibson_qibla, "provenance")` returns a named list with DOI, download
#' URL, MD5 checksum, download date, licence, and full citation string.
#'
#' @section Audit log:
#' `attr(gibson_qibla, "audit_log")` returns a data frame recording every
#' cleaning action applied to the source data, including column renaming,
#' case standardisation, and country-name corrections.
#'
#' @section Missing fields:
#' `attr(gibson_qibla, "missing_fields")` lists fields specified in the
#' qiblalab data model (design doc §3) that are absent from the Gibson source.
#'
#' @source
#' Gibson, Dan (2021). *Early Islamic Qibla Database 2021*. figshare. Dataset.
#' <https://doi.org/10.6084/m9.figshare.13570655.v2>. Licence: CC BY 4.0.
#'
#' @examples
#' data(gibson_qibla)
#' nrow(gibson_qibla)
#' attr(gibson_qibla, "provenance")$citation
"gibson_qibla"
