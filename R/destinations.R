#' Candidate destination table
#'
#' @description
#' A table of candidate qibla destinations for use with [bearing_to_destination()]
#' and [bearing_to_candidates()]. Contains Mecca, Petra, and Jerusalem with
#' coordinates for the specific reference points stated in `rationale`.
#'
#' **This is an editable hypothesis table, not a declaration of historical
#' truth.** All three destinations are included as equal-status hypotheses
#' (ADR-07). Researchers may add, remove, or replace rows.
#'
#' @format A tibble with 3 rows and 8 columns:
#' \describe{
#'   \item{id}{Character. Machine-readable identifier (`"mecca"`, `"petra"`,
#'     `"jerusalem"`).}
#'   \item{name}{Character. Human-readable name.}
#'   \item{latitude}{Double. Decimal degrees, WGS84.}
#'   \item{longitude}{Double. Decimal degrees, WGS84.}
#'   \item{valid_from}{Integer. Start of the period for which this destination
#'     is historically proposed, CE. `NA` if unbounded or unknown.}
#'   \item{valid_to}{Integer. End of the period. `NA` if unbounded or unknown.}
#'   \item{rationale}{Character. Description of the specific reference point
#'     used and the historical argument for this destination.}
#'   \item{citation}{Character. Key citation(s) for the hypothesis.}
#' }
#'
#' @section Adding destinations:
#' ```r
#' my_dest <- rbind(
#'   destinations,
#'   tibble::tibble(
#'     id = "medina", name = "Medina", latitude = 24.4672, longitude = 39.6150,
#'     valid_from = NA_integer_, valid_to = NA_integer_,
#'     rationale = "Proposed early qibla direction toward Medina.",
#'     citation  = NA_character_
#'   )
#' )
#' bearing_to_candidates(gibson_qibla, my_dest)
#' ```
#'
#' @examples
#' data(destinations)
#' destinations[, c("id", "name", "latitude", "longitude")]
"destinations"
