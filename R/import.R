#' Import the Early Islamic Qibla Database from a local XLSX file
#'
#' Reads the raw Figshare spreadsheet and returns a data frame with all 12
#' original columns preserved plus an immutable `row_id`. No cleaning is
#' applied — call [validate_qibla_data()] to surface anomalies and
#' [prepare_qibla_data()] to produce the analysis-ready form.
#'
#' @param path Character scalar. Path to `Mosques_Jan2021.xlsx`. If `NULL`
#'   (default), calls [download_qibla_data()] to locate or fetch the file.
#'
#' @return A data frame with 160 rows and 13 columns (12 original + `row_id`).
#'   Column names are exactly as in the source file. The `provenance` attribute
#'   records the source file, DOI, MD5, and import timestamp.
#'
#' @seealso [download_qibla_data()], [validate_qibla_data()], [prepare_qibla_data()]
#' @export
#' @examples
#' \dontrun{
#' raw <- import_qibla_data()
#' names(raw)
#' }
import_qibla_data <- function(path = NULL) {
  rlang::check_installed("readxl", reason = "to read the Figshare XLSX file")

  if (is.null(path)) {
    path <- download_qibla_data()
  }
  checkmate::assert_file_exists(path, extension = "xlsx")

  raw <- readxl::read_excel(path, sheet = "mosques")
  raw <- tibble::add_column(raw, row_id = seq_len(nrow(raw)), .before = 1L)

  attr(raw, "provenance") <- list(
    source_file  = basename(path),
    doi          = .FIGSHARE_DOI,
    download_url = .FIGSHARE_URL,
    md5          = as.character(tools::md5sum(path)),
    licence      = .FIGSHARE_LICENCE,
    licence_url  = "https://creativecommons.org/licenses/by/4.0/",
    citation     = paste0(
      "Gibson, Dan (2021). Early Islamic Qibla Database 2021. ",
      "figshare. Dataset. https://doi.org/", .FIGSHARE_DOI
    ),
    imported_at  = format(Sys.time(), "%Y-%m-%dT%H:%M:%S%z")
  )

  raw
}
