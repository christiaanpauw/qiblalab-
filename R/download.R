# Figshare version 2 constants - update if a new release is pinned
.FIGSHARE_URL      <- "https://ndownloader.figshare.com/files/26042708"
.FIGSHARE_DOI      <- "10.6084/m9.figshare.13570655.v2"
.FIGSHARE_MD5      <- "af8a9b535d3930d989b395744a85e4df"
.FIGSHARE_FILENAME <- "Mosques_Jan2021.xlsx"
.FIGSHARE_LICENCE  <- "CC BY 4.0"

qibla_cache_dir <- function() {
  tools::R_user_dir("qiblalab", "cache")
}

#' Download the Early Islamic Qibla Database from Figshare
#'
#' Downloads `Mosques_Jan2021.xlsx` (Gibson 2021, DOI:
#' 10.6084/m9.figshare.13570655.v2) from Figshare and caches it locally.
#' Verifies the MD5 checksum after download. Will not overwrite an existing
#' cached file unless `force = TRUE`.
#'
#' @param dir Character scalar. Directory in which to save the file.
#'   Defaults to `tools::R_user_dir("qiblalab", "cache")`.
#' @param force Logical scalar. Re-download even if the file already exists.
#'   Default `FALSE`. Does not silently replace a file whose MD5 matches - only
#'   re-downloads if forced or if the cached file is corrupt.
#'
#' @return Invisibly, the path to the local file.
#'
#' @section Data attribution:
#' Gibson, Dan (2021). *Early Islamic Qibla Database 2021*. figshare.
#' <https://doi.org/10.6084/m9.figshare.13570655.v2>. Licence: CC BY 4.0.
#'
#' @seealso [import_qibla_data()], [validate_qibla_data()], [prepare_qibla_data()]
#' @export
#' @examples
#' \dontrun{
#' path <- download_qibla_data()
#' }
download_qibla_data <- function(dir = qibla_cache_dir(), force = FALSE) {
  checkmate::assert_string(dir)
  checkmate::assert_flag(force)

  dest <- file.path(dir, .FIGSHARE_FILENAME)

  if (file.exists(dest) && !force) {
    actual <- as.character(tools::md5sum(dest))
    if (actual == .FIGSHARE_MD5) {
      cli::cli_inform(c("v" = "Using cached file: {.file {dest}}"))
      return(invisible(dest))
    }
    expected_md5 <- .FIGSHARE_MD5
    cli::cli_warn(c(
      "Cached file exists but MD5 does not match the pinned version.",
      "i" = "Expected: {.val {expected_md5}}",
      "i" = "Actual:   {.val {actual}}",
      "i" = "Re-downloading. Use {.code force = TRUE} to always re-download."
    ))
  }

  if (!dir.exists(dir)) {
    dir.create(dir, recursive = TRUE, showWarnings = FALSE)
  }

  src_url <- .FIGSHARE_URL
  cli::cli_inform("Downloading {.url {src_url}}")
  utils::download.file(.FIGSHARE_URL, destfile = dest, mode = "wb", quiet = TRUE)

  actual <- as.character(tools::md5sum(dest))
  if (actual != .FIGSHARE_MD5) {
    file.remove(dest)
    expected_md5 <- .FIGSHARE_MD5
    doi          <- .FIGSHARE_DOI
    cli::cli_abort(c(
      "MD5 mismatch after download - file removed.",
      "x" = "Expected: {.val {expected_md5}}",
      "x" = "Actual:   {.val {actual}}",
      "i" = "Try again or check the Figshare page: {.url https://doi.org/{doi}}"
    ))
  }

  cli::cli_inform(c("v" = "Downloaded and verified: {.file {dest}}"))
  invisible(dest)
}
