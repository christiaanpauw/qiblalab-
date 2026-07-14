## ggplot2-based visualisation functions.
## All functions require ggplot2 (Suggests); they call rlang::check_installed()
## at entry and return a customisable ggplot2 object.

# ── plot_rose() ───────────────────────────────────────────────────────────────

#' Rose diagram of mosque orientations
#'
#' Draws a circular histogram (rose diagram) of observed azimuths in compass
#' convention: 0° at the top (North), increasing clockwise.
#'
#' @param data A mosque data frame.
#' @param azimuth_col Name of the azimuth column. Default `"azimuth"`.
#' @param bins Number of equal-width directional bins. Default `36` (10° each).
#' @param fill Bar fill colour. Default `"steelblue"`.
#' @param alpha Bar transparency. Default `0.8`.
#' @param title Optional plot title. If `NULL`, a default is generated.
#'
#' @return A `ggplot2` object. Add further layers with `+`.
#' @seealso [plot_orientation_time()], [plot_cluster_profile()]
#' @export
#' @examples
#' if (requireNamespace("ggplot2", quietly = TRUE)) {
#'   data(qibla_scenarios)
#'   plot_rose(qibla_scenarios$mecca_tradition)
#' }
plot_rose <- function(data,
                      azimuth_col = "azimuth",
                      bins        = 36L,
                      fill        = "steelblue",
                      alpha       = 0.8,
                      title       = NULL) {
  rlang::check_installed("ggplot2", reason = "for plot_rose()")
  checkmate::assert_data_frame(data)
  checkmate::assert_string(azimuth_col)
  checkmate::assert_count(bins, positive = TRUE)
  if (!azimuth_col %in% names(data)) {
    cli::cli_abort("Column {.val {azimuth_col}} not found in {.arg data}.")
  }

  az  <- data[[azimuth_col]]
  az  <- az[!is.na(az)]
  df  <- data.frame(azimuth = az)
  ttl <- if (!is.null(title)) title else
    paste0("Rose diagram  (n = ", length(az), "  bins = ", bins, ")")

  ggplot2::ggplot(df, ggplot2::aes(x = .data$azimuth)) +
    ggplot2::geom_histogram(
      binwidth = 360 / bins,
      boundary = 0,
      closed   = "left",
      fill     = fill,
      alpha    = alpha,
      colour   = "white",
      linewidth = 0.3
    ) +
    ggplot2::coord_polar(start = -pi / 2, direction = -1) +
    ggplot2::scale_x_continuous(
      limits = c(0, 360),
      breaks = seq(0, 315, by = 45),
      labels = c("N", "NE", "E", "SE", "S", "SW", "W", "NW"),
      expand = c(0, 0)
    ) +
    ggplot2::labs(x = NULL, y = "Count", title = ttl) +
    ggplot2::theme_minimal() +
    ggplot2::theme(axis.title.y = ggplot2::element_blank())
}


# ── plot_residuals() ──────────────────────────────────────────────────────────

#' Histogram of angular residuals
#'
#' Plots the distribution of angular errors between observed azimuths and
#' theoretical bearings. Accepts a numeric vector of errors or a
#' `qibla_test_result` object directly.
#'
#' @param x Numeric vector of angular errors, or a `qibla_test_result`.
#' @param signed Logical. If `TRUE` (default) and `x` is a `qibla_test_result`,
#'   use signed errors; otherwise use absolute errors.
#' @param bins Number of histogram bins. Default `30`.
#' @param fill Bar fill colour. Default `"steelblue"`.
#' @param title Optional plot title.
#'
#' @return A `ggplot2` object.
#' @seealso [test_qibla_hypothesis()], [plot_sensitivity()]
#' @export
#' @examples
#' if (requireNamespace("ggplot2", quietly = TRUE)) {
#'   data(qibla_scenarios)
#'   h   <- qibla_hypothesis("Mecca", candidate = "mecca", tolerance = 5)
#'   res <- test_qibla_hypothesis(h, qibla_scenarios$mecca_tradition)
#'   plot_residuals(res)
#' }
plot_residuals <- function(x,
                           signed = TRUE,
                           bins   = 30L,
                           fill   = "steelblue",
                           title  = NULL) {
  rlang::check_installed("ggplot2", reason = "for plot_residuals()")
  if (inherits(x, "qibla_test_result")) {
    x <- if (signed) x$signed_errors else x$errors
  }
  checkmate::assert_numeric(x)
  checkmate::assert_count(bins, positive = TRUE)

  df  <- data.frame(error = x[!is.na(x)])
  lbl <- if (signed) "Signed angular error (degrees)" else "Absolute angular error (degrees)"
  ttl <- if (!is.null(title)) title else "Distribution of angular residuals"

  ggplot2::ggplot(df, ggplot2::aes(x = .data$error)) +
    ggplot2::geom_histogram(bins   = bins,
                            fill   = fill,
                            colour = "white",
                            alpha  = 0.8,
                            linewidth = 0.3) +
    ggplot2::geom_vline(xintercept = 0, linetype = "dashed",
                        colour = "grey40", linewidth = 0.5) +
    ggplot2::labs(x = lbl, y = "Count", title = ttl) +
    ggplot2::theme_minimal()
}


# ── plot_orientation_time() ───────────────────────────────────────────────────

#' Observed azimuth vs. construction date
#'
#' Scatter plot of observed mosque orientations against construction year, with
#' an optional colour aesthetic.
#'
#' @param data A mosque data frame.
#' @param azimuth_col Azimuth column. Default `"azimuth"`.
#' @param time_col Numeric date column. Default `"year_ce_min"`.
#' @param colour_col Optional column name for point colour.
#' @param title Optional plot title.
#'
#' @return A `ggplot2` object.
#' @seealso [plot_rose()]
#' @export
#' @examples
#' if (requireNamespace("ggplot2", quietly = TRUE)) {
#'   data(qibla_scenarios)
#'   plot_orientation_time(qibla_scenarios$chronological)
#' }
plot_orientation_time <- function(data,
                                  azimuth_col = "azimuth",
                                  time_col    = "year_ce_min",
                                  colour_col  = NULL,
                                  title       = NULL) {
  rlang::check_installed("ggplot2", reason = "for plot_orientation_time()")
  checkmate::assert_data_frame(data)
  checkmate::assert_string(azimuth_col)
  checkmate::assert_string(time_col)
  checkmate::assert_string(colour_col, null.ok = TRUE)
  for (col in c(azimuth_col, time_col)) {
    if (!col %in% names(data)) {
      cli::cli_abort("Column {.val {col}} not found in {.arg data}.")
    }
  }
  if (!is.null(colour_col) && !colour_col %in% names(data)) {
    cli::cli_abort("Colour column {.val {colour_col}} not found in {.arg data}.")
  }

  ttl <- if (!is.null(title)) title else "Mosque orientation vs. construction date"

  p <- ggplot2::ggplot(
    data,
    ggplot2::aes(x = .data[[time_col]], y = .data[[azimuth_col]])
  ) +
    ggplot2::geom_point(alpha = 0.6, size = 1.5) +
    ggplot2::scale_y_continuous(
      limits = c(0, 360),
      breaks = c(0, 90, 180, 270, 360),
      labels = c("N (0 deg)", "E (90 deg)", "S (180 deg)", "W (270 deg)", "N (360 deg)")
    ) +
    ggplot2::labs(x = "Construction year (CE)", y = "Observed azimuth", title = ttl) +
    ggplot2::theme_minimal()

  if (!is.null(colour_col)) {
    p <- p + ggplot2::aes(colour = .data[[colour_col]])
  }

  p
}


# ── plot_sensitivity() ────────────────────────────────────────────────────────

#' Sensitivity curve: proportion consistent vs. tolerance
#'
#' Plots how the proportion of consistent mosques changes as the angular
#' tolerance threshold varies. Accepts either a sensitivity tibble (from
#' [test_qibla_hypothesis()] or [run_sensitivity_analysis()]) or a
#' `qibla_test_result` directly.
#'
#' @param x A sensitivity tibble with columns `tolerance` and `proportion`, or
#'   a `qibla_test_result`.
#' @param title Optional plot title.
#'
#' @return A `ggplot2` object.
#' @seealso [test_qibla_hypothesis()], [run_sensitivity_analysis()]
#' @export
#' @examples
#' if (requireNamespace("ggplot2", quietly = TRUE)) {
#'   data(qibla_scenarios)
#'   h   <- qibla_hypothesis("Mecca", candidate = "mecca", tolerance = 5)
#'   res <- test_qibla_hypothesis(h, qibla_scenarios$mecca_tradition)
#'   plot_sensitivity(res)
#' }
plot_sensitivity <- function(x, title = NULL) {
  rlang::check_installed("ggplot2", reason = "for plot_sensitivity()")
  if (inherits(x, "qibla_test_result")) x <- x$sensitivity
  checkmate::assert_data_frame(x)
  if (!all(c("tolerance", "proportion") %in% names(x))) {
    cli::cli_abort("{.arg x} must have columns {.val tolerance} and {.val proportion}.")
  }

  ttl <- if (!is.null(title)) title else "Sensitivity to angular tolerance"

  has_sd <- "sd_proportion" %in% names(x)

  p <- ggplot2::ggplot(x, ggplot2::aes(x = .data$tolerance, y = .data$proportion)) +
    ggplot2::geom_line(colour = "steelblue", linewidth = 0.8) +
    ggplot2::geom_point(colour = "steelblue", size = 2) +
    ggplot2::scale_y_continuous(
      limits = c(0, 1),
      labels = function(v) paste0(round(v * 100), "%")
    ) +
    ggplot2::labs(x = "Tolerance (degrees)", y = "Proportion consistent", title = ttl) +
    ggplot2::theme_minimal()

  if (has_sd) {
    p <- p + ggplot2::geom_ribbon(
      ggplot2::aes(
        ymin = pmax(0, .data$proportion - .data$sd_proportion),
        ymax = pmin(1, .data$proportion + .data$sd_proportion)
      ),
      alpha = 0.2, fill = "steelblue"
    )
  }

  p
}


# ── plot_candidate_comparison() ───────────────────────────────────────────────

#' Bar chart of mosques by nearest candidate destination
#'
#' Summarises the output of [compare_qibla_hypotheses()] as a bar chart showing
#' how many mosques are closest to each candidate.
#'
#' @param compare_result A tibble from [compare_qibla_hypotheses()].
#' @param exclude_ambiguous Logical. If `TRUE`, exclude mosques flagged as
#'   `ambiguous`. Default `FALSE`.
#' @param fill Bar fill colour. Default `"steelblue"`.
#' @param title Optional plot title.
#'
#' @return A `ggplot2` object.
#' @seealso [compare_qibla_hypotheses()]
#' @export
#' @examples
#' if (requireNamespace("ggplot2", quietly = TRUE)) {
#'   data(qibla_scenarios)
#'   cmp <- compare_qibla_hypotheses(qibla_scenarios$two_traditions)
#'   plot_candidate_comparison(cmp)
#' }
plot_candidate_comparison <- function(compare_result,
                                      exclude_ambiguous = FALSE,
                                      fill              = "steelblue",
                                      title             = NULL) {
  rlang::check_installed("ggplot2", reason = "for plot_candidate_comparison()")
  checkmate::assert_data_frame(compare_result)
  if (!"nearest_id" %in% names(compare_result)) {
    cli::cli_abort("{.arg compare_result} must have a {.val nearest_id} column.",
                   "i" = "Pass the output of {.fn compare_qibla_hypotheses}.")
  }

  df <- compare_result[!is.na(compare_result$nearest_id), ]
  if (exclude_ambiguous && "ambiguous" %in% names(df)) {
    df <- df[!df$ambiguous, ]
  }
  counts <- as.data.frame(table(nearest_id = df$nearest_id), stringsAsFactors = FALSE)

  ttl <- if (!is.null(title)) title else "Mosques by nearest candidate destination"

  ggplot2::ggplot(counts,
                  ggplot2::aes(x = .data$nearest_id, y = .data$Freq)) +
    ggplot2::geom_col(fill = fill, alpha = 0.85) +
    ggplot2::labs(x = "Nearest candidate", y = "Number of mosques", title = ttl) +
    ggplot2::theme_minimal()
}


# ── plot_cluster_profile() ────────────────────────────────────────────────────

#' Faceted rose diagrams per cluster
#'
#' Draws one polar histogram per cluster, making it easy to compare the
#' directional distributions across cluster solutions.
#'
#' @param cluster_result A `qibla_cluster_result` from [cluster_qiblas()].
#' @param bins Number of rose bins per cluster. Default `24` (15° each).
#' @param fill Bar fill colour. Default `"steelblue"`.
#' @param title Optional plot title.
#'
#' @return A `ggplot2` object.
#' @seealso [cluster_qiblas()], [plot_rose()]
#' @export
#' @examples
#' if (requireNamespace("ggplot2", quietly = TRUE)) {
#'   data(qibla_scenarios)
#'   res <- cluster_qiblas(qibla_scenarios$two_traditions, k = 2, seed = 1)
#'   plot_cluster_profile(res)
#' }
plot_cluster_profile <- function(cluster_result,
                                 bins  = 24L,
                                 fill  = "steelblue",
                                 title = NULL) {
  rlang::check_installed("ggplot2", reason = "for plot_cluster_profile()")
  checkmate::assert_class(cluster_result, "qibla_cluster_result")
  checkmate::assert_count(bins, positive = TRUE)

  data <- cluster_result$data
  az   <- data[[cluster_result$azimuth_col]]
  cl   <- cluster_result$assignments

  ok <- !is.na(az) & !is.na(cl)
  df <- data.frame(azimuth = az[ok],
                   cluster = factor(paste0("C", cl[ok])))

  ttl <- if (!is.null(title)) title else
    paste0("Cluster orientation profiles  (k = ", cluster_result$k, ")")

  ggplot2::ggplot(df, ggplot2::aes(x = .data$azimuth)) +
    ggplot2::geom_histogram(
      binwidth  = 360 / bins,
      boundary  = 0,
      closed    = "left",
      fill      = fill,
      colour    = "white",
      alpha     = 0.8,
      linewidth = 0.3
    ) +
    ggplot2::coord_polar(start = -pi / 2, direction = -1) +
    ggplot2::scale_x_continuous(
      limits = c(0, 360),
      breaks = c(0, 90, 180, 270),
      labels = c("N", "E", "S", "W"),
      expand = c(0, 0)
    ) +
    ggplot2::facet_wrap(~cluster) +
    ggplot2::labs(x = NULL, y = "Count", title = ttl) +
    ggplot2::theme_minimal() +
    ggplot2::theme(axis.title.y = ggplot2::element_blank())
}
