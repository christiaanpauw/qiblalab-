## Geographic map functions for mosque locations and cluster assignments.
## Requires ggplot2 (Suggests). Returns customisable ggplot2 objects so users
## can add sf layers, basemaps, or facets with `+`.

# ── map_mosques() ─────────────────────────────────────────────────────────────

#' Map of mosque locations
#'
#' Plots mosque latitude/longitude as a point map. No basemap is bundled; add
#' one via the `sf` or `ggmap` ecosystem using `+`.
#'
#' @param data A mosque data frame with `latitude` and `longitude` columns.
#' @param lat_col,lon_col Column names. Default `"latitude"` / `"longitude"`.
#' @param colour_col Optional column name for point colour.
#' @param size Point size. Default `1.5`.
#' @param alpha Point transparency. Default `0.7`.
#' @param title Optional plot title.
#'
#' @return A `ggplot2` object.
#' @seealso [map_clusters()]
#' @export
#' @examples
#' \dontrun{
#' data(gibson_qibla)
#' map_mosques(gibson_qibla)
#' map_mosques(gibson_qibla, colour_col = "age_group")
#' }
map_mosques <- function(data,
                        lat_col    = "latitude",
                        lon_col    = "longitude",
                        colour_col = NULL,
                        size       = 1.5,
                        alpha      = 0.7,
                        title      = NULL) {
  rlang::check_installed("ggplot2", reason = "for map_mosques()")
  checkmate::assert_data_frame(data)
  checkmate::assert_string(lat_col)
  checkmate::assert_string(lon_col)
  checkmate::assert_string(colour_col, null.ok = TRUE)
  for (col in c(lat_col, lon_col)) {
    if (!col %in% names(data)) {
      cli::cli_abort("Column {.val {col}} not found in {.arg data}.")
    }
  }
  if (!is.null(colour_col) && !colour_col %in% names(data)) {
    cli::cli_abort("Colour column {.val {colour_col}} not found in {.arg data}.")
  }

  n   <- nrow(data)
  ttl <- if (!is.null(title)) title else paste0("Mosque locations  (n = ", n, ")")

  p <- ggplot2::ggplot(
    data,
    ggplot2::aes(x = .data[[lon_col]], y = .data[[lat_col]])
  ) +
    ggplot2::geom_point(size = size, alpha = alpha) +
    ggplot2::coord_fixed() +
    ggplot2::labs(x = "Longitude", y = "Latitude", title = ttl) +
    ggplot2::theme_minimal()

  if (!is.null(colour_col)) {
    p <- p + ggplot2::aes(colour = .data[[colour_col]])
  }

  p
}


# ── map_clusters() ────────────────────────────────────────────────────────────

#' Map of cluster assignments
#'
#' Plots mosque locations coloured by cluster assignment from [cluster_qiblas()].
#' Mosques without an assigned cluster are plotted in grey.
#'
#' @param cluster_result A `qibla_cluster_result` from [cluster_qiblas()].
#' @param lat_col,lon_col Latitude/longitude column names in the embedded data
#'   frame. Default `"latitude"` / `"longitude"`.
#' @param size Point size. Default `1.5`.
#' @param alpha Point transparency. Default `0.7`.
#' @param title Optional plot title.
#'
#' @return A `ggplot2` object.
#' @seealso [cluster_qiblas()], [map_mosques()]
#' @export
#' @examples
#' \dontrun{
#' data(gibson_qibla)
#' res <- cluster_qiblas(gibson_qibla, k = 3, seed = 1)
#' map_clusters(res)
#' }
map_clusters <- function(cluster_result,
                         lat_col = "latitude",
                         lon_col = "longitude",
                         size    = 1.5,
                         alpha   = 0.7,
                         title   = NULL) {
  rlang::check_installed("ggplot2", reason = "for map_clusters()")
  checkmate::assert_class(cluster_result, "qibla_cluster_result")
  checkmate::assert_string(lat_col)
  checkmate::assert_string(lon_col)

  data <- cluster_result$data
  for (col in c(lat_col, lon_col)) {
    if (!col %in% names(data)) {
      cli::cli_abort("Column {.val {col}} not found in cluster_result$data.")
    }
  }

  cl_raw <- cluster_result$assignments
  df     <- data.frame(
    lon     = data[[lon_col]],
    lat     = data[[lat_col]],
    cluster = factor(ifelse(is.na(cl_raw), "NA", paste0("C", cl_raw)))
  )

  ttl <- if (!is.null(title)) title else
    paste0("Cluster assignments  (k = ", cluster_result$k, ")")

  ggplot2::ggplot(df, ggplot2::aes(x = .data$lon, y = .data$lat,
                                    colour = .data$cluster)) +
    ggplot2::geom_point(size = size, alpha = alpha) +
    ggplot2::coord_fixed() +
    ggplot2::scale_colour_discrete(na.value = "grey70") +
    ggplot2::labs(x = "Longitude", y = "Latitude",
                  colour = "Cluster", title = ttl) +
    ggplot2::theme_minimal()
}
