## Circular clustering of mosque orientations.
## ADR-09: primary method circular_kmeans (built-in); von_mises_mixture (movMF).
## ADR-07: gibson_classification is never used as a clustering feature.

# ── Low-level helpers ─────────────────────────────────────────────────────────

# n × m matrix of 1 - cos(x_i - c_j)  (chord dissimilarity, in [0, 2])
.circ_dist <- function(x_rad, cents_rad) {
  outer(x_rad, cents_rad, function(a, c) 1 - cos(a - c))
}

# n × n angular distance matrix (radians, in [0, pi])
.circ_dist_mat <- function(x_rad) {
  outer(x_rad, x_rad, function(a, b) acos(pmax(-1, pmin(1, cos(a - b)))))
}

# Circular mean of a set of angles in radians; returns NA if length is 0
.circ_mean_rad <- function(x_rad) {
  if (length(x_rad) == 0L) return(NA_real_)
  Arg(mean(exp(1i * x_rad)))
}

# Within-cluster circular dispersion: sum over clusters of (1 - Rbar_k) * n_k
.total_dispersion <- function(x_rad, assign, k) {
  sum(vapply(seq_len(k), function(j) {
    m <- x_rad[assign == j]
    if (length(m) < 2L) return(0)
    (1 - Mod(mean(exp(1i * m)))) * length(m)
  }, numeric(1L)))
}

# Per-cluster centroids in radians
.centroids_rad <- function(x_rad, assign, k) {
  vapply(seq_len(k), function(j) {
    m <- x_rad[assign == j]
    if (length(m) == 0L) return(NA_real_)
    .circ_mean_rad(m)
  }, numeric(1L))
}

# ── Circular k-means (built-in) ───────────────────────────────────────────────

# Returns list(assignments, centroids_rad, total_dispersion, iter_used)
.circular_kmeans_fit <- function(x_rad, k, max_iter = 100L, n_init = 10L) {
  n         <- length(x_rad)
  best_disp <- Inf
  best      <- NULL

  for (init in seq_len(n_init)) {
    cent_rad <- x_rad[sample.int(n, k)]

    for (iter in seq_len(max_iter)) {
      dm     <- .circ_dist(x_rad, cent_rad)
      assign <- max.col(-dm)

      new_cent <- vapply(seq_len(k), function(j) {
        m <- x_rad[assign == j]
        if (length(m) == 0L) return(x_rad[sample.int(n, 1L)])
        .circ_mean_rad(m)
      }, numeric(1L))

      diffs <- acos(pmax(-1, pmin(1, cos(new_cent - cent_rad))))
      if (all(diffs < 1e-8)) break
      cent_rad <- new_cent
    }

    td <- .total_dispersion(x_rad, assign, k)
    if (td < best_disp) {
      best_disp <- td
      best      <- list(assignments      = assign,
                        centroids_rad    = cent_rad,
                        total_dispersion = td,
                        iter_used        = iter,
                        bic              = NA_real_)
    }
  }
  best
}

# ── von Mises–Fisher mixture (movMF, in Suggests) ────────────────────────────

.fit_vmf <- function(x_rad, k) {
  rlang::check_installed("movMF",
    reason = "for von_mises_mixture clustering in cluster_qiblas()")
  x_unit <- cbind(cos(x_rad), sin(x_rad))
  fit    <- movMF::movMF(x_unit, k = k, control = list(nruns = 5L))
  assign <- as.integer(stats::predict(fit, x_unit))
  bic    <- tryCatch(stats::BIC(fit), error = function(e) NA_real_)
  list(assignments      = assign,
       centroids_rad    = .centroids_rad(x_rad, assign, k),
       total_dispersion = NA_real_,
       bic              = bic,
       model            = fit,
       x_unit           = x_unit)
}

# ── Hierarchical clustering ───────────────────────────────────────────────────

.fit_hierarchical <- function(x_rad, k) {
  d_obj  <- stats::as.dist(.circ_dist_mat(x_rad))
  hc     <- stats::hclust(d_obj, method = "ward.D2")
  assign <- as.integer(stats::cutree(hc, k = k))
  td     <- .total_dispersion(x_rad, assign, k)
  list(assignments      = assign,
       centroids_rad    = .centroids_rad(x_rad, assign, k),
       total_dispersion = td,
       bic              = NA_real_,
       hclust           = hc)
}

# ── cluster_qiblas() ──────────────────────────────────────────────────────────

#' Cluster mosque orientations into directional groups
#'
#' Applies a circular clustering method to the observed azimuth column of a
#' mosque data frame. Three methods are available:
#' \describe{
#'   \item{`circular_kmeans`}{Built-in circular k-means using the chord
#'     dissimilarity 1 − cos(θ). Multiple random starts are used.}
#'   \item{`von_mises_mixture`}{von Mises–Fisher mixture model via
#'     `movMF::movMF()`. Requires the `movMF` package (in Suggests).}
#'   \item{`hierarchical`}{Agglomerative hierarchical clustering with angular
#'     distance and Ward linkage.}
#' }
#'
#' The function always fits k = 1 through `k_max` and attaches a model
#' selection table to the output (ADR-09). Cluster assignments are entirely
#' destination-blind: `gibson_classification` and `gibson_classification_source`
#' are forbidden as clustering features (ADR-07).
#'
#' @param data A mosque data frame (e.g. [gibson_qibla]).
#' @param k Positive integer. Number of clusters.
#' @param method Clustering method (see Details).
#' @param azimuth_col Column containing observed azimuths. Default `"azimuth"`.
#' @param features `NULL` (cluster on `azimuth_col`) or a single character
#'   column name to cluster on instead (must not be a forbidden column).
#' @param k_max Maximum k for the model selection scan. Default `8`.
#' @param n_init Number of random starts for `"circular_kmeans"`. Default `10`.
#' @param seed Optional integer seed.
#'
#' @return A `qibla_cluster_result` object. Key fields:
#' \describe{
#'   \item{`assignments`}{Integer vector (one per row of `data`). Cluster label,
#'     or `NA` for mosques without an azimuth.}
#'   \item{`centroids`}{Tibble with `cluster`, `direction_deg`, `direction_rad`,
#'     `n_members`, `dispersion` (1 − R̄ per cluster).}
#'   \item{`model_selection`}{Tibble with `k`, `criterion`, `value` for k =
#'     1..\code{k_max}.}
#' }
#'
#' @seealso [validate_clusters()]
#' @export
#' @examples
#' data(gibson_qibla)
#' res <- cluster_qiblas(gibson_qibla, k = 3, seed = 1)
#' print(res)
cluster_qiblas <- function(data,
                            k           = 3L,
                            method      = c("circular_kmeans",
                                            "von_mises_mixture",
                                            "hierarchical"),
                            azimuth_col = "azimuth",
                            features    = NULL,
                            k_max       = 8L,
                            n_init      = 10L,
                            seed        = NULL) {
  method <- match.arg(method)
  checkmate::assert_data_frame(data)
  checkmate::assert_count(k, positive = TRUE)
  checkmate::assert_count(k_max, positive = TRUE)
  checkmate::assert_count(n_init, positive = TRUE)
  checkmate::assert_string(azimuth_col)
  checkmate::assert_character(features, null.ok = TRUE, max.len = 1L)
  if (!is.null(seed)) checkmate::assert_int(seed)

  # ADR-07: forbidden features
  forbidden <- c("gibson_classification", "gibson_classification_source")
  bad <- intersect(c(features, azimuth_col), forbidden)
  if (length(bad) > 0L) {
    cli::cli_abort(c(
      "Gibson classifications must not be used as clustering features (ADR-07).",
      "i" = "Remove {.val {bad}} from {.arg features} or {.arg azimuth_col}.",
      "i" = "These columns may only be used for post-hoc external validation."
    ))
  }

  clust_col <- if (!is.null(features)) features else azimuth_col
  checkmate::assert_true(clust_col %in% names(data),
                         .var.name = paste0("'", clust_col, "' in names(data)"))

  if (k > k_max) k_max <- k
  if (!is.null(seed)) set.seed(seed)

  has_val  <- !is.na(data[[clust_col]])
  n_clust  <- sum(has_val)

  if (n_clust < k) {
    cli::cli_abort(
      "Fewer mosques with values in {.val {clust_col}} ({n_clust}) than requested clusters ({k})."
    )
  }
  if (n_clust < 10L) {
    cli::cli_warn(
      "Only {n_clust} mosques available for clustering. Results are unreliable.",
      .frequency = "once", .frequency_id = "cluster_small_n"
    )
  }

  az_deg <- data[[clust_col]][has_val]
  az_rad <- az_deg * pi / 180

  # ── Fit k = 1..k_max for model selection ────────────────────────────────────
  k_range    <- seq_len(k_max)
  model_list <- vector("list", k_max)
  sel_vals   <- numeric(k_max)

  for (ki in k_range) {
    fit_i <- switch(method,
      circular_kmeans   = .circular_kmeans_fit(az_rad, ki, n_init = n_init),
      von_mises_mixture = .fit_vmf(az_rad, ki),
      hierarchical      = .fit_hierarchical(az_rad, ki)
    )
    model_list[[ki]] <- fit_i
    sel_vals[ki]     <- switch(method,
      circular_kmeans   = fit_i$total_dispersion,
      von_mises_mixture = if (!is.na(fit_i$bic)) fit_i$bic else NA_real_,
      hierarchical      = fit_i$total_dispersion
    )
  }

  crit <- switch(method,
    circular_kmeans   = "total_dispersion",
    von_mises_mixture = "BIC",
    hierarchical      = "total_dispersion"
  )
  model_selection <- tibble::tibble(k = k_range, criterion = crit, value = sel_vals)

  # ── Primary fit at user-specified k ─────────────────────────────────────────
  pfit        <- model_list[[k]]
  assign_sub  <- pfit$assignments

  centroids <- tibble::tibble(
    cluster       = seq_len(k),
    direction_deg = vapply(seq_len(k), function(j) {
      m <- az_rad[assign_sub == j]
      if (length(m) == 0L) return(NA_real_)
      (Arg(mean(exp(1i * m))) * 180 / pi + 360) %% 360
    }, numeric(1L)),
    direction_rad = vapply(seq_len(k), function(j) {
      m <- az_rad[assign_sub == j]
      if (length(m) == 0L) return(NA_real_)
      .circ_mean_rad(m)
    }, numeric(1L)),
    n_members     = vapply(seq_len(k), function(j) sum(assign_sub == j), integer(1L)),
    dispersion    = vapply(seq_len(k), function(j) {
      m <- az_rad[assign_sub == j]
      if (length(m) < 2L) return(0)
      1 - Mod(mean(exp(1i * m)))
    }, numeric(1L))
  )

  # Map assignments back to full data
  assignments_full <- rep(NA_integer_, nrow(data))
  assignments_full[has_val] <- assign_sub

  structure(
    list(
      method          = method,
      k               = k,
      k_max           = k_max,
      assignments     = assignments_full,
      centroids       = centroids,
      model_selection = model_selection,
      model_list      = model_list,
      primary_model   = pfit,
      n_clustered     = n_clust,
      n_total         = nrow(data),
      azimuth_col     = clust_col,
      data            = data
    ),
    class = "qibla_cluster_result"
  )
}

#' @export
print.qibla_cluster_result <- function(x, ...) {
  cli::cli_h2("Qibla cluster result  ({x$method},  k = {x$k})")
  cli::cli_bullets(c(
    "*" = "Mosques clustered: {x$n_clustered} / {x$n_total}",
    "*" = "k_max scanned:     {x$k_max}"
  ))
  cli::cli_h3("Cluster centroids")
  print(dplyr::select(x$centroids, -"direction_rad"), n = Inf)
  cli::cli_h3("Model selection  ({x$model_selection$criterion[1]}  vs k)")
  print(x$model_selection, n = Inf)
  invisible(x)
}
