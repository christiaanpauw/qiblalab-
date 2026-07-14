## Cluster validation: internal, stability, external, and predictive.
## ADR-10: all three mandatory sections must be computed before describing
## a cluster solution as historically meaningful.
## ADR-07: gibson_classification may only appear as an external label, never
## as a clustering feature; all external sections are clearly labelled post-hoc.

# ── Agreement metrics ─────────────────────────────────────────────────────────

# Adjusted Rand Index between two integer label vectors.
# NA values in either vector are excluded pairwise.
.ari <- function(a, b) {
  ok <- !is.na(a) & !is.na(b)
  a  <- a[ok]; b <- b[ok]
  n  <- length(a)
  if (n < 2L) return(NA_real_)

  tab     <- table(a, b)
  comb2   <- function(x) x * (x - 1) / 2          # C(x, 2)
  sum_ij  <- sum(comb2(as.vector(tab)))
  sum_a   <- sum(comb2(rowSums(tab)))
  sum_b   <- sum(comb2(colSums(tab)))
  comb_n  <- comb2(n)

  expected <- sum_a * sum_b / comb_n
  max_ri   <- (sum_a + sum_b) / 2

  if (max_ri == expected) return(if (sum_ij == expected) 1 else 0)
  (sum_ij - expected) / (max_ri - expected)
}

# Normalised Mutual Information (arithmetic mean normalisation).
.nmi <- function(a, b) {
  ok <- !is.na(a) & !is.na(b)
  a  <- a[ok]; b <- b[ok]
  n  <- length(a)
  if (n == 0L) return(NA_real_)

  tab      <- table(a, b)
  p        <- tab / n
  pa       <- rowSums(p)
  pb       <- colSums(p)
  ha       <- -sum(pa[pa > 0] * log(pa[pa > 0]))
  hb       <- -sum(pb[pb > 0] * log(pb[pb > 0]))
  if ((ha + hb) == 0) return(1)

  outer_p  <- outer(pa, pb)
  nz       <- p > 0
  mi       <- sum(p[nz] * log(p[nz] / outer_p[nz]))
  2 * mi / (ha + hb)
}

# Cluster purity: (1/n) * sum_k max_j(n_kj)
.purity <- function(assignments, labels) {
  ok <- !is.na(assignments) & !is.na(labels)
  a  <- assignments[ok]; b <- labels[ok]
  n  <- length(a)
  if (n == 0L) return(NA_real_)
  tab <- table(a, b)
  sum(apply(tab, 1L, max)) / n
}


# ── validate_clusters() ───────────────────────────────────────────────────────

#' Validate a cluster solution across internal, stability, and external criteria
#'
#' Evaluates a [cluster_qiblas()] result against the three mandatory validation
#' sections specified in ADR-10:
#'
#' 1. **Internal**: within-cluster circular dispersion, minimum centroid
#'    separation, and (for `von_mises_mixture`) proportion of ambiguous soft
#'    assignments.
#' 2. **Stability**: bootstrap adjusted Rand index (ARI) across `bootstrap_n`
#'    resamples. Mean ARI > 0.6 is required to claim stability (ADR-10).
#' 3. **External**: ARI, normalised mutual information (NMI), and cluster
#'    purity against columns you supply. Gibson's classification may only
#'    appear here — never in the clustering input — and is labelled
#'    "external validation against Gibson's classification" in all output.
#' 4. **Predictive** (when n ≥ 60): mean angular prediction error on held-out
#'    mosques across `predictive_splits` random 80/20 train-test splits.
#'
#' @param result A `qibla_cluster_result` from [cluster_qiblas()].
#' @param external Character vector of column names to use as external labels.
#'   May include `"gibson_classification"` (post-hoc only). Default `NULL`
#'   (skip external validation).
#' @param bootstrap_n Number of bootstrap resamples for stability. Default 100.
#' @param predictive_splits Number of train/test splits. Default 20.
#' @param seed Optional integer seed.
#'
#' @return A `qibla_cluster_validation` object. Print it for a structured
#'   report with pass / warn / fail indicators.
#'
#' @seealso [cluster_qiblas()]
#' @export
#' @examples
#' data(gibson_qibla)
#' res <- cluster_qiblas(gibson_qibla, k = 3, seed = 1)
#' val <- validate_clusters(res, external = c("age_group", "country"),
#'                           bootstrap_n = 20L, seed = 2)
#' print(val)
validate_clusters <- function(result,
                               external          = NULL,
                               bootstrap_n       = 100L,
                               predictive_splits = 20L,
                               seed              = NULL) {
  checkmate::assert_class(result, "qibla_cluster_result")
  checkmate::assert_character(external, null.ok = TRUE)
  checkmate::assert_count(bootstrap_n,       positive = TRUE)
  checkmate::assert_count(predictive_splits, positive = TRUE)
  if (!is.null(seed)) checkmate::assert_int(seed)

  if (!is.null(seed)) set.seed(seed)

  data       <- result$data
  assign_all <- result$assignments
  k          <- result$k
  cents      <- result$centroids
  az_col     <- result$azimuth_col

  has_val    <- !is.na(assign_all)
  az_rad     <- data[[az_col]] * pi / 180
  az_rad_sub <- az_rad[has_val]
  assign_sub <- assign_all[has_val]
  n_sub      <- length(az_rad_sub)

  # ── 1. Internal validation ──────────────────────────────────────────────────
  within_disp      <- cents$dispersion
  mean_within_disp <- mean(within_disp, na.rm = TRUE)

  if (k >= 2L) {
    cent_rad <- cents$direction_rad
    pairs    <- utils::combn(k, 2L)
    sep_vals <- vapply(seq_len(ncol(pairs)), function(j) {
      i1 <- pairs[1L, j]; i2 <- pairs[2L, j]
      acos(pmax(-1, pmin(1, cos(cent_rad[i1] - cent_rad[i2])))) * 180 / pi
    }, numeric(1L))
    min_sep <- min(sep_vals, na.rm = TRUE)
  } else {
    min_sep <- NA_real_
  }
  sep_ok <- is.na(min_sep) || min_sep > 10

  # Ambiguous assignments: vMF posterior-based only
  pct_ambiguous <- NA_real_
  pm <- result$primary_model
  if (result$method == "von_mises_mixture" &&
        !is.null(pm$model) && !is.null(pm$x_unit)) {
    tryCatch({
      posts         <- movMF::Predict.movMF(pm$model, pm$x_unit)
      max_post      <- apply(posts, 1L, max)
      pct_ambiguous <- mean(max_post < 0.6)
    }, error = function(e) NULL)
  }

  internal <- list(
    within_dispersion              = within_disp,
    mean_within_dispersion         = mean_within_disp,
    min_centroid_separation_deg    = min_sep,
    separation_ok                  = sep_ok,
    pct_ambiguous                  = pct_ambiguous
  )

  # ── 2. Stability: bootstrap ARI ─────────────────────────────────────────────
  boot_aris <- vapply(seq_len(bootstrap_n), function(b) {
    idx_b    <- sample.int(n_sub, n_sub, replace = TRUE)
    az_boot  <- az_rad_sub[idx_b]

    fit_b <- tryCatch(switch(result$method,
      circular_kmeans   = .circular_kmeans_fit(az_boot, k, n_init = 3L),
      von_mises_mixture = .fit_vmf(az_boot, k),
      hierarchical      = .fit_hierarchical(az_boot, k)
    ), error = function(e) NULL)

    if (is.null(fit_b) || is.null(fit_b$centroids_rad)) return(NA_real_)

    # Predict all original points using the bootstrap centroids
    dm         <- .circ_dist(az_rad_sub, fit_b$centroids_rad)
    pred_sub   <- as.integer(max.col(-dm))
    .ari(assign_sub, pred_sub)
  }, numeric(1L))

  n_boot_na  <- sum(is.na(boot_aris))
  if (n_boot_na > 0L) {
    cli::cli_warn(
      "{n_boot_na} bootstrap replicates failed and are excluded from the ARI.",
      .frequency = "once", .frequency_id = "boot_fail"
    )
  }
  boot_aris    <- boot_aris[!is.na(boot_aris)]
  mean_boot    <- mean(boot_aris)
  stability_ok <- !is.na(mean_boot) && mean_boot > 0.6

  stability <- list(
    boot_ari_values = boot_aris,
    mean_boot_ari   = mean_boot,
    sd_boot_ari     = stats::sd(boot_aris),
    n_bootstrap     = length(boot_aris),
    stability_ok    = stability_ok
  )

  # ── 3. External validation ───────────────────────────────────────────────────
  ext_list <- NULL
  if (!is.null(external)) {
    ext_list <- lapply(external, function(col) {
      if (!col %in% names(data)) {
        cli::cli_warn("External column {.val {col}} not found in data; skipping.")
        return(NULL)
      }
      labs     <- as.integer(as.factor(data[[col]]))
      labs[is.na(data[[col]])] <- NA_integer_
      list(
        column  = col,
        ari     = .ari(assign_all, labs),
        nmi     = .nmi(assign_all, labs),
        purity  = .purity(assign_all, labs)
      )
    })
    names(ext_list) <- external
    ext_list <- Filter(Negate(is.null), ext_list)
  }

  # ── 4. Predictive validation (n >= 60) ──────────────────────────────────────
  predictive <- NULL
  if (n_sub >= 60L) {
    pred_errors <- vapply(seq_len(predictive_splits), function(s) {
      train_idx <- sample.int(n_sub, floor(0.8 * n_sub))
      test_idx  <- setdiff(seq_len(n_sub), train_idx)

      az_train <- az_rad_sub[train_idx]
      az_test  <- az_rad_sub[test_idx]

      fit_tr <- tryCatch(switch(result$method,
        circular_kmeans   = .circular_kmeans_fit(az_train, k, n_init = 3L),
        von_mises_mixture = .fit_vmf(az_train, k),
        hierarchical      = .fit_hierarchical(az_train, k)
      ), error = function(e) NULL)

      if (is.null(fit_tr) || is.null(fit_tr$centroids_rad)) return(NA_real_)

      dm_test  <- .circ_dist(az_test, fit_tr$centroids_rad)
      pred_clust <- max.col(-dm_test)
      pred_dir <- fit_tr$centroids_rad[pred_clust]
      mean(acos(pmax(-1, pmin(1, cos(az_test - pred_dir)))) * 180 / pi)
    }, numeric(1L))

    pred_errors <- pred_errors[!is.na(pred_errors)]
    predictive  <- list(
      mean_angular_error = mean(pred_errors),
      sd_angular_error   = stats::sd(pred_errors),
      n_splits           = length(pred_errors)
    )
  }

  # ── Safeguard warnings (ADR-10) ──────────────────────────────────────────────
  if (!sep_ok && k >= 2L) {
    cli::cli_warn(
      "Min centroid separation ({round(min_sep, 1)} deg) is <= 10 deg. ",
      "Clusters may not represent meaningfully distinct orientation traditions (ADR-10).",
      .frequency = "once", .frequency_id = "sep_warn"
    )
  }
  if (!is.na(mean_boot) && !stability_ok) {
    cli::cli_warn(
      "Mean bootstrap ARI ({round(mean_boot, 2)}) < 0.6. ",
      "Cluster assignments are not stable across resamples (ADR-10).",
      .frequency = "once", .frequency_id = "stability_warn"
    )
  }
  if (!is.null(pct_ambiguous) && !is.na(pct_ambiguous) && pct_ambiguous > 0.3) {
    cli::cli_warn(
      "{round(pct_ambiguous * 100, 1)}% of mosques have max posterior < 0.6. ",
      "Many cluster assignments are uncertain.",
      .frequency = "once", .frequency_id = "ambig_warn"
    )
  }

  structure(
    list(
      k          = k,
      method     = result$method,
      internal   = internal,
      stability  = stability,
      external   = ext_list,
      predictive = predictive
    ),
    class = "qibla_cluster_validation"
  )
}

#' @export
print.qibla_cluster_validation <- function(x, digits = 3, ...) {
  fmt_val <- function(v) if (is.na(v)) "NA" else as.character(round(v, digits))
  ok_flag <- function(b) if (isTRUE(b)) "[OK]" else if (isFALSE(b)) "[WARN]" else ""

  cli::cli_h2("Cluster validation  ({x$method},  k = {x$k})")

  # Internal
  cli::cli_h3("Internal validation")
  int <- x$internal
  mwd <- fmt_val(int$mean_within_dispersion)
  mcs <- fmt_val(int$min_centroid_separation_deg)
  sok <- ok_flag(int$separation_ok)
  amb <- if (is.na(int$pct_ambiguous)) "N/A" else
           paste0(round(int$pct_ambiguous * 100, 1), "%")
  cli::cli_bullets(c(
    "*" = "Mean within-cluster dispersion:   {mwd}  [lower is better]",
    "*" = "Min centroid separation:          {mcs} deg  {sok}",
    "*" = "Ambiguous assignments (post<0.6): {amb}"
  ))

  # Stability
  cli::cli_h3("Stability  (bootstrap, n = {x$stability$n_bootstrap})")
  st  <- x$stability
  mba <- fmt_val(st$mean_boot_ari)
  sba <- fmt_val(st$sd_boot_ari)
  sok2 <- ok_flag(st$stability_ok)
  cli::cli_bullets(c(
    "*" = "Mean bootstrap ARI: {mba}  SD: {sba}  {sok2}"
  ))

  # External
  if (!is.null(x$external) && length(x$external) > 0L) {
    cli::cli_h3("External validation  (post-hoc only)")
    for (ev in x$external) {
      col <- ev$column
      a   <- fmt_val(ev$ari)
      nm  <- fmt_val(ev$nmi)
      pu  <- fmt_val(ev$purity)
      cli::cli_bullets(c("*" = "{col}:  ARI = {a}  NMI = {nm}  purity = {pu}"))
    }
  }

  # Predictive
  if (!is.null(x$predictive)) {
    cli::cli_h3("Predictive  ({x$predictive$n_splits} splits)")
    pr  <- x$predictive
    mae <- fmt_val(pr$mean_angular_error)
    sae <- fmt_val(pr$sd_angular_error)
    cli::cli_bullets(c("*" = "Mean angular error: {mae} deg  SD: {sae} deg"))
  }

  invisible(x)
}
