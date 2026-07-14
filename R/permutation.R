## Permutation tests for qibla orientation analysis.
## Shuffles azimuth assignments, breaking orientation/group associations while
## optionally preserving geographic structure. Design doc §7.3.

# ── permute_orientations() ────────────────────────────────────────────────────

#' Permute azimuth values in a mosque dataset
#'
#' Returns a copy of `data` with the azimuth column randomly shuffled,
#' breaking any association between observed orientations and geographic,
#' temporal, or classification variables. `NA` positions are held fixed: only
#' the non-`NA` azimuth values are permuted.
#'
#' When `block_col` is supplied, permutation is performed within each level
#' of that column independently (block permutation). This preserves the
#' geographic distribution of azimuths within regions while breaking
#' any region-level systematic association.
#'
#' @param data A mosque data frame.
#' @param azimuth_col Column containing observed azimuths. Default `"azimuth"`.
#' @param block_col Optional character column name. If supplied, azimuths are
#'   permuted within the levels of this column (e.g. `"country"` for within-
#'   country permutation).
#' @param seed Optional integer seed for reproducibility.
#'
#' @return A tibble with the same structure as `data`, `azimuth_col` shuffled.
#'
#' @seealso [permutation_test()]
#' @export
#' @examples
#' data(gibson_qibla)
#' shuffled <- permute_orientations(gibson_qibla, seed = 1)
#' # The set of non-NA azimuths is preserved; their assignment to mosques is not.
#' all.equal(sort(gibson_qibla$azimuth, na.last = NA),
#'           sort(shuffled$azimuth,    na.last = NA))
permute_orientations <- function(data,
                                  azimuth_col = "azimuth",
                                  block_col   = NULL,
                                  seed        = NULL) {
  checkmate::assert_data_frame(data)
  checkmate::assert_string(azimuth_col)
  checkmate::assert_string(block_col, null.ok = TRUE)
  checkmate::assert_true(azimuth_col %in% names(data),
                         .var.name = paste0("'", azimuth_col, "' in names(data)"))
  if (!is.null(block_col)) {
    checkmate::assert_true(block_col %in% names(data),
                           .var.name = paste0("'", block_col, "' in names(data)"))
  }
  if (!is.null(seed)) checkmate::assert_int(seed)

  if (!is.null(seed)) set.seed(seed)

  out <- data

  if (is.null(block_col)) {
    has_val <- !is.na(out[[azimuth_col]])
    out[[azimuth_col]][has_val] <- sample(out[[azimuth_col]][has_val])
  } else {
    for (b in unique(out[[block_col]])) {
      idx     <- which(out[[block_col]] == b)
      has_val <- !is.na(out[[azimuth_col]][idx])
      if (sum(has_val) > 1L) {
        local_idx <- idx[has_val]
        out[[azimuth_col]][local_idx] <- sample(out[[azimuth_col]][local_idx])
      }
    }
  }

  tibble::as_tibble(out)
}


# ── permutation_test() ────────────────────────────────────────────────────────

#' Permutation test for mosque orientation statistics
#'
#' Evaluates a user-supplied test statistic under a permutation null
#' distribution. The null hypothesis is that the observed azimuth values are
#' exchangeable across mosques — i.e., any assignment of azimuths to mosque
#' locations is equally likely. The p-value is the proportion of permuted
#' replicates at least as extreme as the observed value.
#'
#' The statistic function must return a single numeric scalar. If it returns
#' `NA` for permuted data (e.g. because the population filter yields no rows),
#' those replicates are excluded with a warning.
#'
#' @param data A mosque data frame.
#' @param stat_fn A function `f(data)` returning a single numeric scalar. This
#'   is the test statistic.
#' @param n_permutations Positive integer. Number of permutation replicates.
#'   Default `999`.
#' @param alternative `"greater"`, `"less"`, or `"two.sided"`. Direction of
#'   the permutation p-value. Default `"greater"`.
#' @param azimuth_col Column to permute. Default `"azimuth"`.
#' @param block_col Optional: permute within the levels of this column.
#' @param seed Optional integer seed.
#'
#' @return A `qibla_permutation_test` object. Key fields:
#' \describe{
#'   \item{`observed`}{The test statistic on the original data.}
#'   \item{`permuted`}{Numeric vector of length <= `n_permutations`.}
#'   \item{`p.value`}{Permutation p-value.}
#'   \item{`alternative`, `n_permutations`}{As supplied.}
#' }
#'
#' @seealso [permute_orientations()]
#' @export
#' @examples
#' data(gibson_qibla)
#' data(destinations)
#' mecca <- destinations[destinations$id == "mecca", ]
#'
#' # Stat: mean resultant length of residuals (high = concentrated toward Mecca)
#' stat <- function(d) {
#'   b   <- bearing_to_destination(d, mecca)
#'   err <- absolute_angular_error(d$azimuth, b)
#'   circular_resultant(err[!is.na(err)])
#' }
#' \dontrun{
#' res <- permutation_test(gibson_qibla, stat, n_permutations = 99, seed = 1)
#' print(res)
#' }
permutation_test <- function(data,
                              stat_fn,
                              n_permutations = 999L,
                              alternative    = c("greater", "less", "two.sided"),
                              azimuth_col    = "azimuth",
                              block_col      = NULL,
                              seed           = NULL) {
  alternative <- match.arg(alternative)
  checkmate::assert_data_frame(data)
  checkmate::assert_function(stat_fn)
  checkmate::assert_count(n_permutations, positive = TRUE)
  checkmate::assert_string(azimuth_col)
  checkmate::assert_string(block_col, null.ok = TRUE)
  if (!is.null(seed)) checkmate::assert_int(seed)

  if (!is.null(seed)) set.seed(seed)

  observed <- stat_fn(data)
  checkmate::assert_number(observed,
    .var.name = "stat_fn(data) must return a single numeric scalar")

  permuted <- vapply(seq_len(n_permutations), function(i) {
    perm <- permute_orientations(data, azimuth_col = azimuth_col,
                                 block_col = block_col)
    stat_fn(perm)
  }, numeric(1L))

  n_na <- sum(is.na(permuted))
  if (n_na > 0L) {
    cli::cli_warn(
      "{n_na} permuted replicates returned NA and are excluded from the p-value."
    )
    permuted <- permuted[!is.na(permuted)]
  }

  if (n_permutations < 99L) {
    cli::cli_warn(
      "Only {n_permutations} permutations used; p-value resolution is limited to 1/{n_permutations}.",
      .frequency = "once", .frequency_id = "few_permutations"
    )
  }

  p_value <- switch(alternative,
    greater   = mean(permuted >= observed),
    less      = mean(permuted <= observed),
    two.sided = {
      centre <- mean(permuted)
      mean(abs(permuted - centre) >= abs(observed - centre))
    }
  )

  structure(
    list(
      observed       = observed,
      permuted       = permuted,
      p.value        = p_value,
      alternative    = alternative,
      n_permutations = n_permutations,
      azimuth_col    = azimuth_col,
      block_col      = block_col
    ),
    class = "qibla_permutation_test"
  )
}

#' @export
print.qibla_permutation_test <- function(x, digits = 3, ...) {
  cli::cli_h3("Permutation test")
  cli::cli_bullets(c(
    "*" = "Observed statistic:  {round(x$observed, digits)}",
    "*" = "Permuted mean:       {round(mean(x$permuted), digits)}",
    "*" = "Permuted SD:         {round(stats::sd(x$permuted), digits)}",
    "*" = "n permutations used: {length(x$permuted)}",
    "*" = "Alternative:         {x$alternative}",
    "*" = "p-value:             {format.pval(x$p.value, digits = digits)}"
  ))
  invisible(x)
}
