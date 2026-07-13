## Hypothesis specification, testing, and competing-destination comparison.
## Design doc sections 6, 7.1, 7.2, 7.3.
## ADR-08: no default tolerance; every analysis reports sensitivity at 2, 5, 10 deg.

# ── Internal helpers ──────────────────────────────────────────────────────────

# Resolve a candidate (character id or single-row data frame) against the
# destinations table. Returns a single-row data frame.
resolve_candidate <- function(candidate, dest_table) {
  if (is.character(candidate)) {
    row <- dest_table[dest_table$id == candidate, ]
    if (nrow(row) == 0L) {
      avail <- paste(dest_table$id, collapse = ", ")
      cli::cli_abort(c(
        "Candidate {.val {candidate}} not found in destinations table.",
        "i" = "Available ids: {avail}"
      ))
    }
    row[1L, ]
  } else if (is.data.frame(candidate)) {
    if (nrow(candidate) != 1L) {
      cli::cli_abort("{.arg candidate} data frame must have exactly one row.")
    }
    candidate
  } else {
    cli::cli_abort(
      "{.arg candidate} must be a character id or a single-row data frame."
    )
  }
}


# ── qibla_hypothesis() ────────────────────────────────────────────────────────

#' Specify a qibla orientation hypothesis
#'
#' Creates a structured hypothesis object recording all analytical choices
#' before any data are examined. The hypothesis can then be tested with
#' [test_qibla_hypothesis()].
#'
#' @param name Character scalar. Short identifier for the hypothesis
#'   (used in output labels).
#' @param population An unquoted filter expression evaluated against the
#'   mosque data frame (e.g. `year_ce_max <= 700`). Default `TRUE` (all rows).
#' @param candidate Character scalar (id in [destinations]) or single-row
#'   data frame with `latitude` and `longitude`. The proposed qibla destination.
#' @param tolerance Numeric scalar in (0, 180]. Angular tolerance in degrees.
#'   Required: there is no default (ADR-08).
#' @param expected_share Numeric in (0, 1). Proportion of mosques expected to
#'   be consistent with the candidate under H0. Default `0.5`.
#' @param alternative `"greater"`, `"two.sided"`, or `"less"`. Direction of
#'   the proportion test H1. Default `"greater"`.
#' @param description Character scalar. Optional longer description of the
#'   research question.
#' @param comments Character scalar. Optional analyst notes (methodology,
#'   caveats, preregistration details).
#'
#' @return A `qibla_hypothesis` object (list).
#'
#' @seealso [test_qibla_hypothesis()], [compare_qibla_hypotheses()]
#' @export
#' @examples
#' h <- qibla_hypothesis(
#'   name           = "Early Petra orientation",
#'   population     = year_ce_max <= 700,
#'   candidate      = "petra",
#'   tolerance      = 5,
#'   expected_share = 0.5,
#'   alternative    = "greater",
#'   description    = "Most mosques built before 700 CE pointed toward Petra"
#' )
#' h
qibla_hypothesis <- function(name,
                              population     = TRUE,
                              candidate,
                              tolerance,
                              expected_share = 0.5,
                              alternative    = c("greater", "two.sided", "less"),
                              description    = NULL,
                              comments       = NULL) {
  alternative <- match.arg(alternative)
  checkmate::assert_string(name)
  checkmate::assert_number(tolerance, lower = 0, upper = 180, finite = TRUE)
  checkmate::assert_number(expected_share, lower = 0, upper = 1)
  checkmate::assert_string(description, null.ok = TRUE)
  checkmate::assert_string(comments, null.ok = TRUE)

  pop_quo <- rlang::enquo(population)

  structure(
    list(
      name           = name,
      population     = pop_quo,
      candidate      = candidate,
      tolerance      = tolerance,
      expected_share = expected_share,
      alternative    = alternative,
      description    = description,
      comments       = comments
    ),
    class = "qibla_hypothesis"
  )
}

#' @export
print.qibla_hypothesis <- function(x, ...) {
  cli::cli_h3("Qibla hypothesis: {x$name}")
  if (!is.null(x$description)) cli::cli_text("{x$description}")
  cand_str <- if (is.character(x$candidate)) x$candidate else "<custom destination>"
  cli::cli_bullets(c(
    "*" = "Candidate:      {cand_str}",
    "*" = "Population:     {rlang::quo_text(x$population)}",
    "*" = "Tolerance:      {x$tolerance} deg",
    "*" = "Expected share: {x$expected_share} (H0)",
    "*" = "Alternative:    {x$alternative}"
  ))
  if (!is.null(x$comments)) cli::cli_text("Comments: {x$comments}")
  invisible(x)
}


# ── test_qibla_hypothesis() ───────────────────────────────────────────────────

#' Test a qibla orientation hypothesis against mosque data
#'
#' Applies a [qibla_hypothesis()] to a mosque data frame. For each mosque in
#' the specified population with a measured azimuth, it computes the bearing to
#' the candidate destination and classifies the mosque as "consistent" if the
#' absolute angular error is within the stated tolerance. A binomial test
#' evaluates whether the observed proportion of consistent mosques exceeds the
#' null share.
#'
#' Results at the specified tolerance are always accompanied by a sensitivity
#' table showing how conclusions change at t = 2, 5, and 10 degrees (ADR-08).
#'
#' @param hypothesis A `qibla_hypothesis` object from [qibla_hypothesis()].
#' @param data A mosque data frame (typically [gibson_qibla]).
#' @param dest_table A destinations data frame for resolving character
#'   candidate ids. Defaults to the shipped [destinations].
#' @param model Bearing model function. Default [bearing_haversine()].
#' @param lat_col,lon_col,azimuth_col Column names in `data`.
#' @param sensitivity_tolerances Numeric vector of additional tolerances to
#'   include in the sensitivity table. Default `c(2, 5, 10)`.
#'
#' @return A `qibla_test_result` object. Print it for a human-readable report.
#'   Key fields: `$proportion_observed`, `$binom_test`, `$circular_summary`,
#'   `$sensitivity`, `$consistent` (logical vector over analysed mosques).
#'
#' @seealso [qibla_hypothesis()], [compare_qibla_hypotheses()]
#' @export
#' @examples
#' data(gibson_qibla)
#' h <- qibla_hypothesis(
#'   name      = "Early Petra orientation",
#'   population = year_ce_max <= 700,
#'   candidate = "petra",
#'   tolerance = 5
#' )
#' test_qibla_hypothesis(h, gibson_qibla)
test_qibla_hypothesis <- function(hypothesis,
                                   data,
                                   dest_table              = NULL,
                                   model                   = bearing_haversine,
                                   lat_col                 = "latitude",
                                   lon_col                 = "longitude",
                                   azimuth_col             = "azimuth",
                                   sensitivity_tolerances  = c(2, 5, 10)) {
  checkmate::assert_class(hypothesis, "qibla_hypothesis")
  checkmate::assert_data_frame(data)
  checkmate::assert_function(model)
  checkmate::assert_numeric(sensitivity_tolerances, lower = 0, upper = 180,
                            null.ok = TRUE)

  if (is.null(dest_table)) {
    dest_table <- get("destinations", envir = asNamespace("qiblalab"))
  }

  model_label <- rlang::as_label(rlang::enquo(model))

  # 1. Resolve candidate destination
  dest <- resolve_candidate(hypothesis$candidate, dest_table)

  # 2. Filter to population
  filtered <- dplyr::filter(data, !!hypothesis$population)
  n_population <- nrow(filtered)

  if (n_population == 0L) {
    cli::cli_abort(c(
      "Population filter matched 0 rows.",
      "i" = "Filter: {rlang::quo_text(hypothesis$population)}",
      "i" = "Check that the column names in the filter match those in {.arg data}."
    ))
  }

  # 3. Exclude rows with no measured azimuth
  has_azimuth           <- !is.na(filtered[[azimuth_col]])
  n_excluded_no_azimuth <- sum(!has_azimuth)
  analysed              <- filtered[has_azimuth, ]
  n_analysed            <- nrow(analysed)

  if (n_analysed == 0L) {
    cli::cli_abort("No mosques in the population have a measured azimuth.")
  }

  if (n_analysed < 10L) {
    cli::cli_warn(
      "Only {n_analysed} mosques with azimuths in the population. ",
      "Results are highly sensitive to individual sites.",
      .frequency = "once", .frequency_id = "small_population"
    )
  }

  # 4. Bearings and residuals
  bearings      <- bearing_to_destination(analysed, dest, model, lat_col, lon_col)
  errors        <- absolute_angular_error(analysed[[azimuth_col]], bearings)
  signed_errors <- signed_angular_error(analysed[[azimuth_col]], bearings)

  # 5. Classification at specified tolerance
  consistent   <- errors <= hypothesis$tolerance
  n_consistent <- sum(consistent, na.rm = TRUE)
  prop_obs     <- n_consistent / n_analysed

  # 6. Binomial test
  binom_result <- stats::binom.test(
    x           = n_consistent,
    n           = n_analysed,
    p           = hypothesis$expected_share,
    alternative = hypothesis$alternative
  )

  # 7. Circular summary of signed residuals (how far off, and in which direction)
  circ_sum <- circular_summary(signed_errors, na.rm = TRUE)

  # 8. Sensitivity across tolerances (ADR-08)
  sens_tols <- sort(unique(c(sensitivity_tolerances, hypothesis$tolerance)))
  n_vec <- vapply(sens_tols, function(t) sum(errors <= t, na.rm = TRUE), integer(1L))
  p_vec <- vapply(sens_tols, function(t) {
    nc <- sum(errors <= t, na.rm = TRUE)
    stats::binom.test(nc, n_analysed, p = hypothesis$expected_share,
                      alternative = hypothesis$alternative)$p.value
  }, numeric(1L))

  sensitivity <- tibble::tibble(
    tolerance    = sens_tols,
    n_consistent = n_vec,
    proportion   = n_vec / n_analysed,
    binom_p      = p_vec
  )

  # Research safeguard: warn if result depends on very few mosques
  if (n_consistent <= 3L && prop_obs > hypothesis$expected_share) {
    cli::cli_warn(
      "The consistent count is {n_consistent}. Conclusions based on so few ",
      "sites are highly sensitive to individual records.",
      .frequency = "once", .frequency_id = "few_consistent"
    )
  }

  structure(
    list(
      hypothesis            = hypothesis,
      destination           = dest,
      model                 = model_label,
      n_population          = n_population,
      n_excluded_no_azimuth = n_excluded_no_azimuth,
      n_analysed            = n_analysed,
      n_consistent          = n_consistent,
      proportion_observed   = prop_obs,
      binom_test            = binom_result,
      circular_summary      = circ_sum,
      sensitivity           = sensitivity,
      bearings              = bearings,
      errors                = errors,
      signed_errors         = signed_errors,
      consistent            = consistent,
      analysed_data         = analysed
    ),
    class = "qibla_test_result"
  )
}

#' @export
print.qibla_test_result <- function(x, digits = 3, ...) {
  h  <- x$hypothesis
  bt <- x$binom_test

  cli::cli_h2("Test result: {h$name}")

  cand_str <- if (is.character(h$candidate)) h$candidate else "<custom>"
  if (!is.null(x$destination$name)) cand_str <- x$destination$name

  cli::cli_h3("Population & data")
  cli::cli_bullets(c(
    "*" = "Filter:        {rlang::quo_text(h$population)}",
    "*" = "Population:    {x$n_population} mosques",
    "*" = "No azimuth:    {x$n_excluded_no_azimuth} excluded",
    "*" = "Analysed:      {x$n_analysed} mosques"
  ))

  cli::cli_h3("Tolerance classification  (t = {h$tolerance} deg)")
  cli::cli_bullets(c(
    "*" = "Candidate:     {cand_str}",
    "*" = "Bearing model: {x$model}",
    "*" = "Consistent:    {x$n_consistent} / {x$n_analysed}  ({round(x$proportion_observed * 100, 1)}%)",
    "*" = "Null share:    {h$expected_share * 100}%  (H1: {h$alternative})"
  ))

  cli::cli_h3("Binomial test")
  cli::cli_bullets(c(
    "*" = "p-value:   {format.pval(bt$p.value, digits = digits)}",
    "*" = "95% CI:    [{round(bt$conf.int[1], digits)}, {round(bt$conf.int[2], digits)}]"
  ))

  cli::cli_h3("Circular summary of signed residuals")
  print(x$circular_summary)

  cli::cli_h3("Sensitivity across tolerances")
  print(x$sensitivity, n = Inf)

  invisible(x)
}


# ── compare_qibla_hypotheses() ────────────────────────────────────────────────

#' Compare mosque orientations against all candidate destinations
#'
#' For each mosque with a measured azimuth, computes the absolute angular error
#' to every candidate destination, identifies the nearest candidate, and
#' computes the margin over the second-nearest. A small margin flags mosques
#' where two candidates are practically indistinguishable (ADR-08).
#'
#' Returns a tibble with one row per mosque, suitable for further filtering and
#' analysis. Bearings are also attached as columns `bearing_{id}` and errors
#' as `error_{id}` for each candidate.
#'
#' @param data A mosque data frame (typically [gibson_qibla]).
#' @param candidates A destinations data frame. Defaults to [destinations].
#' @param model Bearing model function. Default [bearing_haversine()].
#' @param lat_col,lon_col,azimuth_col Column names in `data`.
#' @param ambiguity_threshold Numeric. If the margin between nearest and second-
#'   nearest candidate errors is below this threshold (degrees), the mosque is
#'   flagged as `ambiguous`. Default `5`.
#'
#' @return A tibble with columns from `data` plus:
#' \describe{
#'   \item{bearing_{id}}{Theoretical bearing to each candidate.}
#'   \item{error_{id}}{Absolute angular error for each candidate.}
#'   \item{nearest_id}{Id of the closest candidate.}
#'   \item{nearest_error}{Absolute angular error to nearest candidate.}
#'   \item{second_nearest_id}{Id of the second-closest candidate.}
#'   \item{second_nearest_error}{Error to second-closest.}
#'   \item{margin}{`second_nearest_error - nearest_error`. Small margin = ambiguous.}
#'   \item{ambiguous}{Logical. `TRUE` when `margin < ambiguity_threshold`.}
#' }
#' Mosques with no measured azimuth are retained with `NA` in all computed
#' columns.
#'
#' @seealso [test_qibla_hypothesis()], [qibla_hypothesis()]
#' @export
#' @examples
#' data(gibson_qibla)
#' cmp <- compare_qibla_hypotheses(gibson_qibla)
#' dplyr::count(cmp, nearest_id)
compare_qibla_hypotheses <- function(data,
                                     candidates          = NULL,
                                     model               = bearing_haversine,
                                     lat_col             = "latitude",
                                     lon_col             = "longitude",
                                     azimuth_col         = "azimuth",
                                     ambiguity_threshold = 5) {
  checkmate::assert_data_frame(data)
  checkmate::assert_function(model)
  checkmate::assert_number(ambiguity_threshold, lower = 0)

  if (is.null(candidates)) {
    candidates <- get("destinations", envir = asNamespace("qiblalab"))
  }

  ids <- candidates$id

  # Compute bearing and error columns for each candidate
  for (id in ids) {
    dest <- candidates[candidates$id == id, ]
    b_col <- paste0("bearing_", id)
    e_col <- paste0("error_",   id)
    data[[b_col]] <- bearing_to_destination(data, dest, model, lat_col, lon_col)
    data[[e_col]] <- absolute_angular_error(data[[azimuth_col]], data[[b_col]])
  }

  # Per-row nearest / second-nearest / margin
  error_cols <- paste0("error_", ids)
  error_mat  <- as.matrix(data[, error_cols, drop = FALSE])

  nr <- nrow(error_mat)

  nearest_idx <- vapply(seq_len(nr), function(i) {
    row <- error_mat[i, ]
    if (all(is.na(row))) NA_integer_ else which.min(row)
  }, integer(1L))

  nearest_id    <- ifelse(is.na(nearest_idx), NA_character_, ids[nearest_idx])
  nearest_error <- error_mat[cbind(seq_len(nr), ifelse(is.na(nearest_idx), 1L, nearest_idx))]
  nearest_error[is.na(nearest_idx)] <- NA_real_

  second_nearest_idx <- vapply(seq_len(nr), function(i) {
    row <- error_mat[i, ]
    if (all(is.na(row))) return(NA_integer_)
    row[which.min(row)] <- Inf
    which.min(row)
  }, integer(1L))

  second_nearest_id    <- ifelse(is.na(second_nearest_idx), NA_character_, ids[second_nearest_idx])
  second_nearest_error <- error_mat[cbind(seq_len(nr), ifelse(is.na(second_nearest_idx), 1L, second_nearest_idx))]
  second_nearest_error[is.na(second_nearest_idx)] <- NA_real_

  margin    <- second_nearest_error - nearest_error
  ambiguous <- margin < ambiguity_threshold

  # NA out computed fields where azimuth is missing
  no_az <- is.na(data[[azimuth_col]])
  nearest_id[no_az]          <- NA_character_
  nearest_error[no_az]       <- NA_real_
  second_nearest_id[no_az]   <- NA_character_
  second_nearest_error[no_az]<- NA_real_
  margin[no_az]               <- NA_real_
  ambiguous[no_az]            <- NA

  data$nearest_id           <- nearest_id
  data$nearest_error        <- nearest_error
  data$second_nearest_id    <- second_nearest_id
  data$second_nearest_error <- second_nearest_error
  data$margin               <- margin
  data$ambiguous            <- ambiguous

  tibble::as_tibble(data)
}
