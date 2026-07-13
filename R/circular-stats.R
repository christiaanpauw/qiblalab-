## Circular (directional) descriptive statistics.
## All inputs are angles in degrees; outputs are in degrees unless noted.
## ADR-01: data are treated as directional (0-360) by default.
## Sources: Mardia & Jupp (2000), Zar (2010).

# Convert degree angles to unit complex numbers
.to_unit_complex <- function(theta_deg) {
  exp(1i * theta_deg * pi / 180)
}


#' Circular (directional) mean
#'
#' Returns the mean direction of a set of angles, computed as the argument of
#' the mean unit vector. Correctly handles wrap-around (e.g. the mean of 1 deg
#' and 359 deg is 0 deg, not 180 deg).
#'
#' @param x Numeric vector of angles in degrees.
#' @param na.rm Logical. Remove `NA` values before computing. Default `FALSE`.
#'
#' @return A single numeric value in \[0, 360), or `NA` if `x` is empty or all
#'   `NA`.
#' @export
#' @examples
#' circular_mean(c(10, 350))   # 0, not 180
#' circular_mean(c(90, 180, 270, 0))
circular_mean <- function(x, na.rm = FALSE) {
  checkmate::assert_numeric(x)
  checkmate::assert_flag(na.rm)
  if (na.rm) x <- x[!is.na(x)]
  if (length(x) == 0L || all(is.na(x))) return(NA_real_)
  z <- mean(.to_unit_complex(x))
  (Arg(z) * 180 / pi + 360) %% 360
}


#' Mean resultant length (circular concentration)
#'
#' The mean resultant length R-bar is the length of the mean unit vector.
#' It ranges from 0 (uniformly scattered) to 1 (all angles identical).
#' A high R-bar indicates a strongly concentrated distribution.
#'
#' @inheritParams circular_mean
#' @return A single numeric value in \[0, 1\].
#' @export
#' @examples
#' circular_resultant(c(0, 0, 0))     # 1: perfect concentration
#' circular_resultant(c(0, 180))      # 0: antipodal pair
#' circular_resultant(c(0, 90, 180, 270))  # ~0: near-uniform
circular_resultant <- function(x, na.rm = FALSE) {
  checkmate::assert_numeric(x)
  checkmate::assert_flag(na.rm)
  if (na.rm) x <- x[!is.na(x)]
  if (length(x) == 0L || all(is.na(x))) return(NA_real_)
  Mod(mean(.to_unit_complex(x)))
}


#' Circular variance
#'
#' Defined as `1 - R-bar`, in \[0, 1\]. Analogous to linear variance but
#' bounded. A value near 0 indicates high concentration; near 1 means near-
#' uniform scatter.
#'
#' @inheritParams circular_mean
#' @return A single numeric value in \[0, 1\].
#' @export
#' @examples
#' circular_var(c(0, 0, 0))    # 0: no spread
#' circular_var(c(0, 90, 180, 270))  # ~1: highly spread
circular_var <- function(x, na.rm = FALSE) {
  1 - circular_resultant(x, na.rm = na.rm)
}


#' Circular standard deviation
#'
#' Defined as `sqrt(-2 * log(R-bar))` (Mardia & Jupp 2000, eq. 2.3.9),
#' returned in degrees. Unlike circular variance, this measure has the same
#' units as the original angles and is unbounded above.
#'
#' Returns `Inf` when R-bar = 0 (perfectly uniform scatter) and `0` when
#' R-bar = 1 (perfect concentration).
#'
#' @inheritParams circular_mean
#' @return A single non-negative numeric value in degrees.
#' @export
#' @examples
#' circular_sd(c(0, 0, 0))        # 0
#' circular_sd(c(160, 170, 180, 190, 200))  # ~14 degrees
circular_sd <- function(x, na.rm = FALSE) {
  rbar <- circular_resultant(x, na.rm = na.rm)
  if (is.na(rbar)) return(NA_real_)
  sqrt(-2 * log(rbar)) * 180 / pi
}


#' Estimate von Mises concentration parameter (kappa)
#'
#' Uses the moment estimator approximation of Best & Fisher (1981).
#' Returns `0` when R-bar = 0 and `Inf` when R-bar = 1.
#'
#' @param rbar Numeric scalar in \[0, 1\]. Mean resultant length from
#'   [circular_resultant()].
#' @return Estimated kappa (numeric scalar >= 0).
#' @export
#' @examples
#' circular_kappa(0.5)
#' circular_kappa(0.9)
circular_kappa <- function(rbar) {
  checkmate::assert_number(rbar, lower = 0, upper = 1)
  if (rbar < 0.53) {
    2 * rbar + rbar^3 + 5 * rbar^5 / 6
  } else if (rbar < 0.85) {
    -0.4 + 1.39 * rbar + 0.43 / (1 - rbar)
  } else {
    1 / (rbar^3 - 4 * rbar^2 + 3 * rbar)
  }
}


#' Rayleigh test of circular uniformity
#'
#' Tests H0: angles are uniformly distributed on \[0, 360\) against H1: there
#' is a preferred mean direction (von Mises alternative). A significant result
#' means the angles cluster around a common direction.
#'
#' The p-value uses the approximation of Mardia & Jupp (2000, equation 6.3.10)
#' which is accurate for n >= 10. For n < 10, interpret the p-value with
#' caution.
#'
#' @inheritParams circular_mean
#' @return A list with:
#' \describe{
#'   \item{statistic}{The Rayleigh Z statistic (n * R-bar^2).}
#'   \item{rbar}{Mean resultant length.}
#'   \item{p.value}{Approximate p-value for the test of uniformity.}
#'   \item{n}{Number of observations used.}
#'   \item{method}{Description string.}
#' }
#' @export
#' @examples
#' # Concentrated data: should reject uniformity
#' rayleigh_test(rnorm(50, mean = 180, sd = 10))
#' # Uniform data: should not reject
#' rayleigh_test(runif(50, 0, 360))
rayleigh_test <- function(x, na.rm = FALSE) {
  checkmate::assert_numeric(x)
  checkmate::assert_flag(na.rm)
  if (na.rm) x <- x[!is.na(x)]
  x <- x[!is.na(x)]
  n <- length(x)
  if (n < 2L) {
    cli::cli_warn("Rayleigh test requires at least 2 observations; got {n}.")
    return(list(statistic = NA_real_, rbar = NA_real_, p.value = NA_real_,
                n = n, method = "Rayleigh test of circular uniformity"))
  }

  rbar <- circular_resultant(x)
  z    <- n * rbar^2

  # Mardia & Jupp (2000) approximation, eq. 6.3.10
  p <- exp(-z) *
    (1 +
       (2 * z - z^2) / (4 * n) -
       (24 * z - 132 * z^2 + 76 * z^3 - 9 * z^4) / (288 * n^2))
  p <- min(max(p, 0), 1)

  if (n < 10L) {
    cli::cli_warn(
      "n = {n} is small; the Rayleigh test p-value is approximate.",
      .frequency = "once", .frequency_id = "rayleigh_small_n"
    )
  }

  list(
    statistic = z,
    rbar      = rbar,
    p.value   = p,
    n         = n,
    method    = "Rayleigh test of circular uniformity"
  )
}


#' Circular summary statistics
#'
#' Computes a standard set of circular descriptive statistics for a vector of
#' angles. All angles are in degrees.
#'
#' @inheritParams circular_mean
#' @return A `qibla_circular_summary` object (list). Print it for a
#'   human-readable table.
#' @export
#' @examples
#' data(gibson_qibla)
#' circular_summary(gibson_qibla$azimuth, na.rm = TRUE)
circular_summary <- function(x, na.rm = FALSE) {
  checkmate::assert_numeric(x)
  checkmate::assert_flag(na.rm)

  x_used <- if (na.rm) x[!is.na(x)] else x
  n_na   <- sum(is.na(x))
  n      <- length(x_used) - n_na

  rtest  <- rayleigh_test(x_used, na.rm = TRUE)

  structure(
    list(
      n           = length(x_used[!is.na(x_used)]),
      n_na        = n_na,
      mean        = circular_mean(x_used, na.rm = TRUE),
      rbar        = circular_resultant(x_used, na.rm = TRUE),
      variance    = circular_var(x_used, na.rm = TRUE),
      sd          = circular_sd(x_used, na.rm = TRUE),
      kappa       = circular_kappa(circular_resultant(x_used, na.rm = TRUE)),
      rayleigh_z  = rtest$statistic,
      rayleigh_p  = rtest$p.value
    ),
    class = "qibla_circular_summary"
  )
}

#' @export
print.qibla_circular_summary <- function(x, digits = 3, ...) {
  cli::cli_h3("Circular summary")
  cli::cli_bullets(c(
    "*" = "n: {x$n} (NA excluded: {x$n_na})",
    "*" = "Mean direction: {round(x$mean, digits)} deg",
    "*" = "Mean resultant length (R-bar): {round(x$rbar, digits)}",
    "*" = "Circular variance: {round(x$variance, digits)}",
    "*" = "Circular SD: {round(x$sd, digits)} deg",
    "*" = "Concentration (kappa): {round(x$kappa, digits)}",
    "*" = "Rayleigh Z: {round(x$rayleigh_z, digits)},  p = {format.pval(x$rayleigh_p, digits = digits)}"
  ))
  invisible(x)
}
