## Angular residual functions.
## ADR-03: signed residual [-180, 180] is primary; absolute is derived.
## ADR-01: all inputs treated as directional (0-360) unless axial = TRUE.

# Wrap angle to [-180, 180]
.wrap180 <- function(x) ((x + 180) %% 360) - 180


#' Signed angular difference between two bearings
#'
#' Computes `a - b` on the circle, wrapped to \[-180, 180\].
#' Positive means `a` is clockwise from `b`; negative means anticlockwise.
#'
#' @param a,b Numeric vectors of bearings in degrees. Need not be in \[0, 360\);
#'   any real value is accepted and interpreted modulo 360.
#'
#' @return Numeric vector in \[-180, 180\].
#'
#' @seealso [absolute_angular_error()], [signed_angular_error()]
#' @export
#' @examples
#' angular_difference(10, 350)   #  20  (10 is 20 deg clockwise from 350)
#' angular_difference(350, 10)   # -20
#' angular_difference(180, 0)    #  180 (or -180 -- exactly opposite)
#' angular_difference(1, 359)    #  2
angular_difference <- function(a, b) {
  checkmate::assert_numeric(a)
  checkmate::assert_numeric(b)
  .wrap180(a - b)
}


#' Absolute angular error between an observed and theoretical bearing
#'
#' Returns the minimum arc between two bearings, always in \[0, 180\].
#' Equivalent to `abs(angular_difference(observed, theoretical))`.
#'
#' This is the quantity used for tolerance-based classification (ADR-08):
#' a mosque is "within t degrees" of a candidate when
#' `absolute_angular_error(observed, theoretical) <= t`.
#'
#' @param observed Numeric vector. Observed azimuth(s), decimal degrees.
#' @param theoretical Numeric vector. Theoretical bearing(s), decimal degrees.
#'
#' @return Numeric vector in \[0, 180\].
#'
#' @seealso [signed_angular_error()], [angular_difference()]
#' @export
#' @examples
#' absolute_angular_error(175, 180)   #  5
#' absolute_angular_error(5, 355)     #  10
#' absolute_angular_error(0, 180)     # 180
absolute_angular_error <- function(observed, theoretical) {
  checkmate::assert_numeric(observed)
  checkmate::assert_numeric(theoretical)
  abs(.wrap180(observed - theoretical))
}


#' Signed angular error between an observed and theoretical bearing
#'
#' Returns `observed - theoretical` wrapped to \[-180, 180\].
#' Positive means the observed azimuth is clockwise from the theoretical
#' bearing; negative means anticlockwise.
#'
#' Use this to detect systematic bias (e.g. all mosques in a region deviate
#' clockwise from a candidate bearing). For tolerance classification, use
#' [absolute_angular_error()].
#'
#' @inheritParams absolute_angular_error
#' @return Numeric vector in \[-180, 180\].
#'
#' @seealso [absolute_angular_error()], [angular_difference()]
#' @export
#' @examples
#' signed_angular_error(185, 180)   #  5 (clockwise overshoot)
#' signed_angular_error(175, 180)   # -5 (anticlockwise undershoot)
#' signed_angular_error(5, 355)     # 10
signed_angular_error <- function(observed, theoretical) {
  checkmate::assert_numeric(observed)
  checkmate::assert_numeric(theoretical)
  .wrap180(observed - theoretical)
}
