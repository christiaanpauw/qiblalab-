# ADR-03: Definition of angular residual

**Status**: Accepted
**Date**: 2026-07-13

## Context

The angular residual measures how far a mosque's observed azimuth deviates from the theoretical bearing to a candidate destination. The choice of definition has downstream effects on sign conventions, statistical tests, and interpretation.

Candidates:

1. **Signed residual**: `observed − theoretical`, wrapped to [−180°, 180°]. Positive = clockwise deviation, negative = anticlockwise. Preserves directional information.
2. **Absolute (unsigned) residual**: `|observed − theoretical|`, in [0°, 180°]. Discards direction of deviation.
3. **Circular distance**: minimum arc between two angles, always in [0°, 180°]. Equivalent to the absolute residual here.

The design doc specifies both `absolute_angular_error()` and `signed_angular_error()` as required functions.

## Decision

- **Primary quantity**: signed residual, wrapped to [−180°, 180°]. Computed as `((observed − theoretical) + 180) %% 360 − 180`.
- **Derived quantity**: absolute residual = `abs(signed_residual)`, in [0°, 180°].
- Both are returned by default when computing residuals; the analyst chooses which to use for a given test.
- Tolerance-based classification uses the **absolute** residual (a mosque within ±t° of the theoretical bearing, regardless of which side).
- Directional bias tests (is there systematic over- or under-shooting?) use the **signed** residual.

## Rationale

- The signed residual preserves information about systematic bias (e.g. all North African mosques deviate clockwise from the Mecca bearing), which is analytically important.
- The absolute residual is the natural quantity for "how close is this mosque to destination X" — the tolerance criterion.
- Returning both avoids forcing analysts to re-derive one from the other.
- Wrapping to [−180°, 180°] rather than [0°, 360°) makes signed residuals interpretable: positive = clockwise, negative = anticlockwise, near-zero = consistent.

## Consequences

- `angular_difference(a, b)` always returns a value in [−180°, 180°].
- `absolute_angular_error(a, b)` always returns a value in [0°, 180°].
- Statistical summaries of signed residuals require circular (not linear) methods because the distribution wraps at ±180°.
- Documentation must clearly state the sign convention wherever residuals appear.
