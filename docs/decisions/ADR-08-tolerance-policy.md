# ADR-08: Default tolerance policy

**Status**: Accepted
**Date**: 2026-07-13

## Context

Tolerance-based classification asks: "is a mosque's observed azimuth close enough to the theoretical bearing to destination X to count as pointing toward X?" This requires a tolerance threshold in degrees.

Existing work in this area uses tolerances ranging from 2° to 10°, sometimes without justification. The choice of tolerance directly controls the count of "consistent" mosques and therefore whether a proportion test passes or fails. A package that silently applies one tolerance would embed an unacknowledged analytical choice.

Gibson's data has no azimuth uncertainty field, so tolerance cannot be automatically set from measurement precision. The `rebuilt` column further complicates single-threshold classification.

## Decision

1. **No universal default tolerance**. Functions that require a tolerance (`classify_by_tolerance()`, `test_qibla_hypothesis()`) require the analyst to supply it explicitly. Omitting the argument is an error with a clear message.

2. **Report across a range**. The standard output for any tolerance-based analysis includes results at t = 2°, 5°, and 10° alongside the analyst-specified value. A sensitivity plot showing the result as a continuous function of t is available via `plot_tolerance_sensitivity()`.

3. **Mosque-specific tolerances** are supported: the `tolerance` argument may be a scalar (applied uniformly) or a vector of length `nrow(data)` (applied per-mosque, e.g. based on azimuth measurement uncertainty if known from an external source).

4. **Probabilistic membership** (soft classification) is available as an alternative to hard thresholding: a mosque receives a membership probability based on how its angular residual compares to a specified distribution (e.g. a half-normal or von Mises centred on zero residual).

## Rationale

- Forcing an explicit tolerance prevents the most common analytical error in this literature: counting "consistent" mosques under an unstated assumption.
- Reporting across a range makes it immediately visible whether a conclusion is tolerance-sensitive.
- Per-mosque tolerances anticipate future datasets that include measurement uncertainty.
- Soft membership is more honest than hard thresholding when residuals cluster near a tolerance boundary.

## Consequences

- `classify_by_tolerance(data, destination, tolerance = NULL)` raises an error if `tolerance` is NULL.
- All analysis outputs include the tolerance value(s) used.
- The standard vignette ("testing the Petra hypothesis") runs the analysis at five tolerances to illustrate sensitivity.
- Researchers comparing results across studies must report and match tolerance choices.
