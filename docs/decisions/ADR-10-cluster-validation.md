# ADR-10: Cluster validation criteria

**Status**: Accepted
**Date**: 2026-07-13

## Context

The design doc explicitly warns against using cluster purity as the only validation measure: "Purity can be made artificially high by increasing the number of clusters." It requires several complementary measures across three categories: internal validation, stability, and external validation.

This ADR defines the minimum set of measures that must pass before a cluster solution is described as "historically meaningful" in any package output.

## Decision

A cluster solution is reported with three mandatory validation sections. No claim of historical meaningfulness is made unless all three sections are computed and reported.

### Internal validation (computed for every fit)

| Measure | Threshold for "acceptable" |
|---|---|
| Mean within-cluster circular dispersion (1 − R̄) | Lower is better; no hard threshold, reported as-is |
| Between-cluster circular separation (min arc between centroids) | > 10° required to distinguish clusters as meaningfully different |
| BIC / log-likelihood (for mixture models) | Reported for all k; used for model selection only |
| Proportion of ambiguous assignments (posterior < 0.6 for any component) | Reported; > 30% triggers a warning |

### Stability (computed by default, suppressible)

| Measure | Threshold |
|---|---|
| Bootstrap adjusted Rand index (100 resamples default) | Mean ARI > 0.6 required to claim stability |
| Leave-one-region-out ARI | Reported; drop > 0.2 from full-data ARI triggers a warning |
| Leave-one-period-out ARI | Reported; same threshold |
| Sensitivity to ±5° azimuth perturbation | Reported |

### External validation (computed post-hoc, never used in fitting)

External labels used: `age_group`, `country`, `gibson_classification`, geographic region (derived from coordinates). Never the azimuth itself.

| Measure | Reported |
|---|---|
| Adjusted Rand index vs. each external label | Yes, with permutation null (999 permutations) |
| Normalised mutual information | Yes |
| Cluster purity vs. each external label | Yes, with balanced-purity correction |
| Cramér's V | Yes |

A result is "statistically associated" with an external label only when the observed ARI exceeds the 95th percentile of the permutation null.

### Predictive validation (when n ≥ 60)

- Fit on a random 80% training split, predict held-out 20%.
- Report mean angular prediction error on held-out mosques.
- Repeat 20 times; report mean and SD.

## Rationale

- No single measure is sufficient: high purity is trivially achievable; high BIC improvement can reflect noise fitting; high bootstrap ARI can occur when clusters are geographically confounded.
- Permutation nulls for external agreement measures are essential because some external labels (e.g. `country`) are strongly spatially structured — a purely geographic cluster will appear to agree with them for spurious reasons.
- The 10° centroid separation threshold ensures clusters represent meaningfully distinct orientation traditions, not numerical artefacts.

## Consequences

- `validate_clusters()` returns a structured list with all three sections; `print()` method shows a summary with pass/warn/fail indicators.
- Functions emit a warning when stability has not been computed (e.g. `bootstrap = FALSE`) and a result is described.
- Vignettes demonstrate the full validation workflow on both real data and synthetic datasets with known structure.
