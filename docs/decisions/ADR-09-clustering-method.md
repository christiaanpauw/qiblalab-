# ADR-09: Selected circular clustering method

**Status**: Accepted
**Date**: 2026-07-13

## Context

The design doc requires destination-blind clustering of mosque orientation traditions as a central feature. It lists four candidate methods:
1. Von Mises mixture models (EM algorithm)
2. Circular k-means or equivalent
3. Hierarchical clustering with circular distance
4. Model-based clustering

Available R packages were evaluated:

| Package | Method | CRAN status | Notes |
|---|---|---|---|
| `movMF` | von Mises–Fisher mixtures | Active | Designed for directional data; handles arbitrary dimension |
| `Rmixmod` | Model-based clustering | Active | Does not natively support circular distributions |
| `circular` | Circular statistics, basic clustering | Active | No mixture EM; provides circular distance for hierarchical |
| `CircStats` | Circular stats | Maintained | Older; overlaps with `circular` |
| `mclust` | Gaussian mixture | Active | Linear only; not appropriate for raw azimuths |

Von Mises mixture models are the most principled probabilistic approach for circular data. `movMF` implements EM for von Mises–Fisher mixtures and is actively maintained on CRAN.

Circular k-means is a lightweight deterministic alternative useful for quick exploration. No single definitive R package exists; it can be implemented in ~20 lines using circular distance.

## Decision

**Primary method: von Mises mixture model** via `movMF` (in `Suggests`).

`cluster_qiblas()` defaults to `method = "von_mises_mixture"` and internally uses `movMF::movMF()`. The number of components is chosen by fitting k = 1 through k_max (default 8) and selecting by BIC.

**Secondary method: circular k-means** implemented directly in the package (`method = "circular_kmeans"`), with no external dependency. Uses circular distance (1 − cos(θ)) as the dissimilarity metric. Serves as a fast, dependency-free alternative for exploration and as a sanity check against the mixture model.

**Hierarchical clustering** (`method = "hierarchical"`) is available via `circular` package, placed in `Suggests`. Uses the circular distance matrix and Ward linkage by default.

Model selection across k is always reported; the package does not automatically select k and hide the model selection process.

## Rationale

- Von Mises mixture models are the natural probabilistic model for directional data; they provide soft cluster assignments, likelihoods, BIC, and uncertainty quantification.
- `movMF` is in `Suggests` rather than `Imports` to keep the core package lightweight — analysts who only need proportion tests don't need it.
- A self-contained circular k-means removes the hard dependency for the common case of exploratory analysis.
- Reporting results across k = 1..8 prevents over-interpreted "optimal" cluster counts.

## Consequences

- `movMF` is listed in `Suggests`; users who call `method = "von_mises_mixture"` are prompted to install it if absent.
- `circular` is listed in `Suggests`; required for hierarchical method and some circular statistics.
- The internal circular k-means implementation is unit-tested against known synthetic datasets.
- Cluster count selection is always exposed in output; no automatic "best k" is returned without showing the full BIC / within-cluster-dispersion curve.
