# qiblalab

**Reproducible analysis of early Islamic mosque orientations.**

<!-- badges: start -->
[![R-CMD-check](https://github.com/christiaanpauw/qiblalab/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/christiaanpauw/qiblalab/actions/workflows/R-CMD-check.yaml)
[![License: MIT](https://img.shields.io/badge/License-MIT-yellow.svg)](https://opensource.org/licenses/MIT)
<!-- badges: end -->

`qiblalab` is an R package for testing competing hypotheses about how early Islamic mosques determined the qibla — the prayer direction toward Mecca. It imports Dan Gibson's *Early Islamic Qibla Database* (2021), computes geodesic bearings, applies circular statistics, and provides a structured framework for hypothesis testing and destination-blind clustering. Every analytical decision is explicit and reproducible.

**The package takes no position on which candidate destination is correct.** All methods treat Mecca, Petra, and Jerusalem symmetrically as hypotheses to be evaluated against the data.

---

## Installation

```r
# install.packages("pak")
pak::pkg_install("christiaanpauw/qiblalab")
```

Optional but recommended for clustering and visualisation:

```r
pak::pkg_install(c("ggplot2", "movMF"))
```

---

## Data

The package ships two ready-to-use datasets:

```r
library(qiblalab)

data(gibson_qibla)   # 160 mosques × 17 columns; 132 have measured azimuths
data(destinations)   # Mecca, Petra, Jerusalem with WGS 84 coordinates
```

`gibson_qibla` carries a `provenance` attribute recording the Figshare DOI and download metadata, and an `audit_log` of every cleaning decision. To re-ingest from the original source:

```r
raw   <- download_qibla_data()          # pins DOI, verifies MD5
df    <- import_qibla_data(raw)
df    <- prepare_qibla_data(df)         # applies documented cleaning rules
```

A third dataset, `qibla_scenarios`, ships ten small synthetic datasets covering archetypal patterns (single-destination noise, competing traditions, chronological transitions, geographic artefacts, and so on) for use in examples and tests without network access.

---

## Quick start

### 1 — Define and test a hypothesis

```r
h <- qibla_hypothesis(
  name       = "Early Petra orientation",
  population = year_ce_max <= 700,    # tidy-eval filter applied to the data
  candidate  = "petra",
  tolerance  = 5                      # degrees
)

result <- test_qibla_hypothesis(h, gibson_qibla)
print(result)
```

`test_qibla_hypothesis()` classifies each mosque as consistent or inconsistent, runs a binomial test against a stated null share, and automatically produces a sensitivity table at ±2°, ±5°, and ±10° so the robustness of conclusions can be evaluated (ADR-08).

### 2 — Compare all candidate destinations

```r
cmp <- compare_qibla_hypotheses(gibson_qibla)
# Returns one row per mosque with nearest_id, nearest_error, margin, ambiguous

dplyr::count(cmp, nearest_id, sort = TRUE)
```

### 3 — Circular statistics

```r
circular_summary(gibson_qibla$azimuth, na.rm = TRUE)
#> mean_direction  rbar    kappa  rayleigh_p  ...
```

### 4 — Destination-blind clustering

Cluster assignments are derived entirely from the observed azimuths. Gibson's classifications are hard-blocked as clustering inputs and may only appear as post-hoc external validation (ADR-07).

```r
res <- cluster_qiblas(gibson_qibla, k = 3, seed = 1)
print(res)

val <- validate_clusters(
  res,
  external    = c("gibson_classification", "age_group"),
  bootstrap_n = 100,
  seed        = 1
)
print(val)
```

Validation runs three mandatory sections before a cluster solution can be described as historically meaningful (ADR-10):

| Section | What it checks |
|---|---|
| Internal | Within-cluster dispersion, minimum centroid separation |
| Stability | Bootstrap adjusted Rand index across 100 resamples |
| External | ARI / NMI / purity against supplied labels — post-hoc only |

### 5 — Uncertainty propagation

```r
unc <- define_uncertainty(coord_sigma = 0.5, azimuth_sigma = 3)

sens <- run_sensitivity_analysis(
  gibson_qibla, h,
  mode      = "probabilistic",
  uncertainty = unc,
  n_samples = 500,
  seed      = 42
)
```

### 6 — Visualisation

```r
# Rose diagram
plot_rose(gibson_qibla)

# Azimuth vs. construction date
plot_orientation_time(gibson_qibla, colour_col = "age_group")

# Sensitivity curve
plot_sensitivity(result)

# Cluster profiles and map
plot_cluster_profile(res)
map_clusters(res)
```

All plot functions return `ggplot2` objects; add layers or themes with `+` in the usual way.

---

## API overview

| Group | Functions |
|---|---|
| **Data import** | `download_qibla_data()`, `import_qibla_data()`, `validate_qibla_data()`, `prepare_qibla_data()` |
| **Bearings** | `bearing_to_destination()`, `bearing_to_candidates()`, `bearing_haversine()`, `bearing_vincenty()`, `bearing_rhumb()` |
| **Error metrics** | `angular_difference()`, `absolute_angular_error()`, `signed_angular_error()` |
| **Circular statistics** | `circular_summary()`, `circular_mean()`, `circular_resultant()`, `circular_var()`, `circular_sd()`, `circular_kappa()`, `rayleigh_test()` |
| **Hypothesis testing** | `qibla_hypothesis()`, `test_qibla_hypothesis()`, `compare_qibla_hypotheses()` |
| **Intermediate bearings** | `intermediate_bearing()`, `infer_latent_destination()` |
| **Uncertainty** | `define_uncertainty()`, `sample_qibla_dataset()`, `run_sensitivity_analysis()` |
| **Permutation tests** | `permute_orientations()`, `permutation_test()` |
| **Clustering** | `cluster_qiblas()`, `validate_clusters()` |
| **Plots** | `plot_rose()`, `plot_residuals()`, `plot_orientation_time()`, `plot_sensitivity()`, `plot_candidate_comparison()`, `plot_cluster_profile()` |
| **Maps** | `map_mosques()`, `map_clusters()` |

---

## Vignettes

```r
vignette("data-provenance",               package = "qiblalab")
vignette("destination-blind-clustering",  package = "qiblalab")
```

---

## Design principles

**Neutrality.** The package is an analytical instrument, not an argument. All candidate destinations are treated symmetrically. Language throughout distinguishes `observed_orientation` from `theoretical_bearing` from `historical_method`, and none of the automated outputs imply ground truth.

**Uncertainty is first-class.** Coordinates, construction dates, and measured azimuths all carry uncertainty. The package supports deterministic, sensitivity, and probabilistic computation modes.

**Auditability.** Every data-cleaning decision is logged. No input is silently modified. All analytical choices are explicit and reproducible.

Twelve architecture decision records in [`docs/decisions/`](docs/decisions/) document the key methodological choices before any implementation code was written.

---

## Data citation

The shipped dataset derives from:

> Gibson, D. (2021). *Early Islamic Qibla Database*. figshare.
> <https://doi.org/10.6084/m9.figshare.13570655.v2> — CC BY 4.0.

When reporting results from `qiblalab`, cite both the package and the original dataset.

---

## License

`qiblalab` is released under the [MIT License](LICENSE.md). The underlying dataset is CC BY 4.0 (Dan Gibson 2021); attribution is required when redistibuting or publishing derived data.
