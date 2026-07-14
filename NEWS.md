# qiblalab 0.1.0

First public release.

## Data

* Ships `gibson_qibla` — Dan Gibson's Early Islamic Qibla Database (2021,
  CC BY 4.0) as a cleaned, documented tibble with provenance and audit-log
  attributes.
* Ships `destinations` — three candidate destinations (Mecca, Petra, Jerusalem)
  as a reference tibble.
* Ships `qibla_scenarios` — ten synthetic datasets covering archetypal
  orientation patterns (design doc §19), for use in examples and tests.

## Functions

### Data layer

* `download_qibla_data()` — pins the Figshare DOI, caches locally, verifies
  MD5.
* `import_qibla_data()` — reads the XLSX, preserves original columns, attaches
  immutable row IDs.
* `validate_qibla_data()` / `validation_errors()` / `validation_warnings()` —
  schema and structural checks.
* `prepare_qibla_data()` — applies documented cleaning rules, writes every
  action to a machine-readable audit log.

### Geodesic calculations

* `bearing_to_destination()` / `bearing_to_candidates()` — great-circle
  initial bearing with pluggable model.
* `bearing_haversine()`, `bearing_vincenty()`, `bearing_rhumb()` — three
  bearing models.
* `angular_difference()`, `absolute_angular_error()`,
  `signed_angular_error()` — circular-correct error metrics.

### Circular statistics

* `circular_mean()`, `circular_resultant()`, `circular_var()`,
  `circular_sd()`, `circular_kappa()`, `rayleigh_test()`,
  `circular_summary()`.

### Hypothesis testing

* `qibla_hypothesis()` — structured hypothesis specification with population
  filter, candidate, tolerance, and expected share.
* `test_qibla_hypothesis()` — tolerance-based classification with sensitivity
  table (ADR-08).
* `compare_qibla_hypotheses()` — nearest-candidate comparison across all
  destinations.

### Intermediate bearings and uncertainty

* `intermediate_bearing()` — five methods: circular midpoint, weighted,
  free latent, common convention, latent cluster.
* `infer_latent_destination()` — optimises a latent destination by
  minimising mean absolute bearing error.
* `define_uncertainty()`, `sample_qibla_dataset()`,
  `run_sensitivity_analysis()` — Monte Carlo uncertainty propagation.
* `permute_orientations()`, `permutation_test()` — permutation testing with
  optional block structure.

### Clustering

* `cluster_qiblas()` — circular k-means, von Mises mixture (movMF), and
  hierarchical clustering; all destination-blind (ADR-07).
* `validate_clusters()` — internal, stability (bootstrap ARI), external
  (post-hoc only), and predictive validation (ADR-10).

### Visualisation

* `plot_rose()`, `plot_residuals()`, `plot_orientation_time()`,
  `plot_sensitivity()`, `plot_candidate_comparison()`,
  `plot_cluster_profile()` — ggplot2-based analytical plots.
* `map_mosques()`, `map_clusters()` — geographic scatter maps.

## Vignettes

* *Data Provenance and Import* — dataset structure, provenance attributes,
  Figshare download workflow.
* *Destination-Blind Clustering* — circular statistics, clustering,
  validation, and neutral reporting guidance.
