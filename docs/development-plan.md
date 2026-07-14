# qiblalab development plan

Derived from `Qiblalab Package Design.pdf`. Tracks phases toward v0.1.0.

---

## Phase 0 — Foundations (before writing any analysis code)

The design doc requires this phase before substantive coding (§23, §22).

1. **Download the Figshare workbook** and produce a data dictionary: field names, types, formats, missing-value patterns, ambiguous columns. This directly informs the data model.
2. **Write 12 architecture decision records** in `docs/decisions/`: axial vs. directional interpretation, geodesic model, angular residual definition, date handling, rebuilt mosques, raw/derived data separation, Gibson classifications treatment, tolerance policy, clustering method, validation criteria, dependency strategy, licensing.
3. **Create the package skeleton** via `usethis::create_package()`, configure GitHub Actions CI (R CMD check on current + previous R release), add renv, configure lintr and spelling checks, add DESCRIPTION/CITATION.cff.

*Exit criterion: package installs cleanly and CI is green before any functions exist.*

---

## Phase 1 — Data layer

- `download_qibla_data()`: pin Figshare DOI, cache locally, record checksum/download date/URL, refuse silent version replacement
- `import_qibla_data()`: read spreadsheet, preserve all original columns, attach immutable row IDs
- `validate_qibla_data()`: schema-check against the canonical field list, surface anomalies
- `prepare_qibla_data()`: apply documented cleaning rules, write every action to a machine-readable audit log; keep raw and derived data strictly separate

*Exit criterion: a researcher can import the dataset, inspect every cleaning decision, and trace any derived row back to the original spreadsheet.*

---

## Phase 2 — Geodesic calculations

- `bearing_to_destination()` / `bearing_to_candidates()` — great-circle initial bearing, stated ellipsoid, replaceable bearing-model argument
- `angular_difference()`, `absolute_angular_error()`, `signed_angular_error()` — correct circular wrap-around
- Ship the canonical destination table (Mecca, Petra, Jerusalem) as an editable hypothesis table, not hardcoded truth
- Unit tests: cardinal directions, antipodal cases, 0°/360° wrap, known reference values

---

## Phase 3 — Circular statistics & basic hypothesis testing

- Wrap/implement: circular mean, mean resultant length, circular variance, von Mises CI, Rayleigh test, uniformity and homogeneity tests
- `qibla_hypothesis()` object — records population filter, candidate, tolerance, null/alternative, uncertainty assumptions
- `test_qibla_hypothesis()` — tolerance-based classification, proportion test
- `compare_qibla_hypotheses()` — competing-destination comparison

*Exit criterion: the "most early mosques pointed to Petra" vignette can be run end-to-end.*

---

## Phase 4 — Intermediate qibla & uncertainty

- `intermediate_bearing()` with five methods: circular midpoint, weighted interpolation, free latent target, common convention, latent cluster
- `define_uncertainty()`, `sample_qibla_dataset()`, `run_sensitivity_analysis()` — deterministic/sensitivity/probabilistic modes
- Permutation tests that break orientation/faction association while preserving geographic structure
- Research safeguard warnings: small samples, weak dates, high sensitivity, near-collinear candidates

---

## Phase 5 — Unsupervised clustering

Central requirement (§10). Must be fully destination-blind at fit time.

- At minimum: von Mises mixture model
- `cluster_qiblas()` with `representation` and `features` arguments
- Cluster validation: within-cluster dispersion, between-cluster separation, bootstrap stability, leave-one-region-out, external agreement (ARI, NMI, purity) via permutation null
- Predictive validation: held-out angular prediction error

---

## Phase 6 — Visualization & reporting

- Core ggplot2 plots: rose/circular histogram, residual distribution, orientation-vs-time, sensitivity curves, candidate comparison, cluster profiles, stability plots
- sf-based maps: mosque locations, orientation rays, candidate bearings, cluster assignments
- Two required vignettes: **data provenance & import**, **destination-blind clustering**
- Parameterized Quarto templates for data provenance, candidate-destination analysis, cluster analysis

---

## Phase 7 — v0.1.0 release

- R CMD check: 0 errors, 0 warnings, 0 avoidable notes
- Complete roxygen2 docs with examples for all public functions
- pkgdown site
- CITATION.cff, NEWS.md
- Synthetic datasets covering the 10 scenarios in §19

---

## Key risks

1. **Figshare workbook quality** — data dictionary in Phase 0 may reveal ambiguous fields that force redesign of the data model. Block everything else on this.
2. **Circular statistics package selection** — evaluate maintenance and numerical suitability before committing. Decision belongs in Phase 0 ADRs.
3. **Clustering method** — if no maintained R package covers von Mises mixtures adequately, implementing from scratch is substantial.
4. **Sample size** — small regional/temporal subsets will hit research safeguard warnings hard; good to understand before committing to test designs.
