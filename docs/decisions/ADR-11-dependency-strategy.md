# ADR-11: Dependency strategy

**Status**: Accepted
**Date**: 2026-07-13

## Context

The package requires a range of capabilities: spatial computation, circular statistics, data manipulation, visualisation, clustering, and reporting. Some of these have heavy or specialist dependencies. The design doc requires the core package to be "reasonably lightweight" and heavy dependencies to be placed behind `Suggests`.

## Decision

### Imports (hard dependencies — always installed)

| Package | Purpose |
|---|---|
| `sf` | Spatial features; coordinate handling; CRS enforcement |
| `dplyr` | Data manipulation |
| `tidyr` | Reshaping |
| `purrr` | Functional iteration |
| `tibble` | Data frames |
| `ggplot2` | All visualisations |
| `cli` | User-facing messages, warnings, errors |
| `checkmate` | Argument validation in all public functions |
| `rlang` | Tidy evaluation; error handling |

`readxl` is **not** in `Imports` — it is only used in `data-raw/qibla_data.R`, which is a developer script, not a user-facing function. The shipped `gibson_qibla.rda` means users never need to read the XLSX.

### Suggests (optional — installed on demand with informative error)

| Package | Purpose | Required by |
|---|---|---|
| `movMF` | Von Mises–Fisher mixture models | `cluster_qiblas(method = "von_mises_mixture")` |
| `circular` | Circular statistics; hierarchical clustering | `cluster_qiblas(method = "hierarchical")`; advanced circular stats |
| `geosphere` | Vincenty / WGS84 ellipsoidal bearings | `bearing_vincenty()` |
| `quarto` | Report rendering | Quarto report templates |
| `knitr` | Vignette engine | Vignettes |
| `rmarkdown` | Vignette support | Vignettes |
| `testthat` | Testing framework | Tests |
| `covr` | Coverage reporting | CI |

### Not used

- `CircStats`: superseded by `circular`; not added.
- `mclust`: Gaussian only; not appropriate for raw circular data.
- Any Bayesian inference package (Stan, brms, rstan): deferred to a future extension; out of scope for v0.1.0.

### Handling missing Suggests

When a function requires a `Suggests` package that is not installed, it raises an informative error via `rlang::check_installed()` with the package name and the installation command. It does not silently fall back to a different method.

### R version compatibility

Target: R ≥ 4.1.0 (pipe operator `|>` is available). CI tests on current release, previous release, and R-devel.

## Rationale

- Keeping `Imports` small reduces installation friction for analysts who only need proportion tests or basic visualisations.
- `sf` in `Imports` (not `Suggests`) is justified: spatial coordinate handling is core to the package, not optional.
- `cli` and `checkmate` in `Imports` enforce consistent, informative user-facing messages and argument validation across the whole API.
- Failing loudly on missing `Suggests` packages is safer than silently falling back — it prevents analysts from accidentally running a different method than they intended.

## Consequences

- `DESCRIPTION` lists nine `Imports` packages and eight `Suggests` packages.
- Every public function that uses a `Suggests` package calls `rlang::check_installed()` at the top of the function body.
- `renv.lock` is committed for the development environment (full dependency tree including `Suggests`).
- `data-raw/` scripts are excluded from `R CMD check` and may use packages not listed in `DESCRIPTION`.
