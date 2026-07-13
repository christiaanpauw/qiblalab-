# ADR-12: Licensing and attribution

**Status**: Accepted
**Date**: 2026-07-13

## Context

The package has two distinct components with different licensing concerns:

1. **Package code** (R functions, tests, vignettes, documentation): original work by the qiblalab authors.
2. **Shipped data** (`data/gibson_qibla.rda`): derived from Dan Gibson's dataset, released under CC BY 4.0.

CC BY 4.0 permits redistribution and adaptation provided attribution is given. Redistributing the cleaned dataset as package data is permitted under this licence.

## Decision

### Package code

Licensed under **MIT**. Rationale: permissive licence maximises reuse in academic and commercial contexts; compatible with all dependencies (sf, ggplot2, etc.); standard for research R packages.

Files: `LICENSE`, `LICENSE.md` (full MIT text), `DESCRIPTION: License: MIT + file LICENSE`.

### Shipped data (`gibson_qibla`)

The `gibson_qibla` dataset carries CC BY 4.0 attribution to Dan Gibson in:
- `?gibson_qibla` documentation: full citation in the `@source` tag
- `attr(gibson_qibla, "provenance")$citation`: machine-readable citation string
- `README.md`: data attribution section
- Every parameterised Quarto report: rendered citation block
- `DESCRIPTION: Authors@R`: Gibson listed as `role = "dtc"` (data contributor)

The canonical citation (required in all publications using this data):

> Gibson, Dan (2021). Early Islamic Qibla Database 2021. figshare. Dataset. https://doi.org/10.6084/m9.figshare.13570655.v2

### Citation metadata

`CITATION.cff` lists the package authors and separately acknowledges the data source. `inst/CITATION` provides `citation("qiblalab")` output for R users, including both package and data citations.

## Consequences

- `DESCRIPTION` includes `License: MIT + file LICENSE`.
- Gibson is added to `Authors@R` with `role = c("dtc")`.
- Any analysis that uses `gibson_qibla` (which is all analyses using the default data) must cite Gibson (2021). The provenance report template enforces this.
- Alternative datasets contributed by future users carry their own licences; the package infrastructure supports `attr(., "provenance")$licence` on any imported dataset.
