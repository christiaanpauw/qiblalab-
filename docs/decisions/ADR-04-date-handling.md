# ADR-04: Handling of uncertain or interval construction dates

**Status**: Accepted
**Date**: 2026-07-13

## Context

Gibson's `year_ce` column contains:
- Exact integer years (majority of records): `"622"`, `"705"`, etc.
- Century ranges for poorly dated sites: `"300-399"`, `"600-699"`, `"700-799"`, `"800-899"` — 8 records.
- Literal `"unknown"` or `"Unknown"` — 1 record in year_ce (and 54 NAs in year_ah).

There are no explicit uncertainty scores, no earliest/latest fields, and no date confidence indicators in the source. The data dictionary (Phase 0) has already parsed `year_ce` into `year_ce_min` and `year_ce_max` integer bounds.

Three analysis modes are required (design doc §5): deterministic, sensitivity, probabilistic.

## Decision

**Representation**: Store dates as a min/max interval pair (`year_ce_min`, `year_ce_max`). Exact dates have `min == max`. Unknown dates have both NA.

**Deterministic mode** (default): use `year_ce_min` as the point estimate. This is conservative — it places the mosque as early as it could be, which is the harder test for "early Petra orientation" hypotheses.

**Sensitivity mode**: repeat the analysis at `year_ce_min`, `(year_ce_min + year_ce_max) / 2`, and `year_ce_max` for all interval-dated records; report how conclusions change.

**Probabilistic mode**: sample uniformly from [`year_ce_min`, `year_ce_max`] for interval-dated records; sample from a narrow Gaussian (σ = 5 yr by default, analyst-adjustable) centred on the point estimate for nominally exact dates (acknowledging that Gibson's "exact" years are often themselves uncertain).

The `year_ce` character column is retained as-is in `gibson_qibla` so analysts can inspect the original strings. The parsed `year_ce_min` / `year_ce_max` columns are used by all analytical functions.

## Rationale

- A min/max interval representation is lossless: it handles exact dates, ranges, and unknowns without forcing a distribution assumption at the data-model layer.
- Using `year_ce_min` as the deterministic point estimate is documented and auditable; using a midpoint would silently introduce an assumption.
- Gibson's "exact" years for early mosques are often approximate; the probabilistic mode's Gaussian acknowledges this without forcing the analyst to specify a prior for every record.
- Uniform sampling over ranges is the maximum-entropy choice given only a calendar-century bound.

## Consequences

- All filter expressions like `year_ce <= 700` must be applied to `year_ce_max` (strict) or `year_ce_min` (inclusive) depending on context; functions document which bound is used.
- The 8 century-ranged records and 1 unknown-date record must be reported in inclusion/exclusion summaries.
- Analysts who need tighter date bounds must supply them externally; the package does not invent precision.
- The `sample_qibla_dataset()` function handles the probabilistic mode; the Gaussian σ parameter is exposed and documented.
