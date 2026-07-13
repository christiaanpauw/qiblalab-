# ADR-05: Treatment of rebuilt mosques and multiple construction phases

**Status**: Accepted
**Date**: 2026-07-13

## Context

Gibson's `rebuilt` column is free-text and contains values such as `"1987 CE"`, `"707 & 1951 CE"`, `"416 & 1214 AH"`, `"9th Century"`, `"never"`, and NA (5 records of unknown status). The column records rebuild dates but gives no information about whether the orientation was changed during rebuilding.

A rebuilt mosque poses an interpretive problem: the measured azimuth may reflect the rebuild, not the original construction. This is particularly acute for sites rebuilt in the 20th century, which post-date the analytical period entirely.

Relevant facts from the data:
- At least 3 mosques were rebuilt in the 20th century CE (e.g. Prophets Mosque rebuilt 1951 CE).
- Several early Umayyad mosques were rebuilt in the Abbasid period.
- The `rebuilt` column is not machine-parseable without manual review (mixed calendars, free text, ranges).

## Decision

1. **Include rebuilt mosques by default**. Excluding them silently would reduce the sample without the analyst's knowledge.

2. **Provide a `rebuilt` filter argument** on all major analysis functions, with three options:
   - `"all"` (default): include all records regardless of rebuild status.
   - `"never"`: restrict to records where `rebuilt == "never"`.
   - `"original_only"`: reserved for future use when a parsed, machine-readable rebuild field is available.

3. **Do not parse the `rebuilt` column automatically**. The free-text format requires manual review to correctly interpret calendar systems and multiple rebuild events. The raw string is retained in `gibson_qibla$rebuilt`; a parsed version can be added later via a community-reviewed data patch.

4. **Emit a research safeguard warning** whenever rebuilt mosques are included in an analysis with a small sample or where a single mosque is influential. Specifically, warn when any mosque with a non-"never" `rebuilt` value contributes more than 20% of the weight in a result.

5. **The provenance report** must list rebuilt mosques included in any analysis.

## Rationale

- Silent exclusion would give the appearance of clean data while hiding a methodological assumption.
- Forced exclusion would eliminate sites that may have retained their original orientation despite structural rebuilding (e.g. many mosques are rebuilt around a preserved mihrab niche).
- The analyst is best placed to decide whether a specific rebuild is orientation-changing; the package's job is to surface the information and make the choice explicit.
- The `rebuilt` column cannot be parsed reliably without domain knowledge; attempting it would introduce unaudited assumptions.

## Consequences

- The `rebuilt` field in `gibson_qibla` remains a raw character string.
- A structured `rebuilt_parsed` field is out of scope until manual review is complete.
- All analysis outputs must include a count of rebuilt sites in the sample.
- Future work: create `data-raw/rebuilt_review.csv` with a manually verified parsed version of the `rebuilt` column.
