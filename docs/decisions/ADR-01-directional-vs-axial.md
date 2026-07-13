# ADR-01: Directional versus axial interpretation of mosque orientations

**Status**: Accepted
**Date**: 2026-07-13

## Context

Mosque orientations can be represented in two ways:

- **Directional** (0°–360°): the bearing the mihrab *faces*, i.e. the direction worshippers look. A mosque facing northeast (45°) is distinct from one facing southwest (225°).
- **Axial** (0°–180°): the orientation of the qibla wall as an undirected line, without distinguishing which end is "forward". Treating northeast and southwest as equivalent.

Gibson's `dir` column contains values ranging 20°–356° across the dataset. The column header and dataset description do not explicitly state whether it records the facing direction or the wall axis. However:

1. Three of the first ten mosques have no azimuth (NA), all early Medinan sites where the qibla is historically documented to have changed — consistent with recording a directional facing, not just a wall line.
2. The "Mosque of the Two Qiblas" (row 3) has NA azimuth, which makes sense if no single direction can be assigned.
3. All non-NA values span the full 0°–360° range, not a compressed 0°–180° axial range.
4. The design doc explicitly states the package must "distinguish a building's forward axis from an undirected architectural line" and "allow the analyst to specify whether an orientation is directional over 360° or axial over 180°."

## Decision

Treat Gibson's `dir` / `azimuth` column as **directional (0°–360°)** by default — the direction the mihrab faces, i.e. the bearing worshippers face during prayer.

All core functions accept an `axial = FALSE` argument. When `axial = TRUE`, the package doubles angles before computing circular statistics (the standard transformation for axial data) and halves results back before returning. Functions document which convention is in use.

Do not silently reverse any azimuth value.

## Rationale

- The facing direction is the analytically meaningful quantity for qibla hypothesis testing: we want to know *which direction* the builders intended, not just the orientation of a wall.
- The 0°–360° range in the source data is consistent with directional recording.
- Axial treatment would collapse Petra-facing and Mecca-facing mosques that happen to lie on the same geographic axis, potentially hiding the very distinctions the package is designed to test.
- Offering `axial = TRUE` as an explicit option preserves the analyst's ability to apply the transformation where justified.

## Consequences

- All circular statistics functions must handle the full 0°–360° range.
- Wrap-around at 0°/360° must be handled everywhere (e.g. a mean of 359° and 1° is 0°, not 180°).
- Analyses using axial statistics must opt in explicitly and document the choice.
- Any future dataset with genuinely axial recordings must be flagged at import time.
