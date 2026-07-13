# ADR-02: Default geodesic model

**Status**: Accepted
**Date**: 2026-07-13

## Context

To compute the theoretical bearing from a mosque to a candidate destination, the package needs an Earth model and a path definition. The main options are:

| Model | Description | Max error vs. WGS84 ellipsoid |
|---|---|---|
| Great-circle on a sphere (Haversine) | Shortest path on a sphere of mean Earth radius | < 0.5° for paths < 10 000 km |
| Great-circle on WGS84 ellipsoid (Vincenty / geodesic) | Shortest path on the reference ellipsoid | 0° (reference) |
| Rhumb line | Constant-compass-bearing path; longer than great-circle | Large for long N–S paths |
| Cardinal / astronomical | Historical orientation by sun/stars, not geometry | Not a geodesic |

The mosque-to-destination distances in this dataset are typically 500–3 000 km. Over that range, the spherical Haversine bearing differs from the WGS84 ellipsoidal bearing by at most ~0.1°–0.3°, well within the measurement noise of the azimuth data (which has no stated precision and is recorded to 2 decimal places at best).

However, the design doc requires the bearing model to be *replaceable*, since historical builders did not use modern geodesy.

## Decision

The **default geodesic model is the great-circle initial bearing computed on a sphere** using the Haversine formula, with Earth radius R = 6 371 km (IAU mean).

The bearing-model argument is a function, not an enum, so analysts can supply any alternative:

```r
bearing_to_destination(mosque, destination, model = bearing_haversine)
bearing_to_destination(mosque, destination, model = bearing_vincenty)
bearing_to_destination(mosque, destination, model = bearing_rhumb)
bearing_to_destination(mosque, destination, model = my_custom_model)
```

The package ships `bearing_haversine()` (default), `bearing_vincenty()` (WGS84 ellipsoid via `geosphere`), and `bearing_rhumb()`. All state their model in the return value's attributes.

## Rationale

- The spherical model is simple, self-contained, and introduces negligible error relative to measurement noise in this dataset.
- Shipping Vincenty as an alternative avoids adding `geosphere` as a hard dependency for the most common use case.
- A function-valued model argument is more extensible than an enum: it allows astronomical orientation models and historically motivated alternatives without modifying package internals.
- Rhumb-line bearings are included because some scholars argue early builders used constant-compass bearings; this should be testable, not assumed.

## Consequences

- All bearing functions carry a `model` argument defaulting to `bearing_haversine`.
- Result objects carry a `geodesic_model` attribute identifying which model was used.
- Reports and vignettes must state the model used.
- The `geosphere` package moves to `Suggests`, not `Imports`.
