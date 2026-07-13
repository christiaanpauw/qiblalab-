# Data dictionary — Early Islamic Qibla Database

**Source**: Dan Gibson, *Early Islamic Qibla Database 2021*
**Figshare DOI**: 10.6084/m9.figshare.13570655.v2
**File**: Mosques_Jan2021.xlsx (sheet: "mosques")
**Downloaded**: 2026-07-13
**MD5**: af8a9b535d3930d989b395744a85e4df
**Licence**: CC BY 4.0
**Records**: 160 rows × 12 source columns

---

## Column inventory

| R name | Original name | Type | NAs | Notes |
|---|---|---|---|---|
| `row_id` | — | integer | 0 | Added by qiblalab; 1-based row index into original spreadsheet; immutable |
| `gibson_classification` | `Gibson Classification` | character | 0 | Gibson's destination hypothesis for each mosque; see value table below |
| `year_ce` | `Year CE` | character | 0 | Construction date in CE; stored as string because some values are ranges or "Unknown"; see parsing notes |
| `year_ah` | `Year AH` | numeric | 54 | Construction date in AH; 54 of 160 records missing |
| `age_group` | `age_group` | character | 0 | Gibson's historical period label; see value table below |
| `city` | `City` | character | 0 | City or site name |
| `country` | `Country` | character | 0 | Country name; several typos in source (see cleaning log) |
| `mosque_name` | `Mosque Name` | character | 0 | Primary name of the mosque or prayer site |
| `rebuilt` | `Rebuilt` | character | 5 | Free-text rebuild date(s); mixed formats — see parsing notes |
| `latitude` | `Latitude` | numeric | 0 | Decimal degrees, WGS84 assumed |
| `longitude` | `Longitude` | numeric | 0 | Decimal degrees, WGS84 assumed |
| `azimuth` | `dir` | numeric | 28 | Measured building or qibla azimuth in degrees clockwise from north; 28 sites have no measured orientation |
| `website` | `Website Link` | character | 0 | URL to Gibson's nabataea.net site entry |

---

## Gibson Classification values

| Value (source) | Count | Meaning |
|---|---|---|
| `Petra` | 37 | Gibson classifies the orientation as pointing toward Petra |
| `Between` | 38 | Orientation falls between bearings to Petra and Mecca |
| `Mecca` | 16 | Orientation consistent with modern Mecca bearing |
| `Parallel` | 27 | Mosque shares a common regional orientation convention (mainly North Africa); not attributed to a destination bearing |
| `Jerusalem` | 3 | Orientation toward Jerusalem |
| `Unknown` | 38 | No measured azimuth, or Gibson did not classify |
| `unknown` | 1 | Identical to `Unknown`; case inconsistency in source |

**Note**: These are Gibson's hypotheses, not ground truth. The package treats all classifications as one analyst's interpretation and never uses them as labels when fitting unsupervised models.

---

## age_group values

Gibson's historical-period labels. Categories are not mutually exclusive and some are compound.

| Value | Count |
|---|---|
| `Umayyad` | 70 |
| `Abbasid / Umayyad` | 25 |
| `Abbasid` | 23 |
| `Umayyad to 2nd Fitna` | 15 |
| `Muhammad` | 7 |
| `2nd Fitna` | 4 |
| `Umar` | 4 |
| `Umayyad after 2nd Fitna` | 4 | (Note: likely "Umayyad after 2nd Fitna" — check Gibson's intention) |
| `Uthman` | 2 |
| `unknown` | 2 |
| `Safavis` | 1 |
| `Timurid` | 1 |
| `Abu Bakr` | 1 |

---

## Parsing notes and data quality issues

### Year CE (critical)
Stored as character because some values are non-numeric:
- Plain integer years (majority): `"622"`, `"705"`, etc.
- Century ranges: `"300-399"`, `"600-699"`, `"700-799"`, `"800-899"` — 8 records
- Literal string `"unknown"` — 1 record; `"Unknown"` — 0 (only in year_ce column)
- All 160 values are populated; missing date information appears as the range values or "Unknown"

When parsing: extract `year_ce_min` and `year_ce_max` as integers. For ranges like `"700-799"` use the explicit bounds. For plain integers, min = max = value. For "unknown", set both to NA.

### Rebuilt
Free-text, mixed calendar systems, mixed formats:
- CE year: `"1987 CE"`, `"749 CE"`, `"714 CE"`
- AH year: `"435 AH"`, `"416 & 1214 AH"`
- Plain year (ambiguous calendar): `"1695"`, `"1251"`, `"1323"`
- Range / multiple rebuilds: `"707 & 1951 CE"`, `"416 & 1214 AH"`
- Century: `"9th Century"`
- Explicit no rebuild: `"never"` (most common value)
- NA (5 records): meaning unclear — possibly never rebuilt or data missing

Do not parse this column automatically. Document it as requiring manual review for analytical use.

### Country — typos in source
| Source value | Corrected value | Count |
|---|---|---|
| `iran` | `Iran` | 1 |
| `Somolia` | `Somalia` | 2 |
| `Uzbeckistan` | `Uzbekistan` | 1 |
| `Lybia` | `Libya` | 1 |
| `Israel/Occupied` | retained as-is | 4 |

All corrections are logged in the audit log and the source value is retained in `country_source`.

### Gibson Classification — case inconsistency
`"unknown"` (1 record) and `"Unknown"` (38 records) are the same category. Standardised to `"Unknown"` in the cleaned dataset. Original value retained in `gibson_classification_source`.

### Missing azimuths (dir / azimuth)
28 records have no measured azimuth. All 28 are classified `"Unknown"` by Gibson. These sites exist in the database for historical completeness but cannot be used in any orientation analysis.

### Coordinate reference system
No CRS is stated in the source. WGS84 decimal degrees is assumed from the coordinate ranges and context.

### No explicit uncertainty fields
The source contains no azimuth uncertainty, coordinate uncertainty, or date confidence scores. The `age_group` column provides rough period groupings. For uncertainty-aware analyses, `year_ce_min`/`year_ce_max` from the parsed Year CE column are the only machine-readable bounds.

---

## Fields absent from the source (design doc requirements not met by Gibson's data)

These fields from the data model specification (§3) have no equivalent in the Figshare dataset and must be added by the researcher if needed:

- Azimuth uncertainty (measurement error)
- Coordinate uncertainty
- Date quality / confidence score
- Measurement method
- Orientation source / archaeological reference
- Factional / movement assignment (beyond `age_group`)
- Dynasty (partially covered by `age_group`)
- Whether structure is original, reconstructed, restored, or uncertain
- Identification of the prayer wall
- Source references / bibliography per mosque
- Data-quality flags per field

These gaps should be documented in the provenance report and flagged via `attr(gibson_qibla, "missing_fields")`.
