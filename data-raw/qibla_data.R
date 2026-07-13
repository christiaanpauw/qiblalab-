## data-raw/qibla_data.R
## Imports Mosques_Jan2021.xlsx and produces data/gibson_qibla.rda
## Source: Gibson, Dan (2021). Early Islamic Qibla Database 2021.
##         figshare. https://doi.org/10.6084/m9.figshare.13570655.v2
## Licence: CC BY 4.0
## Run via: source("data-raw/qibla_data.R") from package root

library(readxl)
library(tools)   # md5sum

# ── Provenance record ────────────────────────────────────────────────────────

SOURCE_FILE  <- "data-raw/Mosques_Jan2021.xlsx"
SOURCE_DOI   <- "10.6084/m9.figshare.13570655.v2"
SOURCE_URL   <- "https://ndownloader.figshare.com/files/26042708"
EXPECTED_MD5 <- "af8a9b535d3930d989b395744a85e4df"

actual_md5 <- as.character(md5sum(SOURCE_FILE))
if (actual_md5 != EXPECTED_MD5) {
  stop(sprintf(
    "MD5 mismatch for %s\n  expected: %s\n  actual:   %s\nRe-run download or pin a new version.",
    SOURCE_FILE, EXPECTED_MD5, actual_md5
  ))
}

provenance <- list(
  source_file     = "Mosques_Jan2021.xlsx",
  doi             = SOURCE_DOI,
  download_url    = SOURCE_URL,
  download_date   = "2026-07-13",
  md5             = EXPECTED_MD5,
  licence         = "CC BY 4.0",
  licence_url     = "https://creativecommons.org/licenses/by/4.0/",
  citation        = paste0(
    "Gibson, Dan (2021). Early Islamic Qibla Database 2021. ",
    "figshare. Dataset. https://doi.org/", SOURCE_DOI
  ),
  processed_by    = paste0("qiblalab data-raw/qibla_data.R"),
  r_version       = paste(R.version$major, R.version$minor, sep = ".")
)

# ── Import raw sheet ─────────────────────────────────────────────────────────

raw <- read_excel(SOURCE_FILE, sheet = "mosques")

# Attach immutable row identifier before any transformation
raw$row_id <- seq_len(nrow(raw))

# ── Audit log ────────────────────────────────────────────────────────────────

audit <- list()

log_action <- function(column, original, corrected, n, reason) {
  audit[[length(audit) + 1]] <<- list(
    column    = column,
    original  = original,
    corrected = corrected,
    n_records = n,
    reason    = reason
  )
}

# ── Cleaning ─────────────────────────────────────────────────────────────────

df <- raw

## 1. Rename columns to snake_case R conventions
old_names <- names(df)
new_names <- c(
  "gibson_classification",
  "year_ce",
  "year_ah",
  "age_group",
  "city",
  "country",
  "mosque_name",
  "rebuilt",
  "latitude",
  "longitude",
  "azimuth",
  "website",
  "row_id"
)
names(df) <- new_names

log_action("(all)", paste(old_names, collapse = ", "),
           paste(new_names, collapse = ", "), nrow(df),
           "Column names converted to snake_case; 'dir' renamed to 'azimuth'")

## 2. Retain source values before corrections
df$gibson_classification_source <- df$gibson_classification
df$country_source                <- df$country

## 3. Standardise Gibson Classification case
n_lower_unknown <- sum(df$gibson_classification == "unknown", na.rm = TRUE)
df$gibson_classification[df$gibson_classification == "unknown"] <- "Unknown"
log_action("gibson_classification", "unknown", "Unknown", n_lower_unknown,
           "Case inconsistency: 'unknown' and 'Unknown' are the same category")

## 4. Fix country name typos
country_fixes <- list(
  list(from = "iran",        to = "Iran",       reason = "Lowercase country name"),
  list(from = "Somolia",     to = "Somalia",    reason = "Spelling error"),
  list(from = "Uzbeckistan", to = "Uzbekistan", reason = "Spelling error"),
  list(from = "Lybia",       to = "Libya",      reason = "Spelling error")
)
for (fix in country_fixes) {
  n <- sum(df$country == fix$from, na.rm = TRUE)
  if (n > 0) {
    df$country[df$country == fix$from] <- fix$to
    log_action("country", fix$from, fix$to, n, fix$reason)
  }
}

## 5. Parse Year CE into numeric min/max bounds
parse_year_ce <- function(x) {
  result <- data.frame(year_ce_min = NA_integer_, year_ce_max = NA_integer_)
  if (is.na(x) || tolower(trimws(x)) == "unknown") return(result)
  x <- trimws(x)
  if (grepl("^\\d{3,4}-\\d{3,4}$", x)) {
    parts <- as.integer(strsplit(x, "-")[[1]])
    result$year_ce_min <- parts[1]
    result$year_ce_max <- parts[2]
  } else if (grepl("^\\d{3,4}$", x)) {
    yr <- as.integer(x)
    result$year_ce_min <- yr
    result$year_ce_max <- yr
  }
  result
}

parsed <- do.call(rbind, lapply(df$year_ce, parse_year_ce))
df$year_ce_min <- parsed$year_ce_min
df$year_ce_max <- parsed$year_ce_max

log_action("year_ce", "character (mixed)", "year_ce_min + year_ce_max (integer)",
           nrow(df),
           paste0("Derived integer bounds from year_ce string. ",
                  "Century ranges (e.g. '700-799') use stated bounds. ",
                  "'unknown'/'Unknown' yields NA. ",
                  "Exact years yield min == max."))

# ── Column order ─────────────────────────────────────────────────────────────

df <- df[, c(
  "row_id",
  "mosque_name",
  "city",
  "country",
  "country_source",
  "latitude",
  "longitude",
  "year_ce",
  "year_ce_min",
  "year_ce_max",
  "year_ah",
  "age_group",
  "gibson_classification",
  "gibson_classification_source",
  "azimuth",
  "rebuilt",
  "website"
)]

# ── Attach metadata ───────────────────────────────────────────────────────────

audit_df <- do.call(rbind, lapply(audit, as.data.frame, stringsAsFactors = FALSE))

attr(df, "provenance")      <- provenance
attr(df, "audit_log")       <- audit_df
attr(df, "missing_fields")  <- c(
  "azimuth_uncertainty",
  "coordinate_uncertainty",
  "date_confidence",
  "measurement_method",
  "orientation_source",
  "faction",
  "dynasty",
  "structure_status",
  "prayer_wall_identification",
  "bibliography",
  "data_quality_flags"
)

# ── Save ─────────────────────────────────────────────────────────────────────

gibson_qibla <- df

save(gibson_qibla, file = "data/gibson_qibla.rda", compress = "xz")

message("Saved data/gibson_qibla.rda  (", nrow(gibson_qibla), " rows x ",
        ncol(gibson_qibla), " cols)")
message("Audit log (", nrow(audit_df), " entries):")
print(audit_df[, c("column", "original", "corrected", "n_records", "reason")])
