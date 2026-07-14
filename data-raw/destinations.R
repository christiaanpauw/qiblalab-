## data-raw/destinations.R
## Creates the canonical candidate destination table shipped as data/destinations.rda.
## Coordinates are for the specific reference points stated in `rationale`.
## This is an editable hypothesis table, not a declaration of historical truth.

destinations <- tibble::tibble(
  id        = c("mecca",    "petra",    "jerusalem"),
  name      = c(
    "Mecca (Kaaba)",
    "Petra (city centre)",
    "Jerusalem (Temple Mount)"
  ),
  latitude  = c(21.4225,   30.3285,   31.7781),
  longitude = c(39.8262,   35.4444,   35.2354),
  valid_from = c(NA_integer_, NA_integer_, NA_integer_),
  valid_to   = c(NA_integer_, NA_integer_, NA_integer_),
  rationale = c(
    "The Kaaba, regarded as the canonical qibla in mainstream Islamic tradition. Coordinates: centre of the Kaaba structure.",
    "Proposed by Dan Gibson as the original qibla. Coordinates: city-centre of ancient Nabataean Petra (Wadi Musa area). The precise reference point within Petra is contested.",
    "Proposed as an early qibla by some scholars. Coordinates: Dome of the Rock / Temple Mount platform centre."
  ),
  citation  = c(
    "Kaaba coordinates from multiple geodetic sources; see e.g. King (1993) DOI:10.1163/157005593X00033.",
    "Gibson, D. (2011). Qur'anic Geography. Independent Scholar's Press.",
    "King, D.A. (1993). Astronomy in the Service of Islam. Variorum."
  )
)

save(destinations, file = "data/destinations.rda", compress = "xz")
message("Saved data/destinations.rda (", nrow(destinations), " rows)")
print(destinations)
