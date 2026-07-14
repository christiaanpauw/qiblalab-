## Generates qibla_scenarios: 10 synthetic datasets demonstrating patterns
## that qiblalab methods should detect or reject (design doc §19).
## Run interactively: source("data-raw/synthetic.R")

devtools::load_all()
set.seed(20240101)

# Destination coordinates
MECCA     <- list(lat = 21.4225, lon = 39.8262)
PETRA     <- list(lat = 30.3285, lon = 35.4444)

# ── Helpers ───────────────────────────────────────────────────────────────────

pos_grid <- function(n, lat_lo, lat_hi, lon_lo, lon_hi) {
  tibble::tibble(
    latitude  = runif(n, lat_lo, lat_hi),
    longitude = runif(n, lon_lo, lon_hi)
  )
}

bear <- function(df, dest) {
  bearing_haversine(df$latitude, df$longitude, dest$lat, dest$lon)
}

noisy <- function(b, sigma = 5) (b + rnorm(length(b), 0, sigma)) %% 360

ids <- function(prefix, n) sprintf("%s%02d", prefix, seq_len(n))

# Near East spread: Iraq, Levant, Arabia, Persia
near_east <- function(n) pos_grid(n, 25, 42, 30, 55)

# ── Scenario 1: Single true destination with measurement noise ────────────────
s1 <- near_east(40)
s1$mosque_id   <- ids("s01_", 40)
s1$year_ce_min <- sample(620L:800L, 40, replace = TRUE)
s1$year_ce_max <- s1$year_ce_min + sample(0L:20L, 40, replace = TRUE)
s1$azimuth     <- noisy(bear(s1, MECCA), sigma = 5)

# ── Scenario 2: Two competing destination traditions ──────────────────────────
s2a <- pos_grid(20, 28, 38, 33, 48)   # tradition A → Mecca
s2b <- pos_grid(20, 28, 38, 33, 48)   # tradition B → Petra
s2  <- rbind(s2a, s2b)
s2$mosque_id   <- ids("s02_", 40)
s2$year_ce_min <- sample(630L:780L, 40, replace = TRUE)
s2$year_ce_max <- s2$year_ce_min + sample(0L:30L, 40, replace = TRUE)
s2$tradition   <- rep(c("mecca", "petra"), each = 20)
s2$azimuth     <- c(noisy(bear(s2a, MECCA), 5), noisy(bear(s2b, PETRA), 5))

# ── Scenario 3: Chronological transition (Petra → Mecca) ─────────────────────
s3 <- near_east(40)
s3$mosque_id   <- ids("s03_", 40)
# First 20: early (pre-650 CE) → Petra; last 20: late (post-690 CE) → Mecca
s3$year_ce_min <- c(sample(580L:630L, 20), sample(690L:760L, 20))
s3$year_ce_max <- s3$year_ce_min + sample(0L:25L, 40, replace = TRUE)
s3$azimuth     <- c(noisy(bear(s3[1:20, ], PETRA), 6),
                    noisy(bear(s3[21:40, ], MECCA), 6))

# ── Scenario 4: Faction-specific intermediate tradition ───────────────────────
s4 <- near_east(60)
s4$mosque_id   <- ids("s04_", 60)
s4$year_ce_min <- sample(630L:730L, 60, replace = TRUE)
s4$year_ce_max <- s4$year_ce_min + sample(0L:20L, 60, replace = TRUE)
s4$faction     <- rep(c("A", "B", "C"), each = 20)

b_mecca  <- bear(s4[1:20, ], MECCA)
b_petra  <- bear(s4[21:40, ], PETRA)
# Faction C: circular midpoint between Mecca and Petra directions
b_mid    <- (Arg(exp(1i * b_mecca[1:20] * pi / 180) +
                   exp(1i * b_petra * pi / 180)) * 180 / pi + 360) %% 360

s4$azimuth <- c(noisy(b_mecca, 5),
                noisy(b_petra,  5),
                noisy(b_mid,   5))

# ── Scenario 5: Region-specific cardinal orientations ─────────────────────────
# Group X orients due south (cardinal); group Y follows Mecca
s5_x <- pos_grid(15, 28, 36, 30, 40)  # Levant/Arabia: cardinal south
s5_y <- pos_grid(15, 28, 36, 30, 40)  # same region: Mecca
s5   <- rbind(s5_x, s5_y)
s5$mosque_id   <- ids("s05_", 30)
s5$year_ce_min <- sample(640L:760L, 30, replace = TRUE)
s5$year_ce_max <- s5$year_ce_min + sample(0L:20L, 30, replace = TRUE)
s5$orientation <- rep(c("cardinal", "mecca"), each = 15)
s5$azimuth     <- c(noisy(rep(180, 15), 4),
                    noisy(bear(s5_y, MECCA), 5))

# ── Scenario 6: Random orientations (null model) ──────────────────────────────
s6 <- near_east(40)
s6$mosque_id   <- ids("s06_", 40)
s6$year_ce_min <- sample(600L:800L, 40, replace = TRUE)
s6$year_ce_max <- s6$year_ce_min + sample(0L:40L, 40, replace = TRUE)
s6$azimuth     <- runif(40, 0, 360)

# ── Scenario 7: False cluster caused by geography ─────────────────────────────
# Both groups point to Mecca; geographic separation produces distinct directions
s7_levant <- pos_grid(20, 30, 36, 33, 39)   # Levant → bearing to Mecca ~140-165
s7_persia <- pos_grid(20, 30, 36, 50, 60)   # Persia → bearing to Mecca ~220-250
s7   <- rbind(s7_levant, s7_persia)
s7$mosque_id   <- ids("s07_", 40)
s7$year_ce_min <- sample(640L:760L, 40, replace = TRUE)
s7$year_ce_max <- s7$year_ce_min + sample(0L:20L, 40, replace = TRUE)
s7$region      <- rep(c("levant", "persia"), each = 20)
s7$azimuth     <- noisy(bear(s7, MECCA), 5)  # both point to Mecca

# ── Scenario 8: False high purity from over-clustering ────────────────────────
# One true cluster (all ~SE toward Mecca from the same region), over-clustering creates k>1
s8 <- pos_grid(40, 30, 34, 35, 40)  # tight geographic area
s8$mosque_id   <- ids("s08_", 40)
s8$year_ce_min <- sample(650L:720L, 40, replace = TRUE)
s8$year_ce_max <- s8$year_ce_min + sample(0L:15L, 40, replace = TRUE)
s8$azimuth     <- noisy(bear(s8, MECCA), 3)  # very tight: sigma=3

# ── Scenario 9: Uncertain dates spanning a change point ──────────────────────
# Wide date ranges (±50-100 yr) cross the ~650 CE Petra→Mecca transition.
# Some mosques could be pre- or post-transition; orientation cannot disambiguate.
s9_petra <- near_east(15)
s9_mecca <- near_east(15)
s9   <- rbind(s9_petra, s9_mecca)
s9$mosque_id   <- ids("s09_", 30)
# Wide ranges crossing 650 CE
s9$year_ce_min <- c(sample(580L:640L, 15), sample(610L:670L, 15))
s9$year_ce_max <- s9$year_ce_min + sample(60L:120L, 30, replace = TRUE)
s9$azimuth     <- c(noisy(bear(s9_petra, PETRA), 7),
                    noisy(bear(s9_mecca, MECCA), 7))

# ── Scenario 10: Rebuilt mosques with biased orientations ─────────────────────
# Original mosques → Petra; rebuilt later → Mecca; original=FALSE for rebuilt
s10 <- near_east(30)
s10$mosque_id   <- ids("s10_", 30)
s10$year_ce_min <- c(sample(580L:640L, 15), sample(700L:760L, 15))
s10$year_ce_max <- s10$year_ce_min + sample(0L:30L, 30, replace = TRUE)
s10$rebuilt     <- c(rep(FALSE, 15), rep(TRUE, 15))
s10$azimuth     <- c(noisy(bear(s10[1:15, ], PETRA), 5),
                     noisy(bear(s10[16:30, ], MECCA), 5))

# ── Assemble list ─────────────────────────────────────────────────────────────
qibla_scenarios <- list(
  mecca_tradition       = tibble::as_tibble(s1),
  two_traditions        = tibble::as_tibble(s2),
  chronological         = tibble::as_tibble(s3),
  intermediate          = tibble::as_tibble(s4),
  cardinal              = tibble::as_tibble(s5),
  random_orientations   = tibble::as_tibble(s6),
  geographic_cluster    = tibble::as_tibble(s7),
  over_clustered        = tibble::as_tibble(s8),
  uncertain_dates       = tibble::as_tibble(s9),
  rebuilt_mosques       = tibble::as_tibble(s10)
)

usethis::use_data(qibla_scenarios, overwrite = TRUE)
