# =====================================================================
# North-South divide in English mortality by cause of death, 1981-2025
# Script 1 of 4: cause decomposition, with London separated from the rest of the South
#
# Iain Buchan, 15 Nov 2026. Exploratory work in progress; not peer reviewed.
#
# Data (in ./data)
#   deathsnorthsouthlondon1981to2025.xlsx
#       ONS user-requested extract, November 2026: deaths registered 1981-2025 by underlying cause
#       (obesity: any mention), region group (North, South, London), sex and five-year age group.
#       In this file 'South' means the South EXCLUDING London.
#   nomis_pop_nsl_1981_2025.csv
#       ONS mid-year population estimates (NOMIS table NM_31_1, downloaded 14 Nov 2026), regions
#       summed to North, South (excluding London) and London, by sex and age code 1-19.
#
# Areas        North; South (= rest of South + London, the definition used in the Lancet Public
#              Health papers); SouthXL (South excluding London); London.
# Comparisons  N_vs_S, N_vs_SXL, N_vs_L, L_vs_SXL - the first-named area is 'A', the second 'B'.
# Causes       All causes, Cardiovascular, Diabetes, Accidents, Suicide, Alcohol (alcohol-related)
#              and Other causes (= all causes minus those five), plus two memo items outside that
#              sum: Drug (drug poisoning, supplied from 1993) and Obesity (any mention). The ICD
#              codes are in the workbook's 'Definition' sheet. These categories overlap (see the
#              README); script 2 uses groups that do not.
#
# Method notes
#   * Rates are directly standardised to the 2013 European Standard Population over 19 age groups;
#     a band rate uses the weights of the age groups in the band. 'P' (persons) pools the sexes
#     within each age group.
#   * Gap = DSR(A) - DSR(B); ratio = DSR(A) / DSR(B).
#   * Rate ratio (irr): for each year and age band, a Poisson model of deaths on area A, age group
#     and, for persons, sex, with an offset of log(population) - the within-year model of the
#     paper's script. Wald 95% interval. Fitted with glm.fit() and then iterated three times more,
#     so that the limits are exact to rounding in strata with few deaths; section 4 checks one cell
#     against glm(). irr_status says why a ratio was not estimated.
#   * Excess deaths = observed deaths in A minus the deaths expected at B's age- and sex-specific
#     rates in the same year. A period figure is the mean of the annual figures. Intervals are
#     95%, from Poisson variances with populations taken as fixed, so they are too narrow.
#   * Period rates pool deaths and populations over the years of the period within each age group.
#
# Outputs (written to ./output; counts as whole numbers, everything else to six decimal places)
#   cell_level_deaths_pop.csv                    deaths and population for every cell, as merged here
#   dsr_by_cause_area_sex_band_year.csv          annual standardised rates
#   gaps_by_comparison_cause_sex_band_year.csv   annual gaps, ratios, rate ratios and excess deaths
#   decomposition_by_period.csv                  pooled periods: gap by cause and share of the gap
#
# Needs: readxl, dplyr, tidyr, readr (installed automatically if absent). About a minute.
# =====================================================================

pkgs <- c("readxl", "dplyr", "tidyr", "readr")
new  <- pkgs[!pkgs %in% rownames(installed.packages())]
if (length(new)) install.packages(new)
invisible(lapply(pkgs, library, character.only = TRUE))
if (packageVersion("dplyr") < "1.1.0") stop("dplyr 1.1.0 or later is needed - please update it.")

# ---- configuration ---------------------------------------------------
deaths_file <- file.path("data", "deathsnorthsouthlondon1981to2025.xlsx")
pop_file    <- file.path("data", "nomis_pop_nsl_1981_2025.csv")
outdir      <- "output"

for (f in c(deaths_file, pop_file))
  if (!file.exists(f)) stop("Cannot find '", f, "' from ", getwd(), " - setwd() to the '2026 Causes' folder.")
dir.create(outdir, showWarnings = FALSE)

years  <- 1981:2025
ages   <- c("<1","01-04","05-09","10-14","15-19","20-24","25-29","30-34","35-39","40-44","45-49",
            "50-54","55-59","60-64","65-69","70-74","75-79","80-84","85+")
age_lo <- c(0, 1, seq(5, 85, by = 5))
# 2013 European Standard Population, 19 age groups (90+ and 85-89 combined into 85+)
esp    <- c(1000, 4000, 5500, 5500, 5500, 6000, 6000, 6500, 7000, 7000, 7000, 7000, 6500, 6000,
            5500, 5000, 4000, 2500, 2500)
band4  <- ifelse(age_lo < 25, "0-24", ifelse(age_lo < 50, "25-49", ifelse(age_lo < 75, "50-74", "75+")))
bands  <- list("0-24" = ages[band4 == "0-24"], "25-49" = ages[band4 == "25-49"],
               "50-74" = ages[band4 == "50-74"], "75+" = ages[band4 == "75+"],
               "All ages" = ages, "<75" = ages[age_lo < 75])
cause_map <- c("All causes" = "All causes", "Cardiovascular" = "Cardiovascular", "Diabetes" = "Diabetes",
               "Accidents" = "Accidents", "Suicides" = "Suicide", "Alcohol" = "Alcohol", "Drugs" = "Drug",
               "Obesity" = "Obesity")
causes <- c("All causes", "Cardiovascular", "Diabetes", "Accidents", "Suicide", "Alcohol", "Other causes",
            "Drug", "Obesity")
area_parts  <- list(North = "North", SouthXL = "South", London = "London", South = c("South", "London"))
comparisons <- list(N_vs_S = c("North", "South"), N_vs_SXL = c("North", "SouthXL"),
                    N_vs_L = c("North", "London"), L_vs_SXL = c("London", "SouthXL"))
periods <- list("1981-85" = c(1981, 1985), "1991-95" = c(1991, 1995), "2001-05" = c(2001, 2005),
                "2010-14" = c(2010, 2014), "2015-19" = c(2015, 2019), "2020-24" = c(2020, 2024),
                "2021-25" = c(2021, 2025), "2024" = c(2024, 2024), "2025" = c(2025, 2025))
keys <- c("cause", "area", "sex", "age", "year")

# counts are written as whole numbers and everything else with six decimal places
write_out <- function(df, name, counts = character()) {
  f0 <- function(x) { stopifnot(all(is.na(x) | x == round(x))); ifelse(is.na(x), NA_character_, sprintf("%.0f", x)) }
  f6 <- function(x) ifelse(is.na(x), NA_character_, sub("^-(0\\.0+)$", "\\1", sprintf("%.6f", x)))
  df <- df %>% mutate(across(all_of(counts), f0)) %>% mutate(across(where(is.double), f6))
  write_csv(df, file.path(outdir, name), na = "")
}

# ---- 1. deaths and populations ------------------------------------------
raw <- read_excel(deaths_file, sheet = "Data") %>%
  pivot_longer(all_of(as.character(years)), names_to = "year", values_to = "deaths") %>%
  transmute(cause = unname(cause_map[Cause]), region = RegionGroup,
            sex = unname(c(Male = "M", Female = "F")[Sex]), age = Agegroup,
            year = as.integer(year), deaths = as.numeric(deaths))
stopifnot(nrow(raw) == 894 * 45, !anyNA(raw), all(raw$age %in% ages),
          !any(duplicated(raw[, c("cause", "region", "sex", "age", "year")])))

deaths <- bind_rows(lapply(names(area_parts), function(a)
  raw %>% filter(region %in% area_parts[[a]]) %>%
    group_by(cause, sex, age, year) %>% summarise(deaths = sum(deaths), .groups = "drop") %>%
    mutate(area = a)))
d <- expand_grid(cause = unname(cause_map), area = names(area_parts), sex = c("M", "F"),
                 age = ages, year = years) %>%
  left_join(deaths, by = keys)
# The only cells absent from the file are suicides below age 10 (under 1, 1-4 and 5-9), which ONS does not produce: set to zero.
absent <- filter(d, is.na(deaths))
stopifnot(all(absent$cause == "Suicide"), setequal(unique(absent$age), ages[1:3]),
          nrow(absent) == length(area_parts) * 2 * 3 * length(years))
d$deaths[d$cause == "Suicide" & d$age %in% ages[1:3]] <- 0
# Drug poisoning was not supplied before 1993: the file holds zeros for 1981-1992, set to missing.
stopifnot(all(d$deaths[d$cause == "Drug" & d$year < 1993] == 0))
d$deaths[d$cause == "Drug" & d$year < 1993] <- NA
stopifnot(!anyNA(d$deaths[d$cause != "Drug"]))
other <- d %>%
  filter(cause %in% c("All causes", "Cardiovascular", "Diabetes", "Accidents", "Suicide", "Alcohol")) %>%
  group_by(area, sex, age, year) %>%
  summarise(deaths = deaths[cause == "All causes"] - sum(deaths[cause != "All causes"]), .groups = "drop") %>%
  mutate(cause = "Other causes")
stopifnot(all(other$deaths >= 0))
d <- bind_rows(d, other)

pop <- read_csv(pop_file, show_col_types = FALSE) %>%
  pivot_longer(all_of(as.character(years)), names_to = "year", values_to = "pop") %>%
  transmute(area = ifelse(area == "South", "SouthXL", area), sex, age = ages[age],
            year = as.integer(year), pop = as.numeric(pop))
pop <- bind_rows(pop, pop %>% filter(area %in% c("SouthXL", "London")) %>%
                   group_by(sex, age, year) %>% summarise(pop = sum(pop), .groups = "drop") %>%
                   mutate(area = "South"))
stopifnot(!any(duplicated(pop[, c("area", "sex", "age", "year")])))
d <- d %>% left_join(pop, by = c("area", "sex", "age", "year")) %>%
  mutate(w = esp[match(age, ages)], band = band4[match(age, ages)]) %>%
  select(cause, area, sex, age, year, deaths, pop, w, band)
stopifnot(!anyNA(d$pop), nrow(d) == 9 * 4 * 2 * 19 * 45)
write_out(d, "cell_level_deaths_pop.csv", counts = c("deaths", "pop", "w"))

# ---- 2. directly standardised rates -----------------------------------------
std_rates <- function(df, by) {
  df %>% group_by(across(all_of(by))) %>%
    summarise(dsr = sum(w * deaths / pop) / sum(w) * 1e5,
              se  = sqrt(sum((w / sum(w))^2 * deaths / pop^2)) * 1e5,
              deaths = sum(deaths), pop = sum(pop), .groups = "drop")
}
band_rates <- function(df)
  bind_rows(lapply(names(bands), function(b)
    std_rates(filter(df, age %in% bands[[b]]), c("cause", "area", "sex", "year")) %>% mutate(band = b)))

# persons: sexes pooled within age group
dp <- d %>% group_by(cause, area, age, year, band, w) %>%
  summarise(deaths = sum(deaths), pop = sum(pop), .groups = "drop") %>% mutate(sex = "P")
rates <- bind_rows(band_rates(d), band_rates(dp)) %>%
  mutate(crude = deaths / pop * 1e5) %>%
  select(cause, area, sex, year, deaths, pop, dsr, se, band, crude)
stopifnot(nrow(rates) == 9 * 4 * 3 * 45 * 6)
write_out(rates, "dsr_by_cause_area_sex_band_year.csv", counts = c("deaths", "pop"))

# ---- 3. gaps, rate ratios and excess deaths, by comparison ----------------------
status_text <- c("estimated", "not estimated: fewer than 20 deaths", "not estimated: no deaths in one area",
                 "not estimated: model failed", "not estimated: model did not converge",
                 "not estimated: coefficient not finite")

# design matrix for cells ordered with age fastest, then sex, then area (A first)
design <- function(nage, nsex) {
  g <- expand.grid(age = seq_len(nage), sex = seq_len(nsex), area = 1:2)
  X <- cbind(const = 1, sapply(seq_len(nage)[-1], function(k) as.numeric(g$age == k)))
  if (nsex == 2) X <- cbind(X, female = as.numeric(g$sex == 2))
  cbind(X, exposed = as.numeric(g$area == 1))
}

# Poisson rate ratio for area A with a Wald 95% interval; returns c(irr, lo, hi, status code).
# glm.fit() stops when the deviance has settled. Its standard error comes from the weights of the iteration before
# that, which in a small stratum is good to only four or five figures. Three further iterations of the same
# algorithm from the converged fit make the estimate and its interval exact to rounding.
poisson_irr <- function(y, popn, X) {
  ok <- !is.na(y) & popn > 0
  y <- y[ok]; off <- log(popn[ok]); X <- X[ok, , drop = FALSE]
  k <- ncol(X)                                              # 'exposed' is the last column
  if (sum(y) < 20) return(c(NA, NA, NA, 2))
  if (min(sum(y[X[, k] == 1]), sum(y[X[, k] == 0])) == 0) return(c(NA, NA, NA, 3))
  f <- tryCatch(suppressWarnings(glm.fit(X, y, offset = off, family = poisson(),
                                         control = glm.control(maxit = 100))),
                error = function(e) NULL)
  if (is.null(f)) return(c(NA, NA, NA, 4))
  if (!f$converged) return(c(NA, NA, NA, 5))
  b <- f$coefficients
  if (f$rank < k || !all(is.finite(b))) return(c(NA, NA, NA, 6))
  for (i in 1:3) {
    eta <- drop(X %*% b); mu <- exp(eta + off)
    w   <- lm.wfit(X, eta + (y - mu) / mu, mu)              # one iteratively reweighted least squares step
    b   <- w$coefficients
  }
  if (w$rank < k || !is.finite(b[k])) return(c(NA, NA, NA, 6))
  se <- sqrt(chol2inv(w$qr$qr[seq_len(k), seq_len(k), drop = FALSE])[k, k])
  if (!is.finite(se)) return(c(NA, NA, NA, 6))
  c(exp(b[k]), exp(b[k] - 1.96 * se), exp(b[k] + 1.96 * se), 1)
}

sex_sets  <- list(M = 1L, F = 2L, P = 1:2)                    # index into c("M", "F")
band_idx  <- lapply(bands, match, table = ages)
cube <- function(df, value) {                                 # [age, sex, year, cause] array for one area
  a <- array(NA_real_, dim = c(19, 2, length(years), length(causes)),
             dimnames = list(ages, c("M", "F"), years, causes))
  a[cbind(match(df$age, ages), match(df$sex, c("M", "F")), match(df$year, years), match(df$cause, causes))] <- df[[value]]
  a
}
area_cubes <- lapply(names(area_parts), function(a) {
  x <- filter(d, area == a)
  list(deaths = cube(x, "deaths"), pop = cube(x, "pop"))
})
names(area_cubes) <- names(area_parts)

gap_frames <- list()
for (cname in names(comparisons)) {
  a1 <- comparisons[[cname]][1]; a2 <- comparisons[[cname]][2]
  A <- area_cubes[[a1]]; B <- area_cubes[[a2]]
  rows <- vector("list", length(causes) * length(years) * 3 * length(bands)); n <- 0
  for (ci in seq_along(causes)) for (yi in seq_along(years)) {
    dA <- A$deaths[, , yi, ci]; dB <- B$deaths[, , yi, ci]
    pA <- A$pop[, , yi, ci];    pB <- B$pop[, , yi, ci]
    if (anyNA(dA) || anyNA(dB)) next                          # drug poisoning before 1993
    expd <- dB / pB * pA                                      # expected in A at B's age- and sex-specific rates
    v    <- dA + (pA / pB)^2 * dB                             # Poisson variance of observed minus expected
    for (sx in names(sex_sets)) for (b in names(bands)) {
      s <- sex_sets[[sx]]; i <- band_idx[[b]]
      irr <- poisson_irr(c(as.vector(dA[i, s]), as.vector(dB[i, s])),
                         c(as.vector(pA[i, s]), as.vector(pB[i, s])), design(length(i), length(s)))
      n <- n + 1
      rows[[n]] <- data.frame(cause = causes[ci], sex = sx, band = b, year = years[yi],
                              obs_A = sum(dA[i, s]), exp_A = sum(expd[i, s]),
                              irr = irr[1], irr_lo = irr[2], irr_hi = irr[3],
                              irr_status = status_text[irr[4]], excess_se = sqrt(sum(v[i, s])))
    }
  }
  ex <- bind_rows(rows[seq_len(n)]) %>%
    mutate(excess = obs_A - exp_A,                              # matched on age, sex and year
           excess_lo = excess - 1.96 * excess_se, excess_hi = excess + 1.96 * excess_se,
           excess_pct_of_A = ifelse(obs_A > 0, excess / obs_A * 100, NA))
  side <- function(a, suffix) rates %>% filter(area == a) %>%
    select(cause, sex, band, year, deaths, dsr, pop, se) %>%
    rename_with(~ paste0(.x, "_", suffix), c(deaths, dsr, pop, se))
  g <- inner_join(side(a1, "A"), side(a2, "B"), by = c("cause", "sex", "band", "year")) %>%
    mutate(gap = dsr_A - dsr_B, gap_se = sqrt(se_A^2 + se_B^2),
           ratio = ifelse(!is.na(dsr_B) & dsr_B > 0, dsr_A / dsr_B, NA)) %>%   # blank where B's rate is zero
    left_join(ex, by = c("cause", "sex", "band", "year")) %>%
    mutate(irr_status = ifelse(is.na(irr_status), "not estimated: deaths not supplied", irr_status),
           comparison = cname, area_A = a1, area_B = a2) %>%
    arrange(cause, sex, band, year)
  gap_frames[[cname]] <- g
}
gaps <- bind_rows(gap_frames) %>%
  select(cause, sex, band, year, deaths_A, deaths_B, dsr_A, dsr_B, pop_A, pop_B, se_A, se_B, gap, gap_se, ratio,
         obs_A, exp_A, irr, irr_lo, irr_hi, irr_status, excess_se, excess, excess_lo, excess_hi, excess_pct_of_A,
         comparison, area_A, area_B)
stopifnot(nrow(gaps) == 9 * 3 * 6 * 45 * 4)
write_out(gaps, "gaps_by_comparison_cause_sex_band_year.csv",
          counts = c("deaths_A", "deaths_B", "pop_A", "pop_B", "obs_A"))

# ---- 4. the glm.fit() route checked against glm() for one cell ---------------------
chk <- d %>% filter(cause == "All causes", year == 2025, age %in% bands[["25-49"]], area %in% c("North", "South")) %>%
  mutate(exposed = as.numeric(area == "North"), female = as.numeric(sex == "F"))
m   <- glm(deaths ~ factor(age) + female + exposed + offset(log(pop)), family = poisson, data = chk)
ref <- filter(gaps, comparison == "N_vs_S", cause == "All causes", sex == "P", band == "25-49", year == 2025)
stopifnot(abs(exp(coef(m)[["exposed"]]) - ref$irr) < 1e-6,
          abs(exp(coef(m)[["exposed"]] - 1.96 * coef(summary(m))["exposed", "Std. Error"]) - ref$irr_lo) < 1e-6)

# ---- 5. decomposition by period ----------------------------------------------
# excess deaths per year in a period = the mean of the annual figures; its standard error treats the
# years as independent
period_excess <- bind_rows(lapply(names(periods), function(pn) {
  y <- periods[[pn]]; ny <- y[2] - y[1] + 1
  gaps %>% filter(year >= y[1], year <= y[2], !is.na(excess)) %>%
    group_by(comparison, cause, sex, band) %>%
    summarise(n = n(), excess_per_yr = mean(excess), se = sqrt(sum(excess_se^2)) / ny, .groups = "drop") %>%
    filter(n == ny) %>%
    transmute(comparison, cause, sex, band, period = pn, excess_per_yr,
              excess_per_yr_lo = excess_per_yr - 1.96 * se, excess_per_yr_hi = excess_per_yr + 1.96 * se)
}))
period_rates <- bind_rows(lapply(names(periods), function(pn) {
  y <- periods[[pn]]; ny <- y[2] - y[1] + 1
  bind_rows(lapply(c("M", "F", "P"), function(sx) {
    src <- if (sx == "P") dp else filter(d, sex == sx)
    sub <- filter(src, year >= y[1], year <= y[2])
    if (y[1] < 1993) sub <- filter(sub, cause != "Drug")
    pooled <- sub %>% group_by(cause, area, age, w) %>%
      summarise(deaths = sum(deaths), pop = sum(pop), .groups = "drop")
    bind_rows(lapply(names(bands), function(b)
      pooled %>% filter(age %in% bands[[b]]) %>% group_by(cause, area) %>%
        summarise(dsr = sum(w * deaths / pop) / sum(w) * 1e5, deaths_per_yr = sum(deaths) / ny, .groups = "drop") %>%
        mutate(sex = sx, period = pn, band = b)))
  }))
}))
dec <- bind_rows(lapply(names(comparisons), function(cname) {
  a1 <- comparisons[[cname]][1]; a2 <- comparisons[[cname]][2]
  inner_join(period_rates %>% filter(area == a1) %>% select(sex, period, band, cause, dsr_A = dsr, A_deaths_per_yr = deaths_per_yr),
             period_rates %>% filter(area == a2) %>% select(sex, period, band, cause, dsr_B = dsr),
             by = c("sex", "period", "band", "cause")) %>%
    filter(!is.na(dsr_A)) %>%
    mutate(comparison = cname, area_A = a1, area_B = a2, gap = dsr_A - dsr_B,
           ratio = ifelse(dsr_B > 0, dsr_A / dsr_B, NA))
})) %>%
  left_join(period_excess, by = c("comparison", "cause", "sex", "band", "period"))
allc <- dec %>% filter(cause == "All causes") %>%
  select(comparison, sex, period, band, allcause_gap = gap, allcause_excess = excess_per_yr)
dec <- dec %>% left_join(allc, by = c("comparison", "sex", "period", "band")) %>%
  mutate(share_of_gap_pct = gap / allcause_gap * 100, share_of_excess_pct = excess_per_yr / allcause_excess * 100,
         cause = factor(cause, causes), period = factor(period, names(periods)), band = factor(band, names(bands)),
         sex = factor(sex, c("M", "F", "P")), comparison = factor(comparison, names(comparisons))) %>%
  arrange(comparison, sex, period, band, cause) %>%
  mutate(across(where(is.factor), as.character)) %>%
  select(comparison, area_A, area_B, sex, period, band, cause, dsr_A, dsr_B, gap, ratio, A_deaths_per_yr,
         excess_per_yr, excess_per_yr_lo, excess_per_yr_hi, allcause_gap, allcause_excess,
         share_of_gap_pct, share_of_excess_pct)
write_out(dec, "decomposition_by_period.csv")

# ---- 6. headline check, printed only --------------------------------------------
h <- filter(gaps, comparison == "N_vs_S", cause == "All causes", sex == "P", band == "25-49")
for (y in c(1981, 2019, 2024, 2025)) {
  r <- filter(h, year == y)
  cat(sprintf("25-49, North vs South incl. London, %d: DSR %.1f vs %.1f; IRR %.3f (%.3f-%.3f); excess deaths %s (%s-%s)\n",
              y, r$dsr_A, r$dsr_B, r$irr, r$irr_lo, r$irr_hi, format(round(r$excess), big.mark = ","),
              format(round(r$excess_lo), big.mark = ","), format(round(r$excess_hi), big.mark = ",")))
}
cat("rate ratios by status:\n"); print(table(gaps$irr_status))
cat("done; output in", normalizePath(outdir), "\n")
