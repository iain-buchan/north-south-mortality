# =====================================================================
# North-South divide in English mortality by cause of death, 1981-2025
# Script 2 of 5: ages 25-49 by cause and sex, on the cause definitions of the 2018 paper
#
# Iain Buchan, 15 Nov 2026. Exploratory work in progress; not peer reviewed.
#
# Why
#   The 2026 ONS extract (script 1) defines three causes differently from the 2017 extract behind
#   Kontopantelis et al, Lancet Public Health 2018; 3: e567-75. In the 2017 extract accidents exclude
#   accidental drug and alcohol poisoning (X40-X45); drug-related deaths are F11-F16, F18-F19, X40-X44
#   and X85, with intentional and undetermined drug poisoning left in suicide; and alcohol-related
#   deaths exclude X65. On those definitions the named causes do not overlap, apart from two codes
#   (I42.6 and Y15, the memo series OVL). This script puts 1981-2025 on the 2017 definitions:
#     1981-2012   the 2017 ONS extract (North, South including London), except suicide
#     2013-2025   NOMIS deaths rebuilt on the same definitions (North, South excluding London, London)
#   and checks the sources against each other wherever two of them cover the same deaths.
#
#   Suicide is the exception. Its definition is the same in all three sources, but the 2017 extract's
#   series for the South is higher than the 2026 extract's in every year to 2014, while the North is
#   identical (see like_for_like_suicide_*.csv, and the README). Suicide for 1981-2012 is therefore
#   taken from the 2026 extract, which agrees with NOMIS in every cell for 2013-2025.
#
# Data (in ./data)
#   deathsnorthsouthdivideengland1981to2016.xlsx   ONS extract of 2017, one sheet per cause
#   deathsnorthsouthlondon1981to2025.xlsx          ONS extract of November 2026 (suicide to 2012; checks)
#   nomis_deaths_2018_definitions_by_sex.csv       NOMIS NM_161_1, 2013-2025, ages 25-49, 20 series
#   nomis_pop_nsl_1981_2025.csv                    ONS mid-year population estimates (NOMIS NM_31_1)
#   nomis_suicide_england_wales_2013_16.csv        NOMIS suicides of residents of England and of Wales
#
# Series
#   Main (they add up to ALL): DRUG18, ALC18, SUI, ACC18, CVD, DIAB, CANCER and OTHER18 (the residual).
#   Parts: CANCER_XB and BREAST (of CANCER); RESP, DIGEST_NONALC, MENT_NEURO, COVID and REST18 (of
#   OTHER18; from 2013 only). Memo: OVL, c_X40_44, c_X45, c_X65, c_DRUGSUI, c_E66 (from 2013 only).
#   The drug-related series starts in 1993, so OTHER18 does too.
#
# Method notes
#   * Rates are directly standardised to the 2013 European Standard Population within each age band
#     (25-44, 45-49, 25-49), per 100,000. 'P' (persons) pools the sexes within each age group. A period
#     rate pools deaths and populations over its years.
#   * Excess deaths = observed deaths in area A minus the deaths expected at area B's age- and
#     sex-specific rates in the same year; a period figure is the mean of the annual figures.
#   * Intervals are 95%, normal approximations from Poisson variances with populations taken as fixed:
#     for a gap from the variances of the two standardised rates; for the change in a gap from the
#     variances of the two periods, which share no deaths; for excess deaths from the variance of
#     observed minus expected. Shares carry no intervals. All of them are too narrow, because they
#     ignore error in the population estimates and in the coding of causes.
#
# Outputs (written to ./output; counts as whole numbers, everything else to six decimal places)
#   like_for_like_validation.csv                    the sources against each other, cell by cell
#   like_for_like_cells.csv                         deaths and population for every cell, as joined here
#   like_for_like_annual.csv                        annual rates, gaps, ratios and excess deaths
#   like_for_like_periods.csv                       the same for pooled periods, with shares of the gap
#   like_for_like_change_2014_16_to_2023_25.csv     change in each gap between 2014-16 and 2023-25
#   like_for_like_2018_paper_check.csv              the 2018 paper's excess deaths, recomputed
#   like_for_like_suicide_2017_vs_2026.csv          suicide in the two ONS extracts
#   like_for_like_suicide_south_by_year.csv         the same by year, beside NOMIS England and Wales
#
# Needs: readxl, dplyr, tidyr, readr (installed automatically if absent). A few seconds.
# =====================================================================

pkgs <- c("readxl", "dplyr", "tidyr", "readr")
new  <- pkgs[!pkgs %in% rownames(installed.packages())]
if (length(new)) install.packages(new)
invisible(lapply(pkgs, library, character.only = TRUE))
if (packageVersion("dplyr") < "1.1.0") stop("dplyr 1.1.0 or later is needed - please update it.")

# ---- configuration ---------------------------------------------------
ext17_file <- file.path("data", "deathsnorthsouthdivideengland1981to2016.xlsx")
ext26_file <- file.path("data", "deathsnorthsouthlondon1981to2025.xlsx")
nomis_file <- file.path("data", "nomis_deaths_2018_definitions_by_sex.csv")
pop_file   <- file.path("data", "nomis_pop_nsl_1981_2025.csv")
ew_file    <- file.path("data", "nomis_suicide_england_wales_2013_16.csv")
outdir     <- "output"

for (f in c(ext17_file, ext26_file, nomis_file, pop_file, ew_file))
  if (!file.exists(f)) stop("Cannot find '", f, "' from ", getwd(), " - setwd() to the '2026 Causes' folder.")
dir.create(outdir, showWarnings = FALSE)

all_ages <- c("<1","01-04","05-09","10-14","15-19","20-24","25-29","30-34","35-39","40-44","45-49",
              "50-54","55-59","60-64","65-69","70-74","75-79","80-84","85+")
ages  <- all_ages[7:11]                                              # 25-29 to 45-49
acols <- c("a25_29", "a30_34", "a35_39", "a40_44", "a45_49")         # the same, as named in the NOMIS file
esp   <- c(6000, 6500, 7000, 7000, 7000)                             # 2013 European Standard Population
bands <- list("25-44" = ages[1:4], "45-49" = ages[5], "25-49" = ages)
periods <- list("1993-95" = 1993:1995, "2001-05" = 2001:2005, "2010-14" = 2010:2014, "2014-16" = 2014:2016,
                "2015-19" = 2015:2019, "2021-25" = 2021:2025, "2023-25" = 2023:2025,
                # decades, with the pandemic years set apart; with 2023-25 above they cover 1981-2025.
                # 1993-2000 is there because the drug-related series, and so 'other causes', start in 1993
                "1981-90" = 1981:1990, "1991-2000" = 1991:2000, "1993-2000" = 1993:2000, "2001-10" = 2001:2010,
                "2011-19" = 2011:2019, "2020-22" = 2020:2022)
comparisons <- list(N_vs_S = c("North", "South"), N_vs_SXL = c("North", "SouthXL"),
                    L_vs_SXL = c("London", "SouthXL"), N_vs_L = c("North", "London"))
first_nomis_year <- 2013
key <- c("area", "sex", "age", "year")
idx <- c("comparison", "sex", "band", "series")

label <- c(
  ALL = "All causes",
  DRUG18 = "Drug-related (F11-F16, F18-F19, X40-X44, X85)",
  ALC18 = "Alcohol-related (2017 extract definition: includes X45 and Y15, excludes X65)",
  SUI = "Suicide (X60-X84, Y10-Y34, so including intentional and undetermined drug and alcohol poisoning)",
  ACC18 = "Accidents excluding accidental drug and alcohol poisoning (V01-X59 less X40-X45)",
  CVD = "Cardiovascular (I00-I99)",
  DIAB = "Diabetes (E10-E14)",
  CANCER = "Cancer (C00-C97)",
  OTHER18 = "Other causes (residual: all causes minus the seven groups above)",
  CANCER_XB = "part of cancer: all except breast (C00-C97 less C50)",
  BREAST = "part of cancer: breast (C50)",
  RESP = "part of other: respiratory (J00-J99)",
  DIGEST_NONALC = "part of other: digestive excluding alcohol codes (K00-K93 less K29.2, K70, K73, K74.0-K74.2, K74.6, K86.0)",
  MENT_NEURO = "part of other: mental, behavioural and neurological excluding alcohol and drug codes (F00-F99, G00-G99 less F10-F16, F18-F19, G31.2, G62.1)",
  COVID = "part of other: COVID-19 (U07.1, U07.2, U10.9)",
  REST18 = "part of other: everything else",
  OVL = "memo: the two codes counted in two groups (I42.6 in alcohol and cardiovascular; Y15 in alcohol and suicide)",
  c_X40_44 = "memo: accidental drug poisoning (X40-X44), inside drug-related",
  c_X45 = "memo: accidental alcohol poisoning (X45), inside alcohol-related",
  c_X65 = "memo: intentional alcohol poisoning (X65), inside suicide",
  c_DRUGSUI = "memo: intentional and undetermined drug poisoning (X60-X64, Y10-Y14), inside suicide",
  c_E66 = "memo: obesity as underlying cause (E66), inside other")
main_series  <- c("ALL", "DRUG18", "ALC18", "SUI", "ACC18", "CVD", "DIAB", "CANCER", "OTHER18")
series_order <- names(label)

lookup <- function(x, map) unname(ifelse(x %in% names(map), map[x], x))
wide   <- function(df) pivot_wider(df, id_cols = all_of(key), names_from = "series", values_from = "deaths") %>%
  arrange(area, sex, age, year)

# counts are written as whole numbers and everything else with six decimal places
write_out <- function(df, name, counts = character()) {
  f0 <- function(x) { stopifnot(all(is.na(x) | x == round(x))); ifelse(is.na(x), NA_character_, sprintf("%.0f", x)) }
  f6 <- function(x) ifelse(is.na(x), NA_character_, sub("^-(0\\.0+)$", "\\1", sprintf("%.6f", x)))
  df <- df %>% mutate(across(all_of(counts), f0)) %>% mutate(across(where(is.double), f6))
  write_csv(df, file.path(outdir, name), na = "")
}

# ---- 1. read the four sources ------------------------------------------
# 2017 extract: one sheet per cause, a block of year columns for males and another for females,
# 19 age-group rows for the North and 19 for the South (which here includes London)
read_2017_sheet <- function(sheet) {
  x <- as.matrix(read_excel(ext17_file, sheet = sheet, col_names = FALSE, col_types = "text",
                            range = cell_limits(c(1, 1), c(NA, NA)), .name_repair = "minimal"))
  hdr <- which(trimws(x[, 1]) == "Area")                             # the row of year headings
  females_from <- which(x[hdr - 1, ] == "Females")
  stopifnot(length(hdr) == 1, length(females_from) == 1, sum(x[hdr - 1, ] == "Males", na.rm = TRUE) == 1)
  yr   <- suppressWarnings(as.integer(x[hdr, ]))
  cols <- which(!is.na(yr))
  body <- x[(hdr + 1):nrow(x), , drop = FALSE]
  area <- trimws(body[, 1]); area[!area %in% c("North", "South")] <- NA
  area <- c(NA, area)[cummax(ifelse(is.na(area), 0L, seq_along(area))) + 1]    # carry the area label down
  age  <- trimws(body[, 2]); keep <- age %in% all_ages
  bind_rows(lapply(cols, function(j)
    tibble(cause = sheet, area = area[keep], sex = if (j < females_from) "M" else "F", age = age[keep],
           year = yr[j], deaths = as.numeric(body[keep, j])))) %>%
    filter(!is.na(deaths))                                           # suicide is blank below age 15
}
cause_sheets <- setdiff(excel_sheets(ext17_file), c("Notes and terms & conditions", "Definition", "Geography"))
ext_all <- bind_rows(lapply(cause_sheets, read_2017_sheet))
stopifnot(nrow(ext_all) == 25872, all(ext_all$area %in% c("North", "South")), all(ext_all$year %in% 1981:2016),
          !any(duplicated(ext_all[, c("cause", key)])))
ext <- ext_all %>% filter(age %in% ages) %>%
  transmute(series = lookup(cause, c("All causes" = "ALL", "Cardiovascular" = "CVD", "Diabetes" = "DIAB",
                                     "Accidents" = "ACC18", "Drug" = "DRUG18", "Suicide" = "SUI", "Alcohol" = "ALC18",
                                     "Cancer (exc breast)" = "CANCER_XB", "Breast cancer" = "BREAST",
                                     "Obesity" = "OBES_ANY")),
            area, sex, age, year, deaths)

# 2026 extract ('South' in the file excludes London): suicide for the years before NOMIS, and the checks
x26 <- read_excel(ext26_file, sheet = "Data") %>%
  filter(Agegroup %in% ages) %>%
  pivot_longer(all_of(as.character(1981:2025)), names_to = "year", values_to = "deaths") %>%
  transmute(cause = lookup(Cause, c(Suicides = "Suicide", Drugs = "Drug")), area = lookup(RegionGroup, c(South = "SouthXL")),
            sex = lookup(Sex, c(Male = "M", Female = "F")), age = Agegroup, year = as.integer(year),
            deaths = as.numeric(deaths))
x26 <- bind_rows(x26, x26 %>% filter(area %in% c("SouthXL", "London")) %>% group_by(cause, sex, age, year) %>%
                   summarise(deaths = sum(deaths), .groups = "drop") %>% mutate(area = "South"))
sui26 <- x26 %>% filter(cause == "Suicide") %>% transmute(series = "SUI", area, sex, age, year, deaths)
stopifnot(nrow(sui26) == 4 * 2 * 5 * 45, !anyNA(sui26$deaths))

# NOMIS deaths on the 2017 definitions (areas N, X = South excluding London, L)
nom <- read_csv(nomis_file, show_col_types = FALSE) %>%
  pivot_longer(all_of(acols), names_to = "age", values_to = "deaths") %>%
  transmute(series, area = lookup(area, c(N = "North", X = "SouthXL", L = "London")), sex,
            age = ages[match(age, acols)], year = as.integer(year), deaths = as.numeric(deaths))
nom <- bind_rows(nom, nom %>% filter(area %in% c("SouthXL", "London")) %>% group_by(series, sex, age, year) %>%
                   summarise(deaths = sum(deaths), .groups = "drop") %>% mutate(area = "South"))
stopifnot(!anyNA(nom), nrow(nom) == 20 * 4 * 2 * 5 * 13)

# populations ('South' in the file excludes London)
pop <- read_csv(pop_file, show_col_types = FALSE) %>% filter(age >= 7, age <= 11) %>%
  pivot_longer(all_of(as.character(1981:2025)), names_to = "year", values_to = "pop") %>%
  transmute(area = lookup(area, c(South = "SouthXL")), sex, age = ages[age - 6], year = as.integer(year),
            pop = as.numeric(pop))
pop <- bind_rows(pop, pop %>% filter(area %in% c("SouthXL", "London")) %>% group_by(sex, age, year) %>%
                   summarise(pop = sum(pop), .groups = "drop") %>% mutate(area = "South"))
stopifnot(!anyNA(pop), nrow(pop) == 4 * 2 * 5 * 45)

# ---- 2. the sources against each other -----------------------------------
# cell-by-cell agreement between two sources of the same deaths (cell = area x sex x age group x year)
agreement <- function(check, m, first, second, by) {
  m %>% mutate(g = factor(.data[[by]], unique(.data[[by]])), diff = .data[[second]] - .data[[first]]) %>%
    group_by(g) %>%
    summarise(cells = n(), deaths_first = sum(.data[[first]]), deaths_second = sum(.data[[second]]),
              cells_differing = sum(diff != 0), largest_cell_difference = max(abs(diff)), .groups = "drop") %>%
    transmute(check = check, series = as.character(g), cells = as.numeric(cells), deaths_first, deaths_second,
              cells_differing = as.numeric(cells_differing), largest_cell_difference,
              difference = deaths_second - deaths_first)
}
# (a) the 2017 extract against NOMIS on the 2017 definitions, 2013-2016, North and South
both <- ext %>% filter(year %in% 2013:2016, series != "OBES_ANY") %>% rename(deaths_extract = deaths) %>%
  inner_join(nom %>% filter(area %in% c("North", "South")) %>% rename(deaths_nomis = deaths), by = c("series", key))
val_a <- agreement("a. 2017 extract (first) against NOMIS (second), 2013-2016, North and South",
                   both, "deaths_extract", "deaths_nomis", "series")
# (b) NOMIS, converted to the current definitions, against the 2026 extract, 2013-2025, three areas
cur <- c("All causes", "Cardiovascular", "Diabetes", "Accidents", "Suicide", "Alcohol", "Drug")
n26 <- wide(filter(nom, area != "South")) %>%
  transmute(area, sex, age, year, `All causes` = ALL, Cardiovascular = CVD, Diabetes = DIAB,
            Accidents = ACC18 + c_X40_44 + c_X45, Suicide = SUI, Alcohol = ALC18 + c_X65, Drug = DRUG18 + c_DRUGSUI) %>%
  pivot_longer(all_of(cur), names_to = "cause", values_to = "deaths_nomis")
m26 <- x26 %>% filter(cause %in% cur, year %in% 2013:2025, area != "South") %>% rename(deaths_2026 = deaths) %>%
  inner_join(n26, by = c("cause", key))
stopifnot(nrow(m26) == length(cur) * 3 * 2 * 5 * 13)
val_b <- agreement(paste("b. 2026 extract (first) against NOMIS converted to the current definitions (second),",
                         "2013-2025, North, South excl. London and London"), m26, "deaths_2026", "deaths_nomis", "cause")
# (c) the 2017 extract against the 2026 extract for the causes defined alike in both, over the years
#     taken from the 2017 extract
same <- c(ALL = "All causes", CVD = "Cardiovascular", DIAB = "Diabetes")
m17 <- ext %>% filter(series %in% names(same), year < first_nomis_year) %>%
  transmute(cause = unname(same[series]), area, sex, age, year, deaths_2017 = deaths) %>%
  inner_join(x26 %>% filter(area %in% c("North", "South")) %>% rename(deaths_2026 = deaths), by = c("cause", key))
stopifnot(nrow(m17) == length(same) * 2 * 2 * 5 * (first_nomis_year - 1981))
val_c <- agreement(paste0("c. 2017 extract (first) against 2026 extract (second), 1981-", first_nomis_year - 1,
                          ", North and South, causes defined alike"), m17, "deaths_2017", "deaths_2026", "cause")
val <- bind_rows(val_a, val_b, val_c)
write_out(val, "like_for_like_validation.csv", counts = names(val)[-(1:2)])

# ---- 3. one table of cells, 1981-2025 ----------------------------------------
src_early <- "ONS 2017 extract (suicide: 2026 extract)"
src_nomis <- "NOMIS NM_161_1"
early <- bind_rows(ext %>% filter(year < first_nomis_year, !series %in% c("OBES_ANY", "SUI")),
                   sui26 %>% filter(year < first_nomis_year, area %in% c("North", "South")))
w <- bind_rows(wide(early) %>% mutate(source = src_early), wide(nom) %>% mutate(source = src_nomis)) %>%
  mutate(CANCER  = CANCER_XB + BREAST,
         OTHER18 = ALL - (DRUG18 + ALC18 + SUI + ACC18 + CVD + DIAB + CANCER),   # missing before 1993, when the drug series starts
         REST18  = OTHER18 - (RESP + DIGEST_NONALC + MENT_NEURO + COVID),
         OVL     = c_I426 + c_Y15) %>%
  left_join(pop, by = key)
stopifnot(!anyNA(w$pop), nrow(w) == 2 * 2 * 5 * 32 + 4 * 2 * 5 * 13)
L <- w %>% pivot_longer(all_of(series_order), names_to = "series", values_to = "d") %>%
  arrange(match(series, series_order), source != src_early, area, sex, age, year) %>%
  select(area, sex, age, year, p = pop, source, series, d)
write_out(L, "like_for_like_cells.csv", counts = c("p", "d"))

# ---- 4. rates, gaps, ratios and excess deaths -----------------------------------
# cells for each 'time' (a year, or a period of ny years): deaths and population summed over the years
# and, for persons, over the sexes; deaths are missing unless every year (and both sexes) is present
pool <- function(x) {
  by_sex <- x %>% group_by(time, ny, series, area, sex, age) %>%
    summarise(nd = sum(!is.na(d)), d = sum(d, na.rm = TRUE), p = sum(p), .groups = "drop") %>%
    mutate(d = ifelse(nd < ny, NA, d))
  persons <- x %>% group_by(time, ny, series, area, age) %>%
    summarise(nd = sum(!is.na(d)), d = sum(d, na.rm = TRUE), p = sum(p), .groups = "drop") %>%
    mutate(d = ifelse(nd < 2 * ny, NA, d), sex = "P")
  bind_rows(by_sex, persons) %>% select(-nd)
}
# standardised rate, its standard error and the deaths, for each age band; kept only where every age
# group of the band is present
band_rates <- function(cells) bind_rows(lapply(names(bands), function(b)
  cells %>% filter(age %in% bands[[b]]) %>% mutate(w = esp[match(age, ages)]) %>%
    group_by(time, ny, series, area, sex) %>%
    summarise(complete = n() == length(bands[[b]]) && !anyNA(d),
              dsr = sum(w * d / p) / sum(w) * 1e5,
              se  = sqrt(sum((w / sum(w))^2 * pmax(d, 0) / p^2)) * 1e5,
              deaths = sum(d), .groups = "drop") %>%
    filter(complete) %>% select(-complete) %>% mutate(band = b)))
# area A against area B
compare <- function(rates) bind_rows(lapply(names(comparisons), function(cn) {
  a <- comparisons[[cn]][1]; b <- comparisons[[cn]][2]
  inner_join(rates %>% filter(area == a) %>% select(time, ny, series, sex, band, dsr_A = dsr, se_A = se, deaths_A = deaths),
             rates %>% filter(area == b) %>% select(time, series, sex, band, dsr_B = dsr, se_B = se, deaths_B = deaths),
             by = c("time", "series", "sex", "band")) %>%
    mutate(comparison = cn, area_A = a, area_B = b, description = unname(label[series]),
           deaths_A_per_year = deaths_A / ny, deaths_B_per_year = deaths_B / ny,
           gap = dsr_A - dsr_B, gap_se = sqrt(se_A^2 + se_B^2),
           gap_lo = gap - 1.96 * gap_se, gap_hi = gap + 1.96 * gap_se,
           ratio = ifelse(dsr_B != 0, dsr_A / dsr_B, NA))
}))
# excess deaths in A, year by year: observed minus expected at B's age- and sex-specific rates
excess <- function(cells) bind_rows(lapply(names(comparisons), function(cn) {
  a <- comparisons[[cn]][1]; b <- comparisons[[cn]][2]
  x <- inner_join(cells %>% filter(area == a, sex != "P") %>% select(time, series, sex, age, dA = d, pA = p),
                  cells %>% filter(area == b, sex != "P") %>% select(time, series, sex, age, dB = d, pB = p),
                  by = c("time", "series", "sex", "age")) %>%
    mutate(e = dA - dB / pB * pA, v = pmax(dA, 0) + (pA / pB)^2 * pmax(dB, 0))
  bind_rows(lapply(names(bands), function(bn) {
    xb <- filter(x, age %in% bands[[bn]])
    bind_rows(xb %>% group_by(time, series, sex) %>%
                summarise(excess_deaths_per_year = sum(e), excess_se = sqrt(sum(v)), .groups = "drop"),
              xb %>% group_by(time, series) %>%
                summarise(excess_deaths_per_year = sum(e), excess_se = sqrt(sum(v)), .groups = "drop") %>%
                mutate(sex = "P")) %>%
      mutate(band = bn, comparison = cn)
  }))
}))
# rows in a fixed order: comparison, sex, age band, series, then time
ordered <- function(df, ...) df %>%
  arrange(match(comparison, names(comparisons)), match(sex, c("F", "M", "P")), match(band, names(bands)),
          match(series, series_order), ...)
sources <- function(x) x %>% group_by(time) %>%
  summarise(source = paste(sort(unique(source), method = "radix"), collapse = "; "), .groups = "drop")

# annual
x_year <- L %>% mutate(time = as.character(year), ny = 1L)
c_year <- pool(x_year)
annual <- compare(band_rates(c_year)) %>%
  left_join(excess(c_year), by = c("comparison", "time", "series", "sex", "band")) %>%
  left_join(sources(x_year), by = "time") %>%
  mutate(year = as.integer(time),
         excess_lo = excess_deaths_per_year - 1.96 * excess_se, excess_hi = excess_deaths_per_year + 1.96 * excess_se) %>%
  ordered(year)
stopifnot(!anyNA(annual$excess_deaths_per_year), !any(duplicated(annual[, c(idx, "year")])))
annual_cols <- c("comparison", "area_A", "area_B", "sex", "band", "series", "description", "year", "source",
                 "deaths_A_per_year", "deaths_B_per_year", "dsr_A", "dsr_B", "gap", "gap_lo", "gap_hi", "ratio",
                 "excess_deaths_per_year", "excess_se", "excess_lo", "excess_hi")
write_out(annual[, annual_cols], "like_for_like_annual.csv")

# pooled periods; excess deaths per year in a period = the mean of the annual figures, and its standard
# error treats the years as independent
x_period <- bind_rows(lapply(names(periods), function(pn)
  L %>% filter(year %in% periods[[pn]]) %>% mutate(time = pn, ny = length(periods[[pn]]))))
period_excess <- bind_rows(lapply(names(periods), function(pn) {
  n <- length(periods[[pn]])
  annual %>% filter(year %in% periods[[pn]]) %>% group_by(across(all_of(idx))) %>%
    summarise(n_years = n(), excess_deaths_per_year = mean(excess_deaths_per_year), v = sum(excess_se^2), .groups = "drop") %>%
    filter(n_years == n) %>%
    transmute(across(all_of(idx)), time = pn, excess_deaths_per_year, excess_se = sqrt(v) / n)
}))
per <- compare(band_rates(pool(x_period))) %>%
  left_join(period_excess, by = c(idx, "time")) %>%
  left_join(sources(x_period), by = "time") %>%
  rename(period = time, years = ny) %>%
  mutate(excess_lo = excess_deaths_per_year - 1.96 * excess_se, excess_hi = excess_deaths_per_year + 1.96 * excess_se)
stopifnot(!anyNA(per$excess_deaths_per_year), !any(duplicated(per[, c(idx, "period")])))
allc <- per %>% filter(series == "ALL") %>%
  select(comparison, sex, band, period, allcause_gap = gap, allcause_excess = excess_deaths_per_year)
per <- per %>% inner_join(allc, by = c("comparison", "sex", "band", "period")) %>%
  mutate(pct_of_allcause_gap = gap / allcause_gap * 100,
         pct_of_allcause_excess = excess_deaths_per_year / allcause_excess * 100) %>%
  ordered(match(period, names(periods)))
period_cols <- c("comparison", "area_A", "area_B", "sex", "band", "series", "description", "period", "years", "source",
                 annual_cols[10:21], "pct_of_allcause_gap", "pct_of_allcause_excess")
write_out(per[, period_cols], "like_for_like_periods.csv", counts = "years")

# ---- 5. change in each gap between 2014-16 and 2023-25 -----------------------------
p0 <- per %>% filter(period == "2014-16") %>% select(all_of(idx), description, gap, gap_se, dsr_A, dsr_B, ratio)
p1 <- per %>% filter(period == "2023-25") %>% select(all_of(idx), gap, gap_se, dsr_A, dsr_B, ratio)
chg <- inner_join(p0, p1, by = idx, suffix = c("_0", "_1")) %>%
  mutate(change_se = sqrt(gap_se_0^2 + gap_se_1^2)) %>%          # the two periods share no deaths, so the variances add
  transmute(across(all_of(idx)), description, gap_2014_16 = gap_0, gap_2023_25 = gap_1, change_in_gap = gap_1 - gap_0,
            change_lo = change_in_gap - 1.96 * change_se, change_hi = change_in_gap + 1.96 * change_se,
            dsr_A_2014_16 = dsr_A_0, dsr_A_2023_25 = dsr_A_1, change_in_A = dsr_A_1 - dsr_A_0,
            dsr_B_2014_16 = dsr_B_0, dsr_B_2023_25 = dsr_B_1, change_in_B = dsr_B_1 - dsr_B_0,
            ratio_2014_16 = ratio_0, ratio_2023_25 = ratio_1)
stopifnot(nrow(chg) == nrow(p0), nrow(chg) == nrow(p1))
chg <- chg %>%
  inner_join(chg %>% filter(series == "ALL") %>% select(comparison, sex, band, allcause_change = change_in_gap),
             by = c("comparison", "sex", "band")) %>%
  mutate(pct_of_allcause_change = change_in_gap / allcause_change * 100) %>% select(-allcause_change) %>%
  ordered()
write_out(chg, "like_for_like_change_2014_16_to_2023_25.csv")

# ---- 6. the 2018 paper's own figures: northern excess deaths, 2014-16, ages 25-44 ------------
# from the 2017 extract as the paper used it. The paper grouped diabetes and obesity (any mention)
# with cardiovascular causes. Published values are from its Results.
published <- tribble(~sex, ~cause, ~published_2018,
                     "F", "All causes", 1881, "F", "Alcohol-related", 369,
                     "F", "Cardiovascular, with diabetes and obesity", 330, "F", "Drug-related", 258,
                     "F", "Cancer", 246, "F", "Other causes", 526,
                     "M", "All causes", 3530, "M", "Suicide", 417, "M", "Accidents", 264, "M", "Cancer", 178,
                     "M", "Other causes", 752)
groups <- c("Accidents", "Alcohol-related", "Cardiovascular, with diabetes and obesity", "Drug-related", "Suicide", "Cancer")
e <- wide(ext %>% filter(year %in% 2014:2016, age %in% bands[["25-44"]])) %>%
  transmute(area, sex, age, year, `All causes` = ALL, Accidents = ACC18, `Alcohol-related` = ALC18,
            `Cardiovascular, with diabetes and obesity` = CVD + DIAB + OBES_ANY, `Drug-related` = DRUG18,
            Suicide = SUI, Cancer = CANCER_XB + BREAST) %>%
  mutate(`Other causes` = `All causes` - (Accidents + `Alcohol-related` + `Cardiovascular, with diabetes and obesity` +
                                            `Drug-related` + Suicide + Cancer)) %>%
  pivot_longer(-all_of(key), names_to = "cause", values_to = "deaths")
s26_label <- "Suicide, with the 2026 extract series (South corrected for 2014)"
e <- bind_rows(e, sui26 %>% filter(year %in% 2014:2016, age %in% bands[["25-44"]], area %in% c("North", "South")) %>%
                 transmute(area, sex, age, year, cause = s26_label, deaths)) %>%
  inner_join(pop, by = key)
paper <- inner_join(e %>% filter(area == "North") %>% select(sex, age, year, cause, dN = deaths, pN = pop),
                    e %>% filter(area == "South") %>% select(sex, age, year, cause, dS = deaths, pS = pop),
                    by = c("sex", "age", "year", "cause")) %>%
  group_by(sex, cause) %>% summarise(excess_deaths_2014_16 = sum(dN - dS / pS * pN), .groups = "drop") %>%
  group_by(sex) %>% mutate(pct_of_total = excess_deaths_2014_16 / excess_deaths_2014_16[cause == "All causes"] * 100) %>%
  ungroup() %>% left_join(published, by = c("sex", "cause")) %>%
  arrange(sex, match(cause, c("All causes", groups, "Other causes", s26_label)))
write_out(paper, "like_for_like_2018_paper_check.csv", counts = "published_2018")

# ---- 7. suicide in the 2017 extract against the 2026 extract --------------------------
s17 <- ext %>% filter(series == "SUI") %>% select(all_of(key), deaths_2017 = deaths) %>%
  inner_join(sui26 %>% select(all_of(key), deaths_2026 = deaths), by = key) %>%
  mutate(band = ifelse(year <= 2014, "1981-2014", "2015-2016"))
sc <- s17 %>% group_by(band, area) %>%
  summarise(extract_2017 = sum(deaths_2017), extract_2026 = sum(deaths_2026), cells = as.numeric(n()),
            cells_differing = as.numeric(sum(deaths_2017 != deaths_2026)), .groups = "drop") %>%
  mutate(pct_difference = (extract_2017 / extract_2026 - 1) * 100)
write_out(sc, "like_for_like_suicide_2017_vs_2026.csv", counts = c("extract_2017", "extract_2026", "cells", "cells_differing"))
# the same year by year, beside NOMIS suicides of residents of England and of Wales (public data start in 2013)
ew <- read_csv(ew_file, show_col_types = FALSE) %>% pivot_wider(names_from = "area", values_from = "deaths") %>%
  transmute(year = as.integer(year), sex, england_nomis = as.numeric(England), wales_nomis = as.numeric(Wales))
sy <- s17 %>% group_by(year, sex) %>%
  summarise(north_2017_extract = sum(deaths_2017[area == "North"]), north_2026_extract = sum(deaths_2026[area == "North"]),
            south_2017_extract = sum(deaths_2017[area == "South"]), south_2026_extract = sum(deaths_2026[area == "South"]),
            .groups = "drop") %>%
  mutate(south_surplus_in_2017_extract = south_2017_extract - south_2026_extract) %>%
  left_join(ew, by = c("year", "sex"))
stopifnot(all(sy$north_2017_extract == sy$north_2026_extract),
          with(filter(sy, !is.na(england_nomis)), all(north_2026_extract + south_2026_extract == england_nomis)))
write_out(sy, "like_for_like_suicide_south_by_year.csv", counts = names(sy)[-(1:2)])

# ---- 8. summary, printed only ------------------------------------------------
options(width = 200)
cat("1. Agreement between sources, deaths at ages 25-49, cell by cell (area x sex x age group x year)\n")
for (ch in unique(val$check)) {
  cat("  ", ch, "\n")
  print(as.data.frame(val %>% filter(check == ch) %>% select(-check)), row.names = FALSE)
}
cat("\n2. Suicide at ages 25-49 in the 2017 extract against the 2026 extract\n")
print(as.data.frame(sc %>% mutate(pct_difference = round(pct_difference, 1))), row.names = FALSE)
cat("   the South year by year from 2011, with NOMIS suicides of residents of Wales:\n")
print(as.data.frame(sy %>% filter(year >= 2011) %>% group_by(year) %>%
                      summarise(south_2017_extract = sum(south_2017_extract), south_2026_extract = sum(south_2026_extract),
                                surplus = sum(south_surplus_in_2017_extract), wales_nomis = sum(wales_nomis))), row.names = FALSE)
cat("\n3. The 2018 paper, 2014-16, ages 25-44: northern excess deaths from the 2017 extract, against the published figures\n")
print(as.data.frame(paper %>% mutate(across(c(excess_deaths_2014_16, pct_of_total), ~ round(.x, 1)))), row.names = FALSE)
for (sx in c("F", "M", "P")) {
  cat(sprintf("\n4. %s, ages 25-49, North minus South incl. London: gap per 100,000 (95%% CI) in 2014-16 and 2023-25, and the change\n",
              c(F = "Women", M = "Men", P = "Persons")[sx]))
  for (s in main_series) {
    a <- filter(per, comparison == "N_vs_S", band == "25-49", sex == sx, series == s, period == "2014-16")
    b <- filter(per, comparison == "N_vs_S", band == "25-49", sex == sx, series == s, period == "2023-25")
    k <- filter(chg, comparison == "N_vs_S", band == "25-49", sex == sx, series == s)
    cat(sprintf("   %-8s %6.2f (%6.2f to %6.2f)   %6.2f (%6.2f to %6.2f)   change %+6.2f (%+6.2f to %+6.2f)\n",
                s, a$gap, a$gap_lo, a$gap_hi, b$gap, b$gap_lo, b$gap_hi, k$change_in_gap, k$change_lo, k$change_hi))
  }
}
cat("done; output in", normalizePath(outdir), "\n")
