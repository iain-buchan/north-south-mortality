# =====================================================================
# North-South divide in English mortality by cause of death, 1981-2025
# Script 8 of 9: the northern excess over time and the causes that make it up, by age band and sex
#
# Iain Buchan, 19 Nov 2026. Exploratory work in progress; not peer reviewed.
#
# Why
#   Scripts 1, 2 and 4 give each cause's share of the North-South gap for a few periods. This script
#   follows it through time, in the manner of Figure 1 of Leyland et al (BMC Public Health 2007;
#   7: 172): a stack whose height is the relative inequality and whose bands are causes of death,
#   here with time rather than age along the bottom and a panel for each broad age band.
#   To do so at every age on cause groups that do not overlap, it extends the series of script 2
#   (ages 25-49, on the definitions of the 2018 paper) to all ages.
#
# What it does (by sex; North v South including London)
#   1  Joins three sources into one series of deaths by cause, 1981-2025, for 19 age groups:
#        1981-2012   the ONS extract of 2017, except suicide, which is taken from the ONS extract of
#                    November 2026 (as in script 2, and for the reason given there)
#        2013-2025   NOMIS deaths rebuilt on the same definitions: ages 25-49 from the file used by
#                    script 2, the other ages from a second pull made on 19 November 2026
#      and writes out how far the sources agree wherever two of them cover the same deaths.
#      Cause groups, which do not overlap apart from two codes (I42.6 and Y15): cardiovascular,
#      diabetes, cancer, alcohol-related, drug-related, suicide, accidents (excluding accidental
#      poisoning by drugs and alcohol) and other causes, the remainder. Drug-related deaths are
#      identified from 1993: before the period 1993-95 they are within other causes. Suicide follows
#      the ONS definition: intentional self-harm from age 10, undetermined intent from age 15.
#   2  Pools deaths and populations over three-year moving periods, 1981-83 to 2023-25, each
#      plotted at its middle year, in four age bands (0-24, 25-49, 50-74, 75 and over).
#   3  For each period, age band and cause group, takes the gap in the directly standardised death
#      rate (North minus South; 2013 European Standard Population within the band) and expresses
#      it in two ways:
#        points  as a percentage of the South's death rate from all causes. The points of the
#                cause groups add up to the northern excess: the percentage by which the
#                standardised death rate from all causes in the North exceeds the South's.
#        share   as a percentage of the gap from all causes. The shares add up to 100.
#      A cause group with a lower rate in the North has negative points and a negative share.
#      The northern excess here comes from standardised rates, so that it divides exactly among
#      causes. The excess in script 4 and in the 2026 paper comes from a Poisson model; it is given
#      beside it for comparison and differs by about one percentage point.
#      Two memo lines sit outside the sum: drug-related deaths, alcohol-related deaths and suicide
#      together (from 1993-95), and deaths under 28 days at ages 0-24 (from 2013-15). NOMIS gives
#      deaths under 28 days no underlying cause, so they are within other causes.
#   4  95% intervals from 10,000 simulated sets of deaths: the deaths in each area, age group and
#      cause group are drawn as Poisson counts around those observed in the period, and the rates,
#      points and shares are recomputed each time. They allow for Poisson variation in the deaths
#      only. Moving periods overlap, so neighbouring estimates are not independent.
#   5  A share says little where causes pull in opposite directions and the gap is small: some
#      shares then exceed 100% and others fall below -100%. Shares are marked as not drawn where
#      the causes that narrow the gap offset more than a quarter of it. The points have no such
#      difficulty.
#   6  Three figures: ages 25-49 as points and as shares; the four age bands as points; and the
#      four age bands as shares.
#   Causes are coded to ICD-9 to 2000 and to ICD-10 from 2001, which can shift deaths between
#   groups at that point, more so at older ages.
#
# Checks made when the script runs
#   Sources: every cell they share is compared (see source_checks.csv), and the script stops if
#   causes defined alike differ by more than a few deaths in any cell.
#   At ages 25-49 the joined series is identical to the one written by script 2, and the gaps and
#   shares for 1993-95, 2014-16, 2020-22 and 2023-25 match those written by script 2.
#   The cause groups add up to all causes in every cell, the points to the northern excess and the
#   shares to 100.
#
# Data (in ./data)
#   deathsnorthsouthdivideengland1981to2016.xlsx           ONS extract of 2017, one sheet per cause
#   nomis_deaths_2018_definitions_by_sex.csv               NOMIS NM_161_1, 2013-2025, ages 25-49
#   nomis_deaths_2018_definitions_other_ages_by_sex.csv    the same for the other ages (North, South)
#   nomis_other_ages_check_2026_definitions.csv            NOMIS on the definitions of the 2026 extract, for a check
# Inputs (written to ./output by scripts 1 and 2)
#   cell_level_deaths_pop.csv                              the 2026 extract and the populations, as merged by script 1
#   like_for_like_cells.csv, like_for_like_periods.csv     script 2, for the checks at ages 25-49
#
# Outputs (written to ./output/excess_over_time)
#   source_checks.csv                     the sources against each other
#   deaths_by_cause_all_ages.csv          the joined series: deaths and population for every cell
#   excess_by_cause.csv                   rates, gaps, points and shares, with intervals, for each age band, sex and period
#   excess_by_cause_25_49                 .svg and .png
#   excess_by_cause_age_bands             .svg and .png
#   share_by_cause_age_bands              .svg and .png
#   excess_over_time_captions.txt         the captions, as text
#   The figures carry no title or caption, so that captions can be set as text beside them.
#
# Needs: readxl, dplyr, tidyr, readr, ggplot2 (3.4.0 or later), svglite (installed automatically if absent).
# About a minute.
# =====================================================================

pkgs <- c("readxl", "dplyr", "tidyr", "readr", "ggplot2", "svglite")
new  <- pkgs[!pkgs %in% rownames(installed.packages())]
if (length(new)) install.packages(new, repos = "https://cloud.r-project.org")
suppressPackageStartupMessages(invisible(lapply(pkgs, library, character.only = TRUE)))
stopifnot(packageVersion("ggplot2") >= "3.4.0")
if (!isTRUE(l10n_info()[["UTF-8"]]))                     # the minus signs and dashes in the labels need a UTF-8 locale
  for (loc in c("en_GB.UTF-8", "C.UTF-8", "en_US.UTF-8")) if (nzchar(suppressWarnings(Sys.setlocale("LC_CTYPE", loc)))) break

# ---- configuration ---------------------------------------------------
ext17_file <- file.path("data", "deathsnorthsouthdivideengland1981to2016.xlsx")
nomis_2549 <- file.path("data", "nomis_deaths_2018_definitions_by_sex.csv")
nomis_rest <- file.path("data", "nomis_deaths_2018_definitions_other_ages_by_sex.csv")
nomis_chk  <- file.path("data", "nomis_other_ages_check_2026_definitions.csv")
in_cells   <- file.path("output", "cell_level_deaths_pop.csv")
in_2549    <- file.path("output", "like_for_like_cells.csv")
in_period  <- file.path("output", "like_for_like_periods.csv")
outdir     <- file.path("output", "excess_over_time")
fig_font   <- "Arial"
half       <- 1                                          # years either side of the middle year: three-year moving periods
mids       <- 1982:2024                                  # middle years, for 1981-83 to 2023-25
first_nomis_year <- 2013
drug_from  <- 1993                                       # first year in which drug-related deaths are identified
ages  <- c("<1", "01-04", "05-09", "10-14", "15-19", "20-24", "25-29", "30-34", "35-39", "40-44", "45-49",
           "50-54", "55-59", "60-64", "65-69", "70-74", "75-79", "80-84", "85+")
age_cols <- setNames(ages, c("a00", "a01_04", "a05_09", "a10_14", "a15_19", "a20_24", "a25_29", "a30_34", "a35_39", "a40_44", "a45_49",
                             "a50_54", "a55_59", "a60_64", "a65_69", "a70_74", "a75_79", "a80_84", "a85plus"))      # as named in the NOMIS files
bands <- c("0-24", "25-49", "50-74", "75+")              # the age bands of script 1
groups <- c("CVD", "DIAB", "CANCER", "OTHER", "ALC18", "DRUG18", "SUI", "ACC18")          # bottom of the stack first
cause_words <- c(CVD = "Cardiovascular", DIAB = "Diabetes", CANCER = "Cancer", OTHER = "Other causes", ALC18 = "Alcohol-related",
                 DRUG18 = "Drug-related", SUI = "Suicide", ACC18 = "Accidents")
stack_order <- unname(cause_words)
# Colours from one documented palette, given to the causes so that bands which can touch stay apart for normal colour vision
# and with protan or deutan deficiency (OKLab distance x 100 of 9 or more between neighbours; Machado et al 2009)
cause_cols <- c("Cardiovascular" = "#6250D6", "Diabetes" = "#EB6834", "Cancer" = "#1BAF7A", "Other causes" = "#CFCEC6",
                "Alcohol-related" = "#EDA100", "Drug-related" = "#E87BA4", "Suicide" = "#008300", "Accidents" = "#2A78D6")
offset_limit <- 25                                       # shares are not drawn where causes that narrow the gap offset more than this percentage of it
together <- list("Drug-related, alcohol-related and suicide together" = c("DRUG18", "ALC18", "SUI"))      # a memo line from 1993-95; not part of the sum
memo_series <- c(NEONATAL = "Deaths under 28 days (within other causes)")                                  # a memo line from 2013-15; not part of the sum
n_sim <- 10000
seed  <- 20261119
ink <- "#0B0B0B"; ink2 <- "#52514E"; hair <- "#E1E0D9"
minus <- intToUtf8(8722); dash <- intToUtf8(8211); apos <- intToUtf8(8217)

for (f in c(ext17_file, nomis_2549, nomis_rest, nomis_chk)) if (!file.exists(f)) stop("Cannot find '", f, "' from ", getwd(), " - setwd() to the '2026 Causes' folder.")
if (!file.exists(in_cells)) stop("Run script 1 first: ", in_cells, " is missing.")
for (f in c(in_2549, in_period)) if (!file.exists(f)) stop("Run script 2 first: ", f, " is missing.")
dir.create(outdir, recursive = TRUE, showWarnings = FALSE)
span <- function(from, to) ifelse(from %/% 100 == to %/% 100, sprintf("%d-%02d", from, to %% 100), sprintf("%d-%d", from, to))      # 2023-25, 1999-2001
key <- c("area", "sex", "age", "year")
lookup <- function(x, map) unname(ifelse(x %in% names(map), map[x], x))

# ---- 0. the sources -----------------------------------------------------------------------------
# 0a. the ONS extract of 2017 (as script 2 reads it, here for every age group): one sheet per cause, a block of year
#     columns for males and another for females, 19 age-group rows for the North and 19 for the South (including London)
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
  age  <- trimws(body[, 2]); keep <- age %in% ages
  bind_rows(lapply(cols, function(j)
    tibble(cause = sheet, area = area[keep], sex = if (j < females_from) "M" else "F", age = age[keep],
           year = yr[j], deaths = as.numeric(body[keep, j])))) %>%
    filter(!is.na(deaths))                                           # suicide is blank below age 15
}
ext <- bind_rows(lapply(setdiff(excel_sheets(ext17_file), c("Notes and terms & conditions", "Definition", "Geography")), read_2017_sheet))
stopifnot(nrow(ext) == 25872, all(ext$area %in% c("North", "South")), all(ext$year %in% 1981:2016), !any(duplicated(ext[, c("cause", key)])))
ext <- ext %>% filter(cause != "Obesity") %>%
  transmute(series = lookup(cause, c("All causes" = "ALL", "Cardiovascular" = "CVD", "Diabetes" = "DIAB", "Accidents" = "ACC18", "Drug" = "DRUG18",
                                     "Suicide" = "SUI", "Alcohol" = "ALC18", "Cancer (exc breast)" = "CANCER", "Breast cancer" = "CANCER")), area, sex, age, year, deaths) %>%
  group_by(series, area, sex, age, year) %>% summarise(deaths = sum(deaths), .groups = "drop")         # cancer: the two sheets together

# 0b. the ONS extract of November 2026 and the populations, as script 1 merged them
x26 <- read_csv(in_cells, show_col_types = FALSE) %>% filter(area %in% c("North", "South"))
std <- x26 %>% distinct(age, w, band)                    # the standard population and the age bands, as script 1 wrote them
pop <- x26 %>% filter(cause == "All causes") %>% select(all_of(key), pop)
x26 <- x26 %>% select(cause, all_of(key), deaths)
stopifnot(nrow(std) == 19, identical(std$age, ages), setequal(std$band, bands), nrow(pop) == 2 * 2 * 19 * 45, !anyNA(pop$pop))

# 0c. NOMIS on the definitions of the 2017 extract: ages 25-49 (North, South excluding London, London) and the other ages (North, South)
read_nomis <- function(file) read_csv(file, show_col_types = FALSE) %>% pivot_longer(matches("^a[0-9]"), names_to = "age", values_to = "deaths") %>%
  mutate(age = unname(age_cols[age]), area = unname(c(N = "North", X = "South", L = "South", S = "South")[area]),
         series = lookup(series, c(CANCER_XB = "CANCER", BREAST = "CANCER"))) %>%
  group_by(series, area, sex, age, year) %>% summarise(deaths = sum(deaths), .groups = "drop")
nom <- bind_rows(read_nomis(nomis_2549), read_nomis(nomis_rest))
main <- c("ALL", setdiff(groups, "OTHER"))
stopifnot(!anyNA(nom), nrow(nom %>% filter(series %in% main)) == length(main) * 2 * 2 * 19 * (2025 - first_nomis_year + 1))

# 0d. the sources against each other, cell by cell (cell = area x sex x age group x year)
agreement <- function(check, m) m %>% group_by(check = check, series) %>%
  summarise(cells = n(), deaths_first = sum(first), deaths_second = sum(second), cells_differing = sum(first != second),
            largest_cell_difference = max(abs(first - second)), .groups = "drop") %>% mutate(difference = deaths_second - deaths_first)
alike <- c(ALL = "All causes", CVD = "Cardiovascular", DIAB = "Diabetes", SUI = "Suicide")
d26 <- x26 %>% filter(cause %in% alike) %>% transmute(series = names(alike)[match(cause, alike)], area, sex, age, year, deaths)
chk_a <- agreement("a. 2017 extract (first) against NOMIS (second), 2013-2016",
                   inner_join(ext %>% filter(year >= first_nomis_year, series != "SUI") %>% rename(first = deaths), nom %>% rename(second = deaths), by = c("series", key)))
chk_b <- agreement("b. 2026 extract (first) against NOMIS (second), 2013-2025, causes defined alike",
                   inner_join(d26 %>% rename(first = deaths), nom %>% rename(second = deaths), by = c("series", key)))
chk_c <- agreement(paste0("c. 2017 extract (first) against 2026 extract (second), 1981-", first_nomis_year - 1, ", causes defined alike"),
                   inner_join(ext %>% filter(year < first_nomis_year, series != "SUI") %>% rename(first = deaths), d26 %>% rename(second = deaths), by = c("series", key)))
chk_d <- agreement(paste0("d. suicide at ages 15 and over, 2017 extract (first) against 2026 extract (second), 1981-", first_nomis_year - 1, ": why the 2026 extract is used"),
                   inner_join(ext %>% filter(year < first_nomis_year, series == "SUI") %>% rename(first = deaths), d26 %>% rename(second = deaths), by = c("series", key)))
# accidents, alcohol-related deaths and drug poisoning on the definitions of the 2026 extract, ages outside 25-49: totals by year and by age group
other_ages <- setdiff(ages, ages[7:11])
margins <- bind_rows(lapply(strsplit(readLines(nomis_chk)[-1], ","), function(v)
  tibble(series = v[1], area = ifelse(v[2] == "N", "North", "South"), sex = v[3], by = v[4], key = if (v[4] == "year") as.character(first_nomis_year:2025) else other_ages, second = as.numeric(v[-(1:4)]))))
m26 <- x26 %>% filter(cause %in% unique(margins$series), year >= first_nomis_year, age %in% other_ages)
m26 <- bind_rows(m26 %>% group_by(series = cause, area, sex, key = as.character(year)) %>% summarise(first = sum(deaths), .groups = "drop") %>% mutate(by = "year"),
                 m26 %>% group_by(series = cause, area, sex, key = age) %>% summarise(first = sum(deaths), .groups = "drop") %>% mutate(by = "age"))
chk_e <- bind_rows(lapply(c("year", "age"), function(b) agreement(paste0("e. 2026 extract (first) against NOMIS on its definitions (second), 2013-2025, ages outside 25-49: totals by ", b,
                                                                           if (b == "age") " group" else "", ", area and sex"),
                                                                  inner_join(m26 %>% filter(by == b), margins %>% filter(by == b), by = c("series", "area", "sex", "by", "key")))))
checks <- bind_rows(chk_a, chk_b, chk_c, chk_d, chk_e)
stopifnot(nrow(chk_a) == 7, nrow(chk_b) == 4, nrow(chk_c) == 3, nrow(chk_e) == 6, all(chk_e$cells == 2 * 2 * ifelse(grepl("by year", chk_e$check), 13, 14)),
          all(bind_rows(chk_a, chk_b, chk_c)$largest_cell_difference <= 3),                       # causes defined alike: within a few deaths in every cell
          chk_b$cells_differing[chk_b$series == "SUI"] == 0,                                      # suicide: NOMIS with the ONS age rule is the 2026 extract exactly
          all(chk_e$cells_differing[chk_e$series != "Drug"] == 0), all(abs(chk_e$difference / chk_e$deaths_first) < 0.01))
write_csv(checks, file.path(outdir, "source_checks.csv"))

# 0e. one series: the 2017 extract to 2012 (suicide from the 2026 extract), NOMIS from 2013
src_early <- "ONS 2017 extract (suicide: 2026 extract)"
src_nomis <- "NOMIS NM_161_1"
cells <- bind_rows(ext %>% filter(year < first_nomis_year, series != "SUI") %>% mutate(source = src_early),
                   d26 %>% filter(year < first_nomis_year, series == "SUI") %>% mutate(source = src_early),
                   nom %>% filter(series %in% c(main, names(memo_series))) %>% mutate(source = src_nomis)) %>%
  pivot_wider(names_from = series, values_from = deaths) %>%
  mutate(NEONATAL = ifelse(year >= first_nomis_year, coalesce(NEONATAL, 0), NA),                  # NOMIS holds them at ages under 1 only
         OTHER_with_drug = ALL - (CVD + DIAB + CANCER + ALC18 + SUI + ACC18),                     # other causes before drug-related deaths were identified
         OTHER = OTHER_with_drug - DRUG18) %>%
  left_join(pop, by = key) %>% left_join(std, by = "age") %>% arrange(area, sex, match(age, ages), year)
late <- cells$year >= drug_from
stopifnot(nrow(cells) == 2 * 2 * 19 * 45, !anyNA(cells[, c("ALL", "CVD", "DIAB", "CANCER", "ALC18", "SUI", "ACC18", "pop", "w")]),
          all(is.na(cells$DRUG18[!late])), !anyNA(cells$DRUG18[late]), all(cells$OTHER_with_drug >= 0), all(cells$OTHER[late] >= 0),
          all(cells$NEONATAL[cells$age != "<1"] == 0, na.rm = TRUE), all(cells$NEONATAL <= cells$OTHER, na.rm = TRUE))
# at ages 25-49 this is the series of script 2
s2 <- read_csv(in_2549, show_col_types = FALSE) %>% filter(area %in% c("North", "South"), series %in% c(main, "OTHER18")) %>%
  transmute(area, sex, age, year, series = ifelse(series == "OTHER18", "OTHER", series), d, p) %>%
  inner_join(cells %>% select(all_of(key), pop, all_of(c(main, "OTHER"))) %>% pivot_longer(all_of(c(main, "OTHER")), names_to = "series", values_to = "here"), by = c(key, "series"))
stopifnot(nrow(s2) == 9 * 2 * 2 * 5 * 45, all(s2$p == s2$pop), identical(is.na(s2$d), is.na(s2$here)), all(s2$d == s2$here, na.rm = TRUE))
write_csv(cells %>% transmute(area, sex, age, year, source, population = pop, all_causes = ALL, cardiovascular = CVD, diabetes = DIAB, cancer = CANCER, alcohol_related = ALC18,
                              drug_related = DRUG18, suicide = SUI, accidents = ACC18, other_causes = ifelse(late, OTHER, OTHER_with_drug), under_28_days = NEONATAL),
          file.path(outdir, "deaths_by_cause_all_ages.csv"), na = "")
shape <- function(d, cols) d %>% select(sex, band, area, age, year, pop, all_of(cols)) %>% pivot_longer(all_of(cols), names_to = "cause", values_to = "deaths")
cells_early <- shape(cells %>% mutate(OTHER = OTHER_with_drug), setdiff(groups, "DRUG18"))        # seven groups: drug-related deaths within other causes
cells_late  <- shape(cells[late, ], groups)                                                       # eight groups
cells_memo  <- shape(cells %>% filter(year >= first_nomis_year), names(memo_series))
stopifnot(!anyNA(cells_early$deaths), !anyNA(cells_late$deaths), !anyNA(cells_memo$deaths))

# ---- 1. the excess and its parts in one period ------------------------------------------------
z975 <- qnorm(0.975)
parts <- function(z, causes, x = NULL) {                 # z: pooled deaths and person-years by area, age group and cause, for one sex, age band and period; x: the same for memo series
  ages_in <- ages[ages %in% z$age]; n_age <- length(ages_in); n_cause <- length(causes)
  k <- 1e5 * std$w[match(ages_in, std$age)] / sum(std$w[match(ages_in, std$age)])          # weights of the standard population, to a rate per 100,000
  grid <- function(ar, what) { v <- z[z$area == ar, ]; stopifnot(nrow(v) == n_age * n_cause)
    matrix(v[[what]][order(match(v$cause, causes), match(v$age, ages_in))], n_age, n_cause) }
  dN <- grid("North", "deaths"); dS <- grid("South", "deaths"); pN <- grid("North", "pop")[, 1]; pS <- grid("South", "pop")[, 1]
  toN <- diag(n_cause) %x% matrix(k / pN, ncol = 1); toS <- diag(n_cause) %x% matrix(k / pS, ncol = 1)      # deaths (age within cause) to standardised rates by cause
  rN <- as.vector(as.vector(dN) %*% toN); rS <- as.vector(as.vector(dS) %*% toS)                            # standardised rates by cause
  draw <- function(d) matrix(rpois(n_sim * length(d), rep(as.vector(d), each = n_sim)), n_sim)              # simulated deaths, one row for each set
  sN <- draw(dN) %*% toN; sS <- draw(dS) %*% toS
  south <- sum(rS); whole <- sum(rN - rS); sim_south <- rowSums(sS); sim_whole <- rowSums(sN - sS)          # the South's rate and the gap, from all causes
  lim <- function(v, p) quantile(v, p, names = FALSE)
  line <- function(name, kind, n, s, g, sim_g) data.frame(cause = name, kind = kind, deaths_north = n, deaths_south = s, rate_north = g[1], rate_south = g[2], gap = g[1] - g[2],
    points = 100 * (g[1] - g[2]) / south, points_lo = lim(100 * sim_g / sim_south, 0.025), points_hi = lim(100 * sim_g / sim_south, 0.975),
    share = 100 * (g[1] - g[2]) / whole, share_lo = if (kind == "all") NA else lim(100 * sim_g / sim_whole, 0.025), share_hi = if (kind == "all") NA else lim(100 * sim_g / sim_whole, 0.975))
  of <- function(name, kind, j) line(name, kind, sum(dN[, j]), sum(dS[, j]), c(sum(rN[j]), sum(rS[j])), rowSums(sN[, j, drop = FALSE] - sS[, j, drop = FALSE]))      # j: one group, every group, or several together
  both <- together[vapply(together, function(m) all(m %in% causes), logical(1))]
  out <- c(lapply(seq_len(n_cause), function(j) of(causes[j], "group", j)), list(of("All causes", "all", seq_len(n_cause))),
           lapply(names(both), function(m) of(m, "memo", match(both[[m]], causes))))
  for (m in unique(x$cause)) {                           # memo series held apart from the groups: their own deaths, drawn separately
    e <- x[x$cause == m, ]; eN <- e$deaths[e$area == "North"][match(ages_in, e$age[e$area == "North"])]; eS <- e$deaths[e$area == "South"][match(ages_in, e$age[e$area == "South"])]
    stopifnot(!anyNA(c(eN, eS)))
    if (sum(eN, eS) > 0) out <- c(out, list(line(m, "memo", sum(eN), sum(eS), c(sum(eN * k / pN), sum(eS * k / pS)),
                                                 as.vector(matrix(rpois(n_sim * n_age, rep(eN, each = n_sim)), n_sim) %*% (k / pN) - matrix(rpois(n_sim * n_age, rep(eS, each = n_sim)), n_sim) %*% (k / pS)))))
  }
  model <- glm(deaths ~ north + age, family = poisson, offset = log(pop),                  # the model of the 2026 paper, for comparison
               data = data.frame(deaths = c(rowSums(dN), rowSums(dS)), pop = c(pN, pS), north = rep(1:0, each = n_age), age = factor(rep(seq_len(n_age), 2))))
  bind_rows(out) %>% mutate(model_excess = ifelse(kind == "all", 100 * (exp(coef(model)[["north"]]) - 1), NA))
}
run <- function(cells, causes, mids_in, memo = NULL) bind_rows(lapply(mids_in, function(mid) {
  pool <- function(d) d %>% filter(year >= mid - half, year <= mid + half) %>% group_by(sex, band, area, age, cause) %>% summarise(years = n(), deaths = sum(deaths), pop = sum(pop), .groups = "drop")
  pooled <- pool(cells); stopifnot(all(pooled$years == 2 * half + 1))
  px <- if (is.null(memo)) pooled[0, ] else pool(memo) %>% filter(years == 2 * half + 1)                    # memo series only where every year of the period has them
  pooled %>% group_by(sex, band) %>% group_modify(~ {
    set.seed(seed + 100 * mid + 10 * match(.y$band, bands) + (.y$sex == "M"))                              # its own random numbers for each period, age band and sex,
    parts(.x, causes, px[px$sex == .y$sex & px$band == .y$band, ]) }) %>% ungroup() %>%                    # so that no result depends on what else is computed
    mutate(mid_year = mid, period = span(mid - half, mid + half))
}))
res <- bind_rows(run(cells_early, setdiff(groups, "DRUG18"), mids[mids - half < drug_from]),                  # periods that start before 1993: seven groups
                 run(cells_late, groups, mids[mids - half >= drug_from], cells_memo)) %>%                    # 1993-95 onwards: eight groups
  group_by(sex, band, mid_year) %>%
  mutate(offset = -sum(pmin(share[kind == "group"], 0)), share_drawn = offset <= offset_limit) %>% ungroup() %>%      # offset: how much of the gap is cancelled by causes that narrow it, %
  mutate(sex = ifelse(sex == "M", "men", "women"), words = ifelse(kind == "group", cause_words[cause], lookup(cause, memo_series)))

# ---- 2. checks ----------------------------------------------------------------------------------
s <- res %>% filter(kind == "group") %>% group_by(sex, band, mid_year) %>% summarise(points = sum(points), share = sum(share), .groups = "drop") %>%
  left_join(res %>% filter(kind == "all") %>% select(sex, band, mid_year, excess = points), by = c("sex", "band", "mid_year"))
stopifnot(nrow(s) == 2 * 4 * length(mids), max(abs(s$points - s$excess)) < 1e-8, max(abs(s$share - 100)) < 1e-8)      # the points add up to the excess and the shares to 100
ref <- read_csv(in_period, show_col_types = FALSE) %>% filter(comparison == "N_vs_S", band == "25-49", sex %in% c("M", "F"), years == 2 * half + 1) %>%
  transmute(sex = ifelse(sex == "M", "men", "women"), period, cause = ifelse(series == "OTHER18", "OTHER", ifelse(series == "ALL", "All causes", series)), gap2 = gap, share2 = pct_of_allcause_gap) %>%
  inner_join(res %>% filter(band == "25-49") %>% select(sex, period, cause, gap, share), by = c("sex", "period", "cause"))
stopifnot(nrow(ref) == 2 * 4 * 9, max(abs(ref$gap - ref$gap2)) < 1e-5, max(abs(ref$share - ref$share2)) < 1e-4)        # four three-year periods as script 2 wrote them

# ---- 3. the numbers -------------------------------------------------------------------------------
fx <- function(v, d) ifelse(is.na(v), NA_character_, formatC(round(v, d) + 0, format = "f", digits = d))      # numbers for the file, as text with fixed decimals: no 1e-04 and no -0
write_csv(res %>% mutate(ages = ifelse(band == "75+", "75 and over", band), cause = factor(words, c("All causes", rev(stack_order), names(together), unname(memo_series)))) %>%
            arrange(desc(sex), match(band, bands), mid_year, cause) %>%
            transmute(sex, ages, period, mid_year, cause, deaths_north, deaths_south, rate_north = fx(rate_north, 4), rate_south = fx(rate_south, 4), gap = fx(gap, 4),
                      excess_points = fx(points, 2), points_lo = fx(points_lo, 2), points_hi = fx(points_hi, 2), share_of_gap_pct = fx(share, 1), share_lo = fx(share_lo, 1), share_hi = fx(share_hi, 1),
                      share_drawn = ifelse(share_drawn, "yes", "no"), model_excess_pct = fx(model_excess, 2)),
          file.path(outdir, "excess_by_cause.csv"), na = "")

# ---- 4. figures ---------------------------------------------------------------------------------
# All text in Arial. Where Arial is not installed (Linux), the metric-compatible Liberation Sans
# is registered under that name for drawing, and the SVG still names Arial.
if (.Platform$OS.type == "windows") grDevices::windowsFonts(Arial = grDevices::windowsFont("Arial"))
if (requireNamespace("systemfonts", quietly = TRUE) && !any(systemfonts::system_fonts()$family == fig_font)) {
  lib  <- systemfonts::system_fonts() %>% filter(family == "Liberation Sans")
  pick <- function(sty) { f <- lib$path[lib$style == sty]; if (length(f)) f[1] else lib$path[1] }
  if (nrow(lib)) systemfonts::register_font(fig_font, plain = pick("Regular"), bold = pick("Bold"),
                                            italic = pick("Italic"), bolditalic = pick("Bold Italic"))
}
# svglite styles its shapes through a style sheet inside the file. Word and some other programs
# draw SVG without reading style sheets, so every style is written here as plain attributes on
# the element it applies to, and the style sheet is removed (as in scripts 4 to 7).
plain_svg <- function(file) {
  lines <- readLines(file, warn = FALSE, encoding = "UTF-8")
  defaults <- c(fill = "none", stroke = "#000000", "stroke-linecap" = "round", "stroke-linejoin" = "round")
  m  <- gregexpr("<(line|polyline|polygon|path|rect|circle|text)\\b[^>]*>", lines, perl = TRUE)
  el <- regmatches(lines, m)                                      # the elements on each line
  e  <- unlist(el)
  i  <- grepl("\\sstyle='[^']*'", e, perl = TRUE)                 # the clipping rectangles carry no style
  a  <- gsub("\"", "", sub("^.*?\\sstyle='([^']*)'.*$", "\\1", e[i], perl = TRUE), fixed = TRUE)
  a  <- trimws(gsub("\\s*([^:;]+?)\\s*:\\s*([^;]*?)\\s*(;|$)", "\\1='\\2' ", a, perl = TRUE))      # 'key: value;' becomes key='value'
  a  <- sub("font-family='[^']*'", paste0("font-family='", fig_font, "'"), a)
  shape <- !grepl("^<text", e[i])
  for (d in names(defaults)) {
    add <- shape & !grepl(paste0("(^| )", d, "='"), a)
    a[add] <- paste0(a[add], " ", d, "='", defaults[[d]], "'")
  }
  bare <- sub("\\s*style='[^']*'", "", e[i], perl = TRUE)
  end  <- regmatches(bare, regexpr("\\s*/?>$", bare, perl = TRUE))
  e[i] <- paste0(substr(bare, 1, nchar(bare) - nchar(end)), " ", a, end)
  regmatches(lines, m) <- relist(e, el)
  s <- paste(lines, collapse = "\n")
  s <- sub("(?s)<defs>\\s*<style\\b.*?</style>\\s*</defs>\\n?", "", s, perl = TRUE)
  ids <- unique(regmatches(s, gregexpr("(?<=<clipPath id=')[^']+", s, perl = TRUE))[[1]])     # plain names for the clipping paths
  for (k in seq_along(ids)) {
    s <- gsub(paste0("id='", ids[k], "'"), paste0("id='clip", k, "'"), s, fixed = TRUE)
    s <- gsub(paste0("url(#", ids[k], ")"), paste0("url(#clip", k, ")"), s, fixed = TRUE)
  }
  stopifnot(!grepl("\\sstyle=", s, perl = TRUE), !grepl("<style", s, fixed = TRUE))
  writeLines(s, file, useBytes = TRUE)
}
save_fig <- function(p, name, w, h) {
  svg_file <- file.path(outdir, paste0(name, ".svg"))
  ggsave(svg_file, p, device = svglite::svglite, width = w, height = h)
  plain_svg(svg_file)
  ggsave(file.path(outdir, paste0(name, ".png")), p, width = w, height = h, dpi = 200, bg = "white")
}
base_theme <- theme_minimal(base_size = 9, base_family = fig_font) +
  theme(text = element_text(colour = ink), axis.text = element_text(colour = ink2), panel.grid.minor = element_blank(),
        panel.grid.major = element_line(colour = hair, linewidth = 0.25), strip.text = element_text(face = "bold", hjust = 0, colour = ink),
        legend.position = "top", legend.justification = "left", legend.title = element_blank(), plot.margin = margin(6, 10, 6, 6))
span_label <- function(v) sub("-", dash, v, fixed = TRUE)

# A stack that steps from one period to the next. Causes that add to the excess are stacked above zero and causes
# that narrow it below zero, in the same order. A run of years ends where years are left out.
steps <- function(d) d %>% uncount(2, .id = "edge") %>% mutate(x = mid_year + ifelse(edge == 1, -0.4999, 0.4999))      # each period spans its middle year
stack <- function(d) {                                   # d: row, col, mid_year, cause (bottom of the stack first) and v, the width of its band
  d <- d %>% arrange(row, col, mid_year, cause) %>% group_by(row, col, mid_year) %>%
    mutate(top = cumsum(pmax(v, 0)), base = top - pmax(v, 0), low = cumsum(pmin(v, 0)), roof = low - pmin(v, 0)) %>%
    group_by(row, col, cause) %>% mutate(run = cumsum(c(TRUE, diff(mid_year) > 1))) %>% ungroup()
  bind_rows(d %>% transmute(row, col, cause, run, side = "adds", mid_year, ymin = base, ymax = top),
            d %>% transmute(row, col, cause, run, side = "narrows", mid_year, ymin = low, ymax = roof)) %>%
    group_by(row, col, cause, run, side) %>% filter(any(ymax > ymin)) %>% ungroup() %>% steps()      # a cause that never narrows the gap has no band below zero
}
gaps_in <- function(d) d %>% arrange(row, col, mid_year) %>% group_by(row, col) %>% mutate(run = cumsum(c(TRUE, diff(mid_year) > 1))) %>%      # spans of years that are not drawn
  group_by(row, col, run) %>% summarise(x = mean(range(mid_year)), from = min(mid_year), to = max(mid_year), .groups = "drop")
sexes <- c("women" = "Women", "men" = "Men")
band_words <- c("0-24" = paste0("Ages 0", dash, "24"), "25-49" = paste0("Ages 25", dash, "49"), "50-74" = paste0("Ages 50", dash, "74"), "75+" = "Ages 75 and over")
y_points <- paste0("Northern excess, % of the South", apos, "s death rate")
y_share  <- paste0("Share of the North", dash, "South gap, %")
signed   <- function(v) sub("-", minus, as.character(v), fixed = TRUE)
key_words <- setNames(stack_order, stack_order); key_words[["Accidents"]] <- "Accidents, without drug\nor alcohol poisoning"
break_at <- drug_from + half - 0.5                       # drug-related deaths are a group of their own from the period 1993-95
layers <- function(st) list(
  geom_ribbon(data = st, aes(x, ymin = ymin, ymax = ymax, fill = cause, group = interaction(cause, side, run))),
  geom_vline(xintercept = break_at, colour = "white", linewidth = 0.6),
  geom_hline(yintercept = 0, colour = ink2, linewidth = 0.3),
  scale_fill_manual(values = cause_cols, labels = key_words),
  scale_linetype_manual(values = c("All causes" = "solid")),
  labs(x = NULL), base_theme, theme(panel.grid.major.x = element_blank(), panel.spacing = unit(0.5, "cm"), legend.key.height = unit(0.4, "cm"), legend.key.width = unit(0.45, "cm")))
net_line <- function(d) geom_line(data = steps(d), aes(x, v, linetype = "All causes"), colour = ink, linewidth = 0.4)
not_shown <- function(d, upright = FALSE) geom_text(data = d, aes(x, 50, label = if (upright) "Not shown" else "Not\nshown"), angle = if (upright) 90 else 0, size = 7 / .pt, lineheight = 0.95, family = fig_font, colour = ink2)
drawn <- res %>% mutate(cause = factor(words, stack_order))

# 4a. ages 25-49: the excess in points (above) and in shares (below)
rows9 <- c(points = paste0("Northern excess,\n% of the South", apos, "s death rate"), share = paste0("Share of the\nNorth", dash, "South gap, %"))      # the panel labels serve as axis titles
in_row <- function(d, r) d %>% mutate(row = factor(rows9[[r]], rows9))
f <- drawn %>% filter(band == "25-49") %>% mutate(col = factor(sexes[sex], sexes))
hid9 <- gaps_in(in_row(f %>% filter(kind == "all", !share_drawn), "share"))
p9 <- ggplot() + layers(stack(bind_rows(in_row(f %>% filter(kind == "group") %>% mutate(v = points), "points"), in_row(f %>% filter(kind == "group", share_drawn) %>% mutate(v = share), "share")))) +
  geom_hline(data = in_row(data.frame(yintercept = 100), "share"), aes(yintercept = yintercept), colour = ink2, linewidth = 0.3) + net_line(in_row(f %>% filter(kind == "all") %>% mutate(v = points), "points")) +
  facet_grid(row ~ col, scales = "free_y", switch = "y") +
  scale_x_continuous(breaks = seq(1985, 2020, 5), expand = expansion(add = 0.5)) +
  scale_y_continuous(breaks = function(lim) if (lim[2] > 80) seq(0, 100, 25) else seq(-10, 60, 10), labels = signed) +
  guides(fill = guide_legend(reverse = TRUE, order = 1), linetype = guide_legend(order = 2)) + labs(y = NULL) +
  theme(strip.placement = "outside", strip.text.y.left = element_text(face = "plain", size = 9, hjust = 0.5, angle = 90, lineheight = 1), legend.position = "right", legend.justification = "center")
if (nrow(hid9)) p9 <- p9 + not_shown(hid9)
save_fig(p9, "excess_by_cause_25_49", 7.5, 5.6)

# 4b and 4c. the four age bands: points, then shares
g <- drawn %>% mutate(row = factor(sexes[sex], sexes), col = factor(band_words[band], band_words))
wide <- list(facet_grid(row ~ col), scale_x_continuous(breaks = seq(1990, 2020, 10), expand = expansion(add = 0.5)),
             guides(fill = guide_legend(nrow = 2, byrow = TRUE, order = 1), linetype = guide_legend(order = 2)), theme(strip.text.y = element_text(hjust = 0.5)))
p10 <- ggplot() + layers(stack(g %>% filter(kind == "group") %>% mutate(v = points))) + net_line(g %>% filter(kind == "all") %>% mutate(v = points)) + wide +
  scale_y_continuous(breaks = seq(-10, 60, 10), labels = signed) + labs(y = y_points)
save_fig(p10, "excess_by_cause_age_bands", 7.5, 4.7)
hid11 <- gaps_in(g %>% filter(kind == "all", !share_drawn))
p11 <- ggplot() + layers(stack(g %>% filter(kind == "group", share_drawn) %>% mutate(v = share))) + geom_hline(yintercept = 100, colour = ink2, linewidth = 0.3) + wide +
  scale_y_continuous(breaks = seq(0, 100, 25), labels = signed) + labs(y = y_share)
if (nrow(hid11)) p11 <- p11 + not_shown(hid11, upright = TRUE)
save_fig(p11, "share_by_cause_age_bands", 7.5, 4.7)

# ---- 5. captions, as text -------------------------------------------------------------------------
rng <- function(v) gsub("-", dash, v, fixed = TRUE)
hidden_words <- function(r, with_ages) {                 # which shares are not shown, in words
  h <- r %>% filter(kind == "all", !share_drawn) %>% group_by(sex, band) %>% summarise(from = min(mid_year), to = max(mid_year), .groups = "drop")
  if (!nrow(h)) return("No shares are left out by this rule.")
  paste0("This leaves out ", paste(sprintf("%s%s from %s to %s", h$sex, if (with_ages) paste(" aged", rng(ifelse(h$band == "75+", "75 and over", h$band))) else "",
                                           rng(span(h$from - half, h$from + half)), rng(span(h$to - half, h$to + half))), collapse = "; "), ".")
}
moving <- paste0("Three-year moving periods, from 1981", dash, "83 to 2023", dash, "25, each drawn at its middle year.")
rule <- sprintf("Shares are not shown where the causes that narrow the gap offset more than %d%% of it.", offset_limit)
causes_note <- paste("Cause groups are those of the 2018 paper: accidental poisoning by drugs and alcohol is counted with drug-related and alcohol-related deaths, not with accidents.",
                     "The groups do not overlap, apart from two codes that are counted twice (I42.6 in cardiovascular and alcohol-related deaths, Y15 in suicide and alcohol-related deaths).",
                     paste0("The white break falls where drug-related deaths are first identified (1993): before the period 1993", dash, "95 they are within other causes."))
src <- paste0("Sources: deaths registered, from the ONS extract of 2017 for 1981", dash, "2012 (suicide from the ONS extract of November 2026) and from NOMIS for 2013", dash, "2025; ONS mid-year population estimates.")
captions <- c(
  paste0("Figure 9. The northern excess in death rates at ages 25", dash, "49 and the causes that make it up, by sex, 1981", dash, "2025"),
  paste("Upper panels: the percentage by which the directly age-standardised death rate in the North exceeds that in the South (including London), divided among causes of death.",
        "The line is the excess from all causes; a cause with a lower rate in the North is drawn below zero. Lower panels: the same as shares of the gap between North and South.",
        moving, rule, hidden_words(res %>% filter(band == "25-49"), FALSE), causes_note, "The 95% intervals are in excess_by_cause.csv.", src),
  "",
  paste0("Figure 10. The northern excess in death rates and the causes that make it up, by age band and sex, 1981", dash, "2025"),
  paste("The percentage by which the directly age-standardised death rate in the North exceeds that in the South (including London), divided among causes of death.",
        "The line is the excess from all causes; a cause with a lower rate in the North is drawn below zero.", moving, causes_note, "The 95% intervals are in excess_by_cause.csv.", src),
  "",
  paste0("Figure 11. Shares of the North", dash, "South gap in death rates by cause, by age band and sex, 1981", dash, "2025"),
  paste("Each cause group as a percentage of the gap between North and South (including London) in the directly age-standardised death rate from all causes; a cause with a lower rate in the North is drawn below zero.",
        moving, rule, hidden_words(res, TRUE), causes_note, "The 95% intervals are in excess_by_cause.csv.", src))
writeLines(enc2utf8(captions), file.path(outdir, "excess_over_time_captions.txt"), useBytes = TRUE)

# ---- 6. summary -------------------------------------------------------------------------------------
cat("\nThe northern excess over time, by cause, written to", normalizePath(outdir), "\n")
cat("\nThe sources against each other (deaths in the first and the second, cells that differ, largest difference in a cell)\n")
print(as.data.frame(checks %>% transmute(check = substr(check, 1, 2), series, cells, deaths_first, deaths_second, cells_differing, largest = largest_cell_difference)), row.names = FALSE)
all_rows <- res %>% filter(kind == "all")
cat(sprintf("\nNorthern excess from standardised rates against the Poisson model of the 2026 paper: they differ by %.1f percentage points at most (%.1f on average)\n",
            max(abs(all_rows$points - all_rows$model_excess)), mean(abs(all_rows$points - all_rows$model_excess))))
cat("Shares not drawn (causes that narrow the gap offset more than", offset_limit, "% of it):", hidden_words(res, TRUE), "\n")
show <- function(b, p) res %>% filter(band == b, period == p) %>% mutate(cause = factor(words, c("All causes", rev(stack_order), names(together), unname(memo_series)))) %>% arrange(desc(sex), cause) %>%
  transmute(sex, cause, gap = sprintf("%.2f", gap), points = sprintf("%.1f (%.1f to %.1f)", points, points_lo, points_hi), share = ifelse(is.na(share_lo), "", sprintf("%.1f (%.1f to %.1f)", share, share_lo, share_hi)))
for (b in bands) { cat("\nAges", b, ", 2023-25: gap per 100,000, points of the northern excess and share of the gap, % (95% interval)\n"); print(as.data.frame(show(b, "2023-25")), row.names = FALSE) }
