# =====================================================================
# North-South divide in English mortality by cause of death, 1981-2025
# Script 9 of 9: inside the cause groups - what 'other causes' hold at each age, infant deaths,
#                the cancers behind the cancer gap, and the deaths that two cause groups share
#
# Iain Buchan, 20 Nov 2026. Exploratory work in progress; not peer reviewed.
#
# Why
#   Script 8 shows that 'other causes' are the largest part of the North-South gap under 25 and at
#   75 and over, and a large part of it at 50-74. This script looks inside them and settles three
#   related points, all for persons, from public NOMIS counts (table NM_161_1):
#   A. 'Other causes' (2018 definitions) in four age bands, 2015-19 and 2021-25, divided among eight
#      further groups proposed for the next extract of deaths - respiratory; digestive, excluding the
#      alcohol codes; dementia and Alzheimer's disease; other mental, behavioural and neurological;
#      genitourinary; ill-defined causes; other external causes; COVID-19 - and what they leave unnamed.
#   B. Infant deaths within the gap under 25; and, at 50-74 and at 75 and over, the cancers and the
#      respiratory disease behind the gap, with diabetes. 2021-25.
#   C. The deaths that two cause groups share, on the groups of the 2018 paper and on those of the ONS
#      extract of November 2026, and so what a stacked chart of either would count twice.
#
# Data (in ./data)
#   nomis_deaths_by_category_pooled.csv          deaths under 75, persons, pooled over 2015-19, 2020-24 and
#                                                2021-25, by series, area and age group (as in script 3)
#   nomis_deaths_by_category_pooled_75plus.csv   the same series for ages 75-79, 80-84, 85-89 and 90 and over
#   nomis_deaths_2018_definitions_by_sex.csv     deaths at ages 25-49 by sex and year, 2013-2025, with the codes
#                                                that sit in two groups held as series of their own (as in script 2)
#   nomis_pop_nsl_1981_2025.csv                  ONS mid-year population estimates by sex and 19 age groups
#   Areas: N = North, X = South excluding London, L = London; South = X + L throughout.
#
# Method notes
#   * Rates are directly standardised to the 2013 European Standard Population within each age band, per
#     100,000, with deaths and populations pooled over the years of a period. Gap = North minus South.
#   * A share is the gap in one group of causes as a percentage of the gap it is part of.
#   * 'Other causes (2018 definitions)' here are the sum of 19 categories that do not overlap: all causes
#     less cardiovascular disease, diabetes, cancer, accidents, suicide, the alcohol codes and mental
#     disorders due to drug use. Script 8 takes its 'other causes' as a remainder instead, which differs
#     by the two codes that it counts twice (part C).
#   * Deaths under 28 days carry no underlying cause in these statistics, so they are a category of their own.
#   * Intervals are 95%, from 10,000 simulations in which every count of deaths is drawn from a Poisson
#     distribution about the count observed. They allow for nothing else. Each part sets its own seed.
#   * Part C, extract of November 2026: the six groups are accidents (V01-X59), drug poisoning, alcohol-related
#     deaths, suicide, cardiovascular disease and diabetes. A death is in two of them if it is an accidental
#     drug poisoning (X40-X44), an intentional or undetermined drug poisoning (X60-X64, Y10-Y14), or one of the
#     alcohol codes X45, X65, Y15 and I42.6. On the 2018 definitions only I42.6 and Y15 are in two groups.
#     Outside ages 25-49 the single codes are not held; there the figure for the November groups includes
#     assault by drugs (X85, a handful of deaths), and the four alcohol codes together are an upper limit
#     for the two codes of the 2018 definitions.
#
# Checks made here
#   1. The categories add up to all causes in every age group, area and period, in both files of pooled counts.
#   2. The shares in each table add up to 100.
#
# Outputs (written to ./output/inside_the_cause_groups)
#   other_causes_by_age_band.csv        part A: every group and what is left, by age band and period
#   infant_deaths_and_cancers.csv       part B: each estimate with its interval
#   overlap_between_cause_groups.csv    part C: deaths in two groups and the share of the gap counted twice
#
# Needs: dplyr, tidyr, readr (installed automatically if absent). A few seconds.
# =====================================================================

pkgs <- c("dplyr", "tidyr", "readr")
new  <- pkgs[!pkgs %in% rownames(installed.packages())]
if (length(new)) install.packages(new)
invisible(lapply(pkgs, library, character.only = TRUE))
if (packageVersion("dplyr") < "1.1.0") stop("dplyr 1.1.0 or later is needed - please update it.")

# ---- configuration ---------------------------------------------------
young_file <- file.path("data", "nomis_deaths_by_category_pooled.csv")
old_file   <- file.path("data", "nomis_deaths_by_category_pooled_75plus.csv")
sex_file   <- file.path("data", "nomis_deaths_2018_definitions_by_sex.csv")
pop_file   <- file.path("data", "nomis_pop_nsl_1981_2025.csv")
outdir     <- file.path("output", "inside_the_cause_groups")
for (f in c(young_file, old_file, sex_file, pop_file))
  if (!file.exists(f)) stop("Cannot find '", f, "' from ", getwd(), " - setwd() to the '2026 Causes' folder.")
dir.create(outdir, recursive = TRUE, showWarnings = FALSE)

n_sim <- 10000
esp   <- c(1000, 4000, 5500, 5500, 5500, 6000, 6000, 6500, 7000, 7000, 7000, 7000, 6500, 6000, 5500, 5000, 4000, 2500, 2500)      # 2013 European Standard Population, 19 age groups, 85 and over last
young <- c("a00", "a01_04", "a05_09", "a10_14", "a15_19", "a20_24", "a25_29", "a30_34", "a35_39", "a40_44", "a45_49", "a50_54", "a55_59", "a60_64", "a65_69", "a70_74")
bands <- list("0-24" = 1:6, "25-49" = 7:11, "50-74" = 12:16, "75 and over" = 17:19)
q     <- function(v) quantile(v, c(0.025, 0.975), names = FALSE)

# the 19 categories of 'other causes (2018 definitions)', in the eight further groups and what is left
other18 <- c("NEO_OTH", "RESP", "DEMENTIA", "COVID", "LIVER_NONALC", "DIGEST_OTH", "MENT_OTH", "NERVOUS", "INFECT", "ENDO_NONDIAB",
             "BLOOD", "GU", "MSK_SKIN", "PERI_CONG_PREG", "ILLDEF", "EXT_OTH", "U_OTH", "EYE_EAR", "NEONATAL")
further <- list("Respiratory" = "RESP", "Digestive, excluding alcohol codes" = c("LIVER_NONALC", "DIGEST_OTH"), "Dementia and Alzheimer's disease" = "DEMENTIA",
                "Other mental, behavioural and neurological" = c("MENT_OTH", "NERVOUS"), "Genitourinary" = "GU", "Ill-defined causes" = "ILLDEF",
                "Other external causes" = "EXT_OTH", "COVID-19" = "COVID")
rest    <- list("Deaths under 28 days" = "NEONATAL", "Perinatal, congenital and pregnancy" = "PERI_CONG_PREG", "Endocrine other than diabetes" = "ENDO_NONDIAB",
                "Infectious and parasitic" = "INFECT", "Skin and musculoskeletal" = "MSK_SKIN", "Everything else" = c("NEO_OTH", "BLOOD", "U_OTH", "EYE_EAR"))
named   <- c("CVD", "DIAB", "ACC", "SUI", "ALC_EXCL", "CANCER", "MENT_DRUG")      # with the 19 categories these add up to all causes
stopifnot(setequal(unlist(c(further, rest)), other18))

# ---- data ------------------------------------------------------------
lo <- read_csv(young_file, show_col_types = FALSE)
hi <- read_csv(old_file, show_col_types = FALSE)
stopifnot(!anyNA(lo), !anyNA(hi), !any(duplicated(lo[, 1:3])), !any(duplicated(hi[, 1:3])),
          setequal(paste(lo$period, lo$series, lo$area), paste(hi$period, hi$series, hi$area)))
pooled <- bind_rows(lo %>% pivot_longer(all_of(young), names_to = "age", values_to = "d") %>% mutate(age = match(age, young)),
                    hi %>% mutate(a85plus = a85_89 + a90plus) %>% select(-a85_89, -a90plus) %>%
                      pivot_longer(c(a75_79, a80_84, a85plus), names_to = "age", values_to = "d") %>% mutate(age = match(age, c("a75_79", "a80_84", "a85plus")) + 16L))
# check 1: the categories add up to all causes in every age group, area and period
chk <- pooled %>% filter(series %in% c(named, other18)) %>% group_by(period, area, age) %>% summarise(parts = sum(d), .groups = "drop") %>%
  inner_join(pooled %>% filter(series == "ALL") %>% select(period, area, age, all = d), by = c("period", "area", "age"))
stopifnot(nrow(chk) == 3 * 3 * 19, all(chk$parts == chk$all))
cat("Check 1: the categories add up to all causes in all", nrow(chk), "cells of age group, area and period\n")
pooled <- pooled %>% mutate(area = ifelse(area == "N", "N", "S"), d = as.numeric(d)) %>% group_by(period, series, area, age) %>% summarise(d = sum(d), .groups = "drop")
pop <- read_csv(pop_file, show_col_types = FALSE) %>% select(-rowsum) %>% pivot_longer(matches("^[0-9]{4}$"), names_to = "year", values_to = "p") %>%
  mutate(year = as.integer(year), area = ifelse(area == "North", "N", "S"))
stopifnot(setequal(unique(pop$age), 1:19), setequal(unique(pop$sex), c("F", "M")))

# =====================================================================
# Part A. 'Other causes' in four age bands, divided among the eight further groups and the rest
# =====================================================================
periods <- list("2015-19" = 2015:2019, "2021-25" = 2021:2025)
py <- bind_rows(lapply(names(periods), function(pr) pop %>% filter(year %in% periods[[pr]]) %>% group_by(area, age) %>% summarise(py = sum(p), .groups = "drop") %>% mutate(period = pr)))
set.seed(20261119)
one_band <- function(b, pr) {
  idx <- bands[[b]]; w <- 1e5 * esp[idx] / sum(esp[idx])
  p <- py[py$period == pr, ]; pN <- p$py[p$area == "N"][match(idx, p$age[p$area == "N"])]; pS <- p$py[p$area == "S"][match(idx, p$age[p$area == "S"])]
  get <- function(members, ar) { v <- pooled %>% filter(period == pr, series %in% members, area == ar, age %in% idx) %>% group_by(age) %>% summarise(d = sum(d)); v$d[match(idx, v$age)] }
  parts <- c(further, rest); nm <- names(parts)
  dN <- sapply(parts, get, ar = "N"); dS <- sapply(parts, get, ar = "S")                                   # age group by part
  allN <- get("ALL", "N"); allS <- get("ALL", "S"); namedN <- allN - rowSums(dN); namedS <- allS - rowSums(dS); stopifnot(all(namedN >= 0), all(namedS >= 0))
  rate <- function(m, pp) colSums(m * (w / pp)); rN <- rate(dN, pN); rS <- rate(dS, pS); gap <- rN - rS
  gap_all <- sum(allN * w / pN) - sum(allS * w / pS); gap_oth <- sum(gap)
  sim <- function(m, pp) { x <- matrix(rpois(n_sim * length(m), rep(as.vector(m), each = n_sim)), n_sim); x %*% (diag(ncol(m)) %x% matrix(w / pp, ncol = 1)) }
  sN <- sim(dN, pN); sS <- sim(dS, pS); sg <- sN - sS
  s_named <- as.vector(matrix(rpois(n_sim * length(idx), rep(namedN, each = n_sim)), n_sim) %*% (w / pN) - matrix(rpois(n_sim * length(idx), rep(namedS, each = n_sim)), n_sim) %*% (w / pS))
  s_oth <- rowSums(sg); s_all <- s_oth + s_named
  row <- function(lab, j, kind) { g <- sum(gap[j]); s <- rowSums(sg[, j, drop = FALSE]); rr <- rowSums(sN[, j, drop = FALSE]) / rowSums(sS[, j, drop = FALSE])
    data.frame(ages = b, period = pr, item = lab, kind = kind, deaths_north = sum(dN[, j]), deaths_south = sum(dS[, j]), rate_north = sum(rN[j]), rate_south = sum(rS[j]), gap = g,
               rate_ratio = sum(rN[j]) / sum(rS[j]), rr_lo = if (all(is.finite(rr))) q(rr)[1] else NA_real_, rr_hi = if (all(is.finite(rr))) q(rr)[2] else NA_real_,      # no interval where a simulated rate is zero
               pct_of_other_gap = 100 * g / gap_oth, po_lo = q(100 * s / s_oth)[1], po_hi = q(100 * s / s_oth)[2], pct_of_all_gap = 100 * g / gap_all, pa_lo = q(100 * s / s_all)[1], pa_hi = q(100 * s / s_all)[2]) }
  bind_rows(c(lapply(seq_along(parts), function(j) row(nm[j], j, ifelse(j <= length(further), "further group", "left unnamed"))),
              list(row("The eight further groups, together", seq_along(further), "subtotal"), row("The rest, together", length(further) + seq_along(rest), "subtotal"),
                   row("Other causes (2018 definitions)", seq_along(parts), "total")))) %>%
    mutate(all_cause_gap = gap_all, northern_excess_pct = 100 * gap_all / sum(allS * w / pS))
}
part_a <- bind_rows(lapply(names(bands), function(b) bind_rows(lapply(names(periods), function(pr) one_band(b, pr)))))
# check 2: within each band and period the parts add up to the whole
stopifnot(all(abs((part_a %>% filter(kind %in% c("further group", "left unnamed")) %>% group_by(ages, period) %>% summarise(s = sum(pct_of_other_gap), .groups = "drop"))$s - 100) < 1e-8))
write_csv(part_a %>% mutate(across(c(rate_north, rate_south, gap, all_cause_gap), ~ round(.x, 4)), across(c(rate_ratio, rr_lo, rr_hi), ~ round(.x, 4)),
                            across(c(pct_of_other_gap, po_lo, po_hi, pct_of_all_gap, pa_lo, pa_hi, northern_excess_pct), ~ round(.x, 3))), file.path(outdir, "other_causes_by_age_band.csv"))

# =====================================================================
# Part B. Infant deaths under 25; the cancers and the respiratory disease behind the gap; diabetes. 2021-25
# =====================================================================
cnt <- pooled %>% filter(period == "2021-25")
pyb <- py %>% filter(period == "2021-25")
get  <- function(s, ar, idx) { v <- cnt %>% filter(series %in% s, area == ar, age %in% idx) %>% group_by(age) %>% summarise(d = sum(d)); v$d[match(idx, v$age)] }
pyv  <- function(ar, idx) pyb$py[pyb$area == ar][match(idx, pyb$age[pyb$area == ar])]
part_b <- list()
show <- function(ages, measure, est, sim) part_b[[length(part_b) + 1]] <<- data.frame(ages = ages, period = "2021-25", measure = measure, estimate = est, lower = q(sim)[1], upper = q(sim)[2])
band <- function(idx, parts) {            # parts: deaths in North and South by age group for each piece; returns the rates and the simulated rates
  w <- 1e5 * esp[idx] / sum(esp[idx]); pN <- pyv("N", idx); pS <- pyv("S", idx)
  one <- function(dN, dS) list(rN = sum(dN * w / pN), rS = sum(dS * w / pS), sN = as.vector(matrix(rpois(n_sim * length(dN), rep(dN, each = n_sim)), n_sim) %*% (w / pN)),
                               sS = as.vector(matrix(rpois(n_sim * length(dS), rep(dS, each = n_sim)), n_sim) %*% (w / pS)))
  lapply(parts, function(p) one(p$N, p$S))
}
gapof <- function(x) x$rN - x$rS; simof <- function(x) x$sN - x$sS
minus <- function(x, y) list(N = x$N - y$N, S = x$S - y$S)
plus  <- function(...) { l <- list(...); list(N = Reduce(`+`, lapply(l, `[[`, "N")), S = Reduce(`+`, lapply(l, `[[`, "S"))) }
set.seed(20261119)
# under 25: deaths under 28 days, at 28 days to under 1 year, and at ages 1-24
idx <- 1:6; allN <- get("ALL", "N", idx); allS <- get("ALL", "S", idx); neoN <- get("NEONATAL", "N", idx); neoS <- get("NEONATAL", "S", idx)
stopifnot(all(neoN[-1] == 0), all(neoS[-1] == 0))
z <- band(idx, list(neo = list(N = neoN, S = neoS), post = list(N = c(allN[1] - neoN[1], rep(0, 5)), S = c(allS[1] - neoS[1], rep(0, 5))), older = list(N = c(0, allN[-1]), S = c(0, allS[-1]))))
G <- gapof(z$neo) + gapof(z$post) + gapof(z$older); Sg <- simof(z$neo) + simof(z$post) + simof(z$older)
show("0-24", "Infant deaths (under 1 year), % of the all-cause gap", 100 * (gapof(z$neo) + gapof(z$post)) / G, 100 * (simof(z$neo) + simof(z$post)) / Sg)
show("0-24", "Deaths under 28 days, % of the all-cause gap", 100 * gapof(z$neo) / G, 100 * simof(z$neo) / Sg)
show("0-24", "Deaths at 28 days to under 1 year, % of the all-cause gap", 100 * gapof(z$post) / G, 100 * simof(z$post) / Sg)
show("0-24", "Deaths at ages 1-24, % of the all-cause gap", 100 * gapof(z$older) / G, 100 * simof(z$older) / Sg)
show("0-24", "Infant deaths per head aged under 1, rate ratio", (z$neo$rN + z$post$rN) / (z$neo$rS + z$post$rS), (z$neo$sN + z$post$sN) / (z$neo$sS + z$post$sS))
# cancer by site, chronic lower respiratory disease and diabetes within one age band
sites <- function(ages, idx) {
  g1 <- function(s) list(N = get(s, "N", idx), S = get(s, "S", idx))
  all <- g1("ALL"); can <- g1("CANCER"); lung <- g1("m_LUNG"); ugi <- g1("m_UPPER_GI"); liv <- g1("m_LIVER_CA"); hn <- g1("m_HEADNECK"); br <- g1("m_BREAST"); crc <- g1("m_COLORECTAL")
  resp <- g1("RESP"); copd <- g1("m_COPD_ETC"); diab <- g1("DIAB")
  four <- plus(lung, ugi, liv, hn); three <- plus(ugi, liv, hn)
  z <- band(idx, list(lung = lung, three = three, breast = br, crc = crc, can_rest = minus(can, plus(four, br, crc)), copd = copd, resp_rest = minus(resp, copd), diab = diab,
                      rest = minus(all, plus(can, resp, diab))))
  can_g <- gapof(z$lung) + gapof(z$three) + gapof(z$breast) + gapof(z$crc) + gapof(z$can_rest); can_s <- simof(z$lung) + simof(z$three) + simof(z$breast) + simof(z$crc) + simof(z$can_rest)
  all_g <- can_g + gapof(z$copd) + gapof(z$resp_rest) + gapof(z$diab) + gapof(z$rest); all_s <- can_s + simof(z$copd) + simof(z$resp_rest) + simof(z$diab) + simof(z$rest)
  can_rS <- z$lung$rS + z$three$rS + z$breast$rS + z$crc$rS + z$can_rest$rS
  show(ages, "Cancer, % of the all-cause gap", 100 * can_g / all_g, 100 * can_s / all_s)
  show(ages, "Lung cancer (C33-C34), % of the cancer gap", 100 * gapof(z$lung) / can_g, 100 * simof(z$lung) / can_s)
  show(ages, "Cancers of the oesophagus, stomach, liver, lip, mouth and throat (C15-C16, C22, C00-C14), % of the cancer gap", 100 * gapof(z$three) / can_g, 100 * simof(z$three) / can_s)
  show(ages, "Those cancers and lung cancer together, % of the cancer gap", 100 * (gapof(z$lung) + gapof(z$three)) / can_g, 100 * (simof(z$lung) + simof(z$three)) / can_s)
  show(ages, "Breast cancer (C50), % of the cancer gap", 100 * gapof(z$breast) / can_g, 100 * simof(z$breast) / can_s)
  show(ages, "Breast cancer, rate ratio", z$breast$rN / z$breast$rS, z$breast$sN / z$breast$sS)
  show(ages, "Lung cancer, rate ratio", z$lung$rN / z$lung$rS, z$lung$sN / z$lung$sS)
  show(ages, "All cancer, rate ratio", (can_g + can_rS) / can_rS, (z$lung$sN + z$three$sN + z$breast$sN + z$crc$sN + z$can_rest$sN) / (z$lung$sS + z$three$sS + z$breast$sS + z$crc$sS + z$can_rest$sS))
  show(ages, "Lung cancer, % of the all-cause gap", 100 * gapof(z$lung) / all_g, 100 * simof(z$lung) / all_s)
  show(ages, "Chronic lower respiratory disease (J40-J47), % of the all-cause gap", 100 * gapof(z$copd) / all_g, 100 * simof(z$copd) / all_s)
  show(ages, "Lung cancer and chronic lower respiratory disease together, % of the all-cause gap", 100 * (gapof(z$lung) + gapof(z$copd)) / all_g, 100 * (simof(z$lung) + simof(z$copd)) / all_s)
  show(ages, "Diabetes, % of the all-cause gap", 100 * gapof(z$diab) / all_g, 100 * simof(z$diab) / all_s)
  show(ages, "Diabetes, rate ratio", z$diab$rN / z$diab$rS, z$diab$sN / z$diab$sS)
}
sites("50-74", 12:16)
for (b in list(list("25-49", 7:11), list("under 75", 1:16))) {      # diabetes and cancer as a whole in two more bands
  idx <- b[[2]]; g1 <- function(s) list(N = get(s, "N", idx), S = get(s, "S", idx)); all <- g1("ALL"); diab <- g1("DIAB"); can <- g1("CANCER")
  z <- band(idx, list(diab = diab, can = can, rest = minus(all, plus(diab, can)))); G <- gapof(z$diab) + gapof(z$can) + gapof(z$rest); Sg <- simof(z$diab) + simof(z$can) + simof(z$rest)
  show(b[[1]], "Diabetes, % of the all-cause gap", 100 * gapof(z$diab) / G, 100 * simof(z$diab) / Sg); show(b[[1]], "Diabetes, rate ratio", z$diab$rN / z$diab$rS, z$diab$sN / z$diab$sS)
  show(b[[1]], "Cancer, % of the all-cause gap", 100 * gapof(z$can) / G, 100 * simof(z$can) / Sg); show(b[[1]], "All cancer, rate ratio", z$can$rN / z$can$rS, z$can$sN / z$can$sS)
}
sites("75 and over", 17:19)
part_b <- bind_rows(part_b)
write_csv(part_b %>% mutate(across(c(estimate, lower, upper), ~ round(.x, 3))), file.path(outdir, "infant_deaths_and_cancers.csv"))

# =====================================================================
# Part C. The deaths that two cause groups share
# =====================================================================
ages5 <- 7:11; w5 <- 1e5 * esp[ages5] / sum(esp[ages5]); cols5 <- c("a25_29", "a30_34", "a35_39", "a40_44", "a45_49")
bysex <- read_csv(sex_file, show_col_types = FALSE) %>% pivot_longer(all_of(cols5), names_to = "age", values_to = "d") %>% mutate(age = match(age, cols5) + 6L)
stopifnot(setequal(unique(bysex$area), c("N", "X", "L")), !anyNA(bysex$d))
bysex <- bysex %>% mutate(area = ifelse(area == "N", "N", "S")) %>% group_by(series, area, sex, year, age) %>% summarise(d = sum(d), .groups = "drop")
pop5  <- pop %>% filter(age %in% ages5)
shared <- list(
  "2018 definitions: the codes in two groups (I42.6, Y15)"                                              = c("c_I426", "c_Y15"),
  "November 2026 extract: in accidents and in drug poisoning (X40-X44)"                                 = "c_X40_44",
  "November 2026 extract: in suicide and in drug poisoning (X60-X64, Y10-Y14)"                          = "c_DRUGSUI",
  "November 2026 extract: alcohol codes also in accidents, suicide or cardiovascular (X45, X65, Y15, I42.6)" = c("c_X45", "c_X65", "c_Y15", "c_I426"),
  "November 2026 extract: all deaths in two groups"                                                     = c("c_X40_44", "c_DRUGSUI", "c_X45", "c_X65", "c_Y15", "c_I426"))
part_c <- list(); stacks <- list()
set.seed(20261120)
one_sex <- function(yrs, sexes, lab) {
  z <- bysex %>% filter(year %in% yrs, sex %in% sexes) %>% group_by(series, area, age) %>% summarise(d = sum(d), .groups = "drop")
  p <- pop5 %>% filter(year %in% yrs, sex %in% sexes) %>% group_by(area, age) %>% summarise(p = sum(p), .groups = "drop")
  get <- function(s, ar) { v <- z %>% filter(series %in% s, area == ar) %>% group_by(age) %>% summarise(d = sum(d)); v$d[match(ages5, v$age)] }
  pN <- p$p[p$area == "N"][match(ages5, p$age[p$area == "N"])]; pS <- p$p[p$area == "S"][match(ages5, p$age[p$area == "S"])]
  gap <- function(dN, dS) sum(dN * w5 / pN) - sum(dS * w5 / pS)
  sim <- function(dN, dS) as.vector(matrix(rpois(n_sim * 5, rep(dN, each = n_sim)), n_sim) %*% (w5 / pN) - matrix(rpois(n_sim * 5, rep(dS, each = n_sim)), n_sim) %*% (w5 / pS))
  allN <- get("ALL", "N"); allS <- get("ALL", "S"); G <- gap(allN, allS); per <- sprintf("%d-%02d", min(yrs), max(yrs) %% 100)
  for (g in names(shared)) { oN <- get(shared[[g]], "N"); oS <- get(shared[[g]], "S"); stopifnot(all(oN <= allN), all(oS <= allS))
    so <- sim(oN, oS); sr <- sim(allN - oN, allS - oS); sh <- 100 * so / (so + sr)
    part_c[[length(part_c) + 1]] <<- data.frame(ages = "25-49", sex = lab, period = per, item = g, deaths_north_a_year = sum(oN) / length(yrs), deaths_south_a_year = sum(oS) / length(yrs),
      pct_of_all_deaths = 100 * (sum(oN) + sum(oS)) / (sum(allN) + sum(allS)), gap = gap(oN, oS), pct_of_all_cause_gap = 100 * gap(oN, oS) / G, lower = q(sh)[1], upper = q(sh)[2]) }
  # the groups of each definition as shares of the all-cause gap: stacked, and with each death counted once
  g1 <- function(s) gap(get(s, "N"), get(s, "S"))
  nov <- c(accidents = g1(c("ACC18", "c_X40_44", "c_X45")), drug_poisoning = g1(c("DRUG18", "c_DRUGSUI")), alcohol_related = g1(c("ALC18", "c_X65")), suicide = g1("SUI"), cardiovascular = g1("CVD"), diabetes = g1("DIAB"))
  d18 <- c(cardiovascular = g1("CVD"), diabetes = g1("DIAB"), cancer = g1(c("CANCER_XB", "BREAST")), alcohol_related = g1("ALC18"), drug_related = g1("DRUG18"), suicide = g1("SUI"), accidents = g1("ACC18"))
  stacks[[length(stacks) + 1]] <<- bind_rows(
    data.frame(ages = "25-49", sex = lab, period = per, definition = "November 2026 extract", group = c(names(nov), "the six groups stacked", "the six groups with each death counted once"),
               pct_of_all_cause_gap = 100 * c(nov, sum(nov), sum(nov) - g1(shared[[5]])) / G),
    data.frame(ages = "25-49", sex = lab, period = per, definition = "2018 definitions", group = c(names(d18), "the seven groups stacked", "other causes, as the remainder", "the seven groups with each death counted once"),
               pct_of_all_cause_gap = 100 * c(d18, sum(d18), G - sum(d18), sum(d18) - g1(shared[[1]])) / G))
}
for (y in list(2023:2025, 2013:2015)) { one_sex(y, "M", "men"); one_sex(y, "F", "women"); one_sex(y, c("F", "M"), "persons") }
# the other age bands, persons, 2021-25, from the pooled counts (see the method notes)
for (b in names(bands)) {
  idx <- bands[[b]]; w <- 1e5 * esp[idx] / sum(esp[idx]); pN <- pyv("N", idx); pS <- pyv("S", idx)
  gap <- function(dN, dS) sum(dN * w / pN) - sum(dS * w / pS)
  sim <- function(dN, dS) as.vector(matrix(rpois(n_sim * length(idx), rep(dN, each = n_sim)), n_sim) %*% (w / pN) - matrix(rpois(n_sim * length(idx), rep(dS, each = n_sim)), n_sim) %*% (w / pS))
  allN <- get("ALL", "N", idx); allS <- get("ALL", "S", idx); G <- gap(allN, allS)
  sets <- list("November 2026 extract: all deaths in two groups, with assault by drugs (X85)" = list(N = get(c("DRUG_MEMO", "ALC_OVL"), "N", idx) - get("MENT_DRUG", "N", idx), S = get(c("DRUG_MEMO", "ALC_OVL"), "S", idx) - get("MENT_DRUG", "S", idx)),
               "2018 definitions: upper limit, the four alcohol codes (I42.6, X45, X65, Y15)"  = list(N = get("ALC_OVL", "N", idx), S = get("ALC_OVL", "S", idx)))
  for (g in names(sets)) { oN <- sets[[g]]$N; oS <- sets[[g]]$S; stopifnot(all(oN >= 0), all(oS >= 0), all(oN <= allN), all(oS <= allS))
    so <- sim(oN, oS); sr <- sim(allN - oN, allS - oS); sh <- 100 * so / (so + sr)
    part_c[[length(part_c) + 1]] <- data.frame(ages = b, sex = "persons", period = "2021-25", item = g, deaths_north_a_year = sum(oN) / 5, deaths_south_a_year = sum(oS) / 5,
      pct_of_all_deaths = 100 * (sum(oN) + sum(oS)) / (sum(allN) + sum(allS)), gap = gap(oN, oS), pct_of_all_cause_gap = 100 * gap(oN, oS) / G, lower = q(sh)[1], upper = q(sh)[2]) }
}
part_c <- bind_rows(part_c); stacks <- bind_rows(stacks)
write_csv(bind_rows(part_c %>% mutate(table = "deaths in two groups", .before = 1),
                    stacks %>% transmute(table = "groups as shares of the gap", ages, sex, period, item = paste0(definition, ": ", gsub("_", " ", group)), pct_of_all_cause_gap)) %>%
            mutate(across(c(deaths_north_a_year, deaths_south_a_year), ~ round(.x, 1)), across(c(pct_of_all_deaths, gap, pct_of_all_cause_gap, lower, upper), ~ round(.x, 3))),
          file.path(outdir, "overlap_between_cause_groups.csv"), na = "")

# ---- summary ---------------------------------------------------------
f1 <- function(e, l, u) sprintf("%.1f (%.1f to %.1f)", e, l, u)
cat("\nInside the cause groups, written to", normalizePath(outdir), "\n")
cat("\nA. The eight further groups together, as a percentage of the gap in 'other causes', and 'other causes' as a percentage of the all-cause gap (95% interval), persons, 2021-25\n")
print(as.data.frame(part_a %>% filter(period == "2021-25", item %in% c("The eight further groups, together", "Other causes (2018 definitions)")) %>%
                      transmute(ages, item, share = ifelse(kind == "total", f1(pct_of_all_gap, pa_lo, pa_hi), f1(pct_of_other_gap, po_lo, po_hi)))), row.names = FALSE)
cat("\nB. Infant deaths, cancers, respiratory disease and diabetes, persons, 2021-25 (95% interval)\n")
print(as.data.frame(part_b %>% transmute(ages, measure, estimate = ifelse(grepl("rate ratio", measure), sprintf("%.2f (%.2f to %.2f)", estimate, lower, upper), f1(estimate, lower, upper)))), row.names = FALSE)
cat("\nC. Deaths in two cause groups, and the share of the all-cause gap that a stacked chart would count twice, % (95% interval)\n")
print(as.data.frame(part_c %>% filter(period != "2013-15") %>% transmute(ages, sex, period, item, deaths_a_year = round(deaths_north_a_year + deaths_south_a_year), pct_of_all_deaths = sprintf("%.1f", pct_of_all_deaths),
                                                                         share_of_gap = sprintf("%.1f (%.1f to %.1f)", pct_of_all_cause_gap, lower, upper))), row.names = FALSE)
