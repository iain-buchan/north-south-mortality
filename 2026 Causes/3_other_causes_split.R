# =====================================================================
# North-South divide in English mortality by cause of death, 1981-2025
# Script 3 of 6: what is inside the residual 'other causes'
#
# Iain Buchan, 15 Nov 2026. Exploratory work in progress; not peer reviewed.
#
# Why
#   In script 1, 'other causes' is all causes minus cardiovascular, diabetes, accidents, suicide and
#   alcohol-related deaths: a residual that holds a large share of the North-South gap without saying
#   what is in it. This script splits it, using public NOMIS counts (table NM_161_1, deaths registered
#   by underlying cause) for persons under 75, pooled over 2015-19, 2020-24 and 2021-25, into 21
#   categories that do not overlap, and then into five groups plus a remainder:
#       cancer; respiratory; mental, behavioural and neurological (excluding alcohol codes);
#       digestive (excluding alcohol codes); COVID-19; everything else.
#
# Data (in ./data)
#   nomis_deaths_by_category_pooled.csv   deaths, persons, by period, series, area and age group.
#                                         Areas: N = North, X = South excluding London, L = London.
#   nomis_pop_nsl_1981_2025.csv           ONS mid-year population estimates (NOMIS NM_31_1) by sex,
#                                         summed to persons here. 'South' in the file excludes London.
#
# Method notes
#   * Rates are directly standardised to the 2013 European Standard Population, per 100,000, for
#     ages 25-49 and for under 75, with deaths and populations pooled over the five years of a period.
#   * S = X + L is the South including London, the definition used in the Lancet Public Health papers.
#   * No intervals are given here; for intervals on the main causes see scripts 1 and 2.
#
# Checks made here
#   1. The categories that should partition all causes do so in every age group, area and period.
#   2. If script 1 has been run, the NOMIS rates for the named causes are compared with the rates
#      from the ONS extract (output/decomposition_by_period.csv). They differ slightly, because
#      NOMIS and the extract are different snapshots of the registrations.
#
# Outputs (written to ./output; rates and shares to six decimal places)
#   nomis_other_split_categories.csv   every series: rates in the four areas, gaps, ratios and shares
#   nomis_other_split_groups.csv       the five groups, their total, the remainder and the residual
#
# Needs: dplyr, tidyr, readr (installed automatically if absent). A few seconds.
# =====================================================================

pkgs <- c("dplyr", "tidyr", "readr")
new  <- pkgs[!pkgs %in% rownames(installed.packages())]
if (length(new)) install.packages(new)
invisible(lapply(pkgs, library, character.only = TRUE))
if (packageVersion("dplyr") < "1.1.0") stop("dplyr 1.1.0 or later is needed - please update it.")

# ---- configuration ---------------------------------------------------
counts_file <- file.path("data", "nomis_deaths_by_category_pooled.csv")
pop_file    <- file.path("data", "nomis_pop_nsl_1981_2025.csv")
outdir      <- "output"

for (f in c(counts_file, pop_file))
  if (!file.exists(f)) stop("Cannot find '", f, "' from ", getwd(), " - setwd() to the '2026 Causes' folder.")
dir.create(outdir, showWarnings = FALSE)

age_cols <- c("a00", "a01_04", "a05_09", "a10_14", "a15_19", "a20_24", "a25_29", "a30_34", "a35_39", "a40_44",
              "a45_49", "a50_54", "a55_59", "a60_64", "a65_69", "a70_74")
# 2013 European Standard Population for the 16 age groups under 75
esp     <- c(1000, 4000, 5500, 5500, 5500, 6000, 6000, 6500, 7000, 7000, 7000, 7000, 6500, 6000, 5500, 5000)
bands   <- list("25-49" = 7:11, "<75" = 1:16)
periods <- list("2015-19" = 2015:2019, "2020-24" = 2020:2024, "2021-25" = 2021:2025)

# series in the counts file, with the ICD-10 codes each one holds
named <- c(
  ALL = "All causes",
  CVD = "Cardiovascular (I00-I99)",
  DIAB = "Diabetes (E10-E14)",
  ACC = "Accidents (V01-X59)",
  SUI = "Suicide (X60-X84 from age 10; Y10-Y34 from age 15)",
  ALC_EXCL = "Alcohol-related codes outside the other named causes (F10, G31.2, G62.1, K29.2, K70, K73, K74 excl. K74.3-K74.5, K86.0)",
  ALC_OVL = "Alcohol-related codes inside cardiovascular, accidents and suicide (I42.6, X45, X65, Y15)",
  DRUG_MEMO = "Drug poisoning, memo item (F11-F16, F18, F19, X40-X44, X60-X64, X85, Y10-Y14)")
residual <- c(   # these do not overlap; with CVD, DIAB, ACC, SUI and ALC_EXCL they add up to ALL
  CANCER = "Malignant neoplasms (C00-C97)",
  NEO_OTH = "Other neoplasms (D00-D48)",
  RESP = "Respiratory (J00-J99)",
  DEMENTIA = "Dementia and Alzheimer disease (F01, F03, G30)",
  COVID = "COVID-19 (U07.1, U07.2, U10.9)",
  LIVER_NONALC = "Liver disease outside the alcohol list (K70-K77 minus K70, K73, K74.0-K74.2, K74.6)",
  DIGEST_OTH = "Other digestive (K00-K93 minus K70-K77, K29.2, K86.0)",
  MENT_DRUG = "Mental and behavioural disorders due to drug use (F11-F16, F18, F19)",
  MENT_OTH = "Other mental and behavioural (F00-F99 minus F01, F03, F10-F16, F18, F19)",
  NERVOUS = "Nervous system (G00-G99 minus G30, G31.2, G62.1)",
  INFECT = "Infectious and parasitic (A00-B99)",
  ENDO_NONDIAB = "Endocrine, nutritional and metabolic excluding diabetes (E00-E90 minus E10-E14)",
  BLOOD = "Blood and immune (D50-D89)",
  GU = "Genitourinary (N00-N99)",
  MSK_SKIN = "Musculoskeletal and skin (M00-M99, L00-L99)",
  PERI_CONG_PREG = "Perinatal, congenital and pregnancy (P00-P96, Q00-Q99, O00-O99)",
  ILLDEF = "Symptoms, signs and ill-defined (R00-R99)",
  EXT_OTH = "Other external causes (U50.9, V01-Y89 minus accidents and suicide)",
  U_OTH = "Other special-purpose codes (U00-U85 minus COVID-19)",
  EYE_EAR = "Eye and ear (H00-H95)",
  NEONATAL = "Deaths under 28 days, which carry no underlying cause")
groups <- list(  # the five groups, built from the residual categories
  g_CANCER = list(label = "Cancer", members = "CANCER"),
  g_RESP = list(label = "Respiratory", members = "RESP"),
  g_MENT_NEURO = list(label = "Mental, behavioural and neurological, excluding alcohol codes",
                      members = c("DEMENTIA", "MENT_DRUG", "MENT_OTH", "NERVOUS")),
  g_DIGEST_NONALC = list(label = "Digestive, excluding alcohol codes", members = c("LIVER_NONALC", "DIGEST_OTH")),
  g_COVID = list(label = "COVID-19", members = "COVID"))
memo <- c(       # in the counts file for 2021-25 only; each sits inside one of the series above
  m_LUNG = "Lung cancer (C33-C34)", m_COLORECTAL = "Colorectal cancer (C18-C21)", m_BREAST = "Breast cancer (C50)",
  m_UPPER_GI = "Oesophagus and stomach cancer (C15-C16)", m_LIVER_CA = "Liver cancer (C22)",
  m_PANCREAS = "Pancreatic cancer (C25)", m_CERVIX = "Cervical cancer (C53)", m_HAEM = "Haematological cancers (C81-C96)",
  m_HEADNECK = "Lip, oral cavity and pharynx cancer (C00-C14)", m_BRAIN = "Brain cancer (C71)",
  m_COPD_ETC = "Chronic lower respiratory disease (J40-J47)", m_FLU_PNEUM = "Influenza and pneumonia (J09-J18)",
  m_EPILEPSY = "Epilepsy (G40-G41)", m_HIV = "HIV (B20-B24)", m_VIRAL_HEP = "Viral hepatitis (B15-B19)",
  m_SEPSIS = "Sepsis (A40-A41)", m_R99 = "Unknown cause (R99)", m_OBESITY_E66 = "Obesity as underlying cause (E66)",
  m_RENAL_FAIL = "Renal failure (N17-N19)", m_IHD = "Ischaemic heart disease (I20-I25)", m_STROKE = "Stroke (I60-I69)",
  m_ASSAULT = "Assault (X85-Y09)", m_ACC_DRUG_POIS = "Accidental drug poisoning (X40-X44)",
  m_K70_ALD = "Alcoholic liver disease (K70)")
derived <- c(
  ALC_FULL = "Alcohol-related, ONS definition (ALC_EXCL + ALC_OVL)",
  OTHER_DEF = "Other causes (residual): all causes minus cardiovascular, diabetes, accidents, suicide and alcohol-related",
  g_FIVE = "The five groups together",
  g_REST = "Everything else in the residual (OTHER_DEF minus the five groups)")
group_labels <- sapply(groups, function(g) g$label)
label <- c(named, residual, memo, group_labels, derived)
kind_of <- c(setNames(rep("named cause", length(named)), names(named)),
             setNames(rep("residual category", length(residual)), names(residual)),
             setNames(rep("memo item", length(memo)), names(memo)),
             setNames(rep("group", length(groups) + 2), c(names(groups), "g_FIVE", "g_REST")))
kind_of[c("ALL", "ALC_FULL", "OTHER_DEF")] <- c("total", "named cause", "residual total")
series_order <- c("ALL", "CVD", "DIAB", "ACC", "SUI", "ALC_FULL", "ALC_EXCL", "ALC_OVL", "OTHER_DEF", "DRUG_MEMO",
                  names(residual), names(groups), "g_FIVE", "g_REST", names(memo))
stopifnot(all(series_order %in% names(label)), all(series_order %in% names(kind_of)))

write_out <- function(df, name) {
  f6 <- function(x) ifelse(is.na(x), NA_character_, sub("^-(0\\.0+)$", "\\1", sprintf("%.6f", x)))
  write_csv(df %>% mutate(across(where(is.double), f6)), file.path(outdir, name), na = "")
}

# ---- 1. deaths ----------------------------------------------------------
cnt <- read_csv(counts_file, show_col_types = FALSE)
stopifnot(setequal(cnt$area, c("N", "X", "L")), identical(names(cnt)[-(1:3)], age_cols),
          setequal(cnt$period, names(periods)), !anyNA(cnt), !any(duplicated(cnt[, 1:3])))
deaths <- cnt %>% pivot_longer(all_of(age_cols), names_to = "age", values_to = "d") %>%
  mutate(age = match(age, age_cols), d = as.numeric(d))
deaths <- bind_rows(deaths, deaths %>% filter(area %in% c("X", "L")) %>% group_by(period, series, age) %>%
                      summarise(d = sum(d), .groups = "drop") %>% mutate(area = "S"))
wd <- deaths %>% pivot_wider(names_from = "series", values_from = "d")
stopifnot(nrow(wd) == length(periods) * 4 * length(age_cols),
          !anyNA(wd[, c(names(named), names(residual))]))
wd <- wd %>% mutate(ALC_FULL  = ALC_EXCL + ALC_OVL,
                    OTHER_DEF = ALL - CVD - DIAB - ACC - SUI - ALC_FULL)
# check 1: all causes minus the categories that should partition them
partition_gap <- wd$ALL - rowSums(wd[, c("CVD", "DIAB", "ACC", "SUI", "ALC_EXCL", names(residual))])
for (g in names(groups)) wd[[g]] <- rowSums(wd[, groups[[g]]$members, drop = FALSE])
wd$g_FIVE <- rowSums(wd[, names(groups)])
wd$g_REST <- wd$OTHER_DEF - wd$g_FIVE
deaths <- wd %>% pivot_longer(-c(period, area, age), names_to = "series", values_to = "d") %>%
  filter(!is.na(d))                                                 # memo items exist for 2021-25 only

# ---- 2. populations: persons, pooled over the years of each period ----------------
pop <- read_csv(pop_file, show_col_types = FALSE) %>% filter(age <= 16) %>%
  mutate(area = unname(c(North = "N", South = "X", London = "L")[area]))     # 'South' in the file excludes London
pop <- bind_rows(lapply(names(periods), function(pn)
  pop %>% transmute(period = pn, area, age, p = rowSums(across(all_of(as.character(periods[[pn]]))))) %>%
    group_by(period, area, age) %>% summarise(p = sum(p), .groups = "drop")))
pop <- bind_rows(pop, pop %>% filter(area %in% c("X", "L")) %>% group_by(period, age) %>%
                   summarise(p = sum(p), .groups = "drop") %>% mutate(area = "S"))
stopifnot(nrow(pop) == length(periods) * 4 * length(age_cols), !anyNA(pop))

# ---- 3. standardised rates and the tables -------------------------------------
rates <- bind_rows(lapply(names(bands), function(b)
  deaths %>% filter(age %in% bands[[b]]) %>% inner_join(pop, by = c("period", "area", "age")) %>%
    mutate(w = esp[age]) %>% group_by(period, series, area) %>%
    summarise(dsr = sum(w * d / p) / sum(w) * 1e5, deaths = sum(d), .groups = "drop") %>% mutate(band = b)))
tab <- rates %>% pivot_wider(names_from = "area", values_from = c("dsr", "deaths")) %>%
  filter(series %in% series_order) %>%
  mutate(years = unname(lengths(periods)[period]), gap_N_minus_S = dsr_N - dsr_S)
tab <- tab %>%
  inner_join(tab %>% filter(series == "ALL") %>% select(band, period, all_gap = gap_N_minus_S), by = c("band", "period")) %>%
  inner_join(tab %>% filter(series == "OTHER_DEF") %>% select(band, period, other_gap = gap_N_minus_S), by = c("band", "period")) %>%
  transmute(series, kind = unname(kind_of[series]), description = unname(label[series]), band, period,
            dsr_North = dsr_N, dsr_South_incl_London = dsr_S, dsr_South_excl_London = dsr_X, dsr_London = dsr_L,
            gap_N_minus_S, ratio_N_to_S = ifelse(dsr_S != 0, dsr_N / dsr_S, NA),
            gap_N_minus_SXL = dsr_N - dsr_X, ratio_N_to_SXL = ifelse(dsr_X != 0, dsr_N / dsr_X, NA),
            gap_L_minus_SXL = dsr_L - dsr_X, ratio_L_to_SXL = ifelse(dsr_X != 0, dsr_L / dsr_X, NA),
            North_deaths_per_year = deaths_N / years,
            pct_of_allcause_gap_N_minus_S = gap_N_minus_S / all_gap * 100,
            pct_of_residual_gap_N_minus_S = ifelse(kind %in% c("residual category", "group", "residual total"),
                                                   gap_N_minus_S / other_gap * 100, NA)) %>%
  arrange(match(band, names(bands)), match(period, names(periods)), match(series, series_order))
write_out(tab, "nomis_other_split_categories.csv")
grp <- tab %>% filter(series %in% c(names(groups), "g_FIVE", "g_REST", "OTHER_DEF", "ALL"))
write_out(grp, "nomis_other_split_groups.csv")

# ---- 4. checks, printed ---------------------------------------------------
cat(sprintf("1. Partition: largest |all causes minus the sum of the categories| in any age group, area and period = %.0f deaths\n",
            max(abs(partition_gap))))
stopifnot(all(partition_gap == 0))

dec_file <- file.path(outdir, "decomposition_by_period.csv")
if (file.exists(dec_file)) {
  m <- c("All causes" = "ALL", "Cardiovascular" = "CVD", "Diabetes" = "DIAB", "Accidents" = "ACC", "Suicide" = "SUI",
         "Alcohol" = "ALC_FULL", "Other causes" = "OTHER_DEF", "Drug" = "DRUG_MEMO")
  dec <- read_csv(dec_file, show_col_types = FALSE) %>%
    filter(sex == "P", period %in% names(periods), band %in% names(bands), cause %in% names(m))
  ons <- bind_rows(dec %>% filter(comparison == "N_vs_SXL") %>% transmute(cause, band, period, area = "N", ons = dsr_A),
                   dec %>% filter(comparison == "N_vs_SXL") %>% transmute(cause, band, period, area = "X", ons = dsr_B),
                   dec %>% filter(comparison == "L_vs_SXL") %>% transmute(cause, band, period, area = "L", ons = dsr_A))
  chk <- ons %>% mutate(series = unname(m[cause])) %>% inner_join(rates, by = c("series", "band", "period", "area")) %>%
    group_by(cause = factor(cause, names(m))) %>%
    summarise(rates = n(), largest_difference_per_100000 = round(max(abs(dsr - ons)), 3), .groups = "drop")
  cat("2. NOMIS rates against the ONS extract (North, South excl. London, London; two bands; three periods):\n")
  print(as.data.frame(chk), row.names = FALSE)
} else {
  cat("2. Run 1_cause_decomposition.R first to compare the NOMIS rates with the ONS extract.\n")
}

cat("\nNorth minus South incl. London, 2021-25: gap per 100,000 (ratio); share of the residual gap\n")
for (s in c(names(groups), "g_REST", "OTHER_DEF")) {
  a <- filter(tab, series == s, period == "2021-25", band == "25-49")
  b <- filter(tab, series == s, period == "2021-25", band == "<75")
  cat(sprintf("   %-62s 25-49: %6.2f (%.2f) %5.1f%%   <75: %6.2f (%.2f) %5.1f%%\n", substr(label[[s]], 1, 62),
              a$gap_N_minus_S, a$ratio_N_to_S, a$pct_of_residual_gap_N_minus_S,
              b$gap_N_minus_S, b$ratio_N_to_S, b$pct_of_residual_gap_N_minus_S))
}
cat("done; output in", normalizePath(outdir), "\n")
