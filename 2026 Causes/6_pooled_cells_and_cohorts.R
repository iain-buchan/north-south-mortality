# =====================================================================
# North-South divide in English mortality by cause of death, 1981-2025
# Script 6 of 6: pooled cells and birth cohorts - death rates beside the ratios, London taken out of
#                the South, and age-period-cohort models of the excess in drug and alcohol deaths
#
# Iain Buchan, 18 Nov 2026. Exploratory work in progress; not peer reviewed.
#
# Why
#   The contour plots of script 5 show one year and one five-year age group at a time, and half of
#   those cells cannot be told from zero. A ratio can also fall while deaths rise. This script pools
#   the cells, puts death rates beside the ratios, separates London from the rest of the South, and
#   tests whether the excess in drug and alcohol deaths follows birth cohorts.
#
# What it does (cause groups of the 2026 extract, as in scripts 1 and 5; they overlap)
#   1  Lexis cells: five-year age group by five-year period, 1981-85 to 2021-25, North v South
#      (including London). Excess = 100 x (rate ratio - 1), with an exact 95% interval and test:
#      given the deaths in a cell, the number in the North is binomial.
#   2  Age bands by period (five-year periods to 2015-19, then 2020-22 and 2023-25) for the North,
#      the South, the South excluding London, and London: death rates directly standardised to the
#      2013 European Standard Population within the band, and the excess of one area over another
#      from a Poisson model of deaths on area and five-year age group (and sex, for persons), with
#      log person-years as offset.
#   3  Age-period-cohort models of the North:South rate ratio, by sex, in five-year cells for
#      1996-2025: drug poisoning at ages 15-64 and alcohol-related deaths at ages 25-64. Given the
#      deaths in a cell the number in the North is binomial, so a logistic model with
#      log(person-years North / person-years South) as offset models the log rate ratio directly.
#      Age, period and cohort enter as factors. Overdispersion is allowed for: the dispersion is
#      estimated from the full model and tests are F tests. A steady trend cannot be divided between
#      period and cohort. It is given to cohort here, so the period effects have no level and no
#      slope, and the tests of period and of cohort are tests of curvature. Cohort effects are
#      ratios of rate ratios, relative to people born 1956-65.
#   4  Three figures: death rates by area; the Lexis cells for drug and alcohol deaths; the cohort
#      effects.
#   Intervals are 95% and allow for Poisson variation only, except in step 3.
#
# Input (written by script 1 to ./output)
#   cell_level_deaths_pop.csv
#
# Outputs (written to ./output/pooled)
#   lexis_cells.csv                       step 1: every cell for six cause groups and persons, men and women
#   band_rates.csv, band_comparisons.csv  step 2: rates by area, and the excess of one area over another
#   apc_tests.csv, apc_effects.csv        step 3: model comparisons, and cohort and period effects
#   rates_by_area_alcohol_drugs           .svg and .png
#   lexis_excess_drugs_alcohol            .svg and .png
#   cohort_effects_drugs_alcohol          .svg and .png
#   pooled_captions.txt                   the captions, as text
#   The figures carry no title or caption, so that captions can be set as text beside them.
#
# Needs: dplyr, tidyr, readr, ggplot2 (3.4.0 or later), svglite (installed automatically if absent).
# About a minute.
# =====================================================================

pkgs <- c("dplyr", "tidyr", "readr", "ggplot2", "svglite")
new  <- pkgs[!pkgs %in% rownames(installed.packages())]
if (length(new)) install.packages(new, repos = "https://cloud.r-project.org")
suppressPackageStartupMessages(invisible(lapply(pkgs, library, character.only = TRUE)))
stopifnot(packageVersion("ggplot2") >= "3.4.0")
if (!isTRUE(l10n_info()[["UTF-8"]]))                     # the minus signs and dashes in the labels need a UTF-8 locale
  for (loc in c("en_GB.UTF-8", "C.UTF-8", "en_US.UTF-8")) if (nzchar(suppressWarnings(Sys.setlocale("LC_CTYPE", loc)))) break

# ---- configuration ---------------------------------------------------
in_file    <- file.path("output", "cell_level_deaths_pop.csv")
outdir     <- file.path("output", "pooled")
fig_font   <- "Arial"
min_deaths <- 20                                         # a cell with fewer deaths than this, in the two areas together, is flagged and left grey
test_level <- 0.05
ages   <- c("<1", "01-04", sprintf("%02d-%02d", seq(5, 80, 5), seq(9, 84, 5)), "85+")
groups <- list("All" = "All causes", "Accidents" = "Accidents", "Alcohol" = "Alcohol",
               "Cardiovascular, diabetes and obesity" = c("Cardiovascular", "Diabetes", "Obesity"),
               "Drugs" = "Drug", "Suicide" = "Suicide")
bands  <- list("15-24" = 5:6, "25-34" = 7:8, "35-44" = 9:10, "45-54" = 11:12, "55-64" = 13:14, "65 and over" = 15:19,
               "25-44" = 7:10, "45-64" = 11:14, "25-49" = 7:11)
periods <- list("1981-84" = 1981:1984, "1985-89" = 1985:1989, "1990-94" = 1990:1994, "1995-99" = 1995:1999, "2000-04" = 2000:2004,
                "2005-09" = 2005:2009, "2010-14" = 2010:2014, "2015-19" = 2015:2019, "2020-22" = 2020:2022, "2023-25" = 2023:2025)
comparisons <- list(c("North", "South"), c("North", "South excluding London"), c("North", "London"), c("London", "South excluding London"))
apc_spec <- list("Drugs" = 5:14, "Alcohol" = 7:14)       # age groups in the age-period-cohort models: 15-64 and 25-64
apc_from <- 1996                                         # six five-year periods, 1996-2000 to 2021-25
ref_born <- 1961                                         # the reference cohort, by its central birth year: born 1956-65
# colours: the bands of script 5, version b, and three areas
cuts      <- c(-40, -20, 0, 20, 40, 60, 80, 100, 150, 200, 250)
band_cols <- c("#347EC4", "#ABDBFE", "#FEF1A1", "#FBCE25", "#F6A100", "#EB7007", "#DB2E1E", "#AA1546", "#710F47", "#3B0A30")
grey_col  <- "#A6A6A6"
area_cols <- c("North" = "#EB6834", "South excluding London" = "#2A78D6", "London" = "#1BAF7A")
sex_cols  <- c("Women" = "#2A78D6", "Men" = "#EB6834")
ink <- "#0B0B0B"; ink2 <- "#52514E"; hair <- "#E1E0D9"
minus <- intToUtf8(8722); dash <- intToUtf8(8211)

if (!file.exists(in_file)) stop("Run script 1 first: ", in_file, " is missing.")
dir.create(outdir, recursive = TRUE, showWarnings = FALSE)
stopifnot(!anyDuplicated(unlist(periods)), setequal(unlist(periods), 1981:2025))

# ---- 0. deaths and populations ----------------------------------------------------------------
cells <- read_csv(in_file, show_col_types = FALSE) %>%
  mutate(agecat = match(age, ages), area = ifelse(area == "SouthXL", "South excluding London", area))
stopifnot(!anyNA(cells$agecat), all(unlist(groups) %in% cells$cause), setequal(cells$area, c("North", "South", "South excluding London", "London")),
          setequal(cells$sex, c("F", "M")), !anyNA(cells$pop), !any(duplicated(cells[, c("cause", "area", "sex", "age", "year")])))
xs <- bind_rows(lapply(names(groups), function(g) {      # by sex; deaths are NA where any series is missing (drug poisoning before 1993)
  cells %>% filter(cause %in% groups[[g]]) %>% group_by(area, sex, agecat, year) %>%
    summarise(deaths = sum(deaths), pop = first(pop), w = first(w), n = n(), .groups = "drop") %>% mutate(cause = g, parts = length(groups[[g]]))
}))
stopifnot(all(xs$n == xs$parts), nrow(xs) == length(groups) * 4 * 2 * 19 * 45)
xs <- xs %>% transmute(cause, area, sex = ifelse(sex == "M", "men", "women"), agecat, year, deaths, pop, w)
# South = South excluding London + London, in every cell with data
chk <- xs %>% select(cause, area, sex, agecat, year, deaths) %>% pivot_wider(names_from = area, values_from = deaths)
stopifnot(isTRUE(all.equal(chk$South, chk$`South excluding London` + chk$London)))

sig <- function(v, digits = 4) ifelse(is.na(v), NA_character_, formatC(v, format = "g", digits = digits))      # p values as text, so that none is written with spurious decimals
yr_span <- function(a, b) ifelse(a %/% 100 == b %/% 100, sprintf("%d-%02d", a, b %% 100), sprintf("%d-%d", a, b))      # 2001-05, 1996-2000
fmt_num <- function(v, digits = 0) { v <- round(v, digits); v[!is.na(v) & v == 0] <- 0; sub("-", minus, formatC(v, format = "f", digits = digits), fixed = TRUE) }

# ---- 1. Lexis cells: five-year age group by five-year period ------------------------------------
exact_rr <- function(dA, dB, pyA, pyB) {                 # rate ratio A:B with an exact (Clopper-Pearson) interval and binomial test
  n <- dA + dB; ok <- !is.na(n) & n > 0
  lo <- hi <- p <- rep(NA_real_, length(n))
  pl <- ifelse(dA[ok] == 0, 0, qbeta(0.025, dA[ok], dB[ok] + 1)); pu <- ifelse(dB[ok] == 0, 1, qbeta(0.975, dA[ok] + 1, dB[ok]))
  k  <- pyB[ok] / pyA[ok]
  lo[ok] <- pl / (1 - pl) * k; hi[ok] <- pu / (1 - pu) * k
  p[ok]  <- mapply(function(d, m, q) binom.test(d, m, q)$p.value, dA[ok], n[ok], (pyA / (pyA + pyB))[ok])
  data.frame(rate_ratio = ifelse(ok, (dA / pyA) / (dB / pyB), NA_real_), rr_lo = lo, rr_hi = hi, p_value = p)
}
lex <- xs %>% filter(area %in% c("North", "South")) %>% mutate(period = (year - 1981) %/% 5)
lex <- bind_rows(lex, lex %>% mutate(sex = "persons")) %>%
  group_by(cause, sex, area, agecat, period) %>% summarise(deaths = sum(deaths), py = sum(pop), .groups = "drop") %>%
  pivot_wider(names_from = area, values_from = c(deaths, py))
lex <- bind_cols(lex, exact_rr(lex$deaths_North, lex$deaths_South, lex$py_North, lex$py_South)) %>%
  mutate(age = ages[agecat], years = yr_span(1981 + 5 * period, 1985 + 5 * period),
         born = (1983 + 5 * period) - (5 * (agecat - 2) + 2),          # central birth year of the ten-year cohort, for five-year age groups
         deaths_both = deaths_North + deaths_South,
         status = case_when(is.na(deaths_both) ~ "no data", deaths_both < min_deaths ~ "few deaths", TRUE ~ "estimated"),
         rate_North = 1e5 * deaths_North / py_North, rate_South = 1e5 * deaths_South / py_South,
         excess = 100 * (rate_ratio - 1), excess_lo = 100 * (rr_lo - 1), excess_hi = 100 * (rr_hi - 1),
         unclear = status == "estimated" & p_value >= test_level) %>%
  arrange(cause, sex, period, agecat)
stopifnot(nrow(lex) == 6 * 3 * 19 * 9, all(lex$rr_lo <= lex$rate_ratio + 1e-9 | !is.finite(lex$rate_ratio), na.rm = TRUE))
write_csv(lex %>% transmute(cause, sex, age, years, born_about = ifelse(agecat >= 3, born, NA), deaths_North, deaths_South,
                            py_North = round(py_North), py_South = round(py_South), rate_North = round(rate_North, 3), rate_South = round(rate_South, 3),
                            excess_pct = round(excess, 1), excess_lo = round(excess_lo, 1), excess_hi = round(excess_hi, 1),
                            p_value = sig(p_value), status, unclear),
          file.path(outdir, "lexis_cells.csv"))

# ---- 2. age bands by period, with London apart ---------------------------------------------------
period_of <- setNames(rep(names(periods), lengths(periods)), unlist(periods))
agg <- xs %>% mutate(period = unname(period_of[as.character(year)])) %>%
  group_by(cause, sex, area, agecat, period) %>% summarise(deaths = sum(deaths), py = sum(pop), w = first(w), .groups = "drop")
agg <- bind_rows(agg, agg %>% group_by(cause, area, agecat, period) %>% summarise(deaths = sum(deaths), py = sum(py), w = first(w), .groups = "drop") %>% mutate(sex = "persons"))
rates <- bind_rows(lapply(names(bands), function(b) {
  agg %>% filter(agecat %in% bands[[b]]) %>% group_by(cause, sex, area, period) %>%
    summarise(rate = 1e5 * sum(w * deaths / py) / sum(w), se = 1e5 * sqrt(sum(w^2 * deaths / py^2)) / sum(w),      # from the age-specific rates,
              crude = 1e5 * sum(deaths) / sum(py), deaths = sum(deaths), py = sum(py), .groups = "drop") %>%              # before the deaths and person-years are summed
    mutate(band = b)
})) %>% mutate(rate_lo = pmax(0, rate - qnorm(0.975) * se), rate_hi = rate + qnorm(0.975) * se, status = ifelse(is.na(deaths), "no data", "estimated")) %>%
  mutate(cause = factor(cause, names(groups)), sex = factor(sex, c("persons", "men", "women")), band = factor(band, names(bands)),
         period = factor(period, names(periods)), area = factor(area, c("North", "South", "South excluding London", "London"))) %>%
  arrange(cause, sex, band, period, area)
stopifnot(nrow(rates) == 6 * 3 * 9 * 10 * 4)
write_csv(rates %>% transmute(cause, sex, ages = band, period, area, deaths, person_years = round(py), crude_rate = round(crude, 3),
                              standardised_rate = round(rate, 3), rate_lo = round(rate_lo, 3), rate_hi = round(rate_hi, 3), status),
          file.path(outdir, "band_rates.csv"))

poisson_rr <- function(d) {                              # d: one row per area, sex and age group; x = 1 in the first area
  d <- d %>% group_by(sex, agecat) %>% filter(sum(deaths) > 0) %>% ungroup()          # strata with no deaths say nothing about the ratio
  if (anyNA(d$deaths) || sum(d$deaths[d$x == 1]) == 0 || sum(d$deaths[d$x == 0]) == 0) return(c(NA_real_, NA_real_, NA_real_))
  tm <- c("x", if (n_distinct(d$agecat) > 1) "factor(agecat)", if (n_distinct(d$sex) > 1) "sex")
  m  <- glm(reformulate(tm, "deaths"), family = poisson, offset = log(py), data = d)
  b  <- coef(m)[["x"]]; se <- coef(summary(m))["x", "Std. Error"]
  100 * (exp(b + c(0, -1, 1) * qnorm(0.975) * se) - 1)
}
by_sex <- agg %>% filter(sex != "persons")
comp <- bind_rows(lapply(comparisons, function(cp) bind_rows(lapply(names(bands), function(b) {
  d <- by_sex %>% filter(area %in% cp, agecat %in% bands[[b]]) %>% mutate(x = as.numeric(area == cp[1]))
  bind_rows(lapply(c("persons", "men", "women"), function(s) {
    ds <- if (s == "persons") d else d[d$sex == s, ]
    ds %>% group_by(cause, period) %>% group_modify(~ { e <- poisson_rr(.x)
      data.frame(deaths_A = sum(.x$deaths[.x$x == 1]), deaths_B = sum(.x$deaths[.x$x == 0]), excess = e[1], excess_lo = e[2], excess_hi = e[3]) }) %>%
      ungroup() %>% mutate(sex = s)
  })) %>% mutate(band = b, area_A = cp[1], area_B = cp[2])
}))))
comp <- comp %>% mutate(cause = factor(cause, names(groups)), sex = factor(sex, c("persons", "men", "women")), band = factor(band, names(bands)),
                        period = factor(period, names(periods)), comparison = factor(paste(area_A, "v", area_B), sapply(comparisons, paste, collapse = " v "))) %>%
  arrange(cause, sex, band, period, comparison)
stopifnot(nrow(comp) == 6 * 3 * 9 * 10 * 4)
write_csv(comp %>% transmute(cause, sex, ages = band, period, comparison, deaths_A, deaths_B, excess_pct = round(excess, 1), excess_lo = round(excess_lo, 1), excess_hi = round(excess_hi, 1)),
          file.path(outdir, "band_comparisons.csv"))

# ---- 3. age-period-cohort models of the rate ratio -----------------------------------------------
born_span <- function(b) yr_span(b - 5, b + 4)
apc_fit <- function(g, s) {
  d <- lex %>% filter(cause == g, sex == s, agecat %in% apc_spec[[g]], 1981 + 5 * period >= apc_from) %>%
    mutate(off = log(py_North / py_South), A = factor(agecat), P = factor(period), C = relevel(factor(born), ref = as.character(ref_born)),
           drift = period - mean(period))
  stopifnot(!anyNA(d$deaths_both), nrow(d) == length(apc_spec[[g]]) * 6)
  fit <- function(terms, family = binomial) glm(reformulate(terms, "cbind(deaths_North, deaths_South)"), family = family, offset = off, data = d)
  m <- list(A = fit("A"), Ad = fit(c("A", "drift")), AP = fit(c("A", "P")), AC = fit(c("A", "C")), APC = fit(c("A", "P", "C")))
  phi <- sum(residuals(m$APC, "pearson")^2) / m$APC$df.residual
  ftest <- function(small, big, what) {
    ddf <- m[[small]]$df.residual - m[[big]]$df.residual; Fv <- ((deviance(m[[small]]) - deviance(m[[big]])) / ddf) / phi
    data.frame(test = what, F = Fv, df1 = ddf, df2 = m$APC$df.residual, p_value = pf(Fv, ddf, m$APC$df.residual, lower.tail = FALSE))
  }
  tests <- bind_rows(ftest("A", "Ad", "steady trend, given age"), ftest("Ad", "AP", "period curvature, given age and trend"),
                     ftest("Ad", "AC", "cohort curvature, given age and trend"), ftest("AP", "APC", "cohort, given age and period"),
                     ftest("AC", "APC", "period, given age and cohort"))
  fits <- data.frame(model = c("age", "age + trend", "age + period", "age + cohort", "age + period + cohort"),
                     deviance = sapply(m, deviance), df = sapply(m, function(z) z$df.residual))
  # the full model again, with the trend given to cohort: period columns that have no level and no slope
  np <- n_distinct(d$period); Pn <- poly(seq_len(np), degree = np - 1)[, -1, drop = FALSE]
  d$Pc <- Pn[d$period - min(d$period) + 1, , drop = FALSE]
  mq <- fit(c("A", "C", "Pc"), family = quasibinomial)
  stopifnot(abs(deviance(mq) - deviance(m$APC)) < 1e-6, abs(summary(mq)$dispersion / phi - 1) < 1e-4)          # the same model, written another way
  b <- coef(mq); V <- vcov(mq); tq <- qt(0.975, mq$df.residual)
  ic <- grep("^C", names(b)); ip <- grep("^Pc", names(b)); ia <- grep("^A", names(b))
  coh <- data.frame(born = c(ref_born, as.numeric(sub("^C", "", names(b)[ic]))), est = c(0, b[ic]), se = c(0, sqrt(diag(V)[ic])))
  seen <- d %>% group_by(born) %>% summarise(deaths_North = sum(deaths_North), deaths_South = sum(deaths_South),
                                             ages_seen = paste(ages[min(agecat)], "to", ages[max(agecat)]), .groups = "drop")
  coh <- left_join(coh, seen, by = "born") %>% arrange(born) %>%
    transmute(effect = "cohort", level = paste("born", born_span(born)), born, ratio = exp(est), lo = exp(est - tq * se), hi = exp(est + tq * se),
              deaths_North, deaths_South, ages_seen)
  pe <- as.vector(Pn %*% b[ip]); pse <- sqrt(diag(Pn %*% V[ip, ip] %*% t(Pn)))
  per <- data.frame(effect = "period, curvature only", level = sort(unique(d$years)), born = NA, ratio = exp(pe), lo = exp(pe - tq * pse), hi = exp(pe + tq * pse))
  # rate ratio by age for the reference cohort, period effects at their average
  La <- cbind(1, rbind(0, diag(length(ia)))); ea <- as.vector(La %*% b[c(1, ia)]); sa <- sqrt(diag(La %*% V[c(1, ia), c(1, ia)] %*% t(La)))
  age <- data.frame(effect = "age, for the reference cohort (North:South rate ratio)", level = ages[sort(unique(d$agecat))], born = NA,
                    ratio = exp(ea), lo = exp(ea - tq * sa), hi = exp(ea + tq * sa))
  list(tests = cbind(cause = g, sex = s, dispersion = phi, tests), fits = cbind(cause = g, sex = s, dispersion = phi, fits),
       effects = cbind(cause = g, sex = s, bind_rows(age, per, coh)))
}
apc <- unlist(lapply(names(apc_spec), function(g) lapply(c("women", "men"), function(s) apc_fit(g, s))), recursive = FALSE)
apc_tests   <- bind_rows(lapply(apc, `[[`, "tests")); apc_fits <- bind_rows(lapply(apc, `[[`, "fits")); apc_effects <- bind_rows(lapply(apc, `[[`, "effects"))
write_csv(bind_rows(apc_fits %>% transmute(cause, sex, dispersion = round(dispersion, 3), item = paste("fit:", model), deviance = round(deviance, 2), df),
                    apc_tests %>% transmute(cause, sex, dispersion = round(dispersion, 3), item = paste("test:", test), F = round(F, 3), df1, df2, p_value = sig(p_value, 3))),
          file.path(outdir, "apc_tests.csv"), na = "")
write_csv(apc_effects %>% transmute(cause, sex, effect, level, ratio = sprintf("%.4f", ratio), lo = sprintf("%.4f", lo), hi = sprintf("%.4f", hi), deaths_North, deaths_South, ages_seen),
          file.path(outdir, "apc_effects.csv"), na = "")

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
# the element it applies to, and the style sheet is removed (as in scripts 4 and 5).
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
  ggsave(file.path(outdir, paste0(name, ".png")), p, width = w, height = h, dpi = 200)
}
base_theme <- theme_minimal(base_size = 9, base_family = fig_font) +
  theme(text = element_text(colour = ink), axis.text = element_text(colour = ink2), panel.grid.minor = element_blank(),
        panel.grid.major = element_line(colour = hair, linewidth = 0.25), strip.text = element_text(face = "bold", hjust = 0, colour = ink),
        legend.position = "top", legend.justification = "left", legend.title = element_blank(), plot.margin = margin(6, 10, 6, 6))
span_label <- function(v) sub("-", dash, v, fixed = TRUE)

# 4a. death rates in the North, the South outside London, and London
fig_bands <- list("Alcohol" = c("25-44", "45-64", "65 and over"), "Drugs" = c("15-24", "25-44", "45-64"))
cause_words <- c("Alcohol" = "Alcohol-related", "Drugs" = "Drug poisoning")
ra <- bind_rows(lapply(names(fig_bands), function(g) rates %>% filter(cause == g, band %in% fig_bands[[g]]))) %>%
  filter(sex == "persons", area != "South", period %in% names(periods)[4:10], status == "estimated") %>%
  mutate(mid = sapply(periods[as.character(period)], mean), area = factor(as.character(area), names(area_cols)),
         panel = paste0(cause_words[as.character(cause)], ", ages ", sub(" and over", "+", span_label(as.character(band)))))
ra$panel <- factor(ra$panel, unlist(lapply(names(fig_bands), function(g) paste0(cause_words[[g]], ", ages ", sub(" and over", "+", span_label(fig_bands[[g]]))))))
stopifnot(!anyNA(ra$panel))
stopifnot(nrow(ra) == 2 * 3 * 3 * 7)
p_rates <- ggplot(ra, aes(mid, rate, colour = area, shape = area)) +
  annotate("rect", xmin = 2019.5, xmax = 2022.5, ymin = -Inf, ymax = Inf, fill = "#F0EFEC") +
  geom_linerange(aes(ymin = rate_lo, ymax = rate_hi), linewidth = 0.35, show.legend = FALSE) +
  geom_line(linewidth = 0.6) + geom_point(size = 1.7, fill = "white", stroke = 0.7) +
  facet_wrap(~ panel, ncol = 3, scales = "free_y") +
  scale_colour_manual(values = area_cols) + scale_shape_manual(values = c(16, 21, 24)) +
  scale_x_continuous(breaks = sapply(periods[4:10], mean), labels = span_label(names(periods)[4:10])) +
  scale_y_continuous(limits = c(0, NA), expand = expansion(mult = c(0, 0.06))) +
  labs(x = NULL, y = "Deaths per 100,000 a year") + base_theme +
  theme(axis.text.x = element_text(angle = 45, hjust = 1), panel.grid.major.x = element_blank(), panel.spacing = unit(0.5, "cm"))
save_fig(p_rates, "rates_by_area_alcohol_drugs", 7.5, 5.2)

# 4b. the Lexis cells for drug and alcohol deaths, with numbers
lx <- lex %>% filter(cause %in% names(apc_spec), sex != "persons", agecat >= 5, 1981 + 5 * period >= apc_from) %>%
  mutate(band = findInterval(pmin(pmax(excess - 1e-6, cuts[1] + 1e-6), max(cuts) - 1e-6), cuts),
         shown = status == "estimated", label = ifelse(shown, fmt_num(excess), ""),
         panel_cause = factor(cause_words[cause], cause_words[c("Drugs", "Alcohol")]), panel_sex = factor(ifelse(sex == "women", "Women", "Men"), c("Women", "Men")),
         x = period - min(period) + 1, y = agecat - 4)
band_names <- c(paste("below", fmt_num(cuts[2])), paste(fmt_num(cuts[2:9]), "to", fmt_num(cuts[3:10])), paste(fmt_num(cuts[10]), "or more"))
lx$fill <- factor(ifelse(lx$shown, band_names[lx$band], NA), band_names)
grey_key <- sprintf("Fewer than %d deaths", min_deaths); dot_key <- sprintf("Excess not clear\nat the %g%% level", 100 * test_level)
ring_key <- paste("Born", span_label(born_span(1976)))
p_lexis <- ggplot(lx, aes(x, y)) +
  geom_tile(data = lx[!lx$shown, ], aes(alpha = grey_key), fill = grey_col, colour = "white", linewidth = 0.4) +
  geom_tile(data = lx[lx$shown, ], aes(fill = fill), colour = "white", linewidth = 0.4, show.legend = c(fill = TRUE, alpha = FALSE, shape = FALSE, linetype = FALSE)) +
  geom_tile(data = lx[lx$born == 1976, ], aes(linetype = ring_key), fill = NA, colour = ink, linewidth = 0.55) +
  geom_text(aes(label = label, colour = band %in% c(1, 7:10)), size = 6.6 / .pt, family = fig_font, show.legend = FALSE) +
  geom_point(data = lx[lx$unclear, ], aes(x + 0.36, y + 0.3, shape = dot_key), fill = "white", colour = ink, size = 0.9, stroke = 0.3) +
  facet_grid(panel_cause ~ panel_sex) +
  scale_fill_manual(values = setNames(band_cols, band_names), limits = band_names, drop = FALSE, name = "% excess mortality\nin the North",
                    guide = guide_legend(reverse = TRUE, order = 1)) +
  scale_colour_manual(values = c(`TRUE` = "white", `FALSE` = ink)) +
  scale_alpha_manual(name = NULL, values = setNames(1, grey_key), guide = guide_legend(order = 2)) +
  scale_linetype_manual(name = NULL, values = setNames("solid", ring_key), guide = guide_legend(order = 3, override.aes = list(fill = "white"))) +
  scale_shape_manual(name = NULL, values = setNames(21, dot_key), guide = guide_legend(order = 4, override.aes = list(size = 1.6, stroke = 0.4))) +
  scale_x_continuous(breaks = 1:6, labels = span_label(yr_span(apc_from + 5 * 0:5, apc_from + 4 + 5 * 0:5)), expand = c(0, 0)) +
  scale_y_continuous(breaks = 1:15, labels = span_label(ages[5:19]), expand = c(0, 0)) +
  labs(x = "Period", y = "Age group") + base_theme +
  theme(panel.grid = element_blank(), legend.position = "right", legend.justification = "center", legend.title = element_text(colour = ink),
        legend.key.height = unit(0.5, "cm"), legend.key.width = unit(0.5, "cm"), axis.text.x = element_text(angle = 45, hjust = 1),
        strip.text.y = element_text(angle = 270, hjust = 0.5), panel.spacing = unit(0.45, "cm"))
save_fig(p_lexis, "lexis_excess_drugs_alcohol", 7.5, 8.6)

# 4c. the cohort effects
ce <- apc_effects %>% filter(effect == "cohort") %>%
  mutate(panel = factor(cause_words[cause], cause_words[c("Drugs", "Alcohol")]), sex = factor(ifelse(sex == "women", "Women", "Men"), names(sex_cols)),
         xx = born + ifelse(sex == "Women", -0.5, 0.5))
y_rng <- c(0.4, 2.5)
p_cohort <- ggplot(ce, aes(xx, ratio, colour = sex, shape = sex)) +
  geom_hline(yintercept = 1, colour = "#C3C2B7", linewidth = 0.4) +
  geom_linerange(aes(ymin = pmax(lo, y_rng[1]), ymax = pmin(hi, y_rng[2])), linewidth = 0.4, show.legend = FALSE) +
  geom_line(linewidth = 0.6) + geom_point(size = 1.8, fill = "white", stroke = 0.7) +
  facet_wrap(~ panel, ncol = 2) +
  scale_colour_manual(values = sex_cols) + scale_shape_manual(values = c(16, 21)) +
  scale_x_continuous(breaks = seq(1936, 2006, 10), labels = function(b) span_label(born_span(b))) +
  scale_y_log10(breaks = c(0.5, 0.75, 1, 1.5, 2), labels = function(v) formatC(v, format = "g")) +
  coord_cartesian(ylim = y_rng) +
  labs(x = "Born", y = paste0("North:South rate ratio,\nrelative to people born ", span_label(born_span(ref_born)))) + base_theme +
  theme(axis.text.x = element_text(angle = 45, hjust = 1), panel.grid.major.x = element_blank(), panel.spacing = unit(0.6, "cm"))
save_fig(p_cohort, "cohort_effects_drugs_alcohol", 7.5, 3.9)

# ---- 5. captions, as text -------------------------------------------------------------------------
src <- "Source: ONS death registrations and mid-year population estimates; cause groups of the November 2026 ONS extract."
captions <- c(
  paste0("Figure 4. Death rates from alcohol-related causes and drug poisoning in the North, the South outside London, and London, by age, 1995", dash, "2025"),
  paste("Persons. Rates are directly age-standardised to the 2013 European Standard Population within each age band, for five-year periods to",
        paste0("2015", dash, "19"), "and then", paste0("2020", dash, "22"), "(shaded) and", paste0("2023", dash, "25."), "Lines are 95% confidence intervals. Each panel has its own scale.", src),
  "",
  paste0("Figure 5. Northern excess in death rates from drug poisoning and alcohol-related causes, by age group and five-year period, 1996", dash, "2025"),
  paste("Percentage by which the death rate in the North exceeds that in the South (including London), with no adjustment.",
        sprintf("Grey: fewer than %d deaths in the two areas together. A dot marks a cell whose excess cannot be told from zero at the %g%% level (exact binomial test).", min_deaths, 100 * test_level),
        "The outlined cells follow people born", paste0(span_label(born_span(1976)), "."), "The exact 95% confidence intervals are in lexis_cells.csv.", src),
  "",
  "Figure 6. Northern excess in death rates from drug poisoning and alcohol-related causes, by birth cohort",
  paste("Ratio of the North:South rate ratio to that of people born", paste0(span_label(born_span(ref_born)), ","),
        "from age-period-cohort models of the rate ratio in five-year cells,", paste0("1996", dash, "2025"), "(drug poisoning at ages", paste0("15", dash, "64,"),
        "alcohol-related causes at ages", paste0("25", dash, "64)."), "A steady trend cannot be divided between period and cohort and is given to cohort here.",
        "Lines are 95% confidence intervals, which allow for overdispersion.", src))
writeLines(enc2utf8(captions), file.path(outdir, "pooled_captions.txt"), useBytes = TRUE)

# ---- 6. summary -------------------------------------------------------------------------------------
cat("\nPooled cells and cohort models written to", normalizePath(outdir), "\n")
cat(sprintf("Lexis cells: %d; with %d or more deaths %d; of those, %d (%.0f%%) cannot be told from zero excess\n",
            nrow(lex), min_deaths, sum(lex$status == "estimated"), sum(lex$unclear), 100 * sum(lex$unclear) / sum(lex$status == "estimated")))
cat("\nAge-period-cohort models of the North:South rate ratio (F tests, overdispersion allowed for):\n")
print(as.data.frame(apc_tests %>% transmute(cause, sex, dispersion = round(dispersion, 2), test, F = round(F, 2), df1, df2, p_value = signif(p_value, 2))), row.names = FALSE)
