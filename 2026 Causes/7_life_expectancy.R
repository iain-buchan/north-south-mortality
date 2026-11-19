# =====================================================================
# North-South divide in English mortality by cause of death, 1981-2025
# Script 7 of 7: life expectancy - the gap in life expectancy at birth between North and South, and
#                the ages and causes of death that make it up
#
# Iain Buchan, 18 Nov 2026. Exploratory work in progress; not peer reviewed.
#
# Why
#   Rate ratios are largest at working ages, and counts of excess deaths are largest in old age.
#   Life expectancy weighs each death by the years lost, and so shows what the two together amount to.
#
# What it does (by sex; North v South including London)
#   1  Period life expectancy at birth in each year from 1981 to 2025 and in ten periods (five-year
#      periods to 2015-19, then 2020-22 and 2023-25), from abridged life tables: 19 age groups, the
#      last one open at 85. Average years lived in the interval by those who die in it: Coale-Demeny
#      values in infancy and at ages 1-4, half the interval at other ages, and one over the death rate
#      at 85 and over. Intervals for life expectancy and for the gap are Chiang's, with Silcocks'
#      term for the last age group. The same life tables give the chance of dying between the
#      ages of 25 and 65.
#   2  The gap in life expectancy (South minus North) divided among the 19 age groups by Arriaga's
#      method, then summed to four age bands. Arriaga's formula treats its two populations
#      differently, so the answer depends a little on which is taken first. The contribution of an
#      age group is the mean of the two answers (North first and South first), and both are given
#      in the files. Within each age group the contribution is shared among causes in proportion
#      to their part of the difference in death rates.
#      Causes are the groups of the 2026 extract (script 1): cardiovascular, diabetes, accidents,
#      suicide, alcohol-related and other causes. They overlap a little: alcohol-related deaths
#      include codes that also sit in three of the other groups, and 'other causes' subtracts those
#      deaths twice, so the six parts add up to the whole gap. Accidents include accidental
#      poisoning by drugs and alcohol. Drug poisoning is given as a memo item from 1995-99: it cuts
#      across accidents, suicide and other causes and is not part of the sum.
#   3  95% intervals for the contributions and shares, from 2,000 simulated sets of deaths: the
#      deaths in each area, age group and cause group are drawn as Poisson counts around those
#      observed, and the life tables and the division of the gap are recomputed each time.
#   4  Two figures: life expectancy in North and South; the gap by age band of death.
#   Intervals allow for Poisson variation in the deaths only.
#
# Checks made when the script runs
#   The six cause groups add up to all causes in every cell. The contributions add up to the gap.
#   Arriaga's formula, each way round, is checked against replacing the death rates one age group
#   at a time. The simulated interval for the gap is compared with Chiang's.
#   Life expectancy for England in 2021-23 is compared with the national life tables of the Office
#   for National Statistics, which use single years of age (79.1 years for men, 83.0 for women).
#
# Input (written by script 1 to ./output)
#   cell_level_deaths_pop.csv
#
# Outputs (written to ./output/life_expectancy)
#   life_expectancy_by_year.csv, life_expectancy_by_period.csv   step 1: life expectancy and the gap (years), and the chance of dying between 25 and 65 (%)
#   gap_by_age_group.csv                  step 2: the 19 age groups, each period and sex, with the two answers that are averaged
#   gap_by_age_band.csv                   steps 2 and 3: four age bands and ages 25-64, with intervals
#   gap_by_cause.csv                      steps 2 and 3: six cause groups and the drug-poisoning memo item, with intervals
#   gap_by_age_band_and_cause.csv         step 2: age band by cause group
#   life_expectancy_north_south           .svg and .png
#   life_expectancy_gap_by_age            .svg and .png
#   life_expectancy_captions.txt          the captions, as text
#   The figures carry no title or caption, so that captions can be set as text beside them.
#
# Needs: dplyr, tidyr, readr, ggplot2 (3.4.0 or later), svglite (installed automatically if absent).
# About ten seconds.
# =====================================================================

pkgs <- c("dplyr", "tidyr", "readr", "ggplot2", "svglite")
new  <- pkgs[!pkgs %in% rownames(installed.packages())]
if (length(new)) install.packages(new, repos = "https://cloud.r-project.org")
suppressPackageStartupMessages(invisible(lapply(pkgs, library, character.only = TRUE)))
stopifnot(packageVersion("ggplot2") >= "3.4.0")
if (!isTRUE(l10n_info()[["UTF-8"]]))                     # the minus signs and dashes in the labels need a UTF-8 locale
  for (loc in c("en_GB.UTF-8", "C.UTF-8", "en_US.UTF-8")) if (nzchar(suppressWarnings(Sys.setlocale("LC_CTYPE", loc)))) break

# ---- configuration ---------------------------------------------------
in_file  <- file.path("output", "cell_level_deaths_pop.csv")
outdir   <- file.path("output", "life_expectancy")
fig_font <- "Arial"
ages     <- c("<1", "01-04", sprintf("%02d-%02d", seq(5, 80, 5), seq(9, 84, 5)), "85+")
width    <- c(1, 4, rep(5, 16), NA)                      # years in each age group; the last is open
periods  <- list("1981-84" = 1981:1984, "1985-89" = 1985:1989, "1990-94" = 1990:1994, "1995-99" = 1995:1999, "2000-04" = 2000:2004,
                 "2005-09" = 2005:2009, "2010-14" = 2010:2014, "2015-19" = 2015:2019, "2020-22" = 2020:2022, "2023-25" = 2023:2025)
band_of  <- cut(1:19, c(0, 6, 10, 14, 19), labels = c("Under 25", "25-44", "45-64", "65 and over"))
causes   <- c("Cardiovascular", "Diabetes", "Accidents", "Suicide", "Alcohol", "Other causes")      # these add up to all causes
memo     <- "Drug"                                       # drug poisoning: overlaps the groups above, and starts in 1993
n_sim    <- 2000
seed     <- 20261118
ons_england_2021_23 <- c(men = 79.1, women = 83.0)       # ONS national life tables, England, 2021 to 2023
area_cols <- c("North" = "#EB6834", "South" = "#2A78D6")
band_cols <- c("Under 25" = "#86B6EF", "25-44" = "#3987E5", "45-64" = "#1C5CAB", "65 and over" = "#0D366B")
ink <- "#0B0B0B"; ink2 <- "#52514E"; hair <- "#E1E0D9"
minus <- intToUtf8(8722); dash <- intToUtf8(8211)

if (!file.exists(in_file)) stop("Run script 1 first: ", in_file, " is missing.")
dir.create(outdir, recursive = TRUE, showWarnings = FALSE)
stopifnot(!anyDuplicated(unlist(periods)), setequal(unlist(periods), 1981:2025))

# ---- 0. deaths and populations ----------------------------------------------------------------
cells <- read_csv(in_file, show_col_types = FALSE) %>% filter(area %in% c("North", "South"), cause %in% c("All causes", causes, memo)) %>%
  mutate(agecat = match(age, ages), sex = ifelse(sex == "M", "men", "women"))
stopifnot(!anyNA(cells$agecat), !anyNA(cells$pop), nrow(cells) == 8 * 2 * 2 * 19 * 45,
          !any(duplicated(cells[, c("cause", "area", "sex", "agecat", "year")])))
parts <- cells %>% filter(cause %in% causes) %>% group_by(area, sex, agecat, year) %>% summarise(parts = sum(deaths), .groups = "drop") %>%
  left_join(cells %>% filter(cause == "All causes") %>% select(area, sex, agecat, year, all = deaths), by = c("area", "sex", "agecat", "year"))
stopifnot(all(parts$parts == parts$all))                 # the six cause groups add up to all causes in every cell
stopifnot(all(cells$deaths >= 0, na.rm = TRUE))

# ---- 1. life tables -----------------------------------------------------------------------------
life_table <- function(m, sex, ax = NULL) {              # m: death rates in the 19 age groups
  if (is.null(ax)) {
    ax <- width / 2
    ax[1] <- if (sex == "men") 0.045 + 2.684 * m[1] else 0.053 + 2.800 * m[1]       # Coale-Demeny, where infant mortality is low
    ax[2] <- if (sex == "men") 1.651 - 2.816 * m[1] else 1.522 - 1.518 * m[1]
  }
  q <- width * m / (1 + (width - ax) * m); q[19] <- 1
  l <- c(1, cumprod(1 - q[-19])); d <- l * q
  L <- width * (l - d) + ax * d; L[19] <- l[19] / m[19]
  Tx <- rev(cumsum(rev(L)))
  list(l = l, L = L, T = Tx, q = q, ax = ax, e0 = Tx[1])
}
e0_se <- function(lt, deaths) {                          # Chiang (1984), with the term of Silcocks et al. (2001) for the open age group
  e_next <- c(lt$T[-1] / lt$l[-1], 0)
  v <- lt$l^2 * ((width - lt$ax) + e_next)^2 * ifelse(deaths > 0, lt$q^2 * (1 - lt$q) / deaths, 0)
  v[19] <- lt$T[19]^2 / deaths[19]
  sqrt(sum(v))
}
z975 <- qnorm(0.975)
dying_25_65 <- function(lt, deaths) {                    # chance of dying between the ages of 25 and 65, %, with its 95% interval (the variance of q as in Chiang)
  i <- 7:14; alive <- lt$l[15] / lt$l[7]; se_log <- sqrt(sum(lt$q[i]^2 / ((1 - lt$q[i]) * deaths[i])))
  100 * (1 - alive * exp(c(0, z975, -z975) * se_log))
}
e0_table <- function(d) {                                # d: deaths and person-years by area, sex and age group for one span of years
  d <- d %>% arrange(area, sex, agecat)
  out <- d %>% group_by(area, sex) %>% group_modify(~ { lt <- life_table(.x$deaths / .x$py, .y$sex); dy <- dying_25_65(lt, .x$deaths)
                                                        data.frame(e0 = lt$e0, se = e0_se(lt, .x$deaths), dying = dy[1], dying_lo = dy[2], dying_hi = dy[3]) }) %>% ungroup()
  out %>% pivot_wider(names_from = area, values_from = c(e0, se, dying, dying_lo, dying_hi)) %>%
    mutate(gap = e0_South - e0_North, gap_se = sqrt(se_North^2 + se_South^2))
}
allc <- cells %>% filter(cause == "All causes")
by_year <- allc %>% group_by(year, area, sex, agecat) %>% summarise(deaths = sum(deaths), py = sum(pop), .groups = "drop") %>%
  group_by(year) %>% group_modify(~ e0_table(.x)) %>% ungroup()
period_of <- setNames(rep(names(periods), lengths(periods)), unlist(periods))
cells$period <- factor(unname(period_of[as.character(cells$year)]), names(periods))
pooled <- cells %>% group_by(cause, period, area, sex, agecat) %>% summarise(deaths = sum(deaths), py = sum(pop), .groups = "drop")      # deaths are NA where a year is missing
by_period <- pooled %>% filter(cause == "All causes") %>% group_by(period) %>% group_modify(~ e0_table(.x)) %>% ungroup()
fx <- function(v, d) ifelse(is.na(v), NA_character_, formatC(round(v, d) + 0, format = "f", digits = d))      # numbers for the files, as text with fixed decimals: no 1e-04 and no -0
r1 <- function(v) fx(v, 1); r2 <- function(v) fx(v, 2); r3 <- function(v) fx(v, 3); r4 <- function(v) fx(v, 4)
tidy_e0 <- function(d) d %>%
  mutate(North_lo = e0_North - z975 * se_North, North_hi = e0_North + z975 * se_North, South_lo = e0_South - z975 * se_South, South_hi = e0_South + z975 * se_South,
         gap_lo = gap - z975 * gap_se, gap_hi = gap + z975 * gap_se) %>%
  transmute(sex, across(c(e0_North, North_lo, North_hi, e0_South, South_lo, South_hi, gap, gap_lo, gap_hi), r3),
            dying_25_65_North = r2(dying_North), dying_North_lo = r2(dying_lo_North), dying_North_hi = r2(dying_hi_North),
            dying_25_65_South = r2(dying_South), dying_South_lo = r2(dying_lo_South), dying_South_hi = r2(dying_hi_South))
write_csv(bind_cols(year = by_year$year, tidy_e0(by_year)), file.path(outdir, "life_expectancy_by_year.csv"))
write_csv(bind_cols(period = by_period$period, tidy_e0(by_period)), file.path(outdir, "life_expectancy_by_period.csv"))

# England in 2021-23, against the national life tables
eng <- allc %>% filter(year %in% 2021:2023) %>% group_by(sex, agecat) %>% summarise(deaths = sum(deaths), py = sum(pop), .groups = "drop") %>% arrange(sex, agecat)
eng_e0 <- sapply(c("men", "women"), function(s) life_table(with(eng[eng$sex == s, ], deaths / py), s)$e0)
stopifnot(all(abs(eng_e0 - ons_england_2021_23) < 0.3))

# ---- 2. the gap divided by age and cause -------------------------------------------------------
arriaga <- function(A, B) {                              # what each age group adds to e0(B) - e0(A), with A as the first population
  i <- 1:18
  c(A$l[i] * (B$L[i] / B$l[i] - A$L[i] / A$l[i]) + B$T[i + 1] * (A$l[i] / B$l[i] - A$l[i + 1] / B$l[i + 1]),
    A$l[19] * (B$T[19] / B$l[19] - A$T[19] / A$l[19]))
}
one_at_a_time <- function(mA, mB, A, B, sex) {           # the same thing the long way: start from B and give it A's death rates, youngest age first
  m <- mB; ax <- B$ax; e <- B$e0; out <- numeric(19)
  for (x in 1:19) { m[x] <- mA[x]; ax[x] <- A$ax[x]; e_new <- life_table(m, sex, ax)$e0; out[x] <- e - e_new; e <- e_new }
  out
}
divide <- function(dN, dS, pyN, pyS, sex, drugN = NULL, drugS = NULL, check = FALSE) {       # dN, dS: deaths by age group (rows) and cause group (columns)
  mN <- rowSums(dN) / pyN; mS <- rowSums(dS) / pyS
  N <- life_table(mN, sex); S <- life_table(mS, sex); gap <- S$e0 - N$e0
  north_first <- arriaga(N, S); south_first <- -arriaga(S, N)                               # the gap, South minus North, divided each way round
  if (check) stopifnot(abs(sum(north_first) - gap) < 1e-9, abs(sum(south_first) - gap) < 1e-9,
                       max(abs(north_first - one_at_a_time(mN, mS, N, S, sex))) < 1e-9, max(abs(south_first + one_at_a_time(mS, mN, S, N, sex))) < 1e-9)
  con <- (north_first + south_first) / 2
  per_unit <- ifelse(mS == mN, 0, con / (mS - mN))        # years of the gap for each unit of difference in the death rate
  by_cause <- per_unit * (dS / pyS - dN / pyN)            # age group by cause group; each row adds up to that age group's contribution
  drug <- if (is.null(drugN)) NA_real_ else sum(per_unit * (drugS / pyS - drugN / pyN))
  list(gap = gap, age = con, north_first = north_first, south_first = south_first, cause = colSums(by_cause), band_cause = rowsum(by_cause, band_of), drug = drug)
}
wide <- function(d, s, p, ar) {                          # deaths by age group (rows) and cause group (columns)
  z <- d %>% filter(sex == s, period == p, area == ar, cause %in% causes)
  matrix(z$deaths[order(match(z$cause, causes), z$agecat)], 19, length(causes), dimnames = list(NULL, causes))
}
one <- function(d, s, p, ar, ca, what) { z <- d %>% filter(sex == s, period == p, area == ar, cause == ca) %>% arrange(agecat); z[[what]] }
bands_of <- function(v) { b <- tapply(v, band_of, sum); c(b, "25-64" = unname(b["25-44"] + b["45-64"])) }
set.seed(seed)
res <- list(); detail <- list()
for (s in c("women", "men")) for (p in names(periods)) {
  dN <- wide(pooled, s, p, "North"); dS <- wide(pooled, s, p, "South"); pyN <- one(pooled, s, p, "North", "All causes", "py"); pyS <- one(pooled, s, p, "South", "All causes", "py")
  gN <- one(pooled, s, p, "North", memo, "deaths"); gS <- one(pooled, s, p, "South", memo, "deaths"); has_drug <- !anyNA(c(gN, gS))
  est <- divide(dN, dS, pyN, pyS, s, if (has_drug) gN, if (has_drug) gS, check = TRUE)
  # ---- 3. simulated sets of deaths ----
  sim <- t(replicate(n_sim, {
    v <- divide(matrix(rpois(length(dN), dN), 19), matrix(rpois(length(dS), dS), 19), pyN, pyS, s,
                if (has_drug) rpois(19, gN), if (has_drug) rpois(19, gS))
    c(gap = v$gap, bands_of(v$age), setNames(v$cause, causes), Drug = v$drug)
  }))
  point <- c(gap = est$gap, bands_of(est$age), setNames(est$cause, causes), Drug = est$drug)
  stopifnot(identical(names(point), colnames(sim)))
  share <- 100 * sim / sim[, "gap"]
  res[[paste(s, p)]] <- data.frame(sex = s, period = p, item = names(point), years = point, years_lo = apply(sim, 2, quantile, 0.025, na.rm = TRUE), years_hi = apply(sim, 2, quantile, 0.975, na.rm = TRUE),
                                   share_pct = 100 * point / point[["gap"]], share_lo = apply(share, 2, quantile, 0.025, na.rm = TRUE), share_hi = apply(share, 2, quantile, 0.975, na.rm = TRUE),
                                   row.names = NULL)
  bn <- bands_of(est$north_first); bs <- bands_of(est$south_first)
  detail[[paste(s, p)]] <- list(
    age = data.frame(sex = s, period = p, age = ages, years = est$age, share_pct = 100 * est$age / est$gap, years_north_first = est$north_first, years_south_first = est$south_first),
    band = data.frame(sex = s, period = p, item = names(bn), share_north_first = 100 * bn / est$gap, share_south_first = 100 * bs / est$gap, row.names = NULL),
    band_cause = data.frame(sex = s, period = p, ages = rownames(est$band_cause), est$band_cause, check.names = FALSE, row.names = NULL))
}
res <- bind_rows(res) %>% mutate(period = factor(period, names(periods)), across(c(years_lo, years_hi, share_lo, share_hi), ~ ifelse(is.na(years), NA, .x)))
two_ways <- bind_rows(lapply(detail, `[[`, "band")) %>% mutate(period = factor(period, names(periods)))
tidy <- function(d) d %>% transmute(sex, period, item, years = r3(years), years_lo = r3(years_lo), years_hi = r3(years_hi), share_pct = r1(share_pct), share_lo = r1(share_lo), share_hi = r1(share_hi))
write_csv(tidy(res %>% filter(item %in% c(levels(band_of), "25-64"))) %>% left_join(two_ways %>% mutate(across(starts_with("share_"), r1)), by = c("sex", "period", "item")) %>% rename(ages = item),
          file.path(outdir, "gap_by_age_band.csv"))
write_csv(tidy(res %>% filter(item %in% c(causes, "Drug")) %>% mutate(item = ifelse(item == "Drug", "Drug poisoning (memo item outside the sum)", item))) %>% rename(cause = item),
          file.path(outdir, "gap_by_cause.csv"), na = "")
write_csv(bind_rows(lapply(detail, `[[`, "age")) %>% mutate(across(starts_with("years"), r4), share_pct = r1(share_pct)), file.path(outdir, "gap_by_age_group.csv"))
write_csv(bind_rows(lapply(detail, `[[`, "band_cause")) %>% mutate(across(all_of(causes), r4)), file.path(outdir, "gap_by_age_band_and_cause.csv"))
direction_range <- max(abs(two_ways$share_north_first - two_ways$share_south_first)[two_ways$item %in% levels(band_of)])
# the simulated interval for the gap should agree with Chiang's
chk <- res %>% filter(item == "gap") %>% left_join(by_period %>% transmute(sex, period, gap_se), by = c("sex", "period")) %>%
  mutate(width_sim = years_hi - years_lo, width_chiang = 2 * z975 * gap_se)
stopifnot(all(abs(chk$width_sim / chk$width_chiang - 1) < 0.15))

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
# the element it applies to, and the style sheet is removed (as in scripts 4 to 6).
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

# 4a. life expectancy at birth in North and South, year by year
ly <- by_year %>% select(year, sex, North = e0_North, South = e0_South) %>% pivot_longer(c(North, South), names_to = "area", values_to = "e0") %>%
  mutate(panel = factor(ifelse(sex == "women", "Women", "Men"), c("Women", "Men")), area = factor(area, names(area_cols)))
p_e0 <- ggplot(ly, aes(year, e0, colour = area)) +
  annotate("rect", xmin = 2019.5, xmax = 2022.5, ymin = -Inf, ymax = Inf, fill = "#F0EFEC") +
  geom_line(linewidth = 0.7) +
  facet_wrap(~ panel, ncol = 2) +
  scale_colour_manual(values = area_cols) +
  scale_x_continuous(breaks = seq(1985, 2025, 10), expand = expansion(add = 1)) +
  scale_y_continuous(breaks = seq(70, 86, 2)) +
  labs(x = NULL, y = "Life expectancy at birth, years") + base_theme +
  theme(panel.grid.major.x = element_blank(), panel.spacing = unit(0.6, "cm"))
save_fig(p_e0, "life_expectancy_north_south", 7.5, 3.6)

# 4b. the gap, by the age band in which the deaths occur
gb <- res %>% filter(item %in% levels(band_of)) %>%
  mutate(panel = factor(ifelse(sex == "women", "Women", "Men"), c("Women", "Men")), band = factor(item, levels(band_of)))
tot <- gb %>% group_by(sex, panel, period) %>% summarise(top = sum(pmax(years, 0)), gap = sum(years), .groups = "drop") %>%      # a band that narrows the gap stacks below zero
  mutate(label = formatC(gap, format = "f", digits = 1))
stopifnot(all(abs(tot$gap - res$years[res$item == "gap"][match(paste(tot$sex, tot$period), paste(res$sex, res$period)[res$item == "gap"])]) < 1e-9))
p_gap <- ggplot(gb, aes(period, years)) +
  geom_col(aes(fill = band), position = position_stack(reverse = TRUE), width = 0.72, colour = "white", linewidth = 0.4) +
  geom_text(data = tot, aes(period, top, label = label), vjust = -0.5, size = 7.5 / .pt, family = fig_font, colour = ink) +
  facet_wrap(~ panel, ncol = 2) +
  scale_fill_manual(values = band_cols, labels = span_label(names(band_cols)), guide = guide_legend(reverse = TRUE)) +
  scale_x_discrete(labels = span_label(names(periods))) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.1))) +
  labs(x = NULL, y = "Gap in life expectancy at birth, years") + base_theme +
  theme(axis.text.x = element_text(angle = 45, hjust = 1), panel.grid.major.x = element_blank(), panel.spacing = unit(0.6, "cm"),
        legend.position = "right", legend.justification = "center", legend.key.height = unit(0.45, "cm"), legend.key.width = unit(0.45, "cm"))
save_fig(p_gap, "life_expectancy_gap_by_age", 7.5, 3.9)

# ---- 5. captions, as text -------------------------------------------------------------------------
src <- "Source: ONS death registrations and mid-year population estimates."
neg <- gb %>% filter(years < 0)
neg_note <- if (nrow(neg)) paste0("A band that narrows the gap is drawn below zero: ", paste(sprintf("ages %s in %s in %s (%s years)", span_label(as.character(neg$band)), neg$sex,
                                                                                                    span_label(as.character(neg$period)), sub("-", minus, formatC(neg$years, format = "f", digits = 2), fixed = TRUE)), collapse = "; "), ".")
captions <- c(
  paste0("Figure 7. Life expectancy at birth in the North and the South of England, by sex, 1981", dash, "2025"),
  paste("Period life expectancy from abridged life tables for each year. The South includes London. Shading marks", paste0("2020", dash, "22."),
        sprintf("The 95%% confidence intervals reach no more than %.2f years either side of the lines and are in life_expectancy_by_year.csv.", max(z975 * c(by_year$se_North, by_year$se_South))), src),
  "",
  paste0("Figure 8. The North", dash, "South gap in life expectancy at birth, by the age band in which the deaths occur, by sex and period, 1981", dash, "2025"),
  paste(c(paste0("Each bar is the gap in life expectancy (South minus North), in years, divided among four age bands by Arriaga", intToUtf8(8217), "s method (the mean of its two directions); the number above it is the gap."),
          "Five-year periods to", paste0("2015", dash, "19,"), "then", paste0("2020", dash, "22"), "and", paste0("2023", dash, "25."), "The South includes London.", neg_note,
          "The 95% intervals are in gap_by_age_band.csv.", src), collapse = " "))
writeLines(enc2utf8(captions), file.path(outdir, "life_expectancy_captions.txt"), useBytes = TRUE)

# ---- 6. summary -------------------------------------------------------------------------------------
cat("\nLife expectancy and the division of the gap written to", normalizePath(outdir), "\n")
cat(sprintf("Check against the ONS national life tables, England, 2021-23: men %.2f here and %.1f published; women %.2f and %.1f\n",
            eng_e0[["men"]], ons_england_2021_23[["men"]], eng_e0[["women"]], ons_england_2021_23[["women"]]))
keep <- c("1981-84", "2000-04", "2015-19", "2023-25")
cat(sprintf("Simulated interval for the gap against Chiang's: widths differ by %.1f%% at most\n", 100 * max(abs(chk$width_sim / chk$width_chiang - 1))))
cat(sprintf("Arriaga's method, North first against South first: an age band's share of the gap differs by %.1f percentage points at most\n", direction_range))
cat("\nGap in life expectancy at birth, South minus North, years (95% interval)\n")
print(as.data.frame(by_period %>% filter(period %in% keep) %>% arrange(desc(sex), period) %>%
                      transmute(sex, period, gap = sprintf("%.2f (%.2f to %.2f)", gap, gap - z975 * gap_se, gap + z975 * gap_se))), row.names = FALSE)
cat("\nShare of the gap from deaths at ages 25-44 and at ages 25-64, and from cardiovascular disease, % (95% interval)\n")
print(as.data.frame(res %>% filter(item %in% c("25-44", "25-64", "Cardiovascular"), period %in% keep) %>%
                      transmute(sex, period, item, share = sprintf("%.1f (%.1f to %.1f)", share_pct, share_lo, share_hi)) %>% pivot_wider(names_from = item, values_from = share)), row.names = FALSE)
cat("\nChance of dying between the ages of 25 and 65 at the death rates of each period, % (95% interval)\n")
print(as.data.frame(by_period %>% filter(period %in% keep) %>% arrange(desc(sex), period) %>%
                      transmute(sex, period, North = sprintf("%.1f (%.1f to %.1f)", dying_North, dying_lo_North, dying_hi_North), South = sprintf("%.1f (%.1f to %.1f)", dying_South, dying_lo_South, dying_hi_South))), row.names = FALSE)
