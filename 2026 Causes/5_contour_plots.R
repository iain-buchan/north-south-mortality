# =====================================================================
# North-South divide in English mortality by cause of death, 1981-2025
# Script 5 of 5: contour plots - the northern excess in every year and five-year age group, for six
#                cause groups and for persons, men and women
#
# Iain Buchan, 18 Nov 2026. Exploratory work in progress; not peer reviewed.
#
# What it draws
#   For each cause group and sex, the percentage by which the death rate in the North exceeds that in
#   the South (including London), in every year from 1981 to 2025 and every five-year age group, as
#   filled contours with birth-cohort lines. The plots are in the manner of Figure 2 of the 2026
#   paper (code in ../2026: 'R definitive analysis/ns_divide_analysis.R' and 'Stata replicated
#   analysis/16-contour_plots.do'), for the cause groups of the November 2026 ONS extract and on a
#   wider colour scale.
#
# Method
#   Excess = 100 x (rate ratio - 1). The rate ratio is deaths per person in the North over deaths per
#   person in the South in that year and age group, which is what a Poisson model of deaths on area
#   alone gives. Persons pool the two sexes without adjustment.
#   Where North and South together have fewer than 20 deaths in a cell, the ratio is too unstable to
#   show. Each plot is drawn twice:
#     a  the plain contour plot: those cells, and cells with no data, are given zero excess. Zero is
#        the top of the band '-20 to 0', so a block of such cells takes that band's colour and
#        looks like a small northern advantage.
#     b  those cells are grey, and a dot marks every other cell whose excess cannot be told from
#        zero at the 5% level.
#   The test is exact: given the deaths in a cell, the number in the North is binomial, with
#   probability equal to the North's share of the population at risk. It allows for Poisson variation
#   only, and makes no allowance for the number of cells tested: about one cell in 20 with no true
#   excess will pass it.
#   Colours. The excess is capped at the ends of each plot's scale, so the top band also holds every
#   higher value and the bottom band every lower one. A value on a cut belongs to the band below it.
#   Between cells the contours interpolate linearly. In version b each cell is also filled flat with
#   its own colour, which shows wherever a neighbouring cell is grey, so that no cell with 20 or more
#   deaths is hidden.
#   Cause groups are those of the 2026 extract (script 1), and they overlap (see the README):
#   accidents include accidental poisoning by drugs and alcohol, and drug poisoning includes
#   suicides. 'Cardiovascular, diabetes and obesity' adds three series, of which obesity is counted
#   on any mention, so some deaths are counted twice. Drug poisoning was not supplied before 1993.
#   The stepped black lines follow birth cohorts: one age group older every five years.
#   The threshold of 20 deaths is the one that best reproduces contour plots of the same data drawn
#   independently in Stata.
#
# Input (written by script 1 to ./output)
#   cell_level_deaths_pop.csv
#
# Outputs (written to ./output/contours)
#   contour_<cause>_<sex>_a   .svg and .png   18 plots
#   contour_<cause>_<sex>_b   .svg and .png   the same 18, with grey cells and dots
#   contours_overview_a, _b   .svg and .png   all 18 on one sheet, on one colour scale
#   contour_data.csv                          every cell: deaths, populations, rate ratio, excess, test and what is drawn
#   contour_summary.csv                       for each plot: cells drawn, grey, clear, unclear and beyond the scale
#   contour_captions.txt                      the captions, as text
#   The plots carry no title or caption, so that captions can be set as text beside them.
#
# Needs: dplyr, tidyr, readr, ggplot2 (3.4.0 or later), svglite (installed automatically if absent).
# About half a minute.
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
outdir     <- file.path("output", "contours")
fig_font   <- "Arial"
min_deaths <- 20                                         # a cell with fewer deaths than this, North and South together, is not drawn
test_level <- 0.05
years <- 1981:2025
ages  <- c("<1", "01-04", sprintf("%02d-%02d", seq(5, 80, 5), seq(9, 84, 5)), "85+")
cuts  <- c(-40, -20, 0, 20, 40, 60, 80, 100, 150, 200, 250)                  # edges of the colour bands, %
band_cols <- c("#55752F", "#00FF80", "#FFFF4D", "#FFFF00", "#FFD200", "#FF7F00", "#FF0000", "#C10534", "#90353B", "#000000")
grey_col  <- "#C8C8C8"
# cause groups (series of script 1), the top of each plot's colour scale, and the words used in captions
groups <- list("All" = "All causes", "Accidents" = "Accidents", "Alcohol" = "Alcohol",
               "Cardiovascular, diabetes and obesity" = c("Cardiovascular", "Diabetes", "Obesity"),
               "Drugs" = "Drug", "Suicide" = "Suicide")
scale_top <- c("All" = 100, "Accidents" = 150, "Alcohol" = 250, "Cardiovascular, diabetes and obesity" = 150, "Drugs" = 150, "Suicide" = 100)
cause_words <- c("All" = "all causes", "Accidents" = "accidents", "Alcohol" = "alcohol-related causes",
                 "Cardiovascular, diabetes and obesity" = "cardiovascular disease, diabetes and obesity",
                 "Drugs" = "drug poisoning", "Suicide" = "suicide")
sexes     <- c(people = "all", male = "male", female = "female")           # file name -> word in the legend title
sex_words <- c(people = "persons", male = "men", female = "women")
minus <- intToUtf8(8722); dash <- intToUtf8(8211)

if (!file.exists(in_file)) stop("Run script 1 first: ", in_file, " is missing.")
dir.create(outdir, recursive = TRUE, showWarnings = FALSE)
stopifnot(length(band_cols) == length(cuts) - 1, all(scale_top %in% cuts), identical(names(scale_top), names(groups)))

# ---- 1. deaths and populations in every cell ----------------------------------------------
cells <- read_csv(in_file, show_col_types = FALSE) %>% filter(area %in% c("North", "South"))      # South includes London
stopifnot(all(unlist(groups) %in% cells$cause), setequal(cells$age, ages), setequal(cells$year, years), setequal(cells$sex, c("F", "M")),
          !any(duplicated(cells[, c("cause", "area", "sex", "age", "year")])), !anyNA(cells$pop))
by_sex <- bind_rows(lapply(names(groups), function(g) {
  cells %>% filter(cause %in% groups[[g]]) %>% group_by(area, sex, age, year) %>%
    summarise(deaths = sum(deaths), pop = first(pop), n = n(), .groups = "drop") %>%              # deaths are NA if any series is missing
    mutate(cause = g, parts = length(groups[[g]]))
}))
stopifnot(all(by_sex$n == by_sex$parts), nrow(by_sex) == length(groups) * 2 * 2 * length(ages) * length(years))
x <- bind_rows(by_sex %>% mutate(sex = ifelse(sex == "M", "male", "female")),
               by_sex %>% group_by(cause, area, age, year) %>% summarise(deaths = sum(deaths), pop = sum(pop), .groups = "drop") %>% mutate(sex = "people")) %>%
  select(cause, sex, area, age, year, deaths, pop) %>%
  pivot_wider(names_from = area, values_from = c(deaths, pop)) %>%
  mutate(agecat = match(age, ages), deaths_both = deaths_North + deaths_South,
         rate_ratio = (deaths_North / pop_North) / (deaths_South / pop_South),
         excess = 100 * (rate_ratio - 1),
         status = case_when(is.na(deaths_both) ~ "no data", deaths_both < min_deaths ~ "few deaths", TRUE ~ "drawn"))
n_cells <- length(ages) * length(years)
stopifnot(nrow(x) == 18 * n_cells, !anyNA(x$agecat), all(is.finite(x$excess[x$status == "drawn"])))

# exact test of no excess, for the cells that are drawn
drawn <- which(x$status == "drawn")
x$p_value <- NA_real_
x$p_value[drawn] <- mapply(function(d, n, p) binom.test(d, n, p)$p.value,
                           x$deaths_North[drawn], x$deaths_both[drawn], (x$pop_North / (x$pop_North + x$pop_South))[drawn])
x <- x %>% mutate(unclear = status == "drawn" & p_value >= test_level,
                  cap = scale_top[cause],
                  value_a = ifelse(status == "drawn", excess, 0),                 # version a: zero where there are too few deaths or no data
                  value_b = ifelse(status == "drawn", excess, NA_real_)) %>%      # version b: nothing there
  arrange(cause, sex, year, agecat)

write_csv(x %>% transmute(cause, sex, year, age, deaths_North, deaths_South, pop_North, pop_South,
                          rate_ratio = round(rate_ratio, 5), excess_pct = round(excess, 2), status, p_value = signif(p_value, 4), unclear,
                          drawn_a = round(value_a, 2), drawn_b = round(value_b, 2), scale_top = cap),
          file.path(outdir, "contour_data.csv"))
summ <- x %>% group_by(cause, sex) %>%
  summarise(cells = n(), drawn = sum(status == "drawn"), few_deaths = sum(status == "few deaths"), no_data = sum(status == "no data"),
            clear = sum(status == "drawn" & !unclear), unclear = sum(unclear), scale_top = first(cap),
            above_scale = sum(status == "drawn" & excess > cap), below_scale = sum(status == "drawn" & excess < cuts[1]), .groups = "drop")
stopifnot(all(summ$cells == n_cells), all(summ$drawn == summ$clear + summ$unclear))
write_csv(summ, file.path(outdir, "contour_summary.csv"))

# ---- 2. the plots ---------------------------------------------------------------------------
# birth-cohort lines: every second age group in 1981, one group older every five years
cohorts <- bind_rows(lapply(seq(-7, 19, 2), function(k) data.frame(k = k, year = years, agecat = floor((years - 1981) / 5) + k))) %>%
  filter(agecat >= 1, agecat <= 19)
x_breaks <- c(1981, seq(1985, 2020, 5), 2025)
grey_key <- sprintf("Fewer than %d deaths,\nor no data", min_deaths)
dot_key  <- sprintf("Excess not clear\nat the %g%% level", 100 * test_level)
num <- function(v) sub("-", minus, as.character(v), fixed = TRUE)
band_labels <- function(bands)                             # the end bands are open
  c(paste("below", num(cuts[2])), paste(num(cuts[2:(bands - 1)]), "to", num(cuts[3:bands])), paste(num(cuts[bands]), "or more"))

draw <- function(d, version, legend_title, cap, overview = FALSE) {
  bands <- match(cap, cuts) - 1                            # the number of colour bands on this scale
  labs  <- band_labels(bands)
  # capped to the scale, and nudged so that a value on a cut falls in the band below it
  d$z    <- pmin(pmax((if (version == "a") d$value_a else d$value_b) - 1e-6, cuts[1] + 1e-6), cap - 1e-6)
  d$band <- factor(findInterval(d$z, cuts), levels = seq_len(bands), labels = labs)
  p <- ggplot(d, aes(year, agecat))
  if (version == "b")                                      # a grey ground, then each drawn cell flat in its own colour
    p <- p + geom_rect(data = data.frame(key = grey_key), aes(xmin = 1981, xmax = 2025, ymin = 1, ymax = 19, alpha = key),
                       fill = grey_col, inherit.aes = FALSE) +
      geom_rect(data = d[!is.na(d$z), ], aes(xmin = pmax(year - 0.5, 1981), xmax = pmin(year + 0.5, 2025), ymin = pmax(agecat - 0.5, 1),
                                             ymax = pmin(agecat + 0.5, 19), fill = band, colour = band),
                linewidth = 0.1, inherit.aes = FALSE, show.legend = FALSE)
  p <- p + geom_contour_filled(aes(z = z, fill = after_stat(factor(as.integer(level), levels = seq_len(bands), labels = labs))),
                               breaks = cuts[seq_len(bands + 1)], na.rm = TRUE, show.legend = c(fill = TRUE, alpha = FALSE, shape = FALSE)) +
    geom_step(data = cohorts, aes(year, agecat, group = k), direction = "hv", colour = "black", linewidth = if (overview) 0.15 else 0.25,
              inherit.aes = FALSE)
  if (version == "b")
    p <- p + geom_point(data = d[d$unclear, ], aes(shape = dot_key), fill = "white", colour = "black",
                        size = if (overview) 0.35 else 0.9, stroke = if (overview) 0.12 else 0.25) +
      scale_alpha_manual(name = NULL, values = setNames(1, grey_key), guide = guide_legend(order = 2)) +
      scale_shape_manual(name = NULL, values = setNames(21, dot_key), guide = guide_legend(order = 3, override.aes = list(size = 1.6, stroke = 0.4)))
  p + scale_fill_manual(values = setNames(band_cols[seq_len(bands)], labs), limits = labs, drop = FALSE, name = legend_title,
                        aesthetics = c("fill", "colour"), guide = guide_legend(reverse = TRUE, order = 1)) +
    scale_x_continuous(breaks = x_breaks, limits = c(1981, 2025), expand = expansion(add = 0.6)) +
    scale_y_continuous(breaks = seq_along(ages), labels = sub("^<1$", "<01", ages), limits = c(1, 19), expand = expansion(add = 0.5)) +
    labs(x = "Year", y = "Age category") +
    theme_bw(base_size = if (overview) 7.5 else 10, base_family = fig_font) +
    theme(panel.grid = element_blank(), panel.border = element_blank(), axis.line = element_line(colour = "#606060", linewidth = 0.3),
          axis.text.x = element_text(angle = 45, hjust = 1), legend.key.height = unit(if (overview) 0.42 else 0.62, "cm"),
          legend.key.width = unit(if (overview) 0.42 else 0.55, "cm"), strip.background = element_blank(),
          strip.text = element_text(face = "bold", hjust = 0), plot.margin = margin(8, 8, 6, 8))
}
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
# the element it applies to, and the style sheet is removed. (As in script 4, but working line by
# line, which is far quicker on the large sheets.)
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
  ggsave(file.path(outdir, paste0(name, ".png")), p, width = w, height = h, dpi = 160)
}
file_stem <- function(g, s) sprintf("contour_%s_%s", gsub(",", "", gsub(" ", "_", g)), s)
for (g in names(groups)) for (s in names(sexes)) for (v in c("a", "b")) {
  d <- x[x$cause == g & x$sex == s, ]
  stopifnot(nrow(d) == n_cells)
  save_fig(draw(d, v, sprintf("%% %s excess\nmortality in the North", sexes[[s]]), cap = scale_top[[g]]), paste0(file_stem(g, s), "_", v), 7.5, 5)
}
# all 18 on one sheet, on the full colour scale
ov <- x %>% mutate(cause = factor(cause, levels = names(groups)), panel_sex = factor(sex, levels = names(sexes), labels = c("Persons", "Men", "Women")))
for (v in c("a", "b")) {
  p <- draw(ov, v, "% excess mortality\nin the North", cap = max(cuts), overview = TRUE) +
    facet_grid(cause ~ panel_sex, labeller = labeller(cause = label_wrap_gen(22))) +
    theme(strip.text.y = element_text(angle = 270, hjust = 0.5), panel.spacing = unit(0.45, "cm"))
  save_fig(p, paste0("contours_overview_", v), 11, 15)
}

# ---- 3. captions, as text -------------------------------------------------------------------
what <- paste("Percentage by which the death rate in the North exceeds that in the South (including London) in each year and",
              "five-year age group, with no adjustment. The stepped lines follow birth cohorts.")
tail_note <- paste("The end bands of the colour scale are open. Cause groups are those of the November 2026 ONS extract and overlap;",
                   "drug poisoning was not supplied before 1993. Source: ONS death registrations and mid-year population estimates.")
note_a <- paste(what, sprintf("Cells with fewer than %d deaths in the two areas together, and cells with no data, are drawn as zero excess.", min_deaths), tail_note)
note_b <- paste(what, sprintf("Grey marks cells with fewer than %d deaths in the two areas together, or no data.", min_deaths),
                sprintf("A dot marks a cell whose excess cannot be told from zero at the %g%% level (exact binomial test, with no allowance for the number of cells tested).", 100 * test_level),
                tail_note)
span <- paste0(min(years), dash, max(years))
captions <- c(
  sprintf("Figure 3. Northern excess in death rates by year and age group, for six cause groups and by sex, %s", span),
  "", "Version a (contours_overview_a and the single plots ending _a)", note_a,
  "", "Version b (contours_overview_b and the single plots ending _b)", note_b,
  "", "Single plots",
  unlist(lapply(names(groups), function(g) sapply(names(sexes), function(s)
    sprintf("%s: Northern excess in death rates from %s, %s, by year and age group, %s", file_stem(g, s), cause_words[[g]], sex_words[[s]], span)))))
writeLines(enc2utf8(captions), file.path(outdir, "contour_captions.txt"), useBytes = TRUE)

# ---- 4. summary -------------------------------------------------------------------------------
cat("\nContour plots written to", normalizePath(outdir), "\n")
cat(sprintf("Cells: %d in 18 plots; drawn %d, fewer than %d deaths %d, no data %d; of those drawn, %d (%.0f%%) cannot be told from zero excess\n",
            nrow(x), sum(x$status == "drawn"), min_deaths, sum(x$status == "few deaths"), sum(x$status == "no data"),
            sum(x$unclear), 100 * sum(x$unclear) / sum(x$status == "drawn")))
print(as.data.frame(summ), row.names = FALSE)
