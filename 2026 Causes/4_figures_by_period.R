# =====================================================================
# North-South divide in English mortality by cause of death, 1981-2025
# Script 4 of 5: figures - the gap and the northern excess at ages 25-49, by cause, sex and period
#
# Iain Buchan, 16 Nov 2026. Exploratory work in progress; not peer reviewed.
#
# What it draws (women and men, in six periods with the pandemic years 2020-22 set apart)
#   Figure 1  the North-South gap in directly standardised death rates, per 100,000 a year
#   Figure 2  the northern excess: the percentage by which the death rate in the North exceeds
#             the South's
#   Cause groups are those of the 2018 paper (script 2). South includes London.
#
# Method
#   Figure 1 takes the gaps and their 95% intervals from like_for_like_periods.csv (script 2).
#   Figure 2 fits, for each sex, cause and period, the Poisson model of the 2026 paper:
#     deaths ~ north + five-year age group, with log population as offset, on deaths and
#     populations pooled over the years of the period. Excess = (rate ratio - 1) x 100, with a
#     Wald 95% interval on the log scale.
#   Drug-related deaths were not identified separately before 1993. For them and for 'all other
#   causes' there is no 1981-90 point, and the 1990s point covers 1993-2000 (an open circle).
#
# Inputs (written by script 2 to ./output)
#   like_for_like_periods.csv, like_for_like_cells.csv
#
# Outputs (written to ./output)
#   fig1_gap_by_cause_women_men_by_period             .svg and .png
#   fig2_northern_excess_by_cause_women_men_by_period .svg and .png
#   fig1_data.csv, fig2_data.csv                      the numbers plotted
#   figure_captions.txt                               the captions, as text
#   The charts carry no title or caption, so that captions can be set as text beside them.
#
# Needs: dplyr, tidyr, readr, ggplot2, svglite (installed automatically if absent). A few seconds.
# =====================================================================

pkgs <- c("dplyr", "tidyr", "readr", "ggplot2", "svglite")
new  <- pkgs[!pkgs %in% rownames(installed.packages())]
if (length(new)) install.packages(new)
invisible(lapply(pkgs, library, character.only = TRUE))
if (!isTRUE(l10n_info()[["UTF-8"]]))                     # the en dashes in the labels need a UTF-8 locale
  for (loc in c("en_GB.UTF-8", "C.UTF-8", "en_US.UTF-8")) if (nzchar(suppressWarnings(Sys.setlocale("LC_CTYPE", loc)))) break

# ---- configuration ---------------------------------------------------
outdir       <- "output"
periods_file <- file.path(outdir, "like_for_like_periods.csv")
cells_file   <- file.path(outdir, "like_for_like_cells.csv")
fig_font     <- "Arial"
fig_width    <- 6.27                                     # inches: the text width of an A4 page with 2.54 cm margins
fig_height   <- 6.3

for (f in c(periods_file, cells_file))
  if (!file.exists(f)) stop("Cannot find '", f, "' from ", getwd(), " - run 2_like_for_like_2018_definitions.R first.")

periods <- c("1981-90", "1991-2000", "2001-10", "2011-19", "2020-22", "2023-25")
years   <- list("1981-90" = 1981:1990, "1991-2000" = 1991:2000, "1993-2000" = 1993:2000, "2001-10" = 2001:2010,
                "2011-19" = 2011:2019, "2020-22" = 2020:2022, "2023-25" = 2023:2025)
panels  <- c(ALL = "All causes", DRUG18 = "Drug-related", CVD = "Cardiovascular", ALC18 = "Alcohol-related",
             SUI = "Suicide", CANCER = "Cancer", ACC18 = "Accidents", DIAB = "Diabetes",
             OTHER18 = "All other causes")                # in the order drawn, row by row
from_1993 <- c("DRUG18", "OTHER18")                      # no series before 1993
ages      <- c("25-29", "30-34", "35-39", "40-44", "45-49")
dash      <- function(x) gsub("-", "\u2013", x)     # en dash in the labels

# the points to draw: cause x period x sex, with the years that each point covers
pts <- expand.grid(series = names(panels), period_shown = periods, sex = c("F", "M"), stringsAsFactors = FALSE) %>%
  filter(!(series %in% from_1993 & period_shown == "1981-90")) %>%
  mutate(years_covered = ifelse(series %in% from_1993 & period_shown == "1991-2000", "1993-2000", period_shown)) %>%
  arrange(match(series, names(panels)), match(period_shown, periods), sex)
stopifnot(nrow(pts) == 9 * 6 * 2 - 2 * 2)

# ---- 1. Figure 1: the gap in standardised rates, from script 2 -----------------------
per <- read_csv(periods_file, show_col_types = FALSE) %>%
  filter(comparison == "N_vs_S", band == "25-49", sex %in% c("F", "M")) %>%
  select(series, sex, years_covered = period, rate_north = dsr_A, rate_south = dsr_B, est = gap, lo = gap_lo, hi = gap_hi)
f1 <- left_join(pts, per, by = c("series", "sex", "years_covered"))
stopifnot(nrow(f1) == nrow(pts), !anyNA(f1$est))

# ---- 2. Figure 2: the percentage northern excess, from the Poisson model ------------
cells <- read_csv(cells_file, show_col_types = FALSE) %>%
  filter(area %in% c("North", "South"), age %in% ages) %>% rename(deaths = d, pop = p)
excess <- function(s, sx, span) {
  x <- cells %>% filter(series == s, sex == sx, year %in% years[[span]]) %>%
    group_by(area, age) %>% summarise(deaths = sum(deaths), pop = sum(pop), n = n(), .groups = "drop") %>%
    mutate(north = as.numeric(area == "North"))
  stopifnot(nrow(x) == 10, all(x$n == length(years[[span]])), !anyNA(x$deaths))
  x <- x %>% group_by(age) %>% filter(sum(deaths) > 0) %>% ungroup()    # an age group with no deaths in either area carries no information
  m  <- glm(deaths ~ north + factor(age) + offset(log(pop)), family = poisson, data = x,
            control = glm.control(epsilon = 1e-10, maxit = 100))
  stopifnot(m$converged)
  b  <- coef(m)[["north"]]; se <- coef(summary(m))["north", "Std. Error"]
  data.frame(deaths_north = sum(x$deaths[x$north == 1]), deaths_south = sum(x$deaths[x$north == 0]),
             est = (exp(b) - 1) * 100, lo = (exp(b - 1.96 * se) - 1) * 100, hi = (exp(b + 1.96 * se) - 1) * 100)
}
f2 <- bind_cols(pts, bind_rows(Map(excess, pts$series, pts$sex, pts$years_covered)))
stopifnot(nrow(f2) == nrow(pts), !anyNA(f2$est))

tidy <- function(f) f %>% mutate(cause = unname(panels[series]), sex = ifelse(sex == "F", "Women", "Men")) %>%
  select(series, cause, sex, period_shown, years_covered, everything())
write_csv(tidy(f1) %>% rename(gap = est, gap_lo = lo, gap_hi = hi) %>% mutate(across(where(is.double), ~ round(.x, 4))),
          file.path(outdir, "fig1_data.csv"))
write_csv(tidy(f2) %>% rename(northern_excess_pct = est, excess_lo = lo, excess_hi = hi) %>% mutate(across(where(is.double), ~ round(.x, 4))),
          file.path(outdir, "fig2_data.csv"))

# ---- 3. the chart ------------------------------------------------------------------
ink <- "#0b0b0b"; ink2 <- "#52514e"; muted <- "#898781"; grid_col <- "#e1e0d9"; axis_col <- "#c3c2b7"; band_col <- "#f0efea"
paper    <- "#ffffff"
sex_cols <- c(Women = "#2a78d6", Men = "#eb6834")        # distinguishable with colour-vision deficiency
open_key <- "1993\u20132000 only"

# f: the points; xlab: axis title; fmt: how to write the 2023-25 values; lims: x limits for a
# series; brks: axis breaks from a panel's limits; na_x: where 'not available' starts;
# note: an optional remark inside one panel, list(series, text)
draw <- function(f, xlab, fmt, lims, brks, na_x, note = NULL) {
  lev <- unname(panels)
  f <- f %>% mutate(panel = factor(panels[series], levels = lev),
                    Sex = factor(ifelse(sex == "F", "Women", "Men"), levels = names(sex_cols)),
                    y = match(period_shown, periods) + ifelse(sex == "F", -0.22, 0.22),    # women above men
                    open = years_covered != period_shown)
  width  <- vapply(names(panels), function(s) diff(lims(s)), numeric(1))
  rng    <- bind_rows(lapply(names(panels), function(s) data.frame(panel = factor(panels[[s]], levels = lev), x = lims(s))))
  upper  <- vapply(names(panels), function(s) lims(s)[2], numeric(1))
  # the 2023-25 values are written to the right of their intervals; in a panel where one of them
  # would not fit there, both are written to the left (a value and its gap take 16% of the width)
  latest <- f %>% filter(period_shown == "2023-25") %>% group_by(series) %>%
    mutate(left = any(hi + 0.16 * width[series] > upper[series])) %>% ungroup() %>%
    mutate(x = ifelse(left, lo - 0.03 * width[series], hi + 0.03 * width[series]), hjust = as.numeric(left), label = fmt(est))
  gone   <- data.frame(panel = factor(panels[from_1993], levels = lev), x = unname(na_x[from_1993]), y = 1)
  for (s in names(panels)) {                               # everything must sit inside its panel, with room for the labels
    g <- f[f$series == s, ]; l <- latest[latest$series == s, ]
    stopifnot(min(g$lo) > lims(s)[1], max(g$hi) < lims(s)[2],
              all(ifelse(l$left, l$lo - 0.16 * width[[s]] > max(0, lims(s)[1]), l$hi + 0.16 * width[[s]] < lims(s)[2])))
  }
  p <- ggplot(f, aes(y = y)) +
    annotate("rect", xmin = -Inf, xmax = Inf, ymin = 4.5, ymax = 5.5, fill = band_col) +          # the pandemic years
    geom_vline(xintercept = 0, colour = axis_col, linewidth = 0.45) +
    geom_blank(data = rng, aes(x = x), inherit.aes = FALSE) +                                    # sets each panel's limits
    geom_segment(aes(x = lo, xend = hi, yend = y, colour = Sex), linewidth = 0.75, lineend = "round") +
    geom_point(data = function(d) d[!d$open, ], aes(x = est, fill = Sex), shape = 21, colour = paper, size = 2.4, stroke = 0.55) +
    geom_point(data = function(d) d[d$open, ], aes(x = est, colour = Sex, shape = open_key), fill = paper, size = 2.1, stroke = 0.8,
               show.legend = c(colour = FALSE, shape = TRUE)) +
    geom_text(data = latest, aes(x = x, label = label, hjust = hjust), size = 7.6 / .pt, colour = ink, family = fig_font) +
    geom_text(data = gone, aes(x = x, y = y), label = "not available", hjust = 0, size = 7.4 / .pt, colour = muted,
              fontface = "italic", family = fig_font, inherit.aes = FALSE)
  if (!is.null(note))
    p <- p + geom_text(data = data.frame(panel = factor(panels[[note$series]], levels = lev)), aes(x = Inf, y = -Inf), label = note$text,
                       hjust = 1.06, vjust = 1.7, size = 7.6 / .pt, colour = ink2, family = fig_font, inherit.aes = FALSE)
  p + facet_wrap(~ panel, ncol = 3, scales = "free_x") +
    scale_x_continuous(expand = c(0, 0), breaks = brks) +
    scale_y_reverse(breaks = seq_along(periods), labels = dash(periods), limits = c(6.62, 0.38), expand = c(0, 0)) +
    scale_colour_manual(values = sex_cols, name = NULL) +
    scale_fill_manual(values = sex_cols, name = NULL) +
    scale_shape_manual(values = setNames(21, open_key), name = NULL) +
    guides(colour = guide_legend(order = 1), fill = guide_legend(order = 1),
           shape = guide_legend(order = 2, override.aes = list(colour = ink2, fill = paper, size = 2.1, stroke = 0.8))) +
    labs(x = xlab, y = NULL) +
    theme_minimal(base_size = 9, base_family = fig_font) +
    theme(plot.background = element_rect(fill = paper, colour = NA),
          panel.grid.major.y = element_blank(), panel.grid.minor = element_blank(),
          panel.grid.major.x = element_line(colour = grid_col, linewidth = 0.3),
          axis.line.x = element_line(colour = axis_col, linewidth = 0.4),
          axis.ticks = element_blank(),
          axis.text.x = element_text(colour = ink2, size = 8, margin = margin(t = 3)),
          axis.text.y = element_text(colour = ink, size = 8.3, margin = margin(r = 5)),
          axis.title.x = element_text(colour = ink2, size = 9, margin = margin(t = 9)),
          strip.text = element_text(face = "bold", hjust = 0, size = 9, colour = ink, margin = margin(b = 4.5)),
          panel.spacing.x = unit(11, "pt"), panel.spacing.y = unit(15, "pt"),
          legend.position = "top", legend.justification = "left", legend.box = "horizontal", legend.box.just = "left",
          legend.margin = margin(0, 0, 0, 0), legend.box.margin = margin(0, 0, 2, 0), legend.box.spacing = unit(6, "pt"),
          legend.text = element_text(size = 9, colour = ink), legend.key.width = unit(22, "pt"), legend.key.height = unit(11, "pt"),
          plot.margin = margin(4, 10, 4, 4))
}

p1 <- draw(f1, xlab = "North minus South: deaths per 100,000 a year", fmt = function(v) sprintf("%.1f", v),
           lims = function(s) if (s == "ALL") c(-6.5, 26) * 3 else c(-6.5, 26),                  # all causes at one third of the scale
           brks = function(l) if (diff(l) > 60) seq(0, 75, 15) else seq(0, 25, 5),
           na_x = c(DRUG18 = 0.5, OTHER18 = 0.9), note = list(series = "ALL", text = "different scale"))
# The percentage axis is labelled to 100. Estimates and intervals above that are drawn in full, so
# every panel runs on to just past the longest interval; all nine panels share one scale.
x2 <- c(min(0, f2$lo), max(100, f2$hi)) + c(-4.5, 4.5)
p2 <- draw(f2, xlab = "Northern excess: percentage by which the death rate in the North exceeds the South\u2019s",
           fmt = function(v) sprintf("%.0f%%", v), lims = function(s) x2, brks = function(l) seq(0, 100, 50),
           na_x = c(DRUG18 = 6, OTHER18 = 6))

# ---- 4. files ---------------------------------------------------------------------
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
# the element it applies to, and the style sheet is removed.
plain_svg <- function(file) {
  s <- paste(readLines(file, warn = FALSE, encoding = "UTF-8"), collapse = "\n")
  defaults <- c(fill = "none", stroke = "#000000", "stroke-linecap" = "round", "stroke-linejoin" = "round")
  m  <- gregexpr("<(line|polyline|polygon|path|rect|circle|text)\\b[^>]*>", s, perl = TRUE)
  el <- regmatches(s, m)[[1]]
  el <- vapply(el, function(e) {
    shape <- !grepl("^<text", e)
    st    <- regmatches(e, regexpr("style='[^']*'", e))
    decl  <- if (length(st)) strsplit(sub("'$", "", sub("^style='", "", st)), ";", fixed = TRUE)[[1]] else character(0)
    decl  <- trimws(decl); decl <- decl[nzchar(decl)]
    key   <- trimws(sub(":.*$", "", decl)); val <- gsub("\"", "", trimws(sub("^[^:]*:", "", decl)))
    if (any(key == "font-family")) val[key == "font-family"] <- fig_font
    if (!length(st)) return(e)                                    # the clipping rectangles carry no style
    if (shape) for (d in names(defaults)) if (!d %in% key) { key <- c(key, d); val <- c(val, defaults[[d]]) }
    attrs <- paste0(key, "='", val, "'", collapse = " ")
    e <- if (length(st)) sub("\\s*style='[^']*'", "", e) else e
    sub("(\\s*/?>)$", paste0(" ", attrs, "\\1"), e)
  }, character(1), USE.NAMES = FALSE)
  regmatches(s, m) <- list(el)
  s <- sub("(?s)<defs>\\s*<style\\b.*?</style>\\s*</defs>\\n?", "", s, perl = TRUE)
  ids <- unique(regmatches(s, gregexpr("(?<=<clipPath id=')[^']+", s, perl = TRUE))[[1]])     # plain names for the clipping paths
  for (i in seq_along(ids)) {
    s <- gsub(paste0("id='", ids[i], "'"), paste0("id='clip", i, "'"), s, fixed = TRUE)
    s <- gsub(paste0("url(#", ids[i], ")"), paste0("url(#clip", i, ")"), s, fixed = TRUE)
  }
  stopifnot(!grepl("\\sstyle=", s), !grepl("<style", s, fixed = TRUE))
  writeLines(s, file, useBytes = TRUE)
}
save_fig <- function(p, name) {
  svg_file <- file.path(outdir, paste0(name, ".svg"))
  ggsave(svg_file, p, width = fig_width, height = fig_height, device = svglite::svglite)
  plain_svg(svg_file)
  ggsave(file.path(outdir, paste0(name, ".png")), p, width = fig_width, height = fig_height, dpi = 300)
}
save_fig(p1, "fig1_gap_by_cause_women_men_by_period")
save_fig(p2, "fig2_northern_excess_by_cause_women_men_by_period")

# ---- 5. captions, as text -----------------------------------------------------------
captions <- c(
  "Figure 1. North\u2013South gap in death rates at ages 25\u201349, by cause and sex, 1981\u20132025",
  paste("North minus South (including London), in rates directly age-standardised to the 2013 European Standard Population.",
        "Lines are 95% confidence intervals, the numbers are for 2023\u201325 and shading marks the pandemic years. The",
        "all-causes panel is at one third of the scale of the others. Cause groups follow the ONS extract used in the 2018",
        "paper; drug-related deaths, and hence all other causes, are available only from 1993. Source: ONS death",
        "registrations and mid-year population estimates."),
  "",
  "Figure 2. Northern excess in death rates at ages 25\u201349, by cause and sex, 1981\u20132025",
  paste("Percentage by which the death rate in the North exceeds that in the South (including London), from Poisson models",
        "of deaths on area and five-year age group. Lines are 95% confidence intervals, the numbers are for 2023\u201325 and",
        "shading marks the pandemic years. Cause groups and source as in Figure 1."))
writeLines(enc2utf8(captions), file.path(outdir, "figure_captions.txt"), useBytes = TRUE)

cat("\nFigures 1 and 2 written to", normalizePath(outdir), "\n")
cat(sprintf("Figure 1, 2023-25, all causes: gap %.1f (women) and %.1f (men) per 100,000\n",
            f1$est[f1$series == "ALL" & f1$period_shown == "2023-25" & f1$sex == "F"],
            f1$est[f1$series == "ALL" & f1$period_shown == "2023-25" & f1$sex == "M"]))
cat(sprintf("Figure 2, 2023-25, all causes: northern excess %.0f%% (women) and %.0f%% (men)\n",
            f2$est[f2$series == "ALL" & f2$period_shown == "2023-25" & f2$sex == "F"],
            f2$est[f2$series == "ALL" & f2$period_shown == "2023-25" & f2$sex == "M"]))
