# 2026 Causes

Data and R code for work in progress on the causes of death behind the North–South divide in English mortality, 1981–2025.

It follows two papers:

- Kontopantelis E, Zhang X, Green M, Munford L, Barr B, Buchan I. **The north–south divide in English mortality, 1981–2024: a longitudinal population study.** *Lancet Public Health* 2026; 11: e847–56. https://doi.org/10.1016/S2468-2667(26)00195-7 – data and code in [2026](../2026/)
- Kontopantelis E, Buchan I, Webb RT, Ashcroft DM, Mamas MA, Doran T. **Disparities in mortality among 25–44-year-olds in England: a longitudinal, population-based study.** *Lancet Public Health* 2018; 3: e567–75. https://doi.org/10.1016/S2468-2667(18)30177-4

**Status: exploratory. Nothing here has been peer reviewed, and the definitions, data and code may change.**

## Files

| File | What it does |
|---|---|
| [1_cause_decomposition.R](1_cause_decomposition.R) | All ages, 1981–2025, on the cause definitions of the November 2026 ONS extract, with London separated from the rest of the South: standardised rates, gaps, rate ratios and excess deaths by cause, sex and age band |
| [2_like_for_like_2018_definitions.R](2_like_for_like_2018_definitions.R) | Ages 25–49 by cause and sex, 1981–2025, on the cause definitions of the 2018 paper, with the three sources of deaths checked against each other |
| [3_other_causes_split.R](3_other_causes_split.R) | The residual ‘other causes’ of script 1 split into categories that do not overlap, under 75, for 2015–19, 2020–24 and 2021–25 |
| [4_figures_by_period.R](4_figures_by_period.R) | Two charts from the output of script 2, as SVG and PNG: the North–South gap in standardised death rates and the percentage northern excess at ages 25–49, by cause and sex, in six periods from 1981–90 to 2023–25 |
| [5_contour_plots.R](5_contour_plots.R) | Contour plots from an output of script 1, as SVG and PNG: the percentage northern excess in every year and five-year age group, 1981–2025, for six cause groups and for persons, men and women. Each plot is drawn twice: once as a plain contour plot, and once with the cells that have too few deaths left grey, the cells with no clear excess marked, and colours that readers with any type of colour vision can tell apart |
| [6_pooled_cells_and_cohorts.R](6_pooled_cells_and_cohorts.R) | Pooled cells and birth cohorts from an output of script 1: the northern excess in five-year age groups and five-year periods, with exact intervals; death rates by age band and period for the North, the South, the South excluding London, and London; and age–period–cohort models of the North:South rate ratio for drug and alcohol deaths. Three charts, as SVG and PNG |
| [7_life_expectancy.R](7_life_expectancy.R) | Life expectancy from an output of script 1: life expectancy at birth in the North and the South, by sex, in every year and in ten periods; the gap between them divided among the ages and causes of death that make it up, with intervals; and the chance of dying between the ages of 25 and 65. Two charts, as SVG and PNG |
| [data/](data/) | The input files below |

## Data

| File | Content | Source |
|---|---|---|
| `deathsnorthsouthlondon1981to2025.xlsx` | Deaths registered 1981–2025 by cause, region group (North, South, London), sex and five-year age group | Office for National Statistics (David Mais), extract of November 2026, as supplied but saved without its password |
| `deathsnorthsouthdivideengland1981to2016.xlsx` | Deaths registered 1981–2016 by cause, North and South, sex and five-year age group – the data behind the 2018 paper | Office for National Statistics, extract of 2017 |
| `nomis_pop_nsl_1981_2025.csv` | Mid-year population estimates 1981–2025 by sex and 19 age groups (age codes 1–19 = under 1, 1–4, 5–9, then five-year groups to 85 and over) | ONS estimates from NOMIS table [NM_31_1](https://www.nomisweb.co.uk/datasets/pestnew), downloaded 14 November 2026; regions summed to North, South and London |
| `nomis_deaths_2018_definitions_by_sex.csv` | Deaths registered 2013–2025 at ages 25–49 by area, sex, year and five-year age group, for 20 series on the 2018 paper’s definitions | NOMIS table [NM_161_1](https://www.nomisweb.co.uk/datasets/mortsa), 15 November 2026 |
| `nomis_deaths_by_category_pooled.csv` | Deaths under 75, persons, pooled over 2015–19, 2020–24 and 2021–25, by series, area and age group | NOMIS table NM_161_1, 14 November 2026 |
| `nomis_suicide_england_wales_2013_16.csv` | Suicides at ages 25–49 among residents of England and of Wales, by sex, 2013–2016 | NOMIS table NM_161_1, 15 November 2026 |
| `nomis_browser_scripts/` | The JavaScript that pulled the three NOMIS deaths files, with the ICD-10 codes requested for each series. It runs in a browser console on www.nomisweb.co.uk | |

**‘South’ excludes London in the 2026 ONS file, the population file and the NOMIS files (area codes N, X and L).** The scripts build `South` (the rest of the South plus London, the definition used in the papers) and `SouthXL` (the South excluding London). In the 2017 ONS file ‘South’ includes London.

Why NOMIS as well as the ONS extracts:

- **Populations.** The extracts hold deaths only. The ONS mid-year estimates on NOMIS give London separately and reach 2025. Summed to North and South, they are identical in every cell to the populations in the 2026 paper’s dataset for 1981–2021. For 2022–2024 they are the revised estimates that ONS published after the paper’s data were assembled, so all-cause results for those years differ slightly from the paper’s.
- **Deaths on the 2018 definitions.** The 2017 extract stops at 2016 and does not separate London, and the 2026 extract defines accidents, drug and alcohol deaths differently. NOMIS has every ICD-10 underlying cause from 2013.
- **The split of ‘other causes’.** The extracts hold only the named causes.

## Running the code

R 4 with the packages readxl, dplyr (1.1.0 or later), tidyr and readr, plus ggplot2 (3.4.0 or later) and svglite for the charts; the scripts install any that are missing. Set the working directory to this folder and run the scripts in order:

```r
setwd("path/to/2026 Causes")
source("1_cause_decomposition.R")              # about a minute
source("2_like_for_like_2018_definitions.R")   # a few seconds
source("3_other_causes_split.R")               # a few seconds; one of its checks reads an output of script 1
source("4_figures_by_period.R")                # a few seconds; draws from two outputs of script 2
source("5_contour_plots.R")                    # about half a minute; draws from an output of script 1
source("6_pooled_cells_and_cohorts.R")         # about a minute; draws from an output of script 1
source("7_life_expectancy.R")                  # about ten seconds; draws from an output of script 1
```

Each script writes CSV files to `output/`, which it creates, and prints its checks and a short summary. Script 4 writes its two charts there as SVG and PNG, with the numbers plotted and the captions as text. Scripts 5, 6 and 7 do the same for their plots, in `output/contours/`, `output/pooled/` and `output/life_expectancy/`. The header of each script lists its outputs and its methods. Outputs are not stored here.

## Methods in brief

- **Areas.** North: North East, North West, Yorkshire and The Humber, East Midlands and West Midlands. South: East of England, South East, South West and London. London is also analysed on its own, against the rest of the South.
- **Rates** are directly standardised to the 2013 European Standard Population, per 100,000. Persons pool the sexes within each age group. A period rate pools deaths and populations over the years of the period.
- **Gap** = rate in area A minus rate in area B. **Ratio** = rate in A divided by rate in B. Script 1 also gives the **rate ratio** from the within-year Poisson model of the 2026 paper (deaths on area, age group and sex, with log population as offset).
- **Excess deaths** = deaths observed in A minus the deaths expected at B’s age- and sex-specific rates in the same year. A period figure is the mean of the annual figures.
- **Intervals** are 95% and allow only for Poisson variation in the deaths, so they are too narrow: they ignore error in the population estimates and in the coding of causes.
- **Contour plots** (script 5) show the unadjusted rate ratio in each year and five-year age group, as a percentage excess, on the cause groups of the 2026 extract. A single cell often holds few deaths. One version of each plot gives zero excess to cells with fewer than 20 deaths in the two areas together; the other leaves those cells grey and puts a dot on every cell whose excess cannot be told from zero at the 5% level, by an exact test that makes no allowance for the number of cells tested.
- **Age–period–cohort models** (script 6) are fitted to the North:South rate ratio in cells of five-year age group by five-year period. Given the deaths in a cell, the number in the North is binomial, so a logistic model with the log ratio of person-years as offset models the log rate ratio directly. Overdispersion is allowed for. A steady trend cannot be divided between period and cohort: it is given to cohort, and the tests of period and of cohort are tests of curvature.
- **Life expectancy** (script 7) is period life expectancy at birth from abridged life tables with 19 age groups, the last one open at 85. The gap between South and North is divided among the age groups by Arriaga’s method. His formula treats its two populations differently, so the answer depends a little on which is taken first: the contribution of an age group is the mean of the two answers, and both are in the output. Within an age group the contribution is shared among causes in proportion to their part of the difference in death rates, on the cause groups of the 2026 extract. Intervals for life expectancy and the gap are Chiang’s; those for the contributions come from 2,000 simulated sets of deaths.
- Deaths are counted by year of registration.

## Cause definitions

Script 1 uses the groups of the 2026 extract (ICD-10 shown; the ICD-9 codes for 1981–2000 are in the workbook’s ‘Definition’ sheet):

| Cause | ICD-10 codes |
|---|---|
| Cardiovascular | I00–I99 |
| Diabetes | E10–E14 |
| Accidents | V01–X59 |
| Suicide | X60–X84, Y10–Y34 |
| Alcohol-related | F10, G31.2, G62.1, I42.6, K29.2, K70, K73, K74 (excluding K74.3–K74.5), K86.0, X45, X65, Y15 |
| Other causes | All causes minus the five groups above |
| Drug poisoning (memo item, from 1993) | F11–F16, F18–F19, X40–X44, X60–X64, X85, Y10–Y14 |
| Obesity (memo item) | E66, any mention on the death certificate |

These groups overlap. Alcohol-related deaths include codes that also sit in cardiovascular (I42.6), accidents (X45) and suicide (X65, Y15), so ‘other causes’ subtracts those deaths twice. Drug poisoning cuts across accidents, suicide and other causes, which is why it is a memo item and not part of the sum.

Script 2 uses the groups of the 2017 extract, as in the 2018 paper:

| Series | Cause | ICD-10 codes |
|---|---|---|
| `DRUG18` | Drug-related (from 1993) | F11–F16, F18–F19, X40–X44, X85 |
| `ALC18` | Alcohol-related | As above without X65 |
| `SUI` | Suicide | X60–X84, Y10–Y34, so including intentional and undetermined drug and alcohol poisoning |
| `ACC18` | Accidents | V01–X59 excluding X40–X45 |
| `CVD` | Cardiovascular | I00–I99 |
| `DIAB` | Diabetes | E10–E14 |
| `CANCER` | Cancer | C00–C97 |
| `OTHER18` | Other causes (from 1993) | All causes minus the seven groups above |

Here only two codes are counted twice: I42.6 (alcohol-related and cardiovascular) and Y15 (alcohol-related and suicide). The memo series `OVL` holds them. The parts of ‘other’ – respiratory, digestive, mental and neurological, COVID-19 and the rest – are available from 2013 only.

Script 3 lists its 21 categories and five groups, with their codes, at the top of the script and in the `description` column of its output.

## Points to note about the data

1. **Drug poisoning before 1993.** The 2026 extract holds zeros for 1981–1992, when the series was not supplied. Script 1 treats them as missing.
2. **Suicide in the 2017 extract.** At ages 25–49 for 1981–2014 the 2017 extract has 44,890 suicides in the South, against 39,466 in the 2026 extract (13.7% more). The North has 38,917 in both, and 2015–16 are identical in both areas. In 2013 and 2014 the surplus is 206 and 140 deaths, close to the 194 and 119 suicides among residents of Wales in NOMIS. This has not yet been confirmed with ONS. Script 2 therefore takes suicide for 1981–2012 from the 2026 extract and writes the comparison to `like_for_like_suicide_*.csv`.
3. **Alcohol-related deaths, 1981–2000.** The 2026 extract has 62,926, against 65,154 in the 2017 extract (3.4% fewer; 7.1% fewer at ages 25–49). The reason is not yet known. Script 2 uses the 2017 extract for those years; script 1 uses the 2026 extract.
4. **NOMIS against the extracts.** Where they cover the same deaths they agree exactly or to within a few deaths; `like_for_like_validation.csv` gives every comparison.
5. **Suicide below age 10** is not in the 2026 extract and is set to zero.

## How the code was checked

- Script 1 checks its Poisson fit against `glm()`.
- Script 2 compares the sources of deaths cell by cell wherever two of them cover the same deaths, and recomputes the excess deaths published in the 2018 paper.
- Script 3 checks that its categories add up to all causes in every age group, area and period, and compares its NOMIS rates with the rates from the ONS extract.
- Script 5 was compared with contour plots of the same data drawn independently in Stata. Of the 15,390 points in the 18 plots, 96.6% fall in the same colour band and the rest in the band next to it, nearly all with values close to a cut. Its own drawing was then checked point by point: every cell is drawn in the colour that its value calls for. The colours of its second version were chosen by measuring colour differences in normal vision and under simulated colour-vision deficiency; the figures are in the header of the script.
- Script 6: its standardised rates agree with those of script 1 to three decimal places, and its age–period–cohort fits were reproduced with a second formulation (Poisson models with a separate baseline for every age group and period).
- Script 7 checks, as it runs, that its contributions add up to the gap, that Arriaga’s formula gives the same answer as replacing the death rates one age group at a time, and that the simulated interval for the gap agrees with Chiang’s. Life expectancy for England in 2021–23 comes to 79.2 years for men and 83.1 for women, against 79.1 and 83.0 in the ONS national life tables, which use single years of age. Its estimates were recomputed with separately written code and agree to the last decimal place written. The mean of Arriaga’s two answers was compared with a decomposition by continuous change (in the manner of Horiuchi, Wilmoth and Pletcher, 2008), which does not depend on which population is taken first: the age bands’ shares of the gap agree to within 0.1 percentage points, where the two answers themselves differ by up to 2.4.

## Licence and attribution

The ONS workbooks are Crown copyright, reused under the [Open Government Licence](https://www.nationalarchives.gov.uk/doc/open-government-licence/version/3/). Source: Office for National Statistics licensed under the Open Government Licence. The NOMIS files are extracts of ONS data from [NOMIS](https://www.nomisweb.co.uk/), reused under the same licence.
