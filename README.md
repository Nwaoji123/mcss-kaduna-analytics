# MCSS Survey Analytical Tool

Current build: Version 48.

This Shiny application analyzes the shared current-round MCSS files:

- `MCSS_KD_MAIN.csv`
- `MCSS_KD_MEMBERS_MERGED.csv`
- `MCSS_KD_WOMEN_MERGED.csv`
- `MCSS_KD_CHILDREN_MERGED.csv`
- `KD_rural_urban.csv`
- the XLSForm questionnaire (optional, for the data dictionary)

It includes the current-round indicator families in the supplied R script. Round-one/2025 comparisons requiring unshared files are intentionally excluded. Indicators are organized into four user-facing groups: housing/household/respondent characteristics, malaria prevention, health seeking behavior and treatment, and malaria beliefs/exposure to malaria messages. Each indicator still uses its correct underlying source file.

Across all indicator groups, tables with multiple response options or categories are displayed in wide format. Response options/categories labelled `Missing` are suppressed from displayed and downloaded result tables. The headline ANC indicator includes ANC reported for either a recent live birth or a current pregnancy, as captured by `receive_anc`. Detailed ANC and IPTp indicators remain restricted to women reporting a live birth in the last two years. ANC visits are grouped as One, Two, Three, and Four or more.

The app uses a polished native Shiny/Bslib interface with a collapsible vertical left control drawer for file upload, data files, thematic area, indicator, and disaggregation controls. The main canvas remains wide for results, with sticky output navigation, compact indicator context chips, searchable/sortable result tables, and chart/map views. The Chart tab automatically creates a visual for the selected indicator and disaggregation. It uses pie/donut-style charts only for small overall mutually exclusive categorical distributions; eligible mutually exclusive categorical indicators with disaggregation cuts use 100% stacked bars. Multiple-response indicators use grouped or faceted bars because their options can sum to more than 100%. All pies and bars are labelled with their values, and charts can be downloaded as PNG files. Chart subtitles are intentionally suppressed for a cleaner presentation view.

The Map view uses Kaduna LGA boundaries from the bundled Nigeria LGA GeoJSON file and a key-free OpenStreetMap background. It works when `lga` is selected as the disaggregation. For multi-option or categorical indicators, choose the option/category to map. Percentage indicators use Low `0–39.9%` (red), Medium `40–69.9%` (yellow), and High `70–100%` (green); mean indicators use Low/Medium/High tertiles across LGAs. No-data/unmatched LGAs are grey. The selected LGA map can be exported as a clean PNG image or PDF without a web-map background.

The app automatically derives the household wealth index from the housing, water, sanitation, livestock and asset variables using principal component analysis. It creates five wealth quintiles and joins them to the members, women and children files by `hhid`, enabling wealth-quintile disaggregation across the analysis.

The treatment-place distribution is restricted to children with fever for whom treatment was sought (`child_ill == "Yes"` and `seek_treatment == "Yes"`). For malaria testing by source of care, treatment place is shown as wide table columns rather than as a disaggregation filter.

## Start the tool

1. Install R 4.3 or later from <https://cran.r-project.org/>.
2. Open a terminal in this folder.
3. Run `Rscript install_packages.R` once.
4. Run `Rscript run_tool.R` whenever you want to start the tool.
5. Load the four survey CSV files and rural/urban classification through the app. Survey records are not bundled with the tool.

RStudio users can instead open `app.R` and click **Run App**.

## What the MVP provides

- Regrouped indicator menus for housing/household/respondent characteristics, malaria prevention, health seeking behavior/treatment, and malaria beliefs/message exposure.
- Native Shiny/Bslib frontend with dashboard-style styling.
- Collapsible vertical left drawer for upload, data files, thematic area, indicator, and disaggregation controls.
- Disaggregation by available geography and demographic variables.
- Searchable and sortable interactive tables.
- CSV download of the current result.
- Automatic chart view with PNG download.
- LGA map view with Low/Medium/High colour classes.
- An optional questionnaire dictionary displayed within the Help tab.
- Required-column checks and plain-language usage instructions.

## Important analytical note

The current app reproduces a focused subset of the supplied analysis script and uses the existing `weights` field. Before publishing official estimates, compare results with approved tables and confirm indicator denominators, eligibility rules, survey-design variables, and treatment of missing responses.

## Recommended next release

After users validate this MVP, add the remaining approved indicators from the original R script, confidence intervals, branded Excel exports, and a deployment configuration for an organizational server.
