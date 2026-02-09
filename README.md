# Owens Valley Vegetation Condition Report

## Introduction

This repository contains the code and data for the Owens Valley Vegetation Condition Report, which is part of the Inyo-LA Technical Group's annual monitoring program. The report follows the guidelines described in the [Green Book Box I.C.1.a.ii](https://www.inyowater.org/wp-content/uploads/2017/09/GBMemo_SCfeb2017.pdf) for monitoring vegetation response to groundwater pumping. The goal is to identify and evaluate groundwater-dependent vegetation parcels that exhibit significant deviations from baseline conditions.

## Objectives

The primary objectives of this report are:
1. To provide a summary of the annual monitoring program.
2. To flag groundwater-dependent vegetation parcels in wellfield areas with significant deviations from the 1984-87 vegetation baseline.
3. To identify parcels that have been below baseline for five consecutive years.
4. To corroborate significant trends using NDVI time series data.

## Data and Methods

### Baseline Data

From September 1984 to November 1987, LADWP inventoried and mapped vegetation on LA-owned lands in Owens Valley into 2,126 polygons of similar vegetation type, covering 223,168 acres. The monitoring program focuses on a subset of these groundwater-dependent parcels that are affected by groundwater pumping.

### Monitoring Sample Size

Parcels were selected based on several criteria, including the presence of permanent monitoring sites, proximity to pumping wells, and availability of past and current land use data. Over the years, the number of sampled parcels has varied due to staffing availability, with a total of 192 parcels sampled at least once after the baseline period.

### Statistical Methods

Welch's t-test for unequal variance is used to evaluate deviations in total perennial cover and perennial grass cover from baseline conditions. For parcels with low baseline sample sizes, a one-sample t-test is performed. The statistical methods described in Green Book Box I.C.1.a.ii are implemented using a transparent and reproducible data pipeline written in R.

### Data Aggregation

Perennial species cover is analyzed because annual species are not dependent on groundwater. Perennial cover is aggregated into grass, herb, and shrub categories. The proportion of shrub, herb, and grass cover as fractions of total perennial cover is calculated to analyze changes over time.

## Results

The results section includes a comparison of control and wellfield parcel groups, rarefied to those parcels sampled every year since 1992. Linear trends for total cover, perennial herbaceous cover, and shrub cover are reported. Detailed maps showing parcel-level changes in perennial cover and grass cover are provided for each wellfield.

## Discussion

The methods outlined in this report automatically flag wellfield parcels that have been statistically below baseline for the past five consecutive years in terms of total perennial cover or perennial grass cover. The measurability of the effect has been determined according to Green Book Section I.C.1.a. Steps to determine the causation of the measurable change are outlined in Green Book Section I.C.1.b. Detailed information on each flagged parcel is available for further investigation.

## Annual updates

See **[ANNUAL_UPDATE.md](ANNUAL_UPDATE.md)** for step-by-step instructions to update the report each year: set `cYear` in `_targets.R`, add the year’s line-point files, parcel attributes, and depth-to-water file, then run the pipeline and render the site. The same doc outlines a future option to push master LPT, attributes, and DTW to a cloud DB (e.g. MotherDuck) so other projects can use the same source of truth without copying files.

## Repository Structure

- `code/`: Contains R scripts for data processing and analysis.
- `data/`: Contains raw and processed data files (to be added).
- `docs/`: Documentation files.
- `figures/`: Contains figures and maps used in the report.
- `images/`: Contains image files for the report.
- `output/`: Contains generated reports and summary tables.
- `.gitignore`: Lists files and directories to be ignored by git.
- `LICENSE`: License for the project.
- `README.md`: This file.
- `_targets.R`: Configuration file for the targets pipeline.
- `_quarto.yml`: Configuration file for Quarto.
- `index.qmd`: Quarto markdown file for the main report.

## Building the site

- **Home page:** `index.qmd` renders to `docs/index.html` (Annual Summary).  
- **Data page:** `lpt_etl.qmd` → `docs/lpt_etl.html`.  
- **Parcel profiles:** `parcel_profiles.qmd` → `docs/parcel_profiles.html` (optional; currently excluded from default `quarto render` for speed).

**Important:** Always run the render **from the project root** so that output and all assets go into `docs/`:

```bash
cd /path/to/vegetation-condition   # project root
quarto render index.qmd
```

When the render finishes, you should see:

- `docs/index.html`
- `docs/index_files/figure-html/*.png` (figures)
- `docs/site_libs/` (JavaScript/CSS for tables and widgets)

If figures or tables don’t show in the browser:

1. **Check that assets are in `docs/`:**  
   List `docs/index_files/figure-html/` and `docs/site_libs/`; if they’re missing, the render was run from the wrong directory or didn’t complete.

2. **View the site over HTTP, not as a local file:**  
   Opening `docs/index.html` directly (`file://`) can prevent scripts and widgets (DataTables, map) from loading. Use a local server instead:
   ```bash
   quarto preview
   ```
   Or from the project root: `cd docs && python3 -m http.server 8000`, then open `http://localhost:8000`.

If `docs/index.html` shows Parcel Profiles content instead of the Annual Summary, run `quarto render index.qmd` from the project root and wait for it to finish (pandoc can take 10+ minutes).

### Publishing to GitHub Pages

The site is published from the **`docs/`** folder. In the repo **Settings → Pages**:

- **Source:** Deploy from a branch  
- **Branch:** `main`  
- **Folder:** `/docs`  
- **Save**

The repo includes `docs/.nojekyll` so GitHub does not run Jekyll on the built site. After you push changes to `docs/` (e.g. after `quarto render`), the site updates automatically. If the live site still shows an old navbar, theme, or logo, try a hard refresh (Ctrl+Shift+R or Cmd+Shift+R) or clear the cache for the site.

## Getting Started

### Prerequisites

Ensure you have the following software installed:
- R
- RStudio
- Required R packages (listed in the `setup` chunk of the R scripts)

### Installation

1. Clone the repository:
    ```bash
    git clone https://github.com/inyo-gov/vegetation-condition.git
    cd vegetation-condition
    ```

2. Install required R packages:
    ```r
    install.packages(c("targets", "tidyverse", "rmarkdown", "DT", "htmlwidgets", "sf", "tmap", "tmaptools", "ggpmisc", "ggpubr", "gt", "glue", "ggdist", "here", "ggstatsplot", "janitor", "withr", "rprojroot"))
    ```

### Building the Targets Pipeline

Before running the report, you need to build the targets pipeline. This will ensure that all necessary data processing steps are completed.

1. Open the R project in RStudio.
2. Load the `targets` package and build the pipeline:
    ```r
    library(targets)
    tar_make()
    ```

### Running the Analysis

1. Once the targets pipeline is built, open the main analysis script (e.g., `index.qmd`) and execute the code chunks to generate the report.

### Viewing the Report

The final report will be generated as an HTML file in the `output/` directory. Open this file in a web browser to view the results.

## Contributing

Contributions to improve the report and code are welcome. Please submit issues or pull requests on the GitHub repository.

## License

This project is licensed under the MIT License - see the [LICENSE](LICENSE) file for details.

## Acknowledgments

We acknowledge the efforts of the Inyo County Water Department and the Los Angeles Department of Water and Power in data collection and monitoring. Special thanks to all contributors and reviewers of this report.

For more information, visit [Inyo County Water Department](https://inyowater.org).
