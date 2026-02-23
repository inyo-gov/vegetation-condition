# Annual Report Update Guide

Onboarding guide for staff updating the Vegetation Condition report each year. The report has three main outputs: **Data** (ETL pipeline), **Annual Summary** (statistics, tables, maps), and **Parcel Profiles** (QA/QC for chronic parcels). You only need to add the new year’s data files and change one parameter; the pipeline and Quarto site do the rest.

---

## 1. What gets updated

### Single parameter

- **`cYear`** (current year) — in **`_targets.R`**, line ~34:
  ```r
  tar_target(cYear, 2025),   # change to the new year, e.g. 2026
  ```
  Everything that depends on the current year (file paths, labels, filters) uses this.

### Data files to add or replace

Place files under **`data/`**. Required for the annual run:

| Purpose | File(s) | Notes |
|--------|---------|--------|
| **Line point (current year)** | `data/lpt_ICWD_YYYY.csv`, `data/lpt_LADWP_YYYY.csv` | One per agency for the new year. |
| **Master line point (previous year)** | `data/lpt_MASTER_YYYY.csv` | Prior year’s master (e.g. `lpt_MASTER_2025.csv` when updating to 2026). Must already exist from last year’s run. |
| **Parcel attributes** | `data/Attributes.csv` | Parcel IDs, wellfield, type (W/C), etc. Update in place if needed. |
| **Depth to water** | `data/dtw_YYYY.csv` | Current-year DTW by parcel. |

**PDF stacked time series (parcel-level plots):**

- **Script:** `pdf_stacked_timeseries_call.R` in the project root. It loads targets (`transects`, `attributes_pfix`, `rs_pfix`, `dtw_pfix`), then calls `create_ts_plots_pdf_gg_discrete()` from `code/R/gg_timeseries_plots6b.R` to write a multi-page PDF of cover/DTW/NDVI/precip time series per parcel.
- **Parameters to update each year:** In `pdf_stacked_timeseries_call.R`, set:
  - `current_year <- 20XX` (e.g. `2026`)
  - `pdf_file <- "Parcel_TimeSeries_Customized06b_20XX.pdf"` (same year in the filename)
- **When to run:** After `tar_make()` (so targets and any `rs_pfix`/remote sensing are up to date). Run from the project root: `Rscript pdf_stacked_timeseries_call.R`. The PDF is written to the project root unless you change `pdf_file` to a path.

**Downstream (inyoShiny):** After `tar_make()`, four artifacts for the inyoShiny app are written: `data/dtw_pfix_YYYY.csv`, `data/rs_YYYY.csv`, `data/lpt_MASTER_YYYY.csv`, and `data/parcels_YYYY.rds`. Copy these into inyoShiny’s `data/` (see inyoShiny’s `docs/HOW_TO_UPDATE_ANNUALLY.md`).

Optional / less frequent:

- **DTW history:** `_targets.R` references `data/dtw_2024.csv` for historical DTW. If you need a different baseline year, edit `dtw_hist_file` in `_targets.R`.
- **Species:** `data/species.csv` — only if species codes or names change.
- **Baseline (wvcom):** `data/wvcom1.csv` — only if baseline data is revised.
- **GIS:** `data/gisdata/*` — parcels, canals, streams, etc. Only if geometry or IDs change.

---

## 2. Annual update steps

1. **Set the year**  
   In `_targets.R`, set:
   ```r
   tar_target(cYear, 20XX),   # e.g. 2026
   ```

2. **Add/replace data files**  
   - Ensure `data/lpt_ICWD_20XX.csv` and `data/lpt_LADWP_20XX.csv` exist for the new year.  
   - Ensure `data/lpt_MASTER_20XX.csv` exists for the **previous** year (from last year’s run).  
   - Put the new year’s depth-to-water file at `data/dtw_20XX.csv`.  
   - Update `data/Attributes.csv` if parcel list or wellfield/type changed.

3. **Run the pipeline**  
   From the project root:
   ```bash
   Rscript -e "targets::tar_make()"
   ```
   Or in R:
   ```r
   setwd("/path/to/vegetation-condition")
   targets::tar_make()
   ```
   This rebuilds all targets (LPT merge, transects, parcel deltas, significance tests, etc.).

4. **Generate the PDF stacked time series**  
   In `pdf_stacked_timeseries_call.R`, set `current_year` and `pdf_file` to the new year (e.g. `2026` and `"Parcel_TimeSeries_Customized06b_2026.pdf"`). Then from the project root:
   ```bash
   Rscript pdf_stacked_timeseries_call.R
   ```
   The PDF is written to the project root. It uses the same targets (transects, attributes, DTW, remote sensing) as the rest of the report.

5. **Render the website**  
   ```bash
   quarto render
   ```
   This builds:
   - **Data:** `lpt_etl.qmd` → `docs/lpt_etl.html`  
   - **Annual Summary:** `index.qmd` → `docs/index.html`  
   - **Parcel Profiles:** `parcel_profiles.qmd` → `docs/parcel_profiles.html`  

6. **Check outputs**  
   - Open `docs/index.html` (or run `quarto preview` and use the nav).  
   - Confirm the year and counts in the intro, Table 1 (wellfield), Table 2 (control/other), and the below/above baseline summary.  
   - Confirm Parcel Profiles if you use that page.  
   - Confirm the PDF exists (e.g. `Parcel_TimeSeries_Customized06b_20XX.pdf`) and opens correctly.

7. **Commit and push** (if using Git/GitHub Pages)  
   Commit updated `_targets.R`, new/updated data under `data/`, and regenerated `docs/` if you track built output.

---

## 3. Where the three sections get their data

- **Data page (`lpt_etl.qmd`)**  
  Documents and runs the ETL: reads LPT CSVs, attributes, species, etc., and writes merged/master CSVs. Driven by `cYear` and the same `data/` files.

- **Annual Summary (`index.qmd`)**  
  Loads targets (e.g. `deltas_ttest_att`, `parcel_summary`, `boxplot.w.c`, `parcels_shp_ttest`). All of these come from `_targets.R` and thus from the LPT master, attributes, DTW, and parcel shapefiles.

- **Parcel Profiles (`parcel_profiles.qmd`)**  
  Same targets as the summary; filters to wellfield parcels chronic below baseline and builds the profile plots and notes.

No need to edit the Quarto files for a normal annual update—only `cYear` and the data files.

---

## 4. Cloud database (MotherDuck / DuckDB) — source of truth

To avoid copying the same datasets across repos, the plan is to push the **master line point**, **parcel attributes**, and **depth to water** (and any other canonical tables) to a cloud database (e.g. **MotherDuck**) and use **DuckDB** (or DuckLake) to catalog them. This repo would remain the place where new LPT and DTW data are added; after the targets run, a sync step would update the cloud DB. Other projects (dashboards, analyses) would then read from the cloud instead of local copies. This saves needing to update the data in the app bundle so there's no republishing step. 

### Intended setup

- **Database:** MotherDuck (DuckDB in the cloud), or another DuckDB-compatible store.  
- **Tables to publish:**  
  - Master line point (e.g. `lpt_MASTER` or by-year views).  
  - Parcel attributes (`Attributes` or `parcel_attributes`).  
  - Depth to water (`dtw` with Parcel, Year, DTW).  
- **This repo:**  
  - Keep `_targets.R` and `data/` as the place new LPT and DTW files are added.  
  - Add a **target or script** that, after `tar_make()`, connects to MotherDuck and writes/upserts the updated tables (e.g. from `lpt_updated_master`, `attributes_pfix`, `dtw_pfix` or their file outputs).  
- **Other projects:**  
  - Connect to the same MotherDuck database (or a read-only view) and query the latest data; no need to copy CSVs between projects.

### Implementation notes

- **R:** Use `duckdb` and `MotherDuck` (or environment variable `MOTHERDUCK_TOKEN`) to connect; create tables or replace data from the targets pipeline.  
- **`_targets.R`:** There is already a commented `motherduck_con` target; uncomment and add targets that depend on `lpt_updated_master_csv`, `attributes_pfix`, `dtw_pfix`, and write to the cloud.  
- **Secrets:** Store MotherDuck token in an env var or secret manager; do not commit it.  
- **Catalog:** Use a small DuckDB/MotherDuck schema (e.g. `vegetation.lpt_master`, `vegetation.attributes`, `vegetation.dtw`) and document the table and column names so other projects can query consistently.

Once this is in place, the annual update flow stays the same (update `cYear`, add data, `tar_make()`, render); the only extra step is that the sync to MotherDuck runs as part of the pipeline or as a follow-up script, and other projects always read the latest from the cloud.

---

## 5. Guide for AI agents: walking a human through the update
Scenario when gaps in technical knowledge transfer have occured, point an AI assistant to this section to help novice staff get up to speed quickly.

This section is for an AI assistant that is guiding a human through the annual update. Use it to know what to ask, what to verify, and in what order.

### Your role

- Walk the user through the update one step at a time.
- Before each step, state what you’re about to do and what you need from them (if anything).
- After each step, verify success (file exists, command succeeded, output looks correct) before moving on.
- If something fails, use the “When something fails” notes below and don’t skip steps.

### Prerequisites to confirm

1. **Project location**  
   Confirm the path to the repo (e.g. `.../vegetation-condition`). All commands assume the project root is the working directory.

2. **Year**  
   Ask: *“What year are we updating the report to?”* (e.g. 2026). Call it `YYYY` in the steps below.

3. **Data files**  
   Ask: *“Do you have the following for [YYYY] (or the prior year where noted)?”*  
   - `lpt_ICWD_YYYY.csv` and `lpt_LADWP_YYYY.csv` (current year LPT).  
   - `lpt_MASTER_YYYY.csv` for the **previous** year (e.g. when updating to 2026, you need `lpt_MASTER_2025.csv` from last year’s run).  
   - `dtw_YYYY.csv` (current year depth to water).  
   - Whether `Attributes.csv` has changed (parcel list, wellfield, or type).  
   If the previous year’s master is missing, explain that they must use last year’s pipeline output or restore it before proceeding.

### Step-by-step flow (use this order)

**Step 1 — Set the year**

- **Action:** Update `_targets.R`: change the line `tar_target(cYear, 20XX),` to the new year (e.g. `2026`).
- **Location:** `_targets.R`, around line 34.
- **Verify:** Search the file for `cYear` and confirm the numeric year is correct.

**Step 2 — Place data files**

- **Action:** Ensure these files exist under `data/`:
  - `data/lpt_ICWD_YYYY.csv`
  - `data/lpt_LADWP_YYYY.csv`
  - `data/lpt_MASTER_(YYYY-1).csv` (previous year’s master)
  - `data/dtw_YYYY.csv`
  - `data/Attributes.csv` (updated if needed)
- **Verify:** List `data/` and confirm the filenames and year in the names. If the user is unsure about column names or format, suggest spot-checking one CSV (e.g. a few rows of the LPT or Attributes) against the table in Section 1 of this doc.

**Step 3 — Run the pipeline**

- **Action:** From the project root, run:
  ```bash
  Rscript -e "targets::tar_make()"
  ```
- **Verify:** The command exits with status 0. Scan the log for errors (e.g. “Error”, “failed”, “could not find”). If any target fails, read the error and check: missing file, wrong path, or wrong `cYear` (e.g. typo in `_targets.R`).

**Step 4 — Generate the PDF stacked time series**

- **Action:** In `pdf_stacked_timeseries_call.R`, set `current_year <- YYYY` and `pdf_file <- "Parcel_TimeSeries_Customized06b_YYYY.pdf"` to match the report year. Then from the project root run:
  ```bash
  Rscript pdf_stacked_timeseries_call.R
  ```
- **Verify:** Script exits with status 0. Check that the PDF file exists in the project root and the filename year matches.

**Step 5 — Render the website**

- **Action:** From the project root, run:
  ```bash
  quarto render
  ```
- **Verify:** The command completes. Check that `docs/index.html`, `docs/lpt_etl.html`, and `docs/parcel_profiles.html` exist and were updated (e.g. by modification time).

**Step 6 — Spot-check the report**

- **Action:** Have the user open the built site (e.g. `quarto preview` or open `docs/index.html` via a local server). Confirm:
  - The intro shows the correct year and parcel/transect counts.
  - Table 1 (wellfield) and Table 2 (control/other) have data.
  - The “Below vs. above baseline by group” table looks reasonable.
  - The PDF (e.g. `Parcel_TimeSeries_Customized06b_YYYY.pdf`) exists and opens; spot-check a page or two for the correct year range.
- **If something looks wrong:** Note which section (intro, Table 1, PDF, etc.) and suggest re-running the pipeline or checking that the right CSVs and year were used.

**Step 7 — Version control (optional)**

- **Action:** If they use Git: suggest committing `_targets.R`, new/updated files under `data/`, and `docs/` if they track built output, then pushing. Remind them not to commit secrets (e.g. MotherDuck tokens).

### When something fails

- **“Could not find file” or “file does not exist”**  
  Check: (1) path is under `data/`, (2) filename has the correct year (e.g. `dtw_2026.csv` when `cYear` is 2026), (3) previous year’s master exists when updating to a new year.

- **`tar_make()` fails on a specific target**  
  Note the target name. Open `_targets.R` and find that target and its dependencies (e.g. file paths, `cYear`). Confirm those inputs exist and match the year.

- **Quarto render hangs or errors**  
  If it hangs on a large figure or map, suggest rendering one document at a time (e.g. `quarto render index.qmd`). If an R chunk errors, the message usually points to the qmd and chunk; fix the underlying data or code and re-render.

- **Wrong year or counts in the report**  
  The year and counts come from `cYear` and the targets. Re-check `_targets.R` for `cYear` and re-run `tar_make()` then `quarto render`.

### What not to do

- Do not edit the Quarto files (`index.qmd`, `lpt_etl.qmd`, `parcel_profiles.qmd`) for a routine annual update unless the user has a specific content or code change.
- Do not skip verifying that the previous year’s master LPT file exists; the pipeline expects it.
- Do not assume `data/` is in the working directory unless the user has confirmed they are in the project root.
