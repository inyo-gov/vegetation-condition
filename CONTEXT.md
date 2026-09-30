# CONTEXT — vegetation-condition

## Purpose
Owens Valley vegetation-condition LPT / NDVI / parcel-profile monitoring stack (ICWD). Eco workflow artifacts live here; related STM / type-space scores live in stm.

## Status notes

### 2026-09-30 PT — IND026 profile ingest (hillshade + photo + spp-rank)
- Live profile panel: hillshade Leaflet ImageOverlay + AGOL photo `IND026_05_212` + decade ranks (no 1990s; earliest reorder **2005**).
- Assets: `www/flagged_parcel_maps/IND026/`. Helper: `code/R/flagged_parcel_context.R` (graceful skip). ICM: `docs/2026-09-30_ind026-profile-hillshade-spp-rank-ingest.md`.
- Research framing only — not I.C.1.b. Other parcels unchanged until their asset folders exist.


### 2026-09-30 PT — IND026 spp-rank decades (C→B-like MVP; research draft)
- Transition-first MVP **IND026** (Independence; GB Type C; not FSL). Locked method unchanged: N=10, perennial-only, mean of visits present, baseline=parcel baseline year, current=latest LPT year.
- Baseline **1985** (1 visit; SPAI-only perennial); **1990s: 0 visits**; 2000s **5** (2005–2009); 2010s **10**; current **2026**.
- Top-5 baseline: SPAI 49.00 / 1.0000. Top-5 current: ARTR2 17.12 / 0.4560, ATTO 10.06 / 0.2680, ERNA10 4.31 / 0.1148, SPAI 3.56 / 0.0948, ATPO 1.00 / 0.0266.
- Earliest top-10 reorder vs baseline: **2005**. SPAI K≥3 also **2005**. Composition looks C→B-like (SPAI meadow → ATTO/ARTR2 shrubland); DISP absent at baseline.
- Cross-check dominants: baseline `SPAI`; current `ARTR2; ATTO; ATPO` (relative top-3 ARTR2/ATTO/ERNA10; ATPO rank 5).
- Exports: `exports/spp_rank_mvp_2026-09-30/IND026_decade_ranks.csv`, `IND026_rank_delta.csv`. Script: `code/run_spp_rank_decade_IND026_2026-09-30.R`. LAW052_* method demo not overwritten.
- Docs: plan § IND026 + pivot pointer. Gaps: no 1990s LPT; single-year 1985 baseline; pre-2015 vs permanent-network caveat; forcing deferred. Runner-ups IND029/TIN064 not built (IND026 multi-decade OK).
- Not Board policy / not I.C.1.b.



### 2026-09-30 PT — Flagged-parcel spp-rank decades (research draft)
- Plan merged (geo kept): `docs/2026-09-30_flagged-parcel-hillshade-photo-spp-rank-plan.md` — eco species-rank method + LAW052 prototype; geo owns hillshade/boundary/photo; shared ids = `output/nmds_flagged_parcels_index.csv`.
- MVP **LAW052**: baseline **1987**; decades 1990s (4 visits), 2000s (10), 2010s (10); current **2026**. Top-N=10, perennial-only, rank by perennial relative; abs = `parcelMeanCover` from `data/parcel_species_cover_app_2026.csv` (+ Lifecycle join from `lpt_MASTER_2026.csv`).
- Baseline top-5: DISP 18.83 / 0.6764, ERNA10 4.67 / 0.1677, SPAI 3.50 / 0.1257, PSPO 0.50 / 0.0180, NIOC2 0.17 / 0.0061. Current top-5: ERNA10 6.75 / 0.5682, IVAX 2.00 / 0.1684, ATTO 1.88 / 0.1582, SAVE4 0.75 / 0.0631, DISP 0.38 / 0.0320.
- Earliest top-10 reorder vs baseline: **1991** (sparse year; still first dated order-diff). Forcing inference deferred (DTW/NDVI/PPT ready; grazing/fire joins not).
- Exports: `exports/spp_rank_mvp_2026-09-30/LAW052_decade_ranks.csv`, `LAW052_rank_delta.csv`.
- Not Board policy / not I.C.1.b. Cross-check `output/indicators_dominants_2026.csv`.

### 2026-09-30 PT — Flagged-parcel hillshade geo MVP (LAW052)
- Geo built parcel-framed hillshade package for **LAW052** (BWMA gdaldem params) under `../lidar-data/data/processed/flagged_parcel_hillshade/LAW052/`.
- ICM: `docs/2026-09-30_flagged-parcel-hillshade-profile-geo-inventory.md` (inventory n=13 + MVP paths). Desk owns profile embed / Vercel.
- Desk plan: `docs/2026-09-30_flagged-parcel-hillshade-photo-spp-rank-plan.md`.
- Gaps queued (no LPC yet): IND021, IND139, FSP006.


### 2026-09-30 PT — Flagged parcel hillshade + photo + spp-rank plan
- Ask: BWMA-style parcel-framed hillshade (+ boundary, transects, 1 AGOL photo) on I.C.1.a profiles; decade species-rank / rank-shift table (Shiny-like).
- **Plan only** (no UI): `docs/2026-09-30_flagged-parcel-hillshade-photo-spp-rank-plan.md`.
- MVP parcel: **LAW052**. Flagged n=13 (2026 profiles). Geo/Eco lanes split in plan; BWMA CHM retune stays separate.


### 2026-09-30 PT — Support decline-only sign gate
- Support = cover↓ / grass↓ / shrub↑ (independent); Grass↑ / Shrub↓ → Not supported.
- Percentile + sustained counters use same Δ sign gate (no `cur_p < 50`).
- Spot-check FSL054: Grass percentile +27 under Not supported.
- Prod: https://icwd-vegetation-condition.vercel.app (`dpl_HZfDFTu6MtgKsBBKjmLisj2Mkykq`).
- See `docs/2026-09-30_support-decline-only-grass-sign.md`.


### 2026-09-30 PT — Cover stack colors + x-axis ticks
- Lifeform stack palette: grass `#1B5E20`, herb `#7B1FA2`, shrub `#8D6E63`, annual native `#F8BBD0`, annual non-native `#C2185B`.
- Year ticks: drop colliding 5-yr label near `cYear` (e.g. keep 2026, drop 2025) in profiles + PDF 6b.
- Prod: https://icwd-vegetation-condition.vercel.app (`dpl_6k6s2ujq58cLVErrXtXh6VVoFp8R`).

### 2026-09-30 PT — Jul–Sep NDVI 2026 fill
- ee-tools Landsat daily max 2026-09-21; restored seasonal `rs_current` (DOY 196–258).
- `data/rs_2026.csv` max year **2026** (380 parcels). Parcel profile NDVI bars include 2026.
- May pilot composites also filled 2025–2026 for LAW052/FSL044 (exports only).
- Prod: https://icwd-vegetation-condition.vercel.app (`dpl_2jKLK3o7uvvX2BpW3dxrrb78Eerf`).

### 2026-09-30 PT — May NDVI + LPT prior prototype (research draft)
- Pilot: LAW052 + FSL044 (FSL044 = Owens River mainstem, not Fish Slough).
- Model: random-walk / empirical-Bayes local-level prior by lifeform; optional Kalman update from Apr20–Jun10 May Landsat surface NDVI. No brms/rstan (not installed).
- May NDVI in-repo for both pilots 1984–2024 (41 parcel-years each); **2025–2026 May missing** (scene tables end 2024). Jul–Sep `NDVI_SUR` not substituted.
- Matched-year RMSE: shrub 3.45→3.14 with May; grass ~3.85≈3.88; annuals 27.0→24.3; total ~5.46≈5.47 (n=36 / 23 annuals).
- 2026: LPT present; May NDVI absent → prior-only scored (pooled RMSE ≈ 5.26); NDVI path unscored.
- Artifacts: `code/run_may_ndvi_lpt_prior_2026-09-30.R`, `docs/2026-09-30_may-ndvi-lpt-prior-prototype.md`, `exports/may_ndvi_lpt_2026-09-30/`.
- Not a management product / not Board policy.

### 2026-09-30 PT — RS parcel sibling layer (research draft)
- Intersected veg parcels (`allvegparcels_NAD83`, unique PCL) with gravel stub, lakes, canals (15 m buf), Owens River (30 m buf), streams (10 m buf; Fish Slough×FSL* excluded as mislabel).
- No valley-wide roads layer (county/TIGER missing); LORP RT_RoadsPaths not used.
- Goal: edit list for sibling **`rs_parcels`** (Landsat/Sentinel zonal zones only); legal veg shapefile untouched.
- FSL044: gravel stub 19,741 m² = 10.83%; in EE set; Owens 30 m misses (~61 m to centerline).
- Artifacts: `docs/2026-09-30_rs-parcel-layer.md`, `exports/rs_parcel_2026-09-30/parcel_nonveg_intersections.csv`.
- Later (not built): higher-res cells + Bayesian-style vegetation community attribution.
- Not Board policy / research draft only.

### 2026-09-30 PT — Flagged MVP pivot to C→B-like transitions
- New-build order: **IND026 → IND029 → TIN064**; these are the clearest transition-first cases with LiDAR and photos on disk.
- LAW052 remains the existing hillshade/photo/spp-rank reference, with an inflated-baseline caveat; no UI in this pass.
- Decision note: `docs/2026-09-30_flagged-mvp-c-to-b-pivot.md`.
