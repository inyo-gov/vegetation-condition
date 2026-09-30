# CONTEXT — vegetation-condition

## Purpose
Owens Valley vegetation-condition LPT / NDVI / parcel-profile monitoring stack (ICWD). Eco workflow artifacts live here; related STM / type-space scores live in stm.

## Status notes

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
