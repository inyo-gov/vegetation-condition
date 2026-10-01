# SHIPPED — IND026 BWMA-style layer control + CHM bins

**Date:** 2026-09-30 PT  
**Live:** https://icwd-vegetation-condition.vercel.app/docs/parcel_profiles.html#thibaut-sawmill--parcel-ind026  
**ICM:** `docs/2026-09-30_ind026-bwma-style-layer-control.md`

## Layers (panel toggles)

Rasters: Hillshade · CHM height strata · Small-shrub (0.3–3 m) · Tree (≥3 m)  
Vectors: Parcel boundary · Adjoining · Transect starts · Photo point · SAM outlines · SAM labeled

## Thresholds

BWMA liberal_v1 site-consistent: small-shrub **0.3–3.0 m**; tree **≥3.0 m**; strata 1–4 as BWMA.

## Key paths

- Helper: `code/R/flagged_parcel_context.R`
- Assets: `www/flagged_parcel_maps/IND026/height/`
- Build: `lidar-data/scripts/build_ind026_chm_height_layers.py`
