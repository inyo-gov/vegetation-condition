# ICM — IND026 BWMA-style Leaflet layer control + CHM height bins

**Date:** 2026-09-30 PT  
**Lane:** desk (Geo Geraldine executor)  
**Status:** **Built / wired to profile**  
**Live anchor:** https://icwd-vegetation-condition.vercel.app/docs/parcel_profiles.html#thibaut-sawmill--parcel-ind026  
**Research / measurability framing only** — not vegetation TYPE; not I.C.1.b attributability.

---

## Goal

Make the IND026 flagged-parcel Leaflet map less click-cumbersome: BWMA-preview-style checkbox layer panel so Zac can toggle hillshade / CHM / small-shrub / tree / SAM without hunting. Pattern reusable for IND029 / TIN064 when assets land under `www/flagged_parcel_maps/{PCL}/`.

---

## Thresholds (BWMA liberal_v1, site-consistent)

| Class | Height (m) | UI label |
|-------|------------|----------|
| Small-shrub | **0.3 – 3.0** | Small-shrub (dryland label; BWMA “emergent” bin) |
| Tree | **≥ 3.0** | Tree |
| CHM strata | 1 &lt;0.5 · 2 0.5–1.5 · 3 1.5–3 · 4 &gt;3 | CHM height strata |

Source CHM: `lidar-data/data/processed/ind026_ind029_2022/ind026_ind029_2022_chm_0p5m_spikefiltered.tif` clipped to IND026 hillshade frame (`EPSG:6340`, 0.5 m).

---

## Products

### Desk (lidar-data)

| Asset | Path |
|-------|------|
| Build script | `/Users/zac/workspace/lidar-data/scripts/build_ind026_chm_height_layers.py` |
| Frame CHM + vectors + PNGs | `/Users/zac/workspace/lidar-data/data/processed/flagged_parcel_hillshade/IND026/height/` |

### Web embed (vegetation-condition)

| Asset | Absolute path |
|-------|----------------|
| CHM strata PNG | `/Users/zac/workspace/vegetation-condition/www/flagged_parcel_maps/IND026/height/IND026_chm_height_strata_preview.png` |
| Small-shrub mask PNG | `…/height/IND026_small_shrub_mask.png` |
| Tree mask PNG | `…/height/IND026_tree_mask.png` |
| Meta | `…/height/height_layers_meta.json` |
| Helper | `/Users/zac/workspace/vegetation-condition/code/R/flagged_parcel_context.R` (`.fp_inline_leaflet_html`) |
| Styles | `styles.css` (`.lg-chm` / `.lg-shrub` / `.lg-tree`) |

Did **not** overwrite denser SAM segment work under `heterogeneity/` — layer UX reuses current SAM geojsons.

---

## UI behavior

- Floating **Layers** panel (top-right) matching BWMA checkbox UX: Rasters + Vectors.
- Defaults ON: hillshade, boundary, adjoining, transect starts, photo, **SAM labeled**.
- Defaults OFF: CHM strata, small-shrub, tree, **SAM outlines**.
- Collapsed `L.control.layers` also present (bottom-left) as keyboard/mobile fallback.
- Reusable: any parcel with `www/flagged_parcel_maps/{PCL}/height/` (+ optional `heterogeneity/`) gets the same panel.

---

## Explicit non-goals

- No Green Book TYPE language / I.C.1.b claim.
- No denser-AMG SAM retune (parallel lane).
- IND029 / TIN064 height assets not built in this ship (helper is ready).
