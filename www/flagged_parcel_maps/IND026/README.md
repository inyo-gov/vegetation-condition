# IND026 flagged-parcel profile assets

Static ingest for I.C.1.a parcel profile (research / measurability framing — not I.C.1.b).

| Asset | Role |
|-------|------|
| `IND026_hillshade_preview.png` | Leaflet ImageOverlay (0.5 m hillshade) |
| `IND026_hillshade_context_overlay.png` | Static fallback with overlays burned in |
| `hillshade_bounds.json` | WGS84 bounds + epoch / pad metadata |
| `overlays/*.geojson` | Boundary, adjoining parcels, All_2026 transect starts, photo point |
| `photo/IND026_05_212_agol.jpg` | Representative AGOL photo (`IND026_05_212`) |
| `spp_rank/decade_ranks.csv` | Decade perennial relative ranks (Eco MVP) |
| `spp_rank/rank_delta.csv` | Rank-delta / earliest reorder (2005) |

Source geo package: `lidar-data/data/processed/flagged_parcel_hillshade/IND026/` (2022 Sierra DTM).  
Species ranks: `exports/spp_rank_mvp_2026-09-30/IND026_*.csv`.

## Layer panel (BWMA-preview style)

Helper `code/R/flagged_parcel_context.R` → `.fp_inline_leaflet_html` emits a floating checkbox panel + `L.control.layers` fallback.

| `height/IND026_chm_height_strata_preview.png` | CHM height strata (BWMA 1–4 palette) |
| `height/IND026_small_shrub_mask.png` | Small-shrub mask 0.3–3.0 m |
| `height/IND026_tree_mask.png` | Tree mask ≥3.0 m |
| `height/height_layers_meta.json` | Thresholds + counts |

**Thresholds (BWMA liberal_v1, site-consistent):** small-shrub **0.3–3.0 m**; tree **≥3.0 m**. Strata: 1 &lt;0.5 · 2 0.5–1.5 · 3 1.5–3 · 4 &gt;3 m.  
CHM source: `lidar-data/.../ind026_ind029_2022/ind026_ind029_2022_chm_0p5m_spikefiltered.tif` clipped to hillshade frame. Build: `lidar-data/scripts/build_ind026_chm_height_layers.py`.

| `heterogeneity/IND026_labeled_segments_web_wgs84.geojson` | Research overlay: LPT-labeled SAM segments (n=4; simplified) |
| `heterogeneity/IND026_sam_outlines_web_wgs84.geojson` | Faint outlines of all SAM polys (n=209; simplified) |
| `heterogeneity/research_overlay_meta.json` | Overlay meta + residual_matrix training note |

**Research overlay only** — not vegetation TYPE; not I.C.1.b. Pattern reusable for IND029 / TIN064 when `height/` + optional `heterogeneity/` assets exist.
