# ICM — IND026 SAM × LPT research overlay on parcel profile

**Date:** 2026-09-30 PT  
**Lane:** desk (Geo Geraldine executor)  
**Status:** **Built / wired to profile / denser-v2 refresh**  
**Live anchor:** https://icwd-vegetation-condition.vercel.app/docs/parcel_profiles.html#thibaut-sawmill--parcel-ind026  
**Research overlay only** — not vegetation TYPE; not I.C.1.b attributability.

---

## Goal

Show IND026 NAIP 2022 SAM segments with Eco LPT 2022 lifeform labels on the existing flagged-parcel hillshade Leaflet frame (photo → full-width map → spp-rank stack), without inventing TYPE / I.C.1.b language. Keep BWMA-style layer toggles (hillshade / CHM / shrub / tree / SAM).

---

## Inputs (verified — Eco denser-v2)

| Asset | Path |
|-------|------|
| Eco labeled GeoJSON (n=12) | `/Users/zac/workspace/vegetation-condition/exports/naip_segment_lpt_2026-09-30/IND026_labeled_segments_wgs84.geojson` |
| Training labels CSV | `…/exports/naip_segment_lpt_2026-09-30/IND026_segment_training_labels.csv` |
| Hit→segment join | `…/exports/naip_segment_lpt_2026-09-30/IND026_hit_segment_join.csv` |
| Join summary (`sam_version=v2`) | `…/exports/naip_segment_lpt_2026-09-30/IND026_join_summary.json` |
| Eco join ICM | `docs/2026-09-30_naip-segment-lpt-join-IND026.md` |
| Geo denser SAM v2 WGS84 (n=495) | `/Users/zac/workspace/lidar-data/data/processed/flagged_parcel_hillshade/IND026/heterogeneity/segments/IND026_naip2022_sam_segments_v2_wgs84.geojson` |
| Parent hillshade frame | `www/flagged_parcel_maps/IND026/` (unchanged adjoining / photo / ranks / height) |

---

## Product paths (committed / served)

Simplified web GeoJSON under the site asset tree (Quarto `embed-resources` inlines into `docs/parcel_profiles.html`):

| Asset | Absolute path |
|-------|----------------|
| Labeled segments (n=12, simplified ~2.5 m) | `/Users/zac/workspace/vegetation-condition/www/flagged_parcel_maps/IND026/heterogeneity/IND026_labeled_segments_web_wgs84.geojson` |
| All SAM outlines (n=495, simplified ~8 m) | `/Users/zac/workspace/vegetation-condition/www/flagged_parcel_maps/IND026/heterogeneity/IND026_sam_outlines_web_wgs84.geojson` |
| Overlay meta | `/Users/zac/workspace/vegetation-condition/www/flagged_parcel_maps/IND026/heterogeneity/research_overlay_meta.json` |
| Labels / join CSVs (copy) | `…/heterogeneity/IND026_segment_training_labels.csv`, `IND026_hit_segment_join.csv` |
| Helper | `code/R/flagged_parcel_context.R` (`.fp_inline_leaflet_html` loads `heterogeneity/` when present) |
| Styles | `styles.css` (`.fp-research-overlay-note`, legend `.lg-sam` / `.lg-lab`) |

---

## UI behavior

- Same IND026 Leaflet ImageOverlay hillshade + boundary / adjoining / transect starts / photo point.
- **BWMA-style Layers panel** (top-right): hillshade / CHM / small-shrub / tree / SAM outlines / SAM labeled (defaults unchanged — labeled ON, outlines OFF).
- **Faint** gray outlines for all 495 SAM polys (`residual_matrix` dashed).
- **Labeled** polys (n=12, all `sam_amg`) filled by `shrub_abs` (brown scale).
- Popup / tooltip: `segment_id`, `shrub_abs`, `top_spp` (SPAI / ARTR2 / ATTO / ERNA10 ≥1%), `n_hits`, `mask_origin`.
- Caption badge: **Research overlay** — NAIP 2022 SAM × LPT 2022; not TYPE / not I.C.1.b.
- Caveat (denser-v2): **Residual gap closed — 16/16 LPT starts in sam_amg; 12 labeled AMG segments (Eco denser-v2).**

---

## Related

- Join ICM: `docs/2026-09-30_naip-segment-lpt-join-IND026.md`
- Geo SAM MVP: `docs/2026-09-30_ind026-within-parcel-heterogeneity-mvp.md`
- BWMA layer control: `docs/2026-09-30_ind026-bwma-style-layer-control.md`
- Profile ingest: `docs/2026-09-30_ind026-profile-hillshade-spp-rank-ingest.md`

---

## Explicit non-goals

- No Green Book TYPE language.
- No I.C.1.b attributability claim.
- No change to BWMA height-bin thresholds or layer-panel UX.

---

## History

### v1 ship (2026-09-30 ~17:02 PT)

Labeled n=4 + outlines n=209; caveat 12/16 LPT in residual_matrix seg 204.

### Geo denser v2 segments (2026-09-30 18:08 PDT)

Canonical + versioned Eco-keyed segments under `lidar-data/.../heterogeneity/segments/` (`IND026_naip2022_sam_segments.gpkg` / `_v2*`). **16/16** starts in `sam_amg`, **12** unique AMG segs.

### Eco re-join + web overlay refresh (2026-09-30 ~18:15 PT)

Eco drop-in exports (`sam_version=v2`) → www labeled n=12 + outlines n=495; meta note updated; parcel_profiles re-rendered; Vercel prod redeploy.
