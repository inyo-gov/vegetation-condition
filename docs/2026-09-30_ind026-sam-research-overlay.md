# ICM — IND026 SAM × LPT research overlay on parcel profile

**Date:** 2026-09-30 PT  
**Lane:** desk (Geo Geraldine executor)  
**Status:** **Built / wired to profile**  
**Live anchor:** https://icwd-vegetation-condition.vercel.app/docs/parcel_profiles.html#thibaut-sawmill--parcel-ind026  
**Research overlay only** — not vegetation TYPE; not I.C.1.b attributability.

---

## Goal

Show IND026 NAIP 2022 SAM segments with Eco LPT 2022 lifeform labels on the existing flagged-parcel hillshade Leaflet frame (photo → full-width map → spp-rank stack), without inventing TYPE / I.C.1.b language.

---

## Inputs (verified)

| Asset | Path |
|-------|------|
| Eco labeled GeoJSON | `/Users/zac/workspace/vegetation-condition/exports/naip_segment_lpt_2026-09-30/IND026_labeled_segments_wgs84.geojson` |
| Training labels CSV | `…/exports/naip_segment_lpt_2026-09-30/IND026_segment_training_labels.csv` |
| Hit→segment join | `…/exports/naip_segment_lpt_2026-09-30/IND026_hit_segment_join.csv` |
| Eco join ICM | `docs/2026-09-30_naip-segment-lpt-join-IND026.md` |
| Geo all-SAM WGS84 | `/Users/zac/workspace/lidar-data/data/processed/flagged_parcel_hillshade/IND026/heterogeneity/segments/IND026_naip2022_sam_segments_wgs84.geojson` |
| Parent hillshade frame | `www/flagged_parcel_maps/IND026/` (unchanged adjoining / photo / ranks) |

---

## Product paths (committed / served)

Simplified web GeoJSON under the site asset tree (Quarto `embed-resources` inlines into `docs/parcel_profiles.html`):

| Asset | Absolute path |
|-------|----------------|
| Labeled segments (n=4, simplified) | `/Users/zac/workspace/vegetation-condition/www/flagged_parcel_maps/IND026/heterogeneity/IND026_labeled_segments_web_wgs84.geojson` |
| All SAM outlines (n=209, simplified) | `/Users/zac/workspace/vegetation-condition/www/flagged_parcel_maps/IND026/heterogeneity/IND026_sam_outlines_web_wgs84.geojson` |
| Overlay meta | `/Users/zac/workspace/vegetation-condition/www/flagged_parcel_maps/IND026/heterogeneity/research_overlay_meta.json` |
| Labels / join CSVs (copy) | `…/heterogeneity/IND026_segment_training_labels.csv`, `IND026_hit_segment_join.csv` |
| Helper | `code/R/flagged_parcel_context.R` (`.fp_inline_leaflet_html` loads `heterogeneity/` when present) |
| Styles | `styles.css` (`.fp-research-overlay-note`, legend `.lg-sam` / `.lg-lab`) |

Geometry simplify: shapely `preserve_topology` (~2–5 m) so embed size stays ~75 KB labeled + ~134 KB outlines (full sources were ~1.3 MB / ~3.2 MB).

---

## UI behavior

- Same IND026 Leaflet ImageOverlay hillshade + boundary / adjoining / transect starts / photo point.
- **Faint** gray outlines for all 209 SAM polys (`residual_matrix` dashed).
- **Labeled** polys (n=4) filled by `shrub_abs` (brown scale); residual seg **204** dashed / lower opacity.
- Popup / tooltip: `segment_id`, `shrub_abs`, `top_spp` (SPAI / ARTR2 / ATTO / ERNA10 ≥1%), `n_hits`, `mask_origin`.
- Caption badge: **Research overlay** — NAIP 2022 SAM × LPT 2022; not TYPE / not I.C.1.b.
- Caveat: **12/16 LPT starts in residual_matrix seg 204** — training sparse until denser AMG (no denser-AMG retune in this ship).

---

## Related

- Join ICM: `docs/2026-09-30_naip-segment-lpt-join-IND026.md`
- Geo SAM MVP: `docs/2026-09-30_ind026-within-parcel-heterogeneity-mvp.md`
- Profile ingest: `docs/2026-09-30_ind026-profile-hillshade-spp-rank-ingest.md`

---

## Explicit non-goals

- No Green Book TYPE language.
- No I.C.1.b attributability claim.
- No denser SAM AMG retune / planar dissolve (optional Geo follow-up remains on TODO).
