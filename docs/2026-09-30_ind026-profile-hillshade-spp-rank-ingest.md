# ICM — IND026 profile ingest (hillshade + photo + decade spp-rank)

**Date:** 2026-09-30 (PT)  
**Status:** Built (IND026 MVP only; extendable)  
**Live:** https://icwd-vegetation-condition.vercel.app/docs/parcel_profiles.html#thibaut-sawmill--parcel-ind026  
**Deploy:** `dpl_GYRjMuVq9SdaXddCsFPdu3o9dUBZ` → aliased production  
**Framing:** Research / measurability context — **not** I.C.1.b attributability / Board policy.

## What landed

| Piece | Path / behavior |
|-------|-----------------|
| Static assets | `www/flagged_parcel_maps/IND026/` — preview PNG, context overlay PNG, WGS84 bounds JSON, GeoJSON overlays (boundary / adjoining / All_2026 starts / photo point), AGOL photo `IND026_05_212`, Eco `spp_rank/*.csv` |
| Profile helper | `code/R/flagged_parcel_context.R` — graceful skip if `www/flagged_parcel_maps/{PCL}/` missing |
| UI wire | `parcel_profiles.qmd` calls `emit_flagged_parcel_context(pid)` after screening cards |
| Map | Inline Leaflet **ImageOverlay** from preview PNG + bounds (no COG / georaster). Layers: yellow boundary, cyan adjoining, red transect starts, orange photo point. Fallback = context-overlay PNG |
| Photo | AGOL attachment `IND026_05_212` (2024-07-02) |
| Spp-rank | Shiny-like decade table: Baseline (1985) / 2000s / 2010s / 2026; **1990s omitted** (no LPT); earliest top-10 reorder callout **2005** |
| CSS | `styles.css` — `.flagged-parcel-context`, `.spp-rank-*` |

## Sources

- Geo package: `../lidar-data/data/processed/flagged_parcel_hillshade/IND026/` (2022 Sierra DTM; pad 300 m requested / W–N clipped) — ICM `docs/2026-09-30_flagged-parcel-hillshade-IND026.md`
- Eco ranks: `exports/spp_rank_mvp_2026-09-30/IND026_*.csv` (copied into `www/.../spp_rank/`)
- Plan / pivot: `docs/2026-09-30_flagged-parcel-hillshade-photo-spp-rank-plan.md`, `docs/2026-09-30_flagged-mvp-c-to-b-pivot.md`

## Guardrails

- Other flagged parcels unchanged (no assets → skip).
- No COG shipped in `www/` (PNG ImageOverlay path).
- Single-year 1985 baseline + pre-2015 vs permanent-network caveat called on the table.
- Commit scoped to IND026 ingest + profile wire (dirty tree not swept).

## Extend later

Drop the same folder layout under `www/flagged_parcel_maps/{PCL}/` for IND029 / TIN064 / … — helper auto-picks up.
