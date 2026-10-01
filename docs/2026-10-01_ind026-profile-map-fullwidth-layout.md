# ICM — IND026 profile map full-width layout

**Date:** 2026-10-01 PT  
**Lane:** Geo / flagged-parcel profile UX  
**Status:** Shipped (shared helper + baked HTML + prod)  
**Live:** https://icwd-vegetation-condition.vercel.app/docs/parcel_profiles.html#thibaut-sawmill--parcel-ind026  
**Applies to:** All flagged parcels via `code/R/flagged_parcel_context.R` + `styles.css` (IND026 now)

---

## Problem

QA: IND026 map sat in a skinny left column; NMDS / NDVI residual margin dominated the row. Charts were squeezed beside a cramped map. Quarto/page sidebar + Tufte margin both stole width from the Leaflet map.

## Fix

1. **Full-viewport map band** — `.flagged-parcel-context.fp-map-band` uses `100vw` content-bleed (not trapped beside TOC or parcel margin).
2. **Map first** — `emit_flagged_parcel_context()` runs before `.parcel-profile-layout` so the map is a full-width band above cards/figs.
3. **Photo under map** — drop desktop `1.55fr | 0.85fr` map|photo grid; stack map full width, photo below (max ~56rem).
4. **Diagnostics full width** — `.parcel-profile-layout` is a column flex stack; `.parcel-profile-margin` / `.parcel-profile-diagnostics` is a full-width `auto-fit` grid for NMDS + residual (no sticky skinny sidebar).
5. **Heights** — map min-height 520 / 640 / 720px by breakpoint; Layers collapse + Expand unchanged; hillshade URLs stay `/www/flagged_parcel_maps/...` (no `\'` SyntaxError).

## Files

- `code/R/flagged_parcel_context.R` — `fp-map-band` class; stacked map/photo; height breakpoints
- `styles.css` — layout + bleed + diagnostics grid
- `parcel_profiles.qmd` — emit map before layout; diagnostics aside class
- `docs/parcel_profiles.html` — baked CSS + IND026 DOM reorder

## Verify

1. Open IND026 anchor on prod.
2. Map spans substantial page/viewport width (not a skinny sidebar strip).
3. NMDS + residual sit full-width below (side-by-side when both present).
4. Layers / Expand still work; hillshade paints (not black).
