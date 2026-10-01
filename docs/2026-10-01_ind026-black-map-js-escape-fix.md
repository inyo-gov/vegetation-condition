# ICM — IND026 black map (JS escape) + two-column / expand

**Date:** 2026-10-01 (PT)  
**Repo:** inyo-gov/vegetation-condition  
**Live:** https://icwd-vegetation-condition.vercel.app/docs/parcel_profiles.html#thibaut-sawmill--parcel-ind026

## Problem
Production flagged-parcel LiDAR map for IND026 rendered as an empty black rectangle (`#1a1a1a`) with only a **Layers** button. Hillshade ImageOverlays never painted.

## Root cause
A prior surgical HTML edit for the collapse-Layers UX introduced **over-escaped** JS string quotes inside the embedded Leaflet IIFE, e.g.:

```js
document.getElementById(\'fp-map-IND026-…-layers-toggle\');
```

`node --check` failed with `SyntaxError: Invalid or unexpected token`. The map script never ran; the Layers button (plain HTML) still rendered.

The shared helper `code/R/flagged_parcel_context.R` already emitted normal `'…'` quotes via `paste0` — the breakage was in published `docs/parcel_profiles.html`, not the R generator.

## Fix
1. **Mandatory:** Replace over-escaped `\'` in the IND026 toggle IIFE with normal JS quotes; confirm `node --check` clean on the extracted IIFE.
2. Prefer **URL paths** under `/www/flagged_parcel_maps/IND026/...` for hillshade / CHM / height masks / photo (assets already 200 on Vercel) instead of multi-MB base64 — shrinks HTML and reduces fragility.
3. Helper hardening: `.fp_raster_uri()` prefers `/www/flagged_parcel_maps/...` when the file lives under `www/`; fall back to `knitr::image_uri`.
4. **Layout UX:** two-column grid (map | photo) via `.flagged-context-two-col` (≥960px); stacks on narrow viewports. **Expand** control toggles `.fp-map-expanded` (fixed overlay + `invalidateSize`).

## Files
- `code/R/flagged_parcel_context.R` — URI helper, expand control, two-col emit order
- `styles.css` — two-col + expanded map rules
- `docs/parcel_profiles.html` — surgical IND026 IIFE / CSS / layout patch (Quarto rebuild not required for this ship)

## Verify
- Extracted IIFE: `node --check` passes
- Live: hillshade paints; Layers toggle works; Expand grows map
- Do not merge stale zach-nelson fork; BWMA/wet-prior untouched
