# ICM — IND026 map layout UX fix (wider/taller, collapsed panel, better zoom)

**Date:** 2026-10-01 PT  
**Lane:** QA response (Zac request)  
**Status:** **Fixed in shared helper**  
**Live anchor:** https://icwd-vegetation-condition.vercel.app/docs/parcel_profiles.html#thibaut-sawmill--parcel-ind026  
**Applies to:** All flagged parcels using `flagged_parcel_context.R` (IND026 + future profiles)

---

## Problem (Zac QA)

Screenshot showed three UX issues with IND026 LiDAR map:

1. **Map cramped** — too short vs right-column NMDS/residuals charts; competing for vertical space
2. **Layers panel covering map** — panel open by default, eating ~50% of usable map area
3. **Zoom too tight** — fitBounds padding minimal; map zoomed to parcel-only instead of comfortable hillshade frame

---

## Fix

### 1. Map wider and taller

- **Before:** 560px fixed height
- **After:** 680px (mobile), 720px (desktop ≥768px)
- **Rationale:** More vertical space reduces cramped feel vs two-column layouts (map left, charts right); hillshade + CHM context visible without scrolling

**Files:**
- `code/R/flagged_parcel_context.R` — inline CSS: `height: 680px` + `@media (min-width: 768px) { height: 720px; }`
- `styles.css` — `.flagged-context-map` containers: `min-height: 680px` / `720px` with matching breakpoint

---

### 2. Layers panel collapsed by default

- **Before:** Panel always visible (top-right), covering ~250px × full height
- **After:** Panel hidden on load; toggle button ("Layers") in top-right corner expands panel when clicked
- **Behavior:**
  - Button text: "Layers" (collapsed) → "✕" (expanded)
  - CSS transition: smooth slide-out/fade (`.fp-collapsed` class)
  - Panel z-index above map (1000), button above panel (1001)
  - Click outside or re-click button to collapse
  - Semantic HTML: `<button>` with `aria-expanded` for accessibility

**Files:**
- `code/R/flagged_parcel_context.R`:
  - Added `.fp-panel-toggle` button styles (CSS)
  - Added `<button id="{map_id}-toggle">` before panel div
  - Panel starts with `.fp-collapsed` class
  - JavaScript: toggle click handler (add/remove class, update aria, change button text)

---

### 3. Better default zoom (hillshade extent, not tight parcel)

- **Before:** `map.fitBounds(bounds, {padding:[12,12]});` — minimal padding; map zoomed tight to image bounds
- **After:** `map.fitBounds(bounds, {padding:[40,40]});` — generous padding; hillshade fills view comfortably
- **Rationale:** 12px padding felt parcel-tight; 40px gives breathing room so hillshade context (adjoining parcels, full transect spread) is visible without panning

**File:** `code/R/flagged_parcel_context.R` — JavaScript: updated fitBounds padding

---

## Testing

1. Rebuild site: `quarto render parcel_profiles.qmd` (from project root)
2. Navigate to IND026: `/docs/parcel_profiles.html#thibaut-sawmill--parcel-ind026`
3. Verify:
   - Map taller (680px / 720px) — less cramped vs NMDS/residuals
   - Layers panel hidden on load; only "Layers" button visible top-right
   - Click "Layers" → panel slides out; click "✕" or button again → collapses
   - Default zoom shows full hillshade extent with comfortable padding (not tight parcel-only)

---

## Scope

**Shared helper:** `code/R/flagged_parcel_context.R` (`.fp_inline_leaflet_html` function)  
**Applies to:** All flagged parcels with `www/flagged_parcel_maps/{PCL}/` assets (IND026 now; IND029 / TIN064 future)  
**No IND026-only hacks:** Changes are in the reusable map generator; future profiles get the same UX automatically.

---

## Commit

**SHA:** `07ff7de`  
**Branch:** `cursor/ind026-map-layout-fix-fa31`  
**PR:** https://github.com/zach-nelson/vegetation-condition/pull/1  
**Message:**
```
fix: IND026 map layout - wider/taller map, collapsed layers panel, better zoom

Fixes three UI issues with flagged parcel LiDAR maps:
1. Map wider and taller - 680px (mobile) / 720px (desktop)
2. Layers panel collapsed by default with toggle button
3. Better default zoom - fitBounds padding [40,40] for hillshade extent

Applies to all flagged parcels using shared helper (IND026 + future).
```

---

## Non-goals

- No layer functionality changes (CHM / SAM / height bins unchanged)
- No mobile-only overrides (responsive breakpoint at 768px is sufficient)
- No removal of fallback `L.control.layers` (bottom-left; kept for keyboard/mobile accessibility)
