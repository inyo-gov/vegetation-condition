# ICM — Parcel profiles: remove Quarto TOC sidebar column

**Date:** 2026-10-01 PT  
**Lane:** Geo / flagged-parcel profile UX (Desk → Geo)  
**Status:** Shipped  
**Live:** https://icwd-vegetation-condition.vercel.app/docs/parcel_profiles.html#thibaut-sawmill--parcel-ind026  
**Prior:** `docs/2026-10-01_ind026-profile-map-fullwidth-layout.md` (full-viewport `.fp-map-band`)

---

## Goal

Get rid of the Quarto TOC / sidebar column on flagged parcel profiles so it does not eat horizontal width. Keep the full-viewport map band, Layers collapse, and Expand.

## Inputs

- Live QA: TOC/margin column still constrained the page after the map-band ship (`1156289`).
- Site CSS forced `#quarto-content.page-columns` to `1fr | 240–260px` with `#quarto-margin-sidebar` / `#TOC`.
- Baked TOC on this page was effectively one link (“Parcel profiles”) — not useful parcel nav.
- Shared sources: `parcel_profiles.qmd`, `styles.css`, baked `docs/parcel_profiles.html`.

## Process

1. Inspected `_quarto.yml` / `parcel_profiles.qmd` (`toc: true`, `toc-location: right`) and `styles.css` page-columns grid.
2. Set page front matter: `toc: false`, `page-layout: full`.
3. CSS: `#quarto-content.fp-no-toc-sidebar` → single-column main; hide margin sidebar.
4. Replaced sidebar TOC with in-flow **Jump to parcel** nav (`.fp-parcel-jump`) in main panel (qmd emit + baked HTML).
5. Baked HTML: removed `#quarto-margin-sidebar` DOM; added `fp-no-toc-sidebar`; injected CSS + jump nav.
6. Preserved `.fp-map-band`, Layers/Expand, `/www/flagged_parcel_maps/...` URLs.
7. Commit `inyo-gov/vegetation-condition` `main` + Vercel `--prod`.

## Outputs

- Source: `parcel_profiles.qmd`, `styles.css`
- Baked: `docs/parcel_profiles.html`
- ICM: this file
- Prod: icwd-vegetation-condition (see commit / deploy in ship note)

## Open gaps / residual risk

- Other site pages (`index`, `lpt_etl`, `methods`) still use global `toc: true` in `_quarto.yml` — intentional; only flagged profiles opted out.
- Full Quarto re-render of `parcel_profiles.qmd` still needed for a clean rebuild of jump-nav from live `profile_parcels` (baked nav lists current section anchors).
- `100vw` map band can still show a minor horizontal scrollbar on some browsers when a vertical scrollbar is present — monitor, not addressed here.
