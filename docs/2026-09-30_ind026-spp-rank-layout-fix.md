# ICM — IND026 spp-rank columns + photo/hillshade layout fix

**Date:** 2026-09-30 (PT)  
**Status:** Fixed + redeployed  
**Deploy:** `dpl_9EtpnruSjck3wTjWKo5FZeiSJeGc` → aliased production  
**Live:** https://icwd-vegetation-condition.vercel.app/docs/parcel_profiles.html#thibaut-sawmill--parcel-ind026  
**Framing:** Research / measurability context — **not** I.C.1.b attributability / Board policy.

## Bug

Decade species-rank table repeated the **same** ranks/cover in Baseline / 2000s / 2010s / 2026. Cause: `fmt_rank_cell(code, period)` used dplyr `.data$period == period`, so the function arg was shadowed by the column and `slice(1)` always took the first period row for that species.

## Fix

- Base-R subset on `period_key` (no dplyr name clash) in `code/R/flagged_parcel_context.R`.
- Layout: **photo first**, then **full-width LiDAR hillshade** (no sidebar / no shared-width photo+map row); decade ranks + earliest-reorder callout (2005) retained. CSS stack tuned for iPad/iPhone.

## IND026 verify (columns differ)

| Period | Top ranks (perennial rel.) |
|--------|----------------------------|
| Baseline 1985 | **SPAI** 1 @ 49.00% |
| 2000s | ATTO 1, ARTR2 2, ERNA10 3, SPAI 4 |
| 2010s | ATTO 1, ARTR2 2, ERNA10 3, SPAI 4 |
| 2026 | **ARTR2** 1 @ 17.12%, ATTO 2, ERNA10 3, SPAI 4 (shrub-led) |

Earliest top-10 reorder vs baseline: **2005**.
