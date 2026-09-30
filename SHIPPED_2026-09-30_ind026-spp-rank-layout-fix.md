# Vegetation-condition — 2026-09-30 PT

**Shipped:** IND026 decade spp-rank column bugfix + photo→full-width hillshade layout; Quarto render + Vercel prod.

- Bug: dplyr `period` arg shadowed column → all decade columns identical; fixed via base-R `period_key` subset.
- Layout: stacked photo first, full-width LiDAR hillshade, then ranks/callout (no sidebar).
- Live: https://icwd-vegetation-condition.vercel.app/docs/parcel_profiles.html#thibaut-sawmill--parcel-ind026
- ICM: `docs/2026-09-30_ind026-spp-rank-layout-fix.md`
- Confirm: Baseline SPAI-led → 2026 ARTR2/ATTO/ERNA10 shrub-led; columns differ.
- Deploy: `dpl_9EtpnruSjck3wTjWKo5FZeiSJeGc`
