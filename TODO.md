# TODO — vegetation-condition

- [x] Flagged-parcel context panel MVP **IND026** (hillshade ImageOverlay + photo + decade spp-rank) — assets `www/flagged_parcel_maps/IND026/`; helper `code/R/flagged_parcel_context.R`; ICM `docs/2026-09-30_ind026-profile-hillshade-spp-rank-ingest.md`; LAW052 remains method/geo reference

- [x] 2026-09-30 May NDVI + LPT lifeform prior prototype (LAW052, FSL044) — research draft
- [x] Ingest / build May (Apr20–Jun10) Landsat composites for 2025–2026 if scenes become available; do not substitute Jul–Sep NDVI_SUR (pilots LAW052/FSL044 from ee-tools 2026-09-30)
- [ ] Optional: re-fit with brms/rstan hierarchical model if/when stack is intentionally installed
- [ ] Parked science: spring inventory season vs Jul–Sep NDVI aggregation — document, do not silently conflate

- [ ] Build sibling `rs_parcels` (keyed by PCL) from veg parcels with material water/canal/gravel clips per `docs/2026-09-30_rs-parcel-layer.md` — do not edit legal veg shapefile
- [ ] FSL044: erase gravel stub (Gravel 1+2, 10.83%) into `rs_parcels`; do not use Fish Slough stream label; Owens 30 m buffer does not reach
- [ ] Add valley-wide roads layer (local county/TIGER extract) then road-buffer erase — do not ad hoc download huge dataset
- [ ] Parked: higher-res cells inside parcels with Bayesian-style vegetation community attribution (do not build yet)

- [x] 2026-09-30 Flagged-parcel spp-rank method sketch + LAW052 decade prototype (research draft) — plan `docs/2026-09-30_flagged-parcel-hillshade-photo-spp-rank-plan.md`; exports `exports/spp_rank_mvp_2026-09-30/`
- [x] 2026-09-30 IND026 decade spp-rank prototype (research draft) — exports `exports/spp_rank_mvp_2026-09-30/IND026_*.csv`; script `code/run_spp_rank_decade_IND026_2026-09-30.R`; LAW052_* left as method demo; pivot `docs/2026-09-30_flagged-mvp-c-to-b-pivot.md`
- [x] Profile UI decade rank table — IND026 live on parcel_profiles; extend by adding `www/flagged_parcel_maps/{PCL}/`
- [ ] Optional later: forcing co-parcel note when grazing/fire parcel joins exist — still not I.C.1.b

- [ ] Transition-first hillshade packages still queued for **IND029 → TIN064** (geo); IND026 profile ingest UI done; LAW052 remains reference package; see pivot + ingest ICM
