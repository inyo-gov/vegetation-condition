# TODO — vegetation-condition

- [ ] Flagged-parcel context panel (hillshade+boundary+transects+1 photo) + decade spp-rank table — plan `docs/2026-09-30_flagged-parcel-hillshade-photo-spp-rank-plan.md`; MVP LAW052; **no UI until Geo/Eco assets**

- [x] 2026-09-30 May NDVI + LPT lifeform prior prototype (LAW052, FSL044) — research draft
- [x] Ingest / build May (Apr20–Jun10) Landsat composites for 2025–2026 if scenes become available; do not substitute Jul–Sep NDVI_SUR (pilots LAW052/FSL044 from ee-tools 2026-09-30)
- [ ] Optional: re-fit with brms/rstan hierarchical model if/when stack is intentionally installed
- [ ] Parked science: spring inventory season vs Jul–Sep NDVI aggregation — document, do not silently conflate

- [ ] Build sibling `rs_parcels` (keyed by PCL) from veg parcels with material water/canal/gravel clips per `docs/2026-09-30_rs-parcel-layer.md` — do not edit legal veg shapefile
- [ ] FSL044: erase gravel stub (Gravel 1+2, 10.83%) into `rs_parcels`; do not use Fish Slough stream label; Owens 30 m buffer does not reach
- [ ] Add valley-wide roads layer (local county/TIGER extract) then road-buffer erase — do not ad hoc download huge dataset
- [ ] Parked: higher-res cells inside parcels with Bayesian-style vegetation community attribution (do not build yet)
