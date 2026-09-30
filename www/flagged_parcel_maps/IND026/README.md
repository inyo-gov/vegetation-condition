# IND026 flagged-parcel profile assets

Static ingest for I.C.1.a parcel profile (research / measurability framing — not I.C.1.b).

| Asset | Role |
|-------|------|
| `IND026_hillshade_preview.png` | Leaflet ImageOverlay (0.5 m hillshade) |
| `IND026_hillshade_context_overlay.png` | Static fallback with overlays burned in |
| `hillshade_bounds.json` | WGS84 bounds + epoch / pad metadata |
| `overlays/*.geojson` | Boundary, adjoining parcels, All_2026 transect starts, photo point |
| `photo/IND026_05_212_agol.jpg` | Representative AGOL photo (`IND026_05_212`) |
| `spp_rank/decade_ranks.csv` | Decade perennial relative ranks (Eco MVP) |
| `spp_rank/rank_delta.csv` | Rank-delta / earliest reorder (2005) |

Source geo package: `lidar-data/data/processed/flagged_parcel_hillshade/IND026/` (2022 Sierra DTM).  
Species ranks: `exports/spp_rank_mvp_2026-09-30/IND026_*.csv`.

No COG shipped here — profile uses PNG ImageOverlay (BWMA-style simpler path).
