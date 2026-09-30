# ICM plan — Flagged-parcel hillshade + photo + species-rank decades

**Date:** 2026-09-30 (PT)  
**Status:** Plan only — **do not implement UI yet**  
**Owning repo:** `vegetation-condition` (desk / I.C.1.a profiles)  
**Related tracks (separate):**
- BWMA hillshade quality bar: https://bwma-lidar-preview.vercel.app (`lidar-data` / Geo Geraldine)
- Mid-aggressive emergent CHM retune for BWMA (Geo Geraldine — do not fold into this plan)

**Ask (Zac, 2026-09-30):**
1. Continue BWMA-style hillshade for **background measurability / flagged parcel profiles**: hillshade (or hybrid) clipped/framed to each flagged parcel; parcel boundary; optional transect locations; one AGOL photo point to round out the profile.
2. Species table year comparisons (Shiny-app-like): rankings across baseline vs 1990s / 2000s / 2010s; identify when ranking shifted (earliest dated survey, or infer via forcing + co-parcel response).

---

## Goal

Enrich I.C.1.a flagged parcel profiles with (a) a **parcel-framed topographic context panel** (BWMA hillshade bar) and (b) a **decade species-rank comparison table** so staff can see composition reorder timing without leaving the desk. Measurability-only framing — not attributability conclusions.

---

## Inputs (inventory)

### A) Flagged parcel list + profile UI

| Item | Location |
|------|----------|
| **Live desk** | https://icwd-vegetation-condition.vercel.app — [Flagged parcels](parcel_profiles.qmd) |
| **Profile page** | `parcel_profiles.qmd` → `docs/parcel_profiles.html` |
| **Selection rule** | Wellfield parcels with `Cover_sig.counter > 5` **or** `Grass_sig.counter > 5` (`deltas_ttest_att`) |
| **Valley map (not per-parcel)** | `index.qmd` tmap view — red outline = flagged; no per-parcel hillshade today |
| **Flagged index (2026)** | `output/nmds_flagged_parcels_index.csv` (**n = 13**) |
| **Percentile summary** | `output/parcel_percentile_summary_flagged.csv` |
| **IC1b chronic sync (broader)** | `../green-book-IC1b/data/chronic_parcels_sync.csv` (**16** + BLK094 ref; includes TIN050/053/BLK075 not in 2026 profile set) |
| **Procedure** | `config/measurability_procedure.yaml` |
| **Parcel polygons** | `data/gisdata/allvegparcels_NAD83.shp`; app geojson `../inyoShiny/data/parcels.geojson` |

**2026 flagged IDs (profiles):**  
`BLK142`, `FSL044`, `FSL054`, `FSP006`, `IND021`, `IND026`, `IND029`, `IND139`, `LAW035`, `LAW043`, `LAW052`, `LAW082`, `TIN064`

**No UI implement in this pass** — profiles today are percentile / NMDS / cover / NDVI / DTW cards; **no** hillshade widget, transect overlay, or photo embed.

### B) LiDAR / hillshade coverage (vs BWMA bar)

| Source | What exists |
|--------|-------------|
| **Quality bar** | `lidar-data/data/processed/bwma_2024_topo/` — hybrid FEMAR+Sierra DTM + `exports/bwma_hybrid_hillshade_1m.tif`; Vercel Leaflet+COG preview |
| **Chronic catalog** | `lidar-data/data/catalog/chronic_lidar_coverage.csv` |
| **Laws mosaic (2024 FEMAR)** | `lidar-data/data/processed/laws_2024/` — `laws_2024_dtm_0p5m.tif` (+ DSM/CHM); site previews; hillshade used mainly as **REM QA overlays**, not parcel-profile COGs |
| **TA mosaic (2024)** | `lidar-data/data/processed/ta_2024/` — BLK142, TIN050/053/064 |
| **Independence 2022 Sierra** | `lidar-data/data/processed/ind026_ind029_2022/` |

**Coverage vs 13 flagged:**

| Status | Parcels |
|--------|---------|
| **DTM on disk (usable for hillshade clip)** | BLK142, FSL044, FSL054, LAW035, LAW043, LAW052, LAW082, TIN064, IND026, IND029 |
| **Catalog ready, extract not built** | **IND021**, **IND139** (2024 FEMAR); **FSP006** (2022 Sierra seam) |
| **IC1b-only / not in 2026 profiles** | TIN050/053 processed; BLK075 ready_2022_seam |

**Gaps vs BWMA:** no per-flagged-parcel clipped hybrid hillshade COG; no Leaflet profile widget; Laws/TA hillshade not packaged like `bwma_hybrid_hillshade_1m`; Sierra-seam parcels need epoch callout (2022 vs 2024 FEMAR).

### C) Transects + AGOL photo points

| Asset | Path / URL |
|-------|------------|
| **2026 start points + attachments** | `https://services.arcgis.com/0jRlQ17Qmni5zEMr/arcgis/rest/services/All_2026_Startpoints_view/FeatureServer/3` — `line-point-photos/download_photos_2026.py` |
| **Legacy layer** | `…/lpt_points_view/FeatureServer/0` — `download_photos.py` |
| **IDENT format** | `Parcel_Transect_Bearing` (+ optional `_15/_20/_92` extras) — see `line-point-photos/README.md` |
| **Labeled local photos** | `line-point-photos/labeled_photos/{year}/` (e.g. LAW052 has 6 labeled 2026 JPGs) |
| **Gallery / schema** | `line-point-photos` Quarto site; `data/showcase-transects.yaml` |
| **Transect registry (migration)** | `open-gis-migration/docs/transect-registry-and-observation-qa.md` |
| **Auth** | Public **views** preferred; org layers need `ARCGIS_USERNAME` / `ARCGIS_PASSWORD` fallback |

### D) Species composition / Shiny precedents

| Asset | Path |
|-------|------|
| **Shiny species table (year columns)** | `inyoShiny/inyoShiny.R` → `species_table_years` — hard-coded years `1985,1986,1987,1992,2020,2024,2025`; pivot CommonName × Year; DT export. Live: https://inyo.shinyapps.io/inyoShiny/ |
| **App species CSV** | `data/parcel_species_cover_app_2026.csv` (Parcel, Year, Species, Code, CommonName, parcelMeanCover) |
| **Dominants / IndVal** | `output/indicators_dominants_2026.csv`; `code/R/measurability_composition.R` |
| **Headline palette** | `config/headline_species.yaml` |
| **NMDS paths** | `output/figures/nmds_parcels/{PCL}_nmds.png` |
| **Decade rank UI** | **Not built** in vegetation-condition profiles; Shiny is closest precedent (selected years, not decade bins / rank-shift detection) |

---

## Process (proposed — after plan sign-off)

1. **Geo (Geraldine):** For MVP parcel, clip FEMAR (or Sierra) DTM → `gdaldem hillshade` (BWMA params: z=2, az=315, alt=45) → optional COG; overlay parcel boundary + transect start points from AGOL view; export PNG + optional web COG.
2. **Desk:** Embed static PNG (Quarto) first; Leaflet+COG later only if BWMA preview pattern is reused on purpose.
3. **Photo:** Pick one recent labeled start-point photo (prefer odd/even schedule match); cite IDENT + visit date; do not bulk-host AGOL in repo.
4. **Eco (Ernie) + desk:** Build decade rank table from `parcel_species_cover_app_*` (or LPT master perennial subset); document rank-shift rule below; optional forcing note is **descriptive**, not I.C.1.b.

---

## Outputs

| Deliverable | Target |
|-------------|--------|
| This plan | `docs/2026-09-30_flagged-parcel-hillshade-photo-spp-rank-plan.md` |
| MVP static profile add-on (later) | e.g. `exports/flagged_parcel_context_{PCL}/` — hillshade PNG, transect geojson clip, 1 photo copy or URL, decade rank CSV/HTML fragment |
| Optional scale-out | One panel per flagged parcel with LiDAR on disk; queue Geo extracts for IND021/IND139/FSP006 |
| UI in `parcel_profiles.qmd` | **Out of scope until MVP assets exist** |

---

## Gaps

1. **LiDAR extracts missing** for flagged IND021, IND139, FSP006 (catalog ready only).
2. **No parcel-framed hillshade product** yet (mosaics ≠ profile panels); BWMA hybrid pattern not applied to Laws/TA/Independence chronic dirs.
3. **AGOL auth** for non-public layers; prefer public views. Rate limits / attachment download already handled in `line-point-photos` scripts.
4. **Shiny decade bins / rank-shift** not implemented — only sparse year columns; vegetation-condition has dominants + NMDS but no decade rank table.
5. **TIN050/TIN053/BLK075** in IC1b chronic sync but not 2026 profile list — confirm year-over-year counters before scaling map products to “chronic” vs “flagged”.
6. Pre-2015 vs 2015+ transect network eras already noted in profiles — rank-shift method must not over-interpret sampling design change as ecological reorder.

---

## MVP

**One flagged parcel:** **LAW052** (Laws).

**Why LAW052:**
- In 2026 flagged set; long grass-counter history (IC1b sync Grass_sig.counter = 30).
- **2024 FEMAR** DTM/DSM/CHM already in `laws_2024/` (no new LPC download).
- Dense species-year series in `parcel_species_cover_app_2026.csv`; already a May-NDVI / LPT prior pilot parcel.
- Labeled AGOL photos on disk (e.g. 6× 2026, plus 2024/2025).
- Same Laws mosaic as LAW035/043/082 → easy scale-out after one template.

**MVP bundle (build later — not this doc):**
1. Hillshade clipped/framed to LAW052 (+ ~small pad) from `laws_2024_dtm_0p5m.tif`
2. Parcel boundary overlay
3. Transect start points (All_2026 view filtered to LAW052)
4. **One** photo point (suggest `LAW052_05_152` or latest odd/even-appropriate labeled JPG)
5. Decade species-rank table: baseline (1984–1987) | 1990s | 2000s | 2010s | current — top-N perennial cover

**Runner-up:** LAW043 (stronger cover+grass decline; also Laws mosaic) if Eco prefers a clearer composition shift narrative.

---

## Rank-shift method sketch

1. **Universe:** Perennial species (apply `config/species_aliases.yaml`); parcel-mean cover from app CSV or LPT master.
2. **Decade bins:**  
   - Baseline: surveys in 1984–1987 (mean or last baseline year)  
   - 1990s: 1990–1999  
   - 2000s: 2000–2009  
   - 2010s: 2010–2019  
   - Current window: 2020–cYear (or single cYear column)  
   Within-bin: mean cover across survey years present (do **not** invent zeros for unsurveyed years — align with cover-omit-nonsurvey-zeros practice).
3. **Rank table:** Top-N (N=5 or 10) by mean cover per bin; show code + common name + cover.
4. **Shift year (primary):** Among dated surveys in chronological order, find **earliest survey year** where top-N membership or order differs from baseline top-N by a simple rule (e.g. Spearman ρ on shared species &lt; threshold, or #1 species changes and stays changed for ≥2 subsequent surveys).
5. **Optional forcing note (secondary, Eco-owned):** If shift year coincides with known wellfield / surface-water / grazing / fire notes **and** neighboring flagged parcels show concurrent reorder, add a one-line *hypothesis* — explicitly **not** an I.C.1.b determination.
6. **Caveats:** Network redesign ~2015; odd/even agency split; lifeform vs species aggregation; NMDS path remains complementary geometry.

---

## Who owns what

| Lane | Owner | Owns |
|------|-------|------|
| **Geospatial programming** | Geo Geraldine | DTM→hillshade clips, COGs, transect overlay geojson, FEMAR vs Sierra epoch QA; IND021/IND139/FSP006 extract queue; **do not** block on BWMA CHM retune |
| **Quantitative ecology** | Eco Ernie | Decade bin defs, perennial filter, rank-shift rule, forcing-note criteria, top-N choice, sampling-era caveats |
| **Desk / app** | ICWD desk (this repo) | Profile placement later; Shiny-like table UX; photo pick + caption; ICM traces; **no UI until MVP assets land** |

---

## Suggested first messages

### Geo Geraldine (2–4 bullets)
- Please treat **LAW052** as the flagged-parcel hillshade MVP: clip/frame hillshade from existing `lidar-data/.../laws_2024/laws_2024_dtm_0p5m.tif` (BWMA `gdaldem` params), parcel boundary + All_2026 transect starts — quality bar = BWMA preview, **not** a new valley mosaic.
- Separate from mid-aggressive BWMA emergent CHM retune; reuse Laws mosaic only.
- After LAW052 template, same recipe for other `processed_2024` flagged Laws/TA parcels; queue LPC extracts for **IND021, IND139, FSP006**.
- Deliver under something like `lidar-data/data/processed/flagged_parcel_hillshade/LAW052/` (PNG + optional COG + transect geojson); desk will embed later.

### Eco Ernie (2–4 bullets)
- Need a **decade species-rank** view for flagged profiles (baseline / 1990s / 2000s / 2010s / current) — Shiny’s selected-year table is the UX precedent; we want rank-shift timing too.
- Please confirm top-N, perennial-only rules, and the “earliest survey where top-N reorders” criterion (plus whether co-parcel concurrent shift is enough for an optional forcing *note*).
- LAW052 proposed MVP; LAW043 runner-up if you want a sharper narrative parcel.
- Flag sampling-design era (pre-2015 vs permanent network) so we do not over-call a method change as ecology.

---

## Traceability

- Desk pointers: `CONTEXT.md`, `TODO.md` (this repo).
- Geo bar: `lidar-data/docs/2026-09-30_bwma-lidar-naip-vercel-preview.md`, `lidar-data/CONTEXT.md`.
- Photos: `line-point-photos/README.md`.
- Shiny: `inyoShiny/inyoShiny.R` (`species_table_years`).
