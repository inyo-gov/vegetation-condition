## 2026-09-30 PT — Support decline-only grass/shrub sign

- Root: percentile used `cur_p < 50`; Grass Δ +27 (FSL054) landed under Support.
- Fix: Cover↓/Grass↓/Shrub↑ sign gate on Change, Percentile, Counters; intro blurb updated.
- Quarto regen; Vercel prod `dpl_HZfDFTu6MtgKsBBKjmLisj2Mkykq`.

---

## 2026-09-30 PT — Cover omit non-survey zeros

- Root cause: stack path `complete(Year)` + `coalesce(..., 0)` drew fake 0% bars/dots for non-LPT years (FSL054 1988–2005).
- Fix: filter stack to `is.finite(Cover)`; harden PDF `plot_bar_discrete` to finite Mean; NDVI unchanged.
- Quarto + PDF regen; Vercel prod `dpl_796V826mPfprZiAsVzkN6AygtugU`.

---

## 2026-09-30 PT — Cover % stack colors + x-axis

- `parcel_profiles.qmd` lifeform palette + selective year breaks; shrub swatch brown.
- `code/R/gg_timeseries_plots6b.R` `year_axis_breaks()` for PDF panels.
- Quarto + PDF regen; Vercel prod `dpl_6k6s2ujq58cLVErrXtXh6VVoFp8R`.

---

## 2026-09-30 — FSL044 baseline year vs Green Book II.A.1 (ICM)

- Verified FSL044 calendar baseline **1987** (`wvcom1`, `Attributes.Baseline`/`bl.Year.tran`, `parcels_2026.Year`) against Green Book **Table II.A.1 Laws = Feb–Apr 1987** — **agree**.
- `NominalYear=1986` is the 1985–87 collapse in `summarise_to_parcel()`, not a calendar claim.
- NDVI residual (~+28 pp) **does not** clear when pairing Cover@1987 with NDVI@1986 — not a date-mismatch artifact.
- Note: `output/ndvi_cover_baseline/FSL044_baseline_year_ICM_2026-09-30.md` (+ copy under `green-book-IC1b/output/notes/`). No deploy.


## 2026-09-29 — Parcel profile badge colors (orange=bad / green=good)

- Replaced strikethrough `is-off` badges with explicit **green (`is-good`)** / **orange (`is-bad`)** / muted **`is-na`** classes in `styles.css`.
- `parcel_profiles.qmd` `format_parcel_badges`: I.C.1.a flagged → orange; DTW OK → green, DTW deeper → orange; Type C shrub &lt;80% → green "Shrub &lt;80% — OK", shrub ≥80% → orange (no strike).
- Quarto render `parcel_profiles.qmd` → `docs/parcel_profiles.html`. Vercel CLI logged out — prod redeploy pending auth.

---

## 2026-09-11 — Parcel profile cards + Type B cutpoint + FSL044/BGP162/FSL054

- `parcel_profiles.qmd`: two-column cards (Support for measurable change / Next steps) + optional stable-veg-type card; Type C–only shrub≥80% / grass:shrub≈0.2 badges and 0.2 ref line (Type B N/A).
- `code/R/measurability_evidence.R`: clarified Type C/AC-only `shrub_fraction_ge_80_type_C`.
- NDVI↔cover baseline diagnostic for FSL044 under `output/ndvi_cover_baseline/` (+ docs copy).
- Staff notes TSV + green-book-IC1b YAMLs: FSL044, BGP162 (new), FSL054; Five Bridges overlap note.
- Local Quarto render of `parcel_profiles.qmd` → `docs/` (no push).

repo: vegetation-condition
updated: 2026-09-04
period_focus: I.C.1.a living site + paper (graduate list, baseline provenance, Green Book revisions)
tags: [lpt, errata, zenodo, green-book, I.C.1.a, peerj, vegetation-condition, graduate-list, green-book-revisions]
---

# Vegetation Condition — Work Log

Local detailed notes. **`## Workspace summary`** is pulled by `workspace/worklog/sync_repo_worklogs.py`.

---

## Workspace summary

**Why it matters:** Public I.C.1.a automation + paper make measurability a living, citable product and feed the digital Green Book / attributability chain without staffing bottlenecks.

**2026 annual update (blocked on LADWP LPT):** Route A DTW from `kriging-dtw` is in place (`data/dtw_2026.csv`, 1836 parcels). ICWD LPT `data/lpt_ICWD_2026.csv` and prior master `data/lpt_MASTER_2025.csv` are present. **`data/lpt_LADWP_2026.csv` is missing** — do not set `cYear` to 2026 or run full `tar_make()` until LADWP arrives. Prep hist for switch: `data/dtw_thru_2025.csv` (1985–2025) ready to become `dtw_hist_file`.

**Active:** Site is Ingest → Evaluate → Graduate parcels → Paper. **Graduate list** (not “chronic”) = wellfield parcels with `Cover_sig.counter > 5` or `Grass_sig.counter > 5`. Profiles: cover + 100% perennial stack + NDVI + DTW; badges (DTW OK, shrub ≥80%); staff notes in `config/parcel_staff_notes.tsv`. Paper documents automation of measurability, Box I.C.1.a.ii method-update clause, PR-based living Green Book under `green-book-revisions`, baseline provenance (Cooperative Vegetation Study + SCS soils), and inflated-baseline / neighbor-assignment limits on significance tests.

**Cross-repo:** Baseline + spatial-ecology note and RS roadmap updates in `green-book-revisions`. Attributability automation still in `green-book-IC1b`. DTW kriging handoff: `kriging-dtw` → `data/dtw_YYYY.csv` (not used by `pumping-management`).

**Open:** Wait for LADWP 2026 LPT; then `cYear = 2026`, point `dtw_hist_file` at `dtw_thru_2025.csv`, `tar_make()`, render. Also: Neon `veg.lpt_master` load (Backend Ben / open-gis-migration — local master already promoted); Zenodo DOI; export `graduate_parcels.csv`; rename remaining internal `chronic` R symbols; fig/tbl crossrefs site-wide.

**Carryover:** Surgical ICWD errata magnitudes documented May 2026; **local** `lpt_MASTER_2025` promote done 2026-05-26 — Neon gold path still open (`ANNUAL_UPDATE.md`); MotherDuck is not SoR.

---

## Detailed log

### 2026-09-04 — #26 May/June LPT errata: magnitudes + master path (local done; Neon to Backend Ben)

**Time:** 1.0 h

#### Compared (existing artifacts only)

- [x] Re-read `ANNUAL_UPDATE.md` errata section + `output/errata/compare_20260514/` (ICWD-only, blended, LADWP sanity, resampled sensitivity).
- [x] Confirmed **local** promote: `data/lpt_MASTER_2025.csv` SHA-256 == `lpt_MASTER_2025_corrected.csv`; pre-errata frozen under `baseline_20260514/`; `_targets.R` `lpt_master_source = "file"`.
- [x] No second June compare tree — May surgical/compare → May 26 promote → Jun 11 site/PDF.

#### Magnitudes (ICWD-only; report these)

- 79/108 parcel-years nonzero Cover delta; range −20 … +7.8 cover pts; mean |Δ| ≈ 2.5; LADWP-only all zero.
- Prefer ICWD-only over blended (plotid collision inflates blended).

#### Stopped before

- Neon / MotherDuck / secrets writes (Backend Ben owns `veg.lpt_master`).
- Re-copy of local master (already promoted).
- OneDrive #3 (already done).

#### Desk / Backend Ben

- Status export: `../worklog/exports/2026-09-04_lpt_errata_may_june_status.md`
- Neon: `open-gis-migration` → `python scripts/load_veg_tabular.py --data-dir ../vegetation-condition/data --year 2025`

---

### 2026-09-04 — DTW Route A handoff; 2026 pipeline blocked on LADWP

**Time:** 0.5 h

#### Ready
- [x] `data/dtw_2026.csv` from `kriging-dtw` zonal stats (Parcel, DTW, Year; 1836 parcels; mean DTW ~13.7 ft vs ~13.1 in 2025)
- [x] `data/lpt_ICWD_2026.csv` on disk
- [x] `data/lpt_MASTER_2025.csv` (prior master for 2026 update)
- [x] `data/dtw_thru_2025.csv` prepared (hist 1985–2025) for when `cYear` flips to 2026

#### Blocked
- [ ] `data/lpt_LADWP_2026.csv` — **not available**; full `tar_make()` cannot proceed
- [ ] Leave `tar_target(cYear, 2025)` until LADWP arrives

#### When LADWP arrives
1. Place `data/lpt_LADWP_2026.csv`
2. In `_targets.R`: `cYear` → `2026`; `dtw_hist_file` → `"data/dtw_thru_2025.csv"`
3. `targets::tar_make()` then Quarto render / PDF timeseries per `ANNUAL_UPDATE.md`

### 2026-08-08 — PDF wellfield caption (queued)

**Time:** 0.2 h

#### What changed

- [ ] **TODO(FR-2026-008)** in `code/R/gg_timeseries_plots6b.R` `get_title()` + note on `pdf_stacked_timeseries_call.R`: add wellfield to page caption first (keep current parcel order). Controls use `wellfield_area` (closest/area), not a false wellfield membership. Optional later: N→S wellfield page order.

### 2026-07-24 — Paper, terminology, baseline provenance

**Time:** 3.0 h

#### What changed

- [x] Paper: Box I.C.1.a.ii “new methods” quote; living digital Green Book / PR merges in `green-book-revisions`; automation of measurability + next attributability bottleneck.
- [x] Paper + site: **graduate list / sustained measurable** replaces “chronic”; stats section clarifies measurability ≠ I.C.1.c significance; inflated baseline / unknown transect locations / neighbor look-alike assignments.
- [x] Baseline inventory summary: Cooperative Vegetation Study (LADWP 1984–87) vs SCS/NRCS soils (CA802); LTWA adopted inventory because it was available; Landsat 1985+, 1981 aerials, LiDAR/microtopography path.
- [x] `green-book-revisions`: `research/BASELINE_INVENTORY_AND_SPATIAL_ECOLOGY.md`, RS roadmap sensor stack, index links; bib entries `NRCSBentonOwens`, USGS WRI/WSP.
- [x] README + `config/measurability_procedure.yaml` graduate terminology; `catalog.yaml` pending sync refresh.

#### Open / next

- [ ] Re-render full site (`index`, `parcel_profiles`) for graduate labels if not already pushed.
- [ ] Surgical rename of remaining R/`chronic` identifiers when convenient.
- [ ] LPT errata promote + Zenodo (unchanged carryover).

---

### 2026-07-23 — Graduate profiles UI + staff notes

**Time:** 4.0 h

#### What changed

- [x] Site layout: shared `--site-shell` width; ETL defined on ingest; collapses for pithiness; Evaluate reframed parcel-first (no W-vs-C group analyses).
- [x] Graduate profiles: 100% perennial proportions (shrub bottom / 0.8 line); absolute cover panel; NDVI + DTW; DTW OK / shrub ≥80% badges; grass-proportion stability auto-note.
- [x] Staff notes TSV: BLK142, FSL044, FSL054, LAW035, LAW043 (+ prescribed burn / Five Bridges / watch-list summaries).
- [x] Paper theme aligned with site CSS; inyoShiny-style stack order fixed.

#### Open / next

- [ ] Continue parcel-by-parcel staff notes in `config/parcel_staff_notes.tsv`.
- [ ] Commit/push surgical site + paper when requested.

---

### 2026-07-23 — I.C.1.a scientific modernization (scaffold)

**Time:** ~2 h

#### Align vegetation-condition with sibling scientific repos

**Context:** Same modernization pattern as hydro-data, noxious-weeds, revegetation-projects, and especially green-book-IC1b: mandate-cited summary, PeerJ-style paper companion, figure/table captions, first-principles procedure, data inputs that reduce friction between analysis and decisions.

**Done:**

- [x] README + `catalog.yaml` reframed as Green Book I.C.1.a measurability (explicit I.C.1.b–c handoff).
- [x] `config/measurability_procedure.yaml` — Box I.C.1.a.ii checklist (ingest → aggregate → test → counter → graduate).
- [x] `config/paths.yaml` — sibling roots (IC1b, inyoShiny, pumping-management, hydro-data).
- [x] `_quarto.yml` — Quarto `@fig-`/`@tbl-` crossref config; navbar Ingest / Evaluate / Graduate / Paper.
- [x] `index.qmd` — Purpose + three-step mermaid chain (symmetric to IC1b); Discussion handoff language.
- [x] `parcel_profiles.qmd` — graduate list as I.C.1.a→b handoff product (not generic QA/QC).
- [x] `paper/paper.md` + `references.bib` — PeerJ-style living-repository manuscript skeleton.
- [x] `data/README.md` — input contract + friction-reduction goals.

**Next:**

- [ ] Site-wide `#| fig-cap` / `#| tbl-cap` + in-text `@fig-` / `@tbl-` (tables currently mostly heading-only).
- [ ] Export `output/graduate_parcels.csv` (or legacy `chronic_parcels.csv`) from targets for IC1b sync.
- [ ] Promote LPT errata; Zenodo DOI; audit one-sided vs two-sided test language vs Green Book text.
- [ ] MotherDuck/Neon read path for reports (remove `_targets`-only friction).

---

## Detailed log

### 2026-06-18 — LPT errata planning + comparison tooling review

**Time:** 2.0 h  

#### Publish LPT errata and data corrections to master DB

**Context:** ICWD line-point errata for 18 suffixed-transect parcels (2020–2025). Full workflow in `ANNUAL_UPDATE.md` errata section; comparison tooling in `code/R/errata_parcel_compare.R`, `code/R/surgical_fix_icwd.R`.

**Steps:**

- [x] Confirm `output/errata/baseline_20260514/` snapshot exists (pre-errata master frozen).
- [x] Run surgical ICWD re-ETL from quad-format workbooks → `lpt_MASTER_*_corrected.csv`.
- [x] Review `output/errata/compare_20260514/` magnitude tables (ICWD-only 18-parcel diffs). — reconfirmed 2026-09-04; cite ICWD-only not blended.
- [x] Sign-off on errata summary (absolute/percent change by parcel-year). — magnitudes accepted for local promote; staff memo still optional.
- [x] **Promote corrected master** → `data/lpt_MASTER_2025.csv`; set `lpt_master_source` in `_targets.R`; `tar_make()`, re-render site + PDF. — disk promote **2026-05-26**; site/PDF **2026-06-11**; `lpt_master_source = "file"`.
- [ ] **Load to Neon master DB** via open-gis-migration (`veg.lpt_master`, `ingest.batch` audit trail) — **Backend Ben**; not email/FINAL_v2. Handoff: `worklog/exports/2026-09-04_lpt_errata_may_june_status.md`.
- [ ] Publish errata summary on site / staff memo (what changed, why, magnitude).

**Do not:** overwrite production paths before comparison review (`ANNUAL_UPDATE.md` promote gates). Local overwrite already done after compare; do not re-promote without a new errata round.


---

### [todo] Zenodo annual release (posterity + public records)

**Goal:** Annual Zenodo deposit of corrected master datasets and report artifacts — citation DOI for TG/SC/public records; reduces ad-hoc data-request churn.

**Pattern:** Follow `hydro-data/planning/peer_review_pipeline.md` and `hydro-data/zenodo.json` (release-triggered DOI, `CITATION.cff`).

**This year's bundle (draft):**

- [ ] Corrected `lpt_MASTER_2025.csv` (+ prior year chain if errata touches history).
- [ ] `parcels_2025.csv` / RDS, DTW, RS handoff files used in annual report.
- [ ] Rendered `docs/` site snapshot or key PDFs (`Parcel_TimeSeries_*.pdf`).
- [ ] Errata comparison CSVs from `output/errata/compare_*` (document what changed).
- [ ] Add `CITATION.cff` + `zenodo.json` to repo (adapt from hydro-data).
- [ ] GitHub release → Zenodo mint DOI → link from report footer.

**Cadence:** Repeat each annual cycle after targets + Quarto publish (same window as errata promotion when applicable).

---

## Entries

## 2026-09-30 PT — 2026 Jul–Sep NDVI fill (post ee-tools Landsat 2026-09-21)

- Restored `rs_current` seasonal build; 380 parcels × 2026 Jul–Sep NDVI from ee-tools stats.
- May pilot composites for LAW052/FSL044 extended through 2026 (research exports).
- PDF stacked timeseries + `parcel_profiles` Quarto; Vercel prod `dpl_2jKLK3o7uvvX2BpW3dxrrb78Eerf`.
- See `SHIPPED_2026-09-30_rs-2026-julsep-ndvi.md`, ICM `docs/2026-09-30_rs-2026-julsep-ndvi.md`.


*(Earlier entries — add as work completes.)*

### 2026-08-18 — 2026 LPT → LA via ICWD_LA OneDrive (queued)

- Notice: 2026 ICWD line-point Google Sheets ready.
- Plan: `lpt_etl` + LA-facing tabular + download/label photos; drop in shared **ICWD_LA** OneDrive (`LPT_2026/`).
- Hub runbook: `worklog/exports/2026-08-18_lpt_2026_icwd_la_onedrive_handoff.md`.

**Time:** 0.25 h

## 2026-09-28 evening PT — bindttest consecutive-streak fix

- Fixed `bindttest_count_sig_runs` in `code/R/targets_functions.R` (rle within Parcel×cover_type; counter = 1..n per run).
- ICM: `docs/2026-09-28_bindttest_sig_runs_fix.md` (+ worklog mirror).
- Regenerated counters via targets; IND035 Grass 35→3; graduate list 30→13.
- Quarto render + Vercel prod redeploy `icwd-vegetation-condition`.

## 2026-09-29 PT — Mac sync: species aliases + dominants + GLM/ANOVA

- Applied box handoffs (`vc_species_alias_dominants_handoff.tgz`, `vc_glm_anova_handoff.tgz`) into `~/workspace/vegetation-condition`.
- Aliased master live (ELTR/ELTR3→LETR5, ARTRT→ARTR2); pre-alias backup kept.
- `tar_make` OK; IndVal secondary tags with `indicspecies` (67 rows); Quarto full render; Vercel prod `dpl_BNbonSuDgVXZDQxid4KK4MNRm633` → https://icwd-vegetation-condition.vercel.app
- See `SHIPPED_2026-09-29_mac-sync-alias-glm.md`. No email. Git commit pending.


## 2026-09-29 PT — pointer
GB TYPE space / type_drift lives in **stm** (`exports/type_space_2026-09-29/`, `docs/2026-09-29_gb-type-space-nmds.md`); reuses valley-floor NMDS scores, not a new veg-condition target.

## 2026-09-29 evening PT — valley-floor NMDS + Zac UI pass

- Per-parcel valley-floor NMDS path + same-TYPE baseline cloud on flagged profiles (13/13); controls parked.
- UI: Support/Not supported framing; percentile two-line; NDVI mini-table; merged colored cards; intro into “How to interpret”.
- Paper nav off. Redeploy icwd-vegetation-condition.
- Notes: `SHIPPED_2026-09-29_valley-nmds-ui-pass.md`; ICM `worklog/exports/2026-09-29_vegetation-condition-valley-nmds-ui.md`.

## 2026-09-30 PT — NMDS enrich + margin gutter (Mac)

- Enriched per-parcel valley-floor NMDS: Type callout, cloud n (site-years/parcels), DTW envfit arrow, headline species centroids; larger PNG.
- CSS: visible right margin on Support/Not-supported cards vs Tufte margin figs.
- Quarto `parcel_profiles` + Vercel prod. See `SHIPPED_2026-09-30_nmds-enrich-margin.md`.

## 2026-09-29 late PT — redeploy valley NMDS / NDVI / UI

- Vercel prod `dpl_6QUhJdq8BqjLZ3ziqZwLbH63hdjj` → https://icwd-vegetation-condition.vercel.app
- See `SHIPPED_2026-09-29_valley-nmds-ui-pass.md`.

## 2026-09-30 PT — parcel-profile baseline calendar years

- Parcel profile reference lines now use per-parcel `Baseline` / `bl.Year.tran` rather than the `NominalYear` collapse: Cover, Grass, NDVI, and DTW.
- LAW052 rendered with `1987 NDVI`, `Cover 1987 baseline`, `Grass 1987 baseline`, and `1987 DTW`.
- `quarto render parcel_profiles.qmd` passed. NDVI remains the existing Jul–Sep window; no aggregation change.
- Green Book Table II.A.1 spring inventory (Laws Feb–Apr 1987) noted as context only.
- Vercel production deployment `dpl_2HbCQeaJNef7EpEKKY5eXA8aTwD3` aliased to https://icwd-vegetation-condition.vercel.app; live profile route `/docs/parcel_profiles.html` confirms LAW052 `Baseline (1987)` and the rendered timeseries.
- Redeployed after updating `code/R/gg_timeseries_plots6b.R` (active stacked-PDF entry point) with parcel-specific Cover/Grass/NDVI/DTW/Precip reference labels: `dpl_9VzqNgCbBceYY9McxiWVTsJRP1YW`.

## 2026-09-30 PT — May NDVI + LPT prior prototype (research draft)

- Pilot LAW052 + FSL044 (Owens River mainstem; not Fish Slough). Random-walk / EB prior by lifeform; optional May NDVI (Apr20–Jun10) Kalman update in base R.
- May NDVI 1984–2024 present; 2025–2026 May missing. 2026 LPT present → prior-only scored.
- Docs/exports: `docs/2026-09-30_may-ndvi-lpt-prior-prototype.md`, `exports/may_ndvi_lpt_2026-09-30/`, script `code/run_may_ndvi_lpt_prior_2026-09-30.R`. Created root `CONTEXT.md` / `TODO.md`.
- Not management / not Board. No git push, no email.

## 2026-09-30 — historic spreading / Ritch / Lee / STM desk scout
- Plan: `docs/2026-09-30_historic-spreading-ritch-lee-stm-plan.md` (read-only inventory; no raster reprocess).
- 1969 spreading PDFs/zip under Rprojects are 0 B Dropbox stubs; Ritch/Lee digitize present under old_land_use; flooded-acreage is BWMA-scoped.

## 2026-09-30 PT — IND026 spp-rank columns + layout fix
- Fixed decade table (dplyr `period` shadow → base-R `period_key`); photo first + full-width hillshade stack.
- `quarto render parcel_profiles.qmd`; Vercel prod → https://icwd-vegetation-condition.vercel.app
- Verified IND026: Baseline SPAI 49% → 2026 ARTR2/ATTO/ERNA10; columns differ.
- ICM `docs/2026-09-30_ind026-spp-rank-layout-fix.md`.
