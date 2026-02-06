# libraries----

library(targets)
library(tidyverse)
library(tarchetypes)
library(DT)
library(here)
library(sf)
library(tmaptools)
library(tmap)
library(ggpmisc)
library(ggpubr)

# source functions----
source("code/R/targets_functions.R")

# target options----
tar_option_set(
  packages = c(
    "tidyverse",
    "stringr"
  )

)
# targets list----
list(
  # Optional: MotherDuck/DuckDB sync (see ANNUAL_UPDATE.md)
  # Uncomment and define connect_motherduck(); add targets that write
  # lpt_updated_master, attributes_pfix, dtw to cloud so other projects can read.
  # tar_target(
  #   motherduck_con,
  #   connect_motherduck(),
  #   format = "connection"
  # ),

  # Current YEAR parameter----
  tar_target(cYear, 2025),

  # Previous Year
  tar_target(pYear, cYear - 1),

  # Remote sensing seasonal window parameters----
  tar_target(rs_stats_dir, "/Users/zac/workspace/earth-engine-tools/stats"),
  tar_target(rs_doy_start, 196),
  tar_target(rs_doy_end, 258),

  # GIS----
  tar_target(parcels_shp_file, "data/gisdata/LA_parcels_rasterizedd.shp", format = 'file'),
  tar_target(parcels_shp, st_read(parcels_shp_file)),

  tar_target(canals_shp_file, "data/gisdata/canals.shp", format = 'file'),
  tar_target(canals_shp, st_read(canals_shp_file, quiet = TRUE)),

  tar_target(monsites_shp_file, "data/gisdata/monsites_icwd_gps.shp", format = 'file'),
  tar_target(monsites_shp, st_read(monsites_shp_file, quiet = TRUE) %>% filter(!is.na(SITENAME))),

  tar_target(or_shp_file, "data/gisdata/OwensRiver.shp", format = 'file'),
  tar_target(or_shp, st_read(or_shp_file, quiet = TRUE)),

  tar_target(laa_shp_file, "data/gisdata/LA_aqueduct_nad83.shp", format = 'file'),
  tar_target(laa_shp, st_read(laa_shp_file, quiet = TRUE)),

  tar_target(lakes_shp_file, "data/gisdata/lakes.shp", format = 'file'),
  tar_target(lakes_shp, st_read(lakes_shp_file, quiet = TRUE)),

  tar_target(streams_shp_file, "data/gisdata/streams.shp", format = 'file'),
  tar_target(streams_shp, st_read(streams_shp_file, quiet = TRUE)),


  # April Depth to water updated every year ----
  tar_target(dtw_file, paste0("data/dtw_",cYear,".csv"), format = "file"),
  tar_target(dtw_hist_file, "data/dtw_2024.csv", format = "file"),
  tar_target(dtw_hist, read.csv(dtw_hist_file) %>% select(Parcel, DTW, Year)),
  tar_target(dtw_current, read.csv(dtw_file) %>% select(Parcel, DTW, Year)),
  tar_target(dtw, bind_rows(dtw_hist, dtw_current) %>% distinct(Parcel, Year, .keep_all = TRUE)),
  tar_target(dtw_pfix, mult_to_single_parcel_name(x = dtw)),

  # remote sensing added after Sep 15
  tar_target(rs_prev_file, paste0("data/rs_", pYear, ".csv"), format = "file"),
  tar_target(rs_prev, read.csv(rs_prev_file)),
  tar_target(
    rs_current,
    build_rs_seasonal_from_stats(
      stats_dir = rs_stats_dir,
      year = cYear,
      doy_start = rs_doy_start,
      doy_end = rs_doy_end
    )
  ),
  tar_target(
    rs_combined,
    bind_rows(rs_prev, rs_current) %>% distinct(Parcel, Year, .keep_all = TRUE)
  ),
  tar_target(
    rs_output_csv,
    {
      rs_path <- paste0("data/rs_", cYear, ".csv")
      readr::write_csv(rs_combined, rs_path)
      rs_path
    },
    format = "file"
  ),
  tar_target(rs, rs_combined),
  tar_target(rs_pfix, mult_to_single_parcel_name(x = rs)),

  # Parcel attributes----
  tar_target(attributes_file, "data/Attributes.csv", format = "file"),
  tar_target(attributes, read.csv(attributes_file)),
  tar_target(attributes_pfix, mult_to_single_parcel_name(x = attributes)),
  tar_target(attributes_reinv, filter(attributes_pfix,reinv == "r")),

  # Species functional traits----
  tar_target(species_file, "data/species.csv", format = "file"),
  tar_target(species, read.csv(species_file)),

  # ICWD Line Point----
  tar_target(icwd_file,paste0("data/lpt_ICWD_",cYear,".csv"), format = "file"),
  tar_target(icwd_wide, read.csv(icwd_file)),
  tar_target(icwd_long, pivot_longer_icwd(icwd_wide)),
  tar_target(icwd_processed,
             add_species_agency_plotid(long = icwd_long,cYear,species, entity = "ICWD")),

  # LADWP Line Point----
  tar_target(ladwp_file, paste0("data/lpt_LADWP_",cYear,".csv"), format = "file"),
  tar_target(ladwp_long, read.csv(ladwp_file)),
  tar_target(ladwp_processed,
             add_species_agency_plotid(long = ladwp_long,cYear,species, entity = "LADWP")),

  # Output ICWD/LADWP line point processed----
  tar_target(icwd_output_csv, save_csv_and_return_path(processed = icwd_processed,cYear,entity = "ICWD"),
             format = "file"),
  tar_target(ladwp_output_csv, save_csv_and_return_path(processed = ladwp_processed,cYear,entity = "LADWP"),
             format = "file"),

  # Merge ICWD/LADWP Line point data----
  tar_target(icwd_ladwp_bind, bind_rows(icwd_processed, ladwp_processed)),

  # Output merged line point data----
  tar_target(icwd_ladwp_output_csv, save_csv_and_return_path(processed = icwd_ladwp_bind,cYear,entity = "ICWD_LADWP_merged"),
             format = "file"),

  # LPT - Previous Master line point data----
  tar_target(lpt_prev_master_file, paste0("data/lpt_MASTER_",pYear,".csv"), format = "file"),
  tar_target(lpt_prev_master, read.csv(lpt_prev_master_file)),

  # LPT bind current year to previous master----
  tar_target(lpt_updated_master, bind_rows(icwd_ladwp_bind, lpt_prev_master)),

  # Output LPT updated master-----
  tar_target(lpt_updated_master_csv, save_master_csv_and_return_path(processed = lpt_updated_master,cYear,entity = "MASTER"),
             format = "file"),

  # LPT Summaries----
  # filter master LELA, fix parcel names, output csv----
  tar_target(lpt_long_no_lela, filt_lela(data = lpt_updated_master)),
  tar_target(lpt_long_no_lela_pfix, mult_to_single_parcel_name(x = lpt_long_no_lela)),
  tar_target(long_combined_nl_csv, save_csv_and_return_path(processed = lpt_long_no_lela_pfix,cYear,entity = "long_combined_nl"),
             format = "file"),

  # Summary Stats----
  tar_target(n_parcels_all_years, count_parcels_all_years(lpt_updated_master)),
  tar_target(n_parcels_sampled, count_parcels_cyear(n_parcels_all_years, cYear)),
  tar_target(n_transects_sampled, count_transects_cyear(lpt_updated_master,cYear)),

  # Append cloned baseline data----
  tar_target(wvcom_file, "data/wvcom1.csv", format = "file"),
  tar_target(wvcom, read.csv(wvcom_file)),
  tar_target(wvcom_pfix, mult_to_single_parcel_name(x = wvcom)),

  # Transect-level summaries----
  tar_target(transects, summarise_to_transect(x=lpt_long_no_lela_pfix, y=wvcom_pfix)),

  # parcel summary functional type----
  tar_target(parcels, summarise_to_parcel(x= transects)),
  tar_target(parcels_deltas, add_parcel_deltas(parcels)),
  tar_target(parcels_deltas_yoy, add_parcel_deltas_yoy(parcels, cYear)),

  # wellfield and control parcels summary----
  tar_target(wellcont_means, wellfield_control_means(parcels_deltas, attributes_pfix)),
  tar_target(wellcont_means_rarefied, wellfield_control_means_rarefied(parcels_deltas, attributes_pfix)),
  tar_target(plot_wellcontrol, plot_wellfield_control(wellcont_means_rarefied)),
  tar_target(trends.w.c, compute_trend_well_cont(wellcont_means_rarefied)),

  # Wellfield-Control boxplot----
  tar_target(boxplot.w.c, boxplot_well_cont(parcels,attributes_pfix,cYear)),

  # Nested list----
  tar_target(parcel_year_meta, nest_transects(transects, attributes_reinv)),

  # split on baseline n----
  tar_target(parcel_year_meta_2samp,filter(parcel_year_meta, n.y > 4) ),
  tar_target(parcel_year_meta_1samp, filter(parcel_year_meta, n.y <= 4)),

  # Parcel statistical tests----
  tar_target(parcel_year_meta_2samp_results, two_sample_ttest(parcel_year_meta_2samp)),
  tar_target(parcel_year_meta_2samp_results_grass, two_sample_ttest_grass(parcel_year_meta_2samp)),
  tar_target(parcel_year_meta_1samp_results, one_sample_ttest(parcel_year_meta_1samp)),
  tar_target(parcel_year_meta_1samp_results_grass, one_sample_ttest_grass(parcel_year_meta_1samp)),

  # Create counter----
  tar_target(parcel_year_meta_combined_results, bindttest_count_sig_runs(parcel_year_meta_2samp_results_grass,parcel_year_meta_1samp_results_grass,parcel_year_meta_2samp_results,parcel_year_meta_1samp_results)),

  # Rectangularize parcel summaries----
  tar_target(parcel_test_sums, parcel_testadd_sums(parcel_year_meta_combined_results)),

  # Join for summary table
  tar_target(deltas_ttest_att, join_summaries(parcels_deltas,attributes_pfix, parcel_year_meta_combined_results, parcel_test_sums, cYear)),

  # Create data tables ----
  tar_target(parcel_datatable, make_parcel_data_table(deltas_ttest_att,cYear)),
  tar_target(parcel_datatable_significant, make_parcel_data_table_significant(deltas_ttest_att,cYear)),
  tar_target(parcel_datatable_chronic, make_parcel_data_table_chronic(deltas_ttest_att,cYear)),

  # Join GIS parcels to significance tests----
  tar_target(parcels_shp_ttest, left_join(parcels_shp, deltas_ttest_att, by = c("PCL"="Parcel"))),

  # Make panel maps showing stats----
  tar_target(panel_map_lw, panel_map(cYear, parcels_shp_ttest, "Laws", or_shp,streams_shp,canals_shp,laa_shp,lakes_shp, monsites_shp)),
  tar_target(panel_map_bp, panel_map(cYear, parcels_shp_ttest, "Big Pine", or_shp,streams_shp,canals_shp,laa_shp,lakes_shp, monsites_shp)),
  tar_target(panel_map_ta, panel_map(cYear, parcels_shp_ttest, "Taboose-Aberdeen", or_shp,streams_shp,canals_shp,laa_shp,lakes_shp, monsites_shp)),
  tar_target(panel_map_ts, panel_map(cYear, parcels_shp_ttest, "Thibaut-Sawmill", or_shp,streams_shp,canals_shp,laa_shp,lakes_shp, monsites_shp)),
  tar_target(panel_map_io, panel_map(cYear, parcels_shp_ttest, "Independence-Oak", or_shp,streams_shp,canals_shp,laa_shp,lakes_shp, monsites_shp)),
  tar_target(panel_map_ss, panel_map(cYear, parcels_shp_ttest, "Symmes-Shepherd", or_shp,streams_shp,canals_shp,laa_shp,lakes_shp, monsites_shp)),
  tar_target(panel_map_bg, panel_map(cYear, parcels_shp_ttest, "Bairs-George", or_shp,streams_shp,canals_shp,laa_shp,lakes_shp, monsites_shp))

)


