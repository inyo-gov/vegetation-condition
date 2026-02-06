#' Render annual vegetation results to a PowerPoint file.
#'
#' Requires: targets built, and packages officer, rvg (for ggplot in pptx).
#' Install with: install.packages(c("officer", "rvg"))
#'
#' Usage: run from project root after loading targets, or:
#'   source(here("code", "R", "render_annual_pptx.R"))
#'   render_annual_pptx()
#'
#' Output: docs/Vegetation_Annual_Results.pptx

render_annual_pptx <- function(out_dir = here::here("docs"),
                               out_file = "Vegetation_Annual_Results.pptx") {
  if (!requireNamespace("officer", quietly = TRUE)) {
    stop("Install package 'officer': install.packages('officer')")
  }
  use_rvg <- requireNamespace("rvg", quietly = TRUE)
  if (!use_rvg) {
    message("Package 'rvg' not found; plots will be added as PNG images. For vector graphics, install.packages('rvg')")
  }
  if (!requireNamespace("targets", quietly = TRUE)) {
    stop("Install package 'targets': install.packages('targets')")
  }
  if (!requireNamespace("withr", quietly = TRUE)) {
    stop("Install package 'withr': install.packages('withr')")
  }
  if (!requireNamespace("rprojroot", quietly = TRUE)) {
    stop("Install package 'rprojroot': install.packages('rprojroot')")
  }
  if (!requireNamespace("here", quietly = TRUE)) {
    stop("Install package 'here': install.packages('here')")
  }
  suppressPackageStartupMessages({
    library(tidyverse)
  })

  # Load targets (same as index.qmd); run from project root
  source(here::here("code", "R", "targets_functions.R"))
  targets <- c(
    "cYear",
    "n_parcels_sampled",
    "n_transects_sampled",
    "parcels",
    "parcels_deltas",
    "dtw_pfix",
    "deltas_ttest_att",
    "attributes_pfix",
    "boxplot.w.c",
    "trends.w.c"
  )
  tar_dir <- rprojroot::find_root("_targets.R", path = here::here())
  withr::with_dir(tar_dir, {
    for (t in targets) targets::tar_load(!!rlang::sym(t))
  })

  # Build parcel summary table (wellfield only, key columns)
  last_year_from_counter <- function(counter, first_reinv_year) {
    if (is.na(counter)) return(NA_real_)
    inferred <- cYear - counter
    if (is.finite(first_reinv_year) && inferred < first_reinv_year) return(1986)
    inferred
  }
  first_reinv_years <- parcels_deltas %>%
    filter(NominalYear > 1986) %>%
    group_by(Parcel) %>%
    summarise(first_reinv_year = min(NominalYear, na.rm = TRUE), .groups = "drop")

  current_year_deltas <- deltas_ttest_att %>%
    filter(Type == "W") %>%
    mutate(
      below_cover = dplyr::coalesce(Cover_sig.counter, 0) > 0,
      below_grass = dplyr::coalesce(Grass_sig.counter, 0) > 0,
      below_shrub = Shrub.Delta < 0,
      below_types = pmap_chr(
        list(below_cover, below_grass, below_shrub),
        ~ paste(
          c(
            if (isTRUE(..1)) "Cover" else NULL,
            if (isTRUE(..2)) "Grass" else NULL,
            if (isTRUE(..3)) "Shrub" else NULL
          ),
          collapse = "/"
        )
      ),
      chronic = ifelse(
        dplyr::coalesce(Cover_sig.counter, 0) > 5 |
          dplyr::coalesce(Grass_sig.counter, 0) > 5,
        "Yes", "No"
      )
    )

  dtw_current <- dtw_pfix %>%
    group_by(Parcel, Year) %>%
    summarise(DTW = mean(DTW, na.rm = TRUE), .groups = "drop") %>%
    group_by(Parcel) %>%
    mutate(
      rank = rank(DTW, ties.method = "min"),
      n = n(),
      DTW_pct = ifelse(n > 1, 100 * (1 - (rank - 1) / (n - 1)), NA_real_)
    ) %>%
    ungroup() %>%
    filter(Year == cYear) %>%
    select(Parcel, DTW, DTW_pct)

  parcel_summary <- current_year_deltas %>%
    left_join(first_reinv_years, by = "Parcel") %>%
    mutate(
      last_cover_year = mapply(last_year_from_counter, Cover_sig.counter, first_reinv_year),
      last_grass_year = mapply(last_year_from_counter, Grass_sig.counter, first_reinv_year)
    ) %>%
    left_join(dtw_current, by = "Parcel") %>%
    select(
      Parcel, wellfield, GB_TYPE,
      Cover_D = Cover.Delta, Grass_D = Grass.Delta, Shrub_D = Shrub.Delta,
      Cover_Yrs = Cover_sig.counter, Grass_Yrs = Grass_sig.counter,
      Chronic = chronic, Below = below_types,
      DTW, DTW_pct, Cover, Grass, Shrub
    ) %>%
    arrange(desc(Chronic), Parcel)

  # Plots (match index styling)
  p_box <- boxplot.w.c +
    scale_x_discrete(labels = c(C = "Control", W = "Wellfield")) +
    labs(x = "Parcel Type", title = "Wellfield vs. Control: Baseline (1986) vs. Current Year") +
    theme_minimal() +
    theme(
      panel.background = element_rect(fill = "white", color = NA),
      plot.background = element_rect(fill = "white", color = NA),
      strip.background = element_rect(fill = "white", color = "gray80"),
      strip.text = element_text(face = "bold")
    )

  p_trends <- trends.w.c +
    theme(
      panel.background = element_rect(fill = "white", color = NA),
      plot.background = element_rect(fill = "white", color = NA)
    )

  # Build PowerPoint
  doc <- officer::read_pptx()
  # Title slide
  doc <- officer::add_slide(doc, layout = "Title Slide", master = "Office Theme")
  doc <- officer::ph_with(
    doc,
    value = officer::block_list(
      officer::fpar(officer::ftext("Vegetation Condition Annual Results", officer::fp_text(bold = TRUE, font.size = 32))),
      officer::fpar(officer::ftext(paste0("Owens Valley — ", cYear), officer::fp_text(font.size = 20))),
      officer::fpar(officer::ftext(
        paste0(n_parcels_sampled, " parcels, ", n_transects_sampled, " transects"),
        officer::fp_text(font.size = 14)
      ))
    ),
    location = officer::ph_location_type(type = "ctrTitle")
  )
  doc <- officer::ph_with(
    doc,
    value = "Inyo County Water Department",
    location = officer::ph_location_type(type = "subTitle")
  )

  tmp_plots <- character(0)
  add_plot_to_slide <- function(doc, title_val, plot_obj, layout = "Title and Content") {
    doc <- officer::add_slide(doc, layout = layout, master = "Office Theme")
    doc <- officer::ph_with(
      doc,
      value = title_val,
      location = officer::ph_location_type(type = "title")
    )
    if (use_rvg) {
      doc <- officer::ph_with(
        doc,
        value = rvg::dml(ggobj = plot_obj),
        location = officer::ph_location_fullsize()
      )
    } else {
      tmp <- tempfile(fileext = ".png")
      ggplot2::ggsave(tmp, plot = plot_obj, width = 9, height = 5, dpi = 150, bg = "white")
      tmp_plots <<- c(tmp_plots, tmp)
      doc <- officer::ph_with(
        doc,
        value = officer::external_img(tmp),
        location = officer::ph_location_fullsize()
      )
    }
    doc
  }

  # Slide: Wellfield vs Control boxplot
  doc <- add_plot_to_slide(doc, "Wellfield vs. Control — Cover, Grass, and Shrub (1986 vs. Current Year)", p_box)

  # Slide: Trends over time
  doc <- add_plot_to_slide(doc, "Cover Trends Over Time by Parcel Type", p_trends)

  # Slide: Key table (wellfield parcel summary — first 15 rows)
  tbl_slide <- parcel_summary %>%
    head(15) %>%
    mutate(across(where(is.numeric), ~ round(., 1)))
  doc <- officer::add_slide(doc, layout = "Title and Content", master = "Office Theme")
  doc <- officer::ph_with(
    doc,
    value = paste0("Parcel Summary (Wellfield) — Top 15 by Chronic Status (", cYear, ")"),
    location = officer::ph_location_type(type = "title")
  )
  doc <- officer::ph_with(
    doc,
    value = tbl_slide,
    location = officer::ph_location_type(type = "body"),
    header = TRUE
  )

  # Optional: slide with counts (below baseline, chronic)
  n_below <- sum((deltas_ttest_att$Type == "W") &
    ((deltas_ttest_att$Cover.Delta < 0) | (deltas_ttest_att$Grass.Delta < 0)), na.rm = TRUE)
  n_chronic <- sum((deltas_ttest_att$Type == "W") &
    (dplyr::coalesce(deltas_ttest_att$Cover_sig.counter, 0) > 5 |
       dplyr::coalesce(deltas_ttest_att$Grass_sig.counter, 0) > 5), na.rm = TRUE)
  doc <- officer::add_slide(doc, layout = "Title and Content", master = "Office Theme")
  doc <- officer::ph_with(
    doc,
    value = "Summary Statistics",
    location = officer::ph_location_type(type = "title")
  )
  doc <- officer::ph_with(
    doc,
    value = officer::block_list(
      officer::fpar(officer::ftext(paste0("Wellfield parcels below baseline (cover and/or grass) in ", cYear, ": ", n_below), officer::fp_text(font.size = 14))),
      officer::fpar(officer::ftext(paste0("Wellfield parcels chronically below baseline (5+ consecutive years): ", n_chronic), officer::fp_text(font.size = 14)))
    ),
    location = officer::ph_location_type(type = "body")
  )

  out_path <- file.path(out_dir, out_file)
  dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)
  print(doc, target = out_path)
  for (f in tmp_plots) if (file.exists(f)) unlink(f)
  message("Saved: ", out_path)
  invisible(out_path)
}
