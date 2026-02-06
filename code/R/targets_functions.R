library(DBI); library(duckdb)
connect_motherduck <- function(){
  dbConnect(
    duckdb::duckdb_http(),
    url   = Sys.getenv("MOTHERDUCK_URL"),
    token = Sys.getenv("MOTHERDUCK_TOKEN")
  )
}

#' Pivot ICWD Data to Long Format
#'
#' @param icwd_wide A wide-format data frame containing ICWD data.
#' @return A long-format data frame with 'Transect' and 'Cover' columns.
#' @export
#' @examples
#' pivot_longer_icwd(icwd_wide)
pivot_longer_icwd <- function(icwd_wide) {
  icwd_wide %>%
    gather(Transect, Cover, T1:T24) %>%
    filter(!is.na(Cover)) %>%
    mutate(
      Transect = str_replace(Transect, "\\T", ""),
      Transect = as.numeric(Transect)
    )
}


#' Add Species, Agency, and Plot ID Information
#'
#' @param long A long-format data frame.
#' @param cYear The current year.
#' @param species A data frame containing species information.
#' @param entity The entity name.
#' @return A data frame with additional species, agency, and plot ID information.
#' @export
#' @examples
#' add_species_agency_plotid(long, cYear, species, entity)
add_species_agency_plotid <- function(long, cYear, species, entity) {
  long %>%
    left_join(species, join_by(Code == Code)) %>%
    mutate(
      source = 'Joint Monitoring 2015-current year',
      source.abr = 'jm',
      Year = cYear,
      Entity = entity,
      plotid = paste(Parcel, Transect, source.abr, sep = '_'),
      plotid.full = paste(Parcel, Transect, source.abr, Entity, sep = '_'),
      Cover = as.numeric(Cover)
    ) %>%
    select(
      Parcel, Code, Transect, Cover, Year, Entity, plotid, Species, CommonName,
      Order, Family, Genus, Lifecycle, Lifeform, Veg_Type, source, source.abr,
      Phreatophyte, plotid.full
    ) %>%
    arrange(Parcel, Transect, Code)
}




#' Filter Out Invasive Species LELA2
#'
#' @param data A data frame containing species data.
#' @return A data frame excluding invasive species LELA2 and LELA.
#' @export
#' @examples
#' filt_lela(data)
filt_lela <- function(data) {
  data %>% filter(Species != "LELA2", Species != "LELA")
}


#' Revert to Single Parcel Name
#'
#' @param x A data frame containing parcel data.
#' @return A data frame with single parcel names.
#' @export
#' @examples
#' mult_to_single_parcel_name(x)
mult_to_single_parcel_name <- function(x) {
  # ABD012
  x$Parcel[x$Parcel == "BLK029"] <- "ABD012"
  x$Parcel[x$Parcel == "ABD012_BLK029"] <- "ABD012"
  x$Parcel[x$Parcel == "ABD012/BLK029"] <- "ABD012"

  # BIS025
  x$Parcel[x$Parcel == "BIS025_FSL157"] <- "BIS025"

  # BIS026
  x$Parcel[x$Parcel == "BIS026_FSL156"] <- "BIS026"

  # BIS055
  x$Parcel[x$Parcel == "FSL214"] <- "BIS055"
  x$Parcel[x$Parcel == "BIS055_FSL214"] <- "BIS055"
  x$Parcel[x$Parcel == "BIS055/FSL214"] <- "BIS055"

  # BLK002
  x$Parcel[x$Parcel == "TIN061"] <- "BLK002"
  x$Parcel[x$Parcel == "BLK002_TIN061"] <- "BLK002"
  x$Parcel[x$Parcel == "TIN061_BLK002"] <- "BLK002"

  # FSL048
  x$Parcel[x$Parcel == "LAW109"] <- "FSL048"
  x$Parcel[x$Parcel == "LAW109_FSL048"] <- "FSL048"

  # FSP004
  x$Parcel[x$Parcel == "BGP188"] <- "FSP004"
  x$Parcel[x$Parcel == "FSP004_BGP188"] <- "FSP004"
  x$Parcel[x$Parcel == "FSP004_BPG188"] <- "FSP004"
  x$Parcel[x$Parcel == "FSP004/BGP188"] <- "FSP004"

  # FSP006
  x$Parcel[x$Parcel == "BGP182"] <- "FSP006"
  x$Parcel[x$Parcel == "FSP006_BGP182"] <- "FSP006"
  x$Parcel[x$Parcel == "FSP006/BGP182"] <- "FSP006"

  # IND024
  x$Parcel[x$Parcel == "BLK103"] <- "IND024"
  x$Parcel[x$Parcel == "IND024_BLK103"] <- "IND024"
  x$Parcel[x$Parcel == "IND024/BLK103"] <- "IND024"

  # IND139
  x$Parcel[x$Parcel == "MAN005"] <- "IND139"
  x$Parcel[x$Parcel == "IND139_MAN005"] <- "IND139"
  x$Parcel[x$Parcel == "IND139/MAN005"] <- "IND139"

  # IND163
  x$Parcel[x$Parcel == "BEE017"] <- "IND163"
  x$Parcel[x$Parcel == "IND163_BEE017"] <- "IND163"

  # LAW108
  x$Parcel[x$Parcel == "FSL047"] <- "LAW108"
  x$Parcel[x$Parcel == "LAW108_FSL047"] <- "LAW108"
  x$Parcel[x$Parcel == "LAW108/FSL047"] <- "LAW108"

  # LAW137
  x$Parcel[x$Parcel == "PLC210"] <- "LAW137"
  x$Parcel[x$Parcel == "LAW137_PLC210"] <- "LAW137"
  x$Parcel[x$Parcel == "LAW137/PLC210"] <- "LAW137"

  # MAN006
  x$Parcel[x$Parcel == "MAN006_IND229"] <- "MAN006"
  x$Parcel[x$Parcel == "MAN006/IND229"] <- "MAN006"
  x$Parcel[x$Parcel == "IND229"] <- "MAN006"

  # TIN028
  x$Parcel[x$Parcel == "FSP019"] <- "TIN028"
  x$Parcel[x$Parcel == "FSP022"] <- "TIN028"
  x$Parcel[x$Parcel == "TIN028_FSP022_FSP019"] <- "TIN028"
  x$Parcel[x$Parcel == "TIN028_FSP019_FSP022"] <- "TIN028"

  return(x)
}

 #' Build seasonal remote-sensing summaries from Earth Engine stats
 #'
 #' @param stats_dir Path to Earth Engine stats directory.
 #' @param year Target year for seasonal aggregation.
 #' @param doy_start Start day-of-year for seasonal window.
 #' @param doy_end End day-of-year for seasonal window.
 #' @param qa_ok QA value to keep (default 0).
 #' @param max_cloud Maximum CLOUD_SCORE to keep.
 #' @param min_ndvi Minimum NDVI to keep.
 #' @return A data frame with parcel/year seasonal summaries and PPT.
 #' @export
 build_rs_seasonal_from_stats <- function(stats_dir, year, doy_start, doy_end,
                                          qa_ok = 0, max_cloud = 70, min_ndvi = 0) {
  landsat_files <- list.files(
    path = stats_dir,
    pattern = "*landsat_daily.csv",
    recursive = TRUE,
    full.names = TRUE,
    include.dirs = TRUE
  )
  if (length(landsat_files) == 0) {
    stop("No landsat_daily.csv files found in stats_dir.")
  }

  landsat <- purrr::map_dfr(landsat_files, readr::read_csv, show_col_types = FALSE)

  rs_filtered <- landsat %>%
    dplyr::mutate(DATE = lubridate::make_date(YEAR, MONTH, DAY)) %>%
    dplyr::filter(
      YEAR == year,
      QA == qa_ok,
      CLOUD_SCORE < max_cloud,
      dplyr::between(DOY, doy_start, doy_end),
      NDVI_SUR > min_ndvi
    )

  rs_summary <- rs_filtered %>%
    dplyr::group_by(ZONE_NAME, YEAR) %>%
    dplyr::summarise(
      mean.ndvi = mean(NDVI_SUR, na.rm = TRUE),
      mean.ndwi.gr.nir = mean(NDWI_GREEN_NIR_SUR, na.rm = TRUE),
      mean.ndwi.gr.swir1 = mean(NDWI_GREEN_SWIR1_SUR, na.rm = TRUE),
      mean.ndwi.nir.swir1 = mean(NDWI_NIR_SWIR1_SUR, na.rm = TRUE),
      mean.tc.bright = mean(TC_BRIGHT, na.rm = TRUE),
      mean.tc.wet = mean(TC_WET, na.rm = TRUE),
      mean.tc.green = mean(TC_GREEN, na.rm = TRUE),
      sd.ndvi = sd(NDVI_SUR, na.rm = TRUE),
      n = dplyr::n(),
      .groups = "drop"
    )

  gridmet_files <- list.files(
    path = stats_dir,
    pattern = "*gridmet_monthly.csv",
    recursive = TRUE,
    full.names = TRUE,
    include.dirs = TRUE
  )
  if (length(gridmet_files) == 0) {
    stop("No gridmet_monthly.csv files found in stats_dir.")
  }

  gridmet <- purrr::map_dfr(gridmet_files, readr::read_csv, show_col_types = FALSE)

  gridmet_summary <- gridmet %>%
    dplyr::filter(WATER_YEAR == year) %>%
    dplyr::group_by(ZONE_NAME, WATER_YEAR) %>%
    dplyr::summarise(
      wy.ppt = sum(PPT, na.rm = TRUE),
      sd.ppt = sd(PPT, na.rm = TRUE),
      n.month.ppt = dplyr::n(),
      .groups = "drop"
    )

  rs_summary %>%
    dplyr::left_join(gridmet_summary, by = c("ZONE_NAME", "YEAR" = "WATER_YEAR")) %>%
    dplyr::rename(Year = YEAR, NDVI_SUR = mean.ndvi, PPT = wy.ppt, Parcel = ZONE_NAME)
 }


#' Summarise Cover Types to Transect Level
#'
#' @param x A data frame containing parcel data.
#' @param y A data frame containing additional data.
#' @return A data frame summarising cover types to transect level.
#' @export
#' @examples
#' summarise_to_transect(x, y)
summarise_to_transect <- function(x, y) {
  tran.sums <- x %>%
    group_by(Parcel, Year, Transect, source.abr, plotid, Lifecycle, Lifeform) %>%
    summarise(Cover = sum(Cover), .groups = 'drop')

  tran.tlc <- tran.sums %>%
    group_by(Parcel, Year, plotid) %>%
    summarise(tot.live.cover = sum(Cover), .groups = 'drop')

  pft.wide <- tran.sums %>%
    spread(Lifeform, Cover) %>%
    replace(is.na(.), 0) %>%
    mutate(
      Cover = Grass + Herb + Shrub + Herb_Shrub + Tree,
      Shrub = Shrub + Herb_Shrub + Tree
    )

  pft.wide.wtot.cov <- pft.wide %>%
    filter(Lifecycle == "Perennial") %>%
    left_join(tran.tlc, by = c("Parcel", "Year", "plotid"))

  bind_add_proportion <- bind_rows(pft.wide.wtot.cov, y) %>%
    mutate(
      pShrubTran = Shrub / Cover,
      pGrassTran = Grass / Cover,
      pHerbTran = Herb / Cover
    ) %>%
    mutate_if(is.numeric, ~replace_na(., 0))

  return(bind_add_proportion)
}


#' Summarise Data from Transects to Parcels
#'
#' @param x A data frame containing transect data.
#' @return A data frame summarised to the parcel level.
#' @export
#' @examples
#' summarise_to_parcel(x)
summarise_to_parcel <- function(x) {
  p <- x %>%
    group_by(Parcel, Year) %>%
    summarise(
      PerHits = sum(Cover),
      ShrubHits = sum(Shrub),
      HerbHits = sum(Herb),
      GrassHits = sum(Grass),
      Cover = mean(Cover),
      Shrub = mean(Shrub),
      Herb = mean(Herb),
      Grass = mean(Grass),
      TLC = mean(tot.live.cover),
      pShrub = mean(pShrubTran),
      pGrass = mean(pGrassTran),
      pHerb = mean(pHerbTran),
      n.transects = n(),
      .groups = 'drop'
    ) %>%
    mutate(
      NominalYear = case_when(
        Year %in% c(1985, 1986, 1987) ~ 1986,
        TRUE ~ Year
      )
    )

  return(p)
}



#' Add Deltas to Each Year
#'
#' @param parcels A data frame containing parcel data.
#' @return A data frame with deltas added.
#' @export
#' @examples
#' add_parcel_deltas(parcels)
add_parcel_deltas <- function(parcels) {
  p <- parcels
  for (PID in unique(p$Parcel)) {
    baselineRow <- p[p$Parcel == PID & p$NominalYear == 1986, ]
    if (dim(baselineRow)[1] == 0) next

    otherYears <- (p$Parcel == PID & !(p$NominalYear == 1986))
    p$Cover.Delta[otherYears] <- p$Cover[otherYears] - baselineRow$Cover
    p$Shrub.Delta[otherYears] <- p$Shrub[otherYears] - baselineRow$Shrub
    p$Herb.Delta[otherYears] <- p$Herb[otherYears] - baselineRow$Herb
    p$Grass.Delta[otherYears] <- p$Grass[otherYears] - baselineRow$Grass
  }
  return(p)
}



#' Add Year-over-Year Deltas
#'
#' @param parcels A data frame containing parcel data.
#' @param cYear The current year.
#' @return A data frame with year-over-year deltas added.
#' @export
#' @examples
#' add_parcel_deltas_yoy(parcels, cYear)
add_parcel_deltas_yoy <- function(parcels, cYear) {
  p <- parcels %>% filter(NominalYear == cYear | NominalYear == cYear - 1)
  for (PID in unique(p$Parcel)) {
    baselineRow <- p[p$Parcel == PID & p$NominalYear == cYear - 1, ]
    if (dim(baselineRow)[1] == 0) next

    otherYears <- (p$Parcel == PID & !(p$NominalYear == cYear - 1))
    p$Cover.Delta[otherYears] <- p$Cover[otherYears] - baselineRow$Cover
    p$Shrub.Delta[otherYears] <- p$Shrub[otherYears] - baselineRow$Shrub
    p$Herb.Delta[otherYears] <- p$Herb[otherYears] - baselineRow$Herb
    p$Grass.Delta[otherYears] <- p$Grass[otherYears] - baselineRow$Grass
  }
  return(p)
}


#' Count Parcels for All Years
#'
#' @param lpt_updated_master A data frame containing parcel data.
#' @return A data frame with the count of distinct parcels for each year.
#' @export
#' @examples
#' count_parcels_all_years(lpt_updated_master)
count_parcels_all_years <- function(lpt_updated_master) {
  lpt_updated_master %>%
    group_by(Year) %>%
    summarise(n = n_distinct(Parcel), .groups = 'drop')
}


#' Count Parcels for the Current Year
#'
#' @param n_parcels_all_years A data frame with the count of distinct parcels for each year.
#' @param cYear The current year.
#' @return The count of parcels for the current year.
#' @export
#' @examples
#' count_parcels_cyear(n_parcels_all_years, cYear)
count_parcels_cyear <- function(n_parcels_all_years, cYear) {
  n_parcels_sampled_cYear <- n_parcels_all_years %>%
    filter(Year == cYear)
  n_parcels_sampled <- n_parcels_sampled_cYear$n

  return(n_parcels_sampled)
}


#' Count Transects for the Current Year
#'
#' @param lpt_updated_master A data frame containing transect data.
#' @param cYear The current year.
#' @return The count of transects for the current year.
#' @export
#' @examples
#' count_transects_cyear(lpt_updated_master, cYear)
count_transects_cyear <- function(lpt_updated_master, cYear) {
  cyr_transects <- lpt_updated_master %>%
    filter(Year == cYear) %>%
    summarise(n = n_distinct(plotid), .groups = 'drop')

  n_transects <- cyr_transects$n
  return(n_transects)
}


#' Calculate Wellfield and Control Means
#'
#' @param parcels_deltas A data frame containing parcel deltas.
#' @param attributes A data frame containing parcel attributes.
#' @return A data frame with wellfield and control means.
#' @export
#' @examples
#' wellfield_control_means(parcels_deltas, attributes)
wellfield_control_means <- function(parcels_deltas, attributes) {
  Parcels <- attributes %>%
    select(Parcel, Type) %>%
    left_join(parcels_deltas, by = "Parcel")

  Parcels %>%
    group_by(Type, NominalYear) %>%
    dplyr::summarize(
      count = n(),
      Cover = mean(Cover),
      Grass = mean(Grass),
      Herb = mean(Herb),
      Shrub = mean(Shrub),
      .groups = 'drop'
    )
}



#' Compute Trend for Wellfield and Control
#'
#' @param wellcont_wide A data frame containing wellfield and control data.
#' @return A ggplot object showing the trend.
#' @export
#' @examples
#' compute_trend_well_cont(wellcont_wide)
compute_trend_well_cont <- function(wellcont_wide) {
  lmmod <- y ~ x

  wellcont_wide$Type[wellcont_wide$Type == "C"] <- "Control"
  wellcont_wide$Type[wellcont_wide$Type == "W"] <- "Wellfield"

  plot <- wellcont_wide %>%
    pivot_longer(Cover:Shrub, names_to = "Cover.Type", values_to = "Cover") %>%
    filter(!Cover.Type %in% c("Herb")) %>%
    ggplot(aes(x = NominalYear, y = Cover, color = Type)) +
    geom_point() +
    geom_line() +
    geom_smooth(method = 'lm', se = FALSE, formula = lmmod) +
    xlab("Year") +
    ylab("Cover (0-100)") +
    stat_fit_glance(
      method = "lm",
      method.args = list(formula = lmmod),
      label.x = 1989,
      label.y = 40,
      geom = "text",
      aes(label = paste("P = ", signif(..p.value.., digits = 2), sep = ""))
    ) +
    facet_grid(Cover.Type ~ Type) +
    theme(legend.position = "none")

  return(plot)
}


#' Create Boxplot for Wellfield and Control
#'
#' @param parcels A data frame containing parcel data.
#' @param attributes_pfix A data frame containing fixed attributes.
#' @param cYear The current year.
#' @return A ggplot object showing the boxplot.
#' @export
#' @examples
#' boxplot_well_cont(parcels, attributes_pfix, cYear)
boxplot_well_cont <- function(parcels, attributes_pfix, cYear) {
  a <- attributes_pfix %>% select(Parcel, Type)
  parcels <- parcels %>% left_join(a, by = "Parcel")

  parcels.select.compare <- parcels %>%
    select(Parcel, NominalYear, Cover) %>%
    filter(NominalYear %in% c("1986", cYear))

  parcels.spread <- parcels.select.compare %>%
    pivot_wider(names_from = NominalYear, values_from = Cover, names_prefix = "y")

  cYear_col <- paste0("y", cYear)
  Pairwise <- parcels.spread %>%
    filter(!is.na(!!sym(cYear_col)) & !is.na(y1986))

  plot <- parcels %>%
    filter(Parcel %in% Pairwise$Parcel, NominalYear %in% c("1986", cYear)) %>%
    select(Parcel, NominalYear, Year, Type, Cover, Shrub, Grass, TLC) %>%
    gather(Cover.Type, Cover, Cover:TLC) %>%
    filter(Cover.Type != 'TLC', !is.na(Type)) %>%
    ggplot(aes(x = Type, y = Cover, color = as.factor(NominalYear))) +
    geom_boxplot() +
    facet_wrap(~ Cover.Type) +
    stat_compare_means(
      aes(group = NominalYear),
      method = 't.test',
      paired = FALSE,
      label = "p.signif",
      label.y = c(80)
    )

  return(plot)
}



#' Calculate Rarefied Wellfield and Control Means
#'
#' @param parcels_deltas A data frame containing parcel deltas.
#' @param attributes A data frame containing parcel attributes.
#' @return A data frame with rarefied wellfield and control means.
#' @export
#' @examples
#' wellfield_control_means_rarefied(parcels_deltas, attributes)
wellfield_control_means_rarefied <- function(parcels_deltas, attributes) {
  Parcels <- attributes %>%
    select(Parcel, Type) %>%
    left_join(parcels_deltas, by = "Parcel")

  Parcels %>%
    filter(Parcel %in% c(
      "BGP031", "BLK115", "FSL187", "IND096", "IND163", "LNP018",
      "MAN060", "PLC024", "PLC106", "PLC121", "PLC223", "UNW029",
      "UNW039", "BGP154", "BGP162", "BLK009", "BLK016", "BLK024",
      "BLK033", "BLK039", "BLK044", "BLK069", "BLK074", "BLK075",
      "BLK094", "BLK099", "IND011", "IND035", "IND106", "IND111",
      "IND132", "IND139", "IND231", "LAW063", "LAW065", "LAW085",
      "LAW107", "LAW120", "LAW122", "MAN006", "MAN007", "MAN037",
      "TIN028", "TIN068"
    )) %>%
    group_by(Type, NominalYear) %>%
    dplyr::summarize(
      count = n(),
      Cover = mean(Cover),
      Grass = mean(Grass),
      Herb = mean(Herb),
      Shrub = mean(Shrub),
      .groups = 'drop'
    )
}



#' Plot Wellfield and Control Data
#'
#' @param wellcont_means_rarefied A data frame containing rarefied wellfield and control means.
#' @return A ggplot object showing the plot.
#' @export
#' @examples
#' plot_wellfield_control(wellcont_means_rarefied)
plot_wellfield_control <- function(wellcont_means_rarefied) {
  plot <- wellcont_means_rarefied %>%
    select(-Herb) %>%
    pivot_longer(Cover:Shrub, names_to = "type", values_to = "cover") %>%
    ggplot(aes(x = NominalYear, y = cover, color = Type)) +
    geom_point() +
    geom_line() +
    facet_wrap(~ type, ncol = 2)

  return(plot)
}



#' Nest Transects Data
#'
#' @param transects A data frame containing transect data.
#' @param attributes_reinv A data frame containing reinventory attributes.
#' @return A nested data frame of transects by parcel and year.
#' @export
#' @examples
#' nest_transects(transects, attributes_reinv)
nest_transects <- function(transects, attributes_reinv) {
  byparyear <- transects %>%
    filter(Parcel %in% attributes_reinv$Parcel) %>%
    group_by(Parcel, Year) %>%
    nest() %>%
    mutate(n = map_dbl(data, nrow))

  gb <- byparyear %>% filter(Year > 1990)
  bl <- byparyear %>% filter(Year < 1990)
  parcel_year_nested_transects <- gb %>% left_join(bl, by = 'Parcel')

  return(parcel_year_nested_transects)
}

# Two-Sample T-Test for Cover ----
#' Perform Two-Sample T-Test for Cover
#'
#' @param data.x A data frame containing the first sample data.
#' @param data.y A data frame containing the second sample data.
#' @return The result of the two-sample t-test for cover.
#' @export
#' @examples
#' t2samp(data.x, data.y)
t2samp <- function(data.x, data.y) {
  failproof.t <- purrr::possibly(t.test, NA_real_)
  result <- failproof.t(data.x$Cover, data.y$Cover, conf.level = .95)
  return(result)
}

# Two-Sample T-Test for Grass ----
#' Perform Two-Sample T-Test for Grass Cover
#'
#' @param data.x A data frame containing the first sample data.
#' @param data.y A data frame containing the second sample data.
#' @return The result of the two-sample t-test for grass cover.
#' @export
#' @examples
#' t2samp_grass(data.x, data.y)
t2samp_grass <- function(data.x, data.y) {
  failproof.t <- purrr::possibly(t.test, NA_real_)
  result <- failproof.t(data.x$Grass, data.y$Grass, conf.level = .95)
  return(result)
}


# Two-sample t-test for Cover ----
#' Perform Two-sample t-test for Cover
#'
#' @param parcel_year_meta_2samp A data frame containing two-sample t-test meta data for parcels.
#' @return A data frame with t-test results and significance for cover.
#' @export
#' @examples
#' two_sample_ttest(parcel_year_meta_2samp)
two_sample_ttest <- function(parcel_year_meta_2samp) {
  result <- parcel_year_meta_2samp %>%
    mutate(model = map2(data.x, data.y, t2samp)) %>%
    mutate(glance = map(model, broom::glance)) %>%
    unnest(glance) %>%
    mutate(significance = ifelse(p.value < 0.05,
                                 ifelse(statistic > 0, 'ns', 'significant'),
                                 'ns'),
           cover_type = 'Cover')

  return(result)
}


# Two-sample t-test for Grass ----
#' Perform Two-sample t-test for Grass Cover
#'
#' @param parcel_year_meta_2samp A data frame containing two-sample t-test meta data for parcels.
#' @return A data frame with t-test results and significance for grass cover.
#' @export
#' @examples
#' two_sample_ttest_grass(parcel_year_meta_2samp)
two_sample_ttest_grass <- function(parcel_year_meta_2samp) {
  result <- parcel_year_meta_2samp %>%
    mutate(model = map2(data.x, data.y, t2samp_grass)) %>%
    mutate(glance = map(model, broom::glance)) %>%
    unnest(glance) %>%
    mutate(significance = ifelse(p.value < 0.05,
                                 ifelse(statistic > 0, 'ns', 'significant'),
                                 'ns'),
           cover_type = 'Grass')

  return(result)
}


# One-sample t-test for Cover ----
#' Perform One-sample t-test for Cover
#'
#' @param data.x A data frame containing cover data for the test.
#' @param data.y A data frame containing cover data for the baseline mean.
#' @return A list containing the results of the t-test or NA_real_ if the test fails.
#' @export
#' @examples
#' t1samp(data.x, data.y)
t1samp <- function(data.x, data.y) {
  failproof.t <- purrr::possibly(t.test, NA_real_)
  result <- failproof.t(data.x$Cover, mu = mean(data.y$Cover), conf.level = .95)
  return(result)
}


# One-sample t-test for Grass ----
#' Perform One-sample t-test for Grass Cover
#'
#' @param data.x A data frame containing grass cover data for the test.
#' @param data.y A data frame containing grass cover data for the baseline mean.
#' @return A list containing the results of the t-test or NA_real_ if the test fails.
#' @export
#' @examples
#' t1samp_grass(data.x, data.y)
t1samp_grass <- function(data.x, data.y) {
  failproof.t <- purrr::possibly(t.test, NA_real_)
  result <- failproof.t(data.x$Grass, mu = mean(data.y$Grass), conf.level = .95)
  return(result)
}


# One-sample t-test ----
#' Perform One-sample t-test for Cover
#'
#' @param parcel_year_meta_1samp A data frame containing one-sample t-test meta data for parcels.
#' @return A data frame with t-test results and significance for cover.
#' @export
#' @examples
#' one_sample_ttest(parcel_year_meta_1samp)
one_sample_ttest <- function(parcel_year_meta_1samp) {
  result <- parcel_year_meta_1samp %>%
    mutate(model = map2(data.x, data.y, t1samp)) %>%
    mutate(glance = map(model, broom::glance)) %>%
    unnest(glance) %>%
    mutate(significance = ifelse(p.value < 0.05,
                                 ifelse(statistic > 0, 'ns', 'significant'),
                                 'ns'),
           cover_type = 'Cover')

  return(result)
}


# One-sample t-test for Grass ----
#' Perform One-sample t-test for Grass Cover
#'
#' @param parcel_year_meta_1samp A data frame containing one-sample t-test meta data for parcels.
#' @return A data frame with t-test results and significance for grass cover.
#' @export
#' @examples
#' one_sample_ttest_grass(parcel_year_meta_1samp)
one_sample_ttest_grass <- function(parcel_year_meta_1samp) {
  result <- parcel_year_meta_1samp %>%
    mutate(model = map2(data.x, data.y, t1samp_grass)) %>%
    mutate(glance = map(model, broom::glance)) %>%
    unnest(glance) %>%
    mutate(significance = ifelse(p.value < 0.05,
                                 ifelse(statistic > 0, 'ns', 'significant'),
                                 'ns'),
           cover_type = 'Grass')

  return(result)
}




# Append - bind_rows of t-tests ----
#' Combine and Count Significant Runs of t-tests
#'
#' @param data2samp_g A data frame containing two-sample t-test results for grass.
#' @param data1samp_g A data frame containing one-sample t-test results for grass.
#' @param data2samp_c A data frame containing two-sample t-test results for cover.
#' @param data1samp_c A data frame containing one-sample t-test results for cover.
#' @return A data frame with combined t-test results and counts of significant runs.
#' @export
#' @examples
#' bindttest_count_sig_runs(data2samp_g, data1samp_g, data2samp_c, data1samp_c)
bindttest_count_sig_runs <- function(data2samp_g, data1samp_g, data2samp_c, data1samp_c) {
  bound <- bind_rows(data2samp_g, data1samp_g, data2samp_c, data1samp_c)

  count.runs <- bound %>%
    mutate(indicator = case_when(
      significance == 'significant' ~ 1,
      significance == 'ns' ~ 0
    )) %>%
    group_by(Parcel, cover_type, grp = with(rle(indicator), rep(seq_along(lengths), lengths))) %>%
    mutate(counter = seq_along(grp)) %>%
    ungroup() %>%
    select(-grp) %>%
    mutate(sig.counter = indicator * counter)

  return(count.runs)
}


# Summarize Years in a Row Significant ----

#' Summarize Parcel Test Results
#'
#' @param data A data frame containing parcel test results with indicators and significance counters.
#' @return A summarized data frame with counts and proportions of significant years.
#' @export
#' @examples
#' parcel_testadd_sums(data)
parcel_testadd_sums <- function(data) {
  summary <- data %>%
    group_by(Parcel, cover_type) %>%
    summarise(
      n = n(),
      sum.sig = sum(indicator),
      max.run = max(sig.counter),
      prop.sig = round(sum.sig / n, 2)
    )

  return(summary)
}



# Rectangularize - joins----
#'
#' @param parcels_deltas A data frame containing parcel deltas.
#' @param attributes_pfix A data frame containing fixed attributes.
#' @param parcel_year_meta_combined_results A data frame with combined meta results for the parcel year.
#' @param parcel_test_sums A data frame with summary results of the parcel tests.
#' @param cYear The current year.
#'
#' @return A data frame with joined summaries.
#' @export
#'
#' @examples
#' join_summaries(parcels_deltas, attributes_pfix, parcel_year_meta_combined_results, parcel_test_sums, cYear)
join_summaries <- function(parcels_deltas, attributes_pfix, parcel_year_meta_combined_results, parcel_test_sums, cYear) {

  deltas <- parcels_deltas %>%
    filter(Year == cYear) %>%
    select(Parcel, TLC, Cover, Cover.Delta, Grass, pGrass, Grass.Delta, Shrub, pShrub, Shrub.Delta) %>%
    mutate(across(Cover:Shrub.Delta, round, 1))

  test <- parcel_year_meta_combined_results %>%
    filter(Year.x == cYear) %>%
    select(Parcel, Year.x, n.x, n.y, significance, sig.counter, cover_type) %>%
    pivot_wider(
      names_from = cover_type,
      names_glue = "{cover_type}_{.value}",
      values_from = c(significance, sig.counter)
    )

  sums <- parcel_test_sums %>%
    pivot_wider(
      names_from = cover_type,
      names_glue = "{cover_type}_{.value}",
      values_from = c(sum.sig, max.run, prop.sig)
    )

  par_delta_test_att <- deltas %>%
    left_join(test, by = 'Parcel') %>%
    left_join(attributes_pfix, by = 'Parcel') %>%
    left_join(sums, by = 'Parcel')

  return(par_delta_test_att)
}

# Summary Tables----
#' Create Parcel Data Table
#'
#' @param deltas_ttest_att A data frame containing the results of the t-tests.
#' @param cYear The current year.
#' @return A datatable object for parcel summaries.
#' @export
#' @examples
#' make_parcel_data_table(deltas_ttest_att, cYear)
make_parcel_data_table <- function(deltas_ttest_att, cYear) {
  table <- deltas_ttest_att %>%
    select(
      Parcel, GB_TYPE, Holland, wellfield, Type, Cover.Delta,
      Cover_significance, Grass.Delta, Grass_significance,
      pGrass, pShrub, Cover, Grass, Shrub
    ) %>%
    datatable(
      filter = 'top',
      options = list(
        dom = 'Blfrtip',
        buttons = c('copy', 'csv', 'excel', 'pdf', 'print'),
        scrollX = TRUE,
        fixedColumns = list(leftColumns = 2, rightColumns = 0),
        pageLength = 5,
        autoWidth = TRUE,
        colReorder = TRUE
      )
    )

  return(table)
}



#' Create Significant Parcel Data Table
#'
#' @param deltas_ttest_att A data frame containing the results of the t-tests.
#' @param cYear The current year.
#' @return A datatable object for significant parcels.
#' @export
#' @examples
#' make_parcel_data_table_significant(deltas_ttest_att, cYear)
make_parcel_data_table_significant <- function(deltas_ttest_att, cYear) {
  table <- deltas_ttest_att %>%
    filter(Cover_significance == 'significant' | Grass_significance == 'significant', Type == 'W') %>%
    select(
      Parcel, Cover.Delta, Cover_sig.counter, Grass.Delta, Grass_sig.counter, n,
      GB_TYPE, Holland, pGrass, pShrub, Cover, Grass, Shrub
    ) %>%
    datatable(
      filter = 'top',
      options = list(
        dom = 'Blfrtip',
        buttons = c('copy', 'csv', 'excel', 'pdf', 'print'),
        scrollX = TRUE,
        fixedColumns = list(leftColumns = 2, rightColumns = 0),
        pageLength = 5,
        autoWidth = TRUE,
        colReorder = TRUE
      )
    )

  return(table)
}


#' Create Chronic Parcel Data Table
#'
#' @param deltas_ttest_att A data frame containing the results of the t-tests.
#' @param cYear The current year.
#' @return A datatable object for chronic parcels.
#' @export
#' @examples
#' make_parcel_data_table_chronic(deltas_ttest_att, cYear)
make_parcel_data_table_chronic <- function(deltas_ttest_att, cYear) {
  table <- deltas_ttest_att %>%
    filter(Cover_sig.counter > 5 | Grass_sig.counter > 5, Type == 'W') %>%
    select(
      Parcel, Cover.Delta, Grass.Delta, Cover_sig.counter, Grass_sig.counter, n,
      Cover_prop.sig, Grass_prop.sig, GB_TYPE, Holland, pGrass, pShrub, Cover, Grass, Shrub
    ) %>%
    datatable(
      filter = 'top',
      options = list(
        dom = 'Blfrtip',
        buttons = c('copy', 'csv', 'excel', 'pdf', 'print'),
        scrollX = TRUE,
        fixedColumns = list(leftColumns = 2, rightColumns = 0),
        pageLength = 5,
        autoWidth = TRUE,
        colReorder = TRUE
      )
    )

  return(table)
}




# Make Maps ----
#' Create Panel Maps
#'
#' @param cYear The current year.
#' @param parcels_shp_ttest A data frame containing parcel shapefiles with test results.
#' @param wf Wellfield name to filter parcels.
#' @param or A shapefile for the river.
#' @param streams A shapefile for streams.
#' @param canals A shapefile for canals.
#' @param laa A shapefile for the Los Angeles Aqueduct.
#' @param lakes A shapefile for lakes.
#' @param monit.sites A shapefile for monitoring sites.
#'
#' @return A tmap object with arranged maps.
#' @export
#' @examples
panel_map <- function(cYear, parcels_shp_ttest, wf, or, streams, canals, laa, lakes, monit.sites) {

  # Filter parcels for the specified wellfield
  limit <- parcels_shp_ttest %>% filter(grepl(wf, wellfield))

  tmap_mode("plot")

  # Create grass cover map
  tmgrass <-
    tm_shape(limit, group = 'Wellfield - Parcels') +
    tm_polygons(col = "Grass", breaks = c(0, 5, 10, 15, 20, 25, 30, 35, 40, 50, 60, Inf), palette = "Greens", title.col = "PCL_merged", id = "PCL_merged", group = "Wellfield - Parcels")

  # Create grass delta map
  tmgrassd <-
    tm_shape(limit, group = 'Wellfield - Parcels') +
    tm_polygons(col = "Grass.Delta", breaks = c(-40, -30, -20, -10, -5, 5, 10, 20, 30, 40, Inf), palette = "RdYlGn", title.col = "PCL_merged", id = "PCL_merged", group = "Wellfield - Parcels") +

    tm_shape(canals, group = 'Canals') +
    tm_lines(col = "blue", scale = .6, group = 'Canals') +

    tm_shape(streams, group = 'Streams') +
    tm_lines(col = "blue", scale = 1, group = 'Streams') +

    tm_shape(monit.sites, group = 'On/Off Monitoring Sites') +
    tm_text("SITE", col = "white", size = .5, remove.overlap = TRUE, shadow = TRUE, group = 'Labels', auto.placement = .1, bg.color = 'blue') +
    tm_symbols(col = "blue", scale = .05, title.col = "SITE", id = "SITE", group = 'On/Off Monitoring Sites') +

    tm_shape(or, group = 'River') +
    tm_lines(col = "blue", scale = 1, group = 'River') +

    tm_shape(laa, group = 'LAA') +
    tm_lines(col = "blue", scale = 1, group = 'LAA') +

    tm_shape(lakes, group = 'Lakes') +
    tm_polygons(col = "blue", scale = 1, group = 'Lakes')

  # Create cover map
  tmcov <- tm_shape(limit, group = 'Wellfield - Parcels') +
    tm_polygons(col = "Cover", breaks = c(0, 5, 10, 15, 20, 25, 30, 35, 40, 50, 60, Inf), palette = "Greens", title.col = "PCL_merged", id = "PCL_merged", group = "Wellfield - Parcels")

  # Create cover delta map
  tmcovd <-
    tm_shape(limit, group = 'Wellfield - Parcels') +
    tm_polygons(col = "Cover.Delta", breaks = c(-40, -30, -20, -10, -5, 5, 10, 20, 30, 40, Inf), palette = "RdYlGn", title.col = "PCL_merged", id = "PCL_merged", group = "Wellfield - Parcels") +

    tm_shape(canals, group = 'Canals') +
    tm_lines(col = "blue", scale = .6, group = 'Canals') +

    tm_shape(streams, group = 'Streams') +
    tm_lines(col = "blue", scale = .7, group = 'Streams') +

    tm_shape(monit.sites, group = 'On/Off Monitoring Sites') +
    tm_text("SITE", col = "white", size = .5, remove.overlap = TRUE, shadow = TRUE, group = 'Labels', auto.placement = .1, bg.color = 'blue') +
    tm_symbols(col = "blue", scale = .05, title.col = "SITE", id = "SITE", group = 'On/Off Monitoring Sites') +

    tm_shape(or, group = 'River') +
    tm_lines(col = "blue", scale = 1, group = 'River') +

    tm_shape(laa, group = 'LAA') +
    tm_lines(col = "blue", scale = 1, group = 'LAA') +

    tm_shape(lakes, group = 'Lakes') +
    tm_polygons(col = "blue", scale = 1, group = 'Lakes')

  # Arrange maps into a panel
  map <- tmap_arrange(tmgrass, tmgrassd, tmcov, tmcovd, ncol = 2)

  return(map)
}



# Write to output/update master in data directory ----

#' Save Processed Data to Output Directory and Return Path
#'
#' @param processed A data frame containing the processed data.
#' @param cYear The current year.
#' @param entity The entity name.
#' @return The file path of the saved CSV file.
#' @export
#' @examples
#' save_csv_and_return_path(processed, 2023, "entity_name")
save_csv_and_return_path <- function(processed, cYear, entity) {
  file_path <- paste0("output/", entity, "_lpt_", cYear, ".csv")
  processed %>% write_csv(file_path)
  return(file_path)
}

#' Save Master Data to Data Directory and Return Path
#'
#' @param processed A data frame containing the processed data.
#' @param cYear The current year.
#' @param entity The entity name.
#' @return The file path of the saved master CSV file.
#' @export
#' @examples
#' save_master_csv_and_return_path(processed, 2023, "entity_name")
save_master_csv_and_return_path <- function(processed, cYear, entity) {
  file_path <- paste0("data/lpt_MASTER_", cYear, ".csv")
  processed %>% write_csv(file_path)
  return(file_path)
}
