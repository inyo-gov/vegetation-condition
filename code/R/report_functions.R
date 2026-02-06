# Maps view interactive----
#' Title
#'
#' @param cYear
#' @param parcels_shp_ttest
#' @param wf
#' @param or
#' @param streams
#' @param canals
#' @param laa
#' @param lakes
#' @param monit.sites
#'
#' @return
#' @export
#'
#' @examples
panel_map_view<- function(cYear,parcels_shp_ttest, or,streams,canals,laa,lakes, monit.sites){
  # wf,
  # write function to handle custom plot with input as string e.g. 'Laws'
  limit <- parcels_shp_ttest
  # %>% filter(grepl(wf,wellfield))

  tpc.below <- limit %>% filter(Cover_significance == 'significant')

  pgr.below <- limit %>% filter(Grass_significance == 'significant')


  tmap_mode("view")


  tm_shape(limit,
           group = 'Wellfield - Parcels') +
    tm_polygons(col =c("Grass"),
                breaks = c(0,5,10,15,20,25,30,35,40,50,60,Inf),
                palette = "Greens",
                title.col = "PCL_merged",
                id = "PCL_merged",
                popup.vars = c("GB_TYPE","Ecologic_3","COMM_NAME","Grass.Delta","Cover.Delta", "NDVI.delta","NDVI.Baseline","Type"),
                group = "Wellfield - Parcels")+

    tm_shape(pgr.below,
             group = 'Wellfield - Parcels') +
    tm_borders(col = 'red',lwd = 2)+
    tm_text("PCL", size = .5,  col = "white",shadow=TRUE,remove.overlap=FALSE, group = 'Labels', auto.placement = .2, bg.color = 'darkgreen')

  tm_shape(limit, group = 'Wellfield - Parcels') +
    tm_polygons(col =c("Grass.Delta"), breaks = c(-40,-30,-20,-10,-5,5,10,20,30,40,Inf),palette = "RdYlGn",title.col = "PCL_merged",  id = "PCL_merged",popup.vars = c("GB_TYPE","Ecologic_3","COMM_NAME","Grass.Delta","Cover.Delta", "NDVI.delta","NDVI.Baseline","Type"), group = "Wellfield - Parcels")+

    tm_shape(pgr.below, group = 'Wellfield - Parcels') +
    tm_borders(col = 'red',lwd = 1)+

    # cover delta
    tm_shape(limit, group = 'Wellfield - Parcels') +
    tm_polygons(col =c("Cover"), breaks = c(0,5,10,15,20,25,30,35,40,50,60,Inf),palette = "Greens",title.col = "PCL_merged",  id = "PCL_merged",popup.vars = c("GB_TYPE","Ecologic_3","COMM_NAME","Grass.Delta","Cover.Delta", "NDVI.delta","NDVI.Baseline","Type"), group = "Wellfield - Parcels")+

    tm_shape(tpc.below, group = 'Wellfield - Parcels') +
    tm_borders(col = 'red',lwd = 2)+
    tm_text("PCL",  size = .5,col = "white",shadow=TRUE,remove.overlap=FALSE, group = 'Labels', auto.placement = .2, bg.color = 'darkgreen')

  tm_shape(limit, group = 'Wellfield - Parcels') +
    tm_polygons(col =c("Cover.Delta"), breaks = c(-40,-30,-20,-10,-5,5,10,20,30,40,Inf),palette = "RdYlGn",title.col = "PCL_merged",  id = "PCL_merged",popup.vars = c("GB_TYPE","Ecologic_3","COMM_NAME","Grass.Delta","Cover.Delta", "NDVI.delta","NDVI.Baseline","Type"), group = "Wellfield - Parcels")+

    tm_shape(tpc.below, group = 'Wellfield - Parcels') +
    tm_borders(col = 'red')+

    # on/off sites
    tm_shape(monit.sites, group = 'On/Off Monitoring Sites') +
    tm_text("SITE",  col = "white", size=.5,remove.overlap=TRUE,shadow=TRUE,group = 'Labels',auto.placement = .1,bg.color = 'blue') +
    tm_symbols(col = "blue", scale = .05, title.col = "SITE",  id = "SITE",popup.vars = c("SITE","TYPE"),group = 'On/Off Monitoring Sites')+

    # background reference layers
    tm_shape(canals, group = 'Canals') +
    tm_lines(col = "blue", scale = .6, group = 'Canals')+

    tm_shape(streams, group = 'Streams') +
    tm_lines(col = "blue", scale = 1, group = 'Streams')+

    tm_shape(or, group = 'River') +
    tm_lines(col = "blue", scale = 1, group = 'River')+

    tm_shape(laa, group = 'LAA') +
    tm_lines(col = "blue", scale = 1, group = 'LAA')+

    tm_shape(lakes, group = 'Lakes') +
    tm_polygons(col = "blue", scale = 1, group = 'Lakes')


  return(map)


}

#' Count Wellfield and Control Parcels
#'
#' This function counts the number of distinct parcels for each type (e.g., wellfield, control).
#'
#' @param data A data frame containing parcel data with at least two columns: Parcel and Type.
#'
#' @return A data frame with the count of distinct parcels for each type.
#' @export
#'
#' @examples
#' count_wellfield_control_parcels(data)
count_wellfield_control_parcels <- function(data) {
  data %>%
    # Select relevant columns
    select(Parcel, Type) %>%
    # Group by Type
    group_by(Type) %>%
    # Summarize to count distinct parcels
    summarise(n = n_distinct(Parcel))
}



#' Plot Perennial Cover Time Series
#'
#' This function plots the perennial cover time series from 1991 to the current year
#' relative to the baseline (1984-1987). It performs Welch Two Sample t-tests to
#' assess the significance of deviations from the baseline.
#'
#' @param data A data frame containing the time series data.
#' @param cYear The current year for the plot title.
#' @param parcel.select A vector of selected parcels to filter the data.
#'
#' @return A ggplot object showing the perennial cover time series.
#' @export
#'
#' @examples
#' plot_2samptest_timeseries(data, 2023, c("Parcel1", "Parcel2"))
plot_2samptest_timeseries <- function(data, cYear, parcel.select) {
  # Filter data based on selected parcels
  data <- data %>% filter(Parcel %in% parcel.select)

  # Create the plot
  plot <- data %>%
    ggplot(aes(x = Year.x)) +
    ggtitle(paste(
      "Perennial Cover Time Series (1991-", cYear, ") Relative to Baseline (1984-1987).\n",
      "Welch Two Sample t-test (ns p > 0.05, significant p < 0.05)"
    )) +
    geom_errorbar(aes(ymin = conf.low, ymax = conf.high, color = significance, width = 0.2)) +
    geom_line(aes(y = estimate1)) + # reinventory year
    scale_color_manual(values = c("green", "orange", "blue", "red")) +
    geom_line(aes(y = estimate, color = 'Deviation from Baseline\n (relative to zero)')) + # current year
    geom_line(aes(y = estimate2, color = 'Baseline')) + # baseline year
    ylab("Perennial Cover (%)") +
    facet_wrap(~ Parcel, ncol = 1) +
    theme(legend.position = "top")

  return(plot)
}


#' Create Boxplots and Compute Paired t-tests
#'
#' This function creates boxplots for perennial cover in control and wellfield parcels,
#' and computes paired t-tests to compare cover between two specified years.
#'
#' @param attributes A data frame containing parcel attributes.
#' @param parcels A data frame containing parcel data.
#' @param comparison_year The year to compare against the reference year.
#' @param reference_year The baseline year for comparison.
#'
#' @return A ggplot object with boxplots and statistical comparisons.
#' @export
#'
#' @examples
#' boxplot_wc_pft_year_statcompare1(attributes, parcels, 2023, 1987)
boxplot_wc_pft_year_statcompare1 <- function(attributes, parcels, comparison_year, reference_year) {
  # Select unique parcels with their types
  a <- attributes %>%
    select(Parcel, Type) %>%
    unique()

  # Join parcel data with attributes
  parcels2 <- parcels %>%
    unique() %>%
    left_join(a, by = "Parcel")

  # Select columns and filter to comparison years
  parcels.select.compare <- parcels2 %>%
    select(Parcel, NominalYear, Cover) %>%
    filter(NominalYear %in% c(reference_year, comparison_year))

  # Pivot wider with years as columns to keep rows with data in both years
  parcels.spread <- parcels.select.compare %>%
    pivot_wider(names_from = NominalYear, values_from = Cover, names_prefix = "y")

  # Select only rows with data in both the reference year and comparison year
  Pairwise <- parcels.spread %>%
    rowwise() %>%
    filter(!any(is.na(c_across(starts_with("y")))))

  # Select parcel attributes to join
  att <- attributes %>%
    select(Parcel, Type, wellfield, GB_TYPE, Holland)

  pa <- Pairwise %>%
    left_join(att, by = "Parcel")

  # Count control and wellfield parcels
  wpa <- pa %>% filter(Type == 'W') %>% nrow()
  cpa <- pa %>% filter(Type == 'C') %>% nrow()

  title.construct <- paste0("Group size: Control (n=", cpa, ") and Wellfield (n=", wpa, ")")

  p <- parcels2 %>%
    unique() %>%
    filter(Parcel %in% Pairwise$Parcel, NominalYear %in% c(reference_year, comparison_year)) %>%
    select(Parcel, NominalYear, Year, Type, Cover, Shrub, Grass) %>%
    gather(Cover.Type, Cover, Cover:Grass) %>%
    filter(!is.na(Type)) %>%
    mutate(NomYear = as.factor(NominalYear))

  # Create paired ggplot component
  ggpaired_component <- ggpaired(
    p,
    x = "NomYear",
    y = "Cover",
    id = 'Parcel',
    color = "Type",
    notch = TRUE,
    palette = "jco",
    line.color = "lightgray",
    line.size = 0.1,
    facet.by = c('Type', 'Cover.Type'),
    title = title.construct,
    ylab = "Perennial Cover",
    xlab = "Time Period Statistical Comparison",
    panel.labs = list(Type = c("Control", "Wellfield"))
  )

  # Add statistical comparison
  ggpaired_stat_compare_plot <- ggpaired_component +
    stat_compare_means(
      method = 't.test',
      paired = TRUE,
      label = "p.signif"
    )

  return(ggpaired_stat_compare_plot)
}




#' Create Boxplots for Perennial Cover Time Series with Statistical Comparison
#'
#' This function creates boxplots for perennial cover in control and wellfield parcels,
#' and computes paired t-tests to compare cover between different years.
#'
#' @param attributes A data frame containing parcel attributes.
#' @param parcels A data frame containing parcel data.
#'
#' @return A ggplot object with boxplots and statistical comparisons.
#' @export
#'
#' @examples
#' boxplot_wc_pft_timeseries_statcompare(attributes, parcels)
boxplot_wc_pft_timeseries_statcompare <- function(attributes, parcels) {
  # Select unique parcels with their types
  a <- attributes %>%
    select(Parcel, Type) %>%
    unique()

  # Join parcel data with attributes
  parcels2 <- parcels %>%
    unique() %>%
    left_join(a, by = "Parcel")

  # Select columns and filter to comparison years
  parcels.select.compare <- parcels2 %>%
    select(Parcel, NominalYear, Cover)

  # Pivot wider with years as columns to keep rows with data in both years
  parcels.spread <- parcels.select.compare %>%
    pivot_wider(names_from = NominalYear, values_from = Cover, names_prefix = "y")

  # Select only rows with data in both the reference year and comparison year
  Pairwise <- parcels.spread %>%
    rowwise() %>%
    filter(!any(is.na(c_across(starts_with("y")))))

  # Select parcel attributes to join
  att <- attributes %>%
    select(Parcel, Type, wellfield, GB_TYPE, Holland)

  # Join attributes
  pa <- Pairwise %>%
    left_join(att, by = "Parcel")

  # Count control and wellfield parcels
  wpa <- pa %>% filter(Type == 'W') %>% nrow()
  cpa <- pa %>% filter(Type == 'C') %>% nrow()

  # Construct title for the plot
  title.construct <- paste0("Group Comparison of Control (n=", cpa, ") and Wellfield (n=", wpa, ") Parcels.")

  # Prepare data for plotting
  p <- parcels %>%
    unique() %>%
    filter(Parcel %in% Pairwise$Parcel) %>%
    select(Parcel, NominalYear, Year, Type, Cover, Shrub, Grass) %>%
    gather(Cover.Type, Cover, Cover:Grass) %>%
    filter(!is.na(Type)) %>%
    mutate(NomYear = as.factor(NominalYear))

  # Create paired ggplot component
  ggpaired_component <- ggpaired(
    p,
    x = "NomYear",
    y = "Cover",
    id = 'Parcel',
    color = "Type",
    notch = TRUE,
    label = "Parcel",
    label.select = list(top.up = 2),
    repel = TRUE,
    font.label = list(size = 9, face = "plain"),
    palette = "jco",
    line.color = "gray",
    line.size = 0.1,
    facet.by = c('Type', 'Cover.Type'),
    title = title.construct,
    ylab = "Perennial Cover",
    xlab = "Time Period Statistical Comparison",
    panel.labs = list(Type = c("Control", "Wellfield"))
  )

  # Add statistical comparison
  ggpaired_stat_compare_plot <- ggpaired_component +
    stat_compare_means(
      method = 't.test',
      paired = TRUE,
      label = "p.signif"
    )

  return(ggpaired_stat_compare_plot)
}



#' Create Wellfield Table
#'
#' This function creates a summary table for a specified wellfield, highlighting
#' parcels with significant deviations in cover and grass metrics.
#'
#' @param data A data frame containing the wellfield data.
#' @param wf The wellfield to filter the data by.
#'
#' @return A gt table object summarizing the wellfield data.
#' @export
#'
#' @examples
#' make_wellfield_table(data, "Laws")
make_wellfield_table <- function(data, wf) {
  data %>%
    # Filter for wellfield parcels and the specified wellfield
    filter(Type == 'W') %>%
    filter(wellfield == wf) %>%
    # Select relevant columns
    select(Parcel, wellfield, GB_TYPE, Holland, Cover.Delta, Grass.Delta, Cover_sig.counter, Grass_sig.counter, n, Cover, Grass, Shrub) %>%
    # Create gt table grouped by wellfield
    gt(groupname_col = "wellfield") %>%
    # Add summary row for Cover.Delta with average function
    summary_rows(
      groups = TRUE,
      columns = Cover.Delta,
      fns = list(Average = "mean"),
      decimals = 1
    ) %>%
    # Add footnote for Grass_sig.counter
    tab_footnote(
      footnote = "Perennial grass cover below baseline for five years or more.",
      locations = cells_body(
        columns = Grass_sig.counter,
        rows = Grass_sig.counter > 4
      )
    ) %>%
    # Add footnote for Cover_sig.counter
    tab_footnote(
      footnote = "Total perennial cover below baseline for five years or more.",
      locations = cells_body(
        columns = Cover_sig.counter,
        rows = Cover_sig.counter > 4
      )
    )
}




#' Create Grass Map for Wellfield Parcels
#'
#' This function generates a map showing grass cover in wellfield parcels, highlighting
#' parcels with significant deviations, along with canals, streams, monitoring sites, and other features.
#'
#' @param cYear The current year for the map title.
#' @param parcels_shp_ttest A spatial data frame containing the parcels data with significance tests.
#' @param wf The wellfield to filter the data by.
#' @param or The river data.
#' @param streams The streams data.
#' @param canals The canals data.
#' @param laa The Los Angeles Aqueduct data.
#' @param lakes The lakes data.
#' @param monit.sites The monitoring sites data.
#'
#' @return A tmap object showing the grass cover in wellfield parcels.
#' @export
#'
#' @examples
#' grass_map(cYear, parcels_shp_ttest, "Laws", or, streams, canals, laa, lakes, monit.sites)
grass_map <- function(cYear, parcels_shp_ttest, wf, or, streams, canals, laa, lakes, monit.sites) {
  # Filter parcels for the specified wellfield
  limit <- parcels_shp_ttest %>% filter(grepl(wf, wellfield))

  # Identify parcels with significant cover and grass changes
  tpc.below <- limit %>% filter(Cover_significance == 'significant')
  pgr.below <- limit %>% filter(Grass_significance == 'significant')

  # Set tmap mode to plot
  tmap_mode("plot")

  # Create the grass cover map
  tmgrass <- tm_shape(limit, group = 'Wellfield - Parcels') +
    tm_polygons(col = "Grass", breaks = c(0, 10, 20, 30, 40, Inf), palette = "Greens", title.col = "PCL", id = "PCL") +
    tm_shape(pgr.below, group = 'Wellfield - Parcels') +
    tm_borders(col = 'red', lwd = .5) +
    tm_text("PCL", size = .6, col = "black", shadow = FALSE, remove.overlap = FALSE, group = 'Labels', auto.placement = 1, bg.color = "white") +
    tm_shape(canals, group = 'Canals') +
    tm_lines(col = "blue", scale = .5, group = 'Canals') +
    tm_shape(streams, group = 'Streams') +
    tm_lines(col = "blue", scale = .8, group = 'Streams') +
    tm_shape(monit.sites, group = 'On/Off Monitoring Sites') +
    tm_text("SITE", col = "black", size = .8, remove.overlap = TRUE, shadow = FALSE, group = 'Labels', auto.placement = TRUE, bg.color = 'yellow') +
    tm_symbols(col = "yellow", scale = .1, title.col = "SITE", id = "SITE") +
    tm_shape(or, group = 'River') +
    tm_lines(col = "blue", scale = .5, group = 'River') +
    tm_shape(laa, group = 'LAA') +
    tm_lines(col = "blue", scale = 1, group = 'LAA')

  return(tmgrass)
}

