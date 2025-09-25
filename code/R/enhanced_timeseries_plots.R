###############################################################################
## Enhanced Time Series Plots with Perennial Grass Analysis
##
## This script extends the existing plotting functionality to:
## 1. Add horizontal lines at baseline values
## 2. Create perennial grass only datasets
## 3. Add perennial grass time series to plots
## 4. Include least squares trend analysis with significance testing
###############################################################################

library(ggplot2)
library(dplyr)
library(tidyr)
library(patchwork)
library(broom)

# Import functions from the original plotting file
source("code/R/gg_timeseries_plots.R")

# Helper function to determine plot title based on attributes
get_title <- function(PID, attributes) {
  attr <- attributes %>% filter(.data$Parcel == PID)
  type_label <- ifelse(attr$Type == "W", "Wellfield", "Control")
  title <- paste(PID, "-", type_label, "- Holland Type:", attr$GB_TYPE, "- Green Book Type:", attr$Type, "- NRCS Ecological Site:", attr$Ecologic_3)
  return(title)
}

# Enhanced baseline test function
baseline_test <- function(baseline_vec, new_vec) {
  baseline_vec <- na.omit(baseline_vec)
  new_vec <- na.omit(new_vec)

  # Check if we have enough data for testing
  if (length(baseline_vec) == 0 || length(new_vec) == 0) {
    return(NA_real_)
  }
  
  # Need at least 2 observations for t-test
  if (length(new_vec) < 2) {
    return(NA_real_)
  }

  tryCatch({
    if (all(baseline_vec == 0)) {
      fit <- t.test(new_vec, mu = 0, alternative = "less")
    } else if (length(baseline_vec) <= 4) {
      mu.d1 <- mean(baseline_vec, na.rm = TRUE)
      fit <- t.test(new_vec, mu = mu.d1, alternative = "less")
    } else {
      # For two-sample test, need at least 2 observations in each group
      if (length(baseline_vec) < 2) {
        mu.d1 <- mean(baseline_vec, na.rm = TRUE)
        fit <- t.test(new_vec, mu = mu.d1, alternative = "less")
      } else {
        fit <- t.test(new_vec, baseline_vec, alternative = "less")
      }
    }
    fit$p.value
  }, error = function(e) {
    return(NA_real_)
  })
}

# Function to calculate trend analysis
calculate_trend <- function(years, values) {
  if (length(na.omit(values)) < 3) {
    return(list(slope = NA, p_value = NA, r_squared = NA))
  }
  
  # Create data frame for linear model
  trend_data <- data.frame(
    year = years,
    value = values
  ) %>%
    filter(!is.na(.data$value))
  
  if (nrow(trend_data) < 3) {
    return(list(slope = NA, p_value = NA, r_squared = NA))
  }
  
  # Fit linear model
  model <- lm(value ~ year, data = trend_data)
  model_summary <- summary(model)
  
  return(list(
    slope = coef(model)[2],
    p_value = model_summary$coefficients[2, 4],
    r_squared = model_summary$r.squared
  ))
}

# Function to create perennial grass dataset
create_perennial_grass_dataset <- function(master_data) {
  perennial_grass_data <- master_data %>%
    filter(.data$Lifeform == "Grass" & .data$Lifecycle == "Perennial") %>%
    group_by(.data$Parcel, .data$Year, .data$Transect) %>%
    summarise(
      Perennial_Grass_Cover = sum(.data$Cover, na.rm = TRUE),
      .groups = 'drop'
    ) %>%
    group_by(.data$Parcel, .data$Year) %>%
    summarise(
      Perennial_Grass_Cover = mean(.data$Perennial_Grass_Cover, na.rm = TRUE),
      n_transects = n(),
      .groups = 'drop'
    )
  
  return(perennial_grass_data)
}

# Enhanced bar plot function with baseline line and trend analysis
plot_bar_discrete_enhanced <- function(
    PID,
    df,
    column_name,
    baseline_years = 1984:1988,
    year_min = 1984,
    year_max = 2024,
    ylab_text = "Cover (%)",
    fill_color = "darkgreen",
    show_trend = TRUE
) {
  # Filter & summarize
  df_sub <- df %>% filter(.data$Parcel == PID)
  if (nrow(df_sub) == 0) {
    return(ggplot() + theme_void())
  }
  
  # Summaries by year
  stats <- df_sub %>%
    group_by(.data$Year) %>%
    summarise(
      Mean = mean(.data[[column_name]], na.rm = TRUE),
      SEM = sqrt(var(.data[[column_name]], na.rm = TRUE) / length(na.omit(.data[[column_name]])))
    ) %>%
    ungroup()

  # Complete the sequence so missing years appear with NA
  stats <- stats %>%
    complete(Year = year_min:year_max)

  # Calculate p-values
  stats$pval <- NA_real_
  for (i in seq_len(nrow(stats))) {
    y <- stats$Year[i]
    if (!is.na(y) && !(y %in% baseline_years)) {
      baseline_data <- df_sub[[column_name]][df_sub$Year %in% baseline_years]
      new_data <- df_sub[[column_name]][df_sub$Year == y]
      stats$pval[i] <- baseline_test(baseline_data, new_data)
    }
  }

  # Baseline mean
  b_sub <- stats %>% filter(.data$Year %in% baseline_years)
  baseline_mean <- if (nrow(b_sub) > 0) mean(b_sub$Mean, na.rm = TRUE) else NA

  # Convert Year to factor
  stats$YearF <- factor(stats$Year, levels = year_min:year_max)

  # Calculate trend if requested
  trend_info <- NULL
  if (show_trend) {
    trend_info <- calculate_trend(stats$Year, stats$Mean)
  }

  # Create base plot
  g <- ggplot(stats, aes(x = .data$YearF, y = .data$Mean)) +
    geom_col(
      fill = fill_color,
      na.rm = TRUE,
      width = 0.9
    ) +
    geom_errorbar(
      aes(ymin = .data$Mean - 2*.data$SEM, ymax = .data$Mean + 2*.data$SEM),
      width = 0.4,
      na.rm = TRUE
    ) +
    labs(x = NULL, y = ylab_text) +
    theme_minimal(base_size = 11)

  # Add baseline horizontal line
  if (is.finite(baseline_mean)) {
    g <- g +
      geom_hline(yintercept = baseline_mean, linetype = "dashed", color = "red", size = 0.8) +
      annotate(
        "text", x = 1, y = baseline_mean,
        label = "Baseline", vjust = -0.5, size = 3, color = "red"
      )
  }

  # Add trend line if significant
  if (show_trend && !is.na(trend_info$p_value) && trend_info$p_value < 0.05) {
    # Create trend line data
    trend_data <- data.frame(
      YearF = factor(year_min:year_max, levels = year_min:year_max),
      trend_value = trend_info$slope * (year_min:year_max) + 
                   (mean(stats$Mean, na.rm = TRUE) - trend_info$slope * mean(stats$Year, na.rm = TRUE))
    )
    
    g <- g +
      geom_line(data = trend_data, aes(y = .data$trend_value, group = 1), 
                color = "blue", size = 1, alpha = 0.7)
  }

  # Add significance asterisks
  stats_sig <- stats %>% filter(!is.na(.data$pval) & .data$pval < 0.05)
  if (nrow(stats_sig) > 0) {
    offset <- 0.03 * max(stats$Mean, na.rm = TRUE)
    g <- g +
      geom_text(
        data = stats_sig,
        aes(x = .data$YearF, y = .data$Mean + 2*.data$SEM + offset, label = "**"),
        color = "red",
        size = 4,
        na.rm = TRUE
      )
  }

  # Add trend information as text
  if (show_trend && !is.na(trend_info$slope)) {
    trend_text <- paste0(
      "Trend: ", round(trend_info$slope, 3), "/year",
      if (!is.na(trend_info$p_value)) {
        paste0(" (p=", round(trend_info$p_value, 3), ")")
      } else {
        ""
      }
    )
    
    g <- g +
      annotate("text", x = length(levels(stats$YearF)) * 0.7, 
               y = max(stats$Mean, na.rm = TRUE) * 0.9,
               label = trend_text, size = 3, color = "blue")
  }

  # x-axis breaks - start at 1985, then every 5 years
  brk <- seq(1985, year_max, by = 5)
  if (!(year_max %in% brk)) {
    brk <- c(brk, year_max)
  }
  g <- g + scale_x_discrete(breaks = factor(brk, levels = year_min:year_max))

  return(g)
}

# Enhanced function to create perennial grass plot
plot_perennial_grass_discrete <- function(
    PID,
    perennial_grass_data,
    baseline_years = 1984:1988,
    year_min = 1984,
    year_max = 2024
) {
  return(plot_bar_discrete_enhanced(
    PID = PID,
    df = perennial_grass_data,
    column_name = "Perennial_Grass_Cover",
    baseline_years = baseline_years,
    year_min = year_min,
    year_max = year_max,
    ylab_text = "Perennial Grass Cover (%)",
    fill_color = "darkolivegreen",
    show_trend = TRUE
  ))
}

# Enhanced six-panel plot function
plot_six_timeseries_enhanced <- function(
    PID,
    transects,
    perennial_grass_data,
    dtw_data,
    rs_data,
    year_min = 1984,
    year_max = 2024
) {
  # Perennial (existing)
  p_cover <- plot_bar_discrete_enhanced(
    PID, transects, column_name = "Cover",
    baseline_years = 1984:1988,
    year_min = year_min, year_max = year_max,
    ylab_text = "Perennial Cover (%)",
    fill_color = "forestgreen",
    show_trend = TRUE
  )

  # Shrub
  p_shrub <- plot_bar_discrete_enhanced(
    PID, transects, column_name = "Shrub",
    baseline_years = 1984:1988,
    year_min = year_min, year_max = year_max,
    ylab_text = "Shrub Cover (%)",
    fill_color = "firebrick",
    show_trend = TRUE
  )

  # Grass
  p_grass <- plot_bar_discrete_enhanced(
    PID, transects, column_name = "Grass",
    baseline_years = 1984:1988,
    year_min = year_min, year_max = year_max,
    ylab_text = "Grass Cover (%)",
    fill_color = "darkgreen",
    show_trend = TRUE
  )

  # Perennial Grass (new)
  p_perennial_grass <- plot_perennial_grass_discrete(
    PID, perennial_grass_data,
    baseline_years = 1984:1988,
    year_min = year_min, year_max = year_max
  )

  # DTW (existing)
  p_dtw <- plot_dtw_discrete(
    PID, dtw_data,
    baseline_years = 1984:1987,
    year_min = year_min, year_max = year_max
  )

  # NDVI (existing)
  p_ndvi <- plot_ndvi_discrete(
    PID, rs_data,
    baseline_year = 1986,
    year_min = year_min, year_max = year_max
  )

  # PPT (existing)
  p_ppt <- plot_ppt_discrete(
    PID, rs_data,
    baseline_year = 1986,
    year_min = year_min, year_max = year_max
  )

  # Stack them (patchwork)
  combo <- p_cover / p_shrub / p_grass / p_perennial_grass / p_dtw / p_ndvi / p_ppt +
    plot_layout(ncol = 1) +
    plot_annotation(title = PID, theme = theme(plot.title = element_text(hjust = 0.5)))

  return(combo)
}

# Function to create dataset lines plot (like the existing create_parcel_plot)
plot_dataset_lines <- function(
    PID,
    master_data,
    year_min = 1984,
    year_max = 2024
) {
  # Create the different datasets (Even, Odd, Whole) from master data
  master_processed <- master_data %>%
    filter(Parcel == PID, Lifecycle == "Perennial") %>%
    mutate(
      Transect_base = as.numeric(Transect),
      Dataset = case_when(
        Transect_base %% 2 == 0 ~ 'Even',
        Transect_base %% 2 == 1 ~ 'Odd',
        TRUE ~ 'Unknown'
      )
    ) %>%
    filter(Year >= 2015)  # Focus on recent years with Even/Odd data
  
  # Calculate cover by dataset: first sum within transects, then average across transects
  dataset_data <- master_processed %>%
    group_by(Parcel, Year, Dataset, Transect) %>%
    summarise(
      transect_cover = sum(Cover, na.rm = TRUE),  # Sum within transect
      .groups = 'drop'
    ) %>%
    group_by(Parcel, Year, Dataset) %>%
    summarise(
      total_cover = mean(transect_cover, na.rm = TRUE),  # Average across transects
      .groups = 'drop'
    )
  
  # Add baseline data (1984-1987) - use LADWP Baseline source
  baseline_data <- master_data %>%
    filter(Parcel == PID, Lifecycle == "Perennial", source == "LADWP Baseline 1984-1987") %>%
    group_by(Parcel, Year, Transect) %>%
    summarise(
      transect_cover = sum(Cover, na.rm = TRUE),  # Sum within transect
      .groups = 'drop'
    ) %>%
    group_by(Parcel, Year) %>%
    summarise(
      total_cover = mean(transect_cover, na.rm = TRUE),  # Average across transects
      .groups = 'drop'
    ) %>%
    mutate(Dataset = "LADWP Baseline 1984-1987")
  
  # Add ICWD historical data (1991-2014)
  icwd_historical_data <- master_data %>%
    filter(Parcel == PID, Lifecycle == "Perennial", source == "ICWD 1991-2014") %>%
    group_by(Parcel, Year, Transect) %>%
    summarise(
      transect_cover = sum(Cover, na.rm = TRUE),  # Sum within transect
      .groups = 'drop'
    ) %>%
    group_by(Parcel, Year) %>%
    summarise(
      total_cover = mean(transect_cover, na.rm = TRUE),  # Average across transects
      .groups = 'drop'
    ) %>%
    mutate(Dataset = "ICWD 1991-2014")
  
  # Add LADWP historical data (2004-2014)
  ladwp_historical_data <- master_data %>%
    filter(Parcel == PID, Lifecycle == "Perennial", source == "LADWP 2004-2014") %>%
    group_by(Parcel, Year, Transect) %>%
    summarise(
      transect_cover = sum(Cover, na.rm = TRUE),  # Sum within transect
      .groups = 'drop'
    ) %>%
    group_by(Parcel, Year) %>%
    summarise(
      total_cover = mean(transect_cover, na.rm = TRUE),  # Average across transects
      .groups = 'drop'
    ) %>%
    mutate(Dataset = "LADWP 2004-2014")
  
  # Add recent Joint Monitoring data (2015-current)
  joint_monitoring_data <- master_data %>%
    filter(Parcel == PID, Lifecycle == "Perennial", source == "Joint Monitoring 2015-current year") %>%
    group_by(Parcel, Year, Transect) %>%
    summarise(
      transect_cover = sum(Cover, na.rm = TRUE),  # Sum within transect
      .groups = 'drop'
    ) %>%
    group_by(Parcel, Year) %>%
    summarise(
      total_cover = mean(transect_cover, na.rm = TRUE),  # Average across transects
      .groups = 'drop'
    ) %>%
    mutate(Dataset = "Joint Monitoring 2015-current")
  
  # Create combined historical data (ICWD + LADWP combined for 1991-2014)
  # First get the individual ICWD and LADWP values, then average them
  icwd_values <- icwd_historical_data %>%
    select(Year, total_cover) %>%
    rename(icwd_cover = total_cover)
  
  ladwp_values <- ladwp_historical_data %>%
    select(Year, total_cover) %>%
    rename(ladwp_cover = total_cover)
  
  # Combine and average the ICWD and LADWP values for each year
  combined_historical_data <- icwd_values %>%
    full_join(ladwp_values, by = "Year") %>%
    rowwise() %>%
    mutate(
      total_cover = mean(c(icwd_cover, ladwp_cover), na.rm = TRUE)
    ) %>%
    ungroup() %>%
    select(Year, total_cover) %>%
    mutate(Dataset = "Combined Historical")
  
  # Combine all data - don't fill missing years with 0
  all_data <- bind_rows(dataset_data, baseline_data, icwd_historical_data, ladwp_historical_data, joint_monitoring_data, combined_historical_data) %>%
    filter(!is.na(total_cover)) %>%
    # Don't complete missing years - only show years with actual data
    filter(Year >= year_min, Year <= year_max)

  # Calculate baseline mean
  baseline_mean <- baseline_data %>%
    summarise(baseline = mean(total_cover, na.rm = TRUE)) %>%
    pull(baseline)

  # Create the dataset lines plot with discrete x-axis to match bottom plot
  all_data$YearF <- factor(all_data$Year, levels = year_min:year_max)
  
  p <- ggplot(all_data, aes(x = YearF, y = total_cover, color = Dataset)) +
    geom_line(aes(group = Dataset), size = 1.2) +
    geom_point(size = 3) +
    geom_hline(yintercept = baseline_mean, linetype = "dashed", color = "red", size = 0.8) +
    annotate("text", x = 1, y = baseline_mean, 
             label = "Baseline", vjust = -0.5, color = "red", size = 3) +
    labs(
      title = paste("Perennial Cover -", PID),
      x = "Year",
      y = "Perennial Cover (%)",
      color = "Dataset"
    ) +
    theme_minimal(base_size = 11) +
    theme(
      legend.position = "bottom",
      plot.title = element_text(hjust = 0.5)
    ) +
    scale_x_discrete(breaks = factor(seq(1985, year_max, by = 5), levels = year_min:year_max)) +
    scale_y_continuous()  # Let y-axis float to show full range

  return(p)
}

# New function to create combined perennial plots (dataset lines + grass bar plot)
plot_combined_perennial_analysis <- function(
    PID,
    master_data,
    perennial_grass_data,
    year_min = 1984,
    year_max = 2024
) {
  # Create dataset lines plot (top)
  p_dataset_lines <- plot_dataset_lines(
    PID = PID,
    master_data = master_data,
    year_min = year_min,
    year_max = year_max
  )

  # Create perennial grass bar plot with trend (bottom)
  p_perennial_grass <- plot_perennial_grass_discrete(
    PID, perennial_grass_data,
    baseline_years = 1984:1988,
    year_min = year_min, year_max = year_max
  ) +
    labs(title = paste("Perennial Grass Cover -", PID))

  # Combine with patchwork
  combined <- p_dataset_lines / p_perennial_grass +
    plot_layout(ncol = 1, heights = c(1, 1)) +
    plot_annotation(
      title = paste("Perennial Vegetation Analysis -", PID),
      theme = theme(plot.title = element_text(hjust = 0.5, size = 14))
    )

  return(combined)
}

# Function to create enhanced PDF with perennial grass analysis
create_enhanced_ts_plots_pdf <- function(
    PIDs,
    master_data,
    transects,
    dtw_data,
    rs_data,
    pdf_file = "Enhanced_Parcel_TimeSeries_With_PerennialGrass.pdf",
    current_year = 2024
) {
  # Create perennial grass dataset
  perennial_grass_data <- create_perennial_grass_dataset(master_data)
  
  pdf(pdf_file, width = 8.5, height = 11)
  on.exit(dev.off(), add = TRUE)

  for (pid in PIDs) {
    combo <- plot_six_timeseries_enhanced(
      PID = pid,
      transects = transects,
      perennial_grass_data = perennial_grass_data,
      dtw_data = dtw_data,
      rs_data = rs_data,
      year_min = 1984,
      year_max = current_year
    )
    print(combo)
  }

  message("Enhanced PDF saved to: ", pdf_file)
  return(perennial_grass_data)
}

# Function to analyze perennial grass trends across all parcels
analyze_perennial_grass_trends <- function(perennial_grass_data, significance_level = 0.05) {
  trends <- perennial_grass_data %>%
    group_by(.data$Parcel) %>%
    summarise(
      n_years = n(),
      trend_slope = if (n() >= 3) {
        model <- lm(.data$Perennial_Grass_Cover ~ .data$Year, data = pick(everything()))
        coef(model)[2]
      } else NA_real_,
      trend_p_value = if (n() >= 3) {
        model <- lm(.data$Perennial_Grass_Cover ~ .data$Year, data = pick(everything()))
        summary(model)$coefficients[2, 4]
      } else NA_real_,
      r_squared = if (n() >= 3) {
        model <- lm(.data$Perennial_Grass_Cover ~ .data$Year, data = pick(everything()))
        summary(model)$r.squared
      } else NA_real_,
      mean_cover = mean(.data$Perennial_Grass_Cover, na.rm = TRUE),
      .groups = 'drop'
    ) %>%
    mutate(
      significant_trend = !is.na(.data$trend_p_value) & .data$trend_p_value < significance_level,
      trend_direction = case_when(
        is.na(.data$trend_slope) ~ "Insufficient data",
        .data$trend_slope > 0 ~ "Increasing",
        .data$trend_slope < 0 ~ "Decreasing",
        TRUE ~ "No trend"
      )
    )
  
  return(trends)
}
