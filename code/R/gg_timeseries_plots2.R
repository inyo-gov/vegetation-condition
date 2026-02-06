# ggplot_timeseries_customized.R
# Customized time series plot generation using ggplot2

library(ggplot2)
library(dplyr)
library(tidyr)
library(patchwork)

# Helper function to determine plot title based on attributes
get_title <- function(PID, attributes) {
  attr <- attributes %>% filter(Parcel == PID)
  type_label <- ifelse(attr$Type == "W", "Wellfield", "Control")
  title <- paste(PID, "-", type_label, "- Holland Type:", attr$GB_TYPE, "- Green Book Type:", attr$Type, "- NRCS Ecological Site:", attr$Ecologic_3)
  return(title)
}

baseline_test <- function(baseline_vec, new_vec) {
  # Remove missing values
  baseline_vec <- na.omit(baseline_vec)
  new_vec <- na.omit(new_vec)

  # Check if either vector is empty
  if (length(baseline_vec) == 0 || length(new_vec) == 0) {
    return(NA_real_)  # Not enough observations, return NA
  }

  # Check if baseline vector contains all zeros
  if (all(baseline_vec == 0)) {
    # One-sample t-test against 0
    if (length(new_vec) > 1) {
      fit <- t.test(new_vec, mu = 0, alternative = "less")
      return(fit$p.value)
    } else {
      return(NA_real_)  # Not enough data for a valid t-test
    }
  }

  # One-sample t-test for small baseline samples
  if (length(baseline_vec) <= 4) {
    mu.d1 <- mean(baseline_vec, na.rm = TRUE)
    if (length(new_vec) > 1) {
      fit <- t.test(new_vec, mu = mu.d1, alternative = "less")
      return(fit$p.value)
    } else {
      return(NA_real_)  # Not enough data for a valid t-test
    }
  }

  # Two-sample t-test for larger baseline samples
  if (length(baseline_vec) > 1 && length(new_vec) > 1) {
    fit <- t.test(new_vec, baseline_vec, alternative = "less")
    return(fit$p.value)
  } else {
    return(NA_real_)  # Not enough data for a valid two-sample t-test
  }
}


plot_bar_discrete <- function(PID, df, column_name, baseline_years = 1984:1988, year_min = 1985, year_max = 2024, ylab_text = "Cover (%)", fill_color = "darkgreen") {
  df_sub <- df %>% filter(Parcel == PID)
  stats <- df_sub %>%
    group_by(Year) %>%
    summarise(
      Mean = mean(.data[[column_name]], na.rm = TRUE),
      SEM  = ifelse(length(na.omit(.data[[column_name]])) > 1, sqrt(var(.data[[column_name]], na.rm = TRUE) / length(na.omit(.data[[column_name]]))), NA_real_)
    ) %>%
    ungroup() %>%
    complete(Year = year_min:year_max)

  stats$pval <- sapply(stats$Year, function(y) {
    if (y %in% baseline_years) return(NA_real_)
    baseline_data <- df_sub[[column_name]][df_sub$Year %in% baseline_years]
    new_data <- df_sub[[column_name]][df_sub$Year == y]
    if (length(new_data) < 1 || length(baseline_data) < 1) {
      return(NA_real_)  # Skip t-test if data is insufficient
    }
    baseline_test(baseline_data, new_data)
  })

  stats$YearF <- factor(stats$Year, levels = year_min:year_max)

  g <- ggplot(stats, aes(x = YearF, y = Mean)) +
    geom_col(fill = fill_color, width = 0.6) +  # Make bars thinner
    geom_errorbar(aes(ymin = Mean, ymax = Mean + 2*SEM), width = 0.2, na.rm = TRUE) +  # Only upper error bars
    geom_hline(yintercept = mean(stats$Mean[stats$Year %in% baseline_years], na.rm = TRUE), linetype = "dashed") +
    annotate("text", x = 3, y = mean(stats$Mean[stats$Year %in% baseline_years], na.rm = TRUE), label = "Baseline", hjust = -0.1) +  # Move label to the right
    scale_x_discrete(breaks = factor(seq(year_min, year_max, by = 5), levels = year_min:year_max)) +
    labs(y = ylab_text) +
    theme_minimal(base_size = 11)

  return(g)
}


plot_dtw_discrete <- function(PID, dtw_data, baseline_years = 1984:1987, year_min = 1985, year_max = 2024) {
  df_sub <- dtw_data %>% filter(Parcel == PID)
  stats <- df_sub %>%
    group_by(Year) %>%
    summarise(DTW = mean(DTW, na.rm = TRUE)) %>%
    ungroup() %>%
    complete(Year = year_min:year_max) %>%
    mutate(YearF = factor(Year, levels = year_min:year_max))  # Ensure YearF is created

  g <- ggplot(stats, aes(x = YearF, y = DTW)) +
    geom_line(color = "blue") +
    geom_point(color = "blue") +
    geom_hline(yintercept = mean(stats$DTW[stats$Year %in% baseline_years], na.rm = TRUE), linetype = "dashed", color = "blue") +
    annotate("text", x = 3, y = mean(stats$DTW[stats$Year %in% baseline_years], na.rm = TRUE), label = "DTW Baseline", hjust = -0.1, color = "blue") +
    scale_y_reverse() +
    scale_x_discrete(breaks = factor(seq(year_min, year_max, by = 5), levels = year_min:year_max)) +
    labs(y = "DTW (ft BGS)") +
    theme_minimal(base_size = 11)

  return(g)
}


create_ts_plots_pdf_gg_discrete <- function(PIDs, pdf_file = "Parcel_TimeSeries_DiscreteYears.pdf", attributes, transects, dtw_data, rs_data, current_year = 2024) {
  pdf(pdf_file, width = 8.5, height = 11)
  on.exit(dev.off(), add = TRUE)

  for (pid in PIDs) {
    title <- get_title(pid, attributes)
    plot_combo <- plot_bar_discrete(pid, transects, "Cover", year_min = 1985, year_max = current_year, ylab_text = "Perennial Cover (%)", fill_color = "forestgreen") /
      plot_bar_discrete(pid, transects, "Shrub", year_min = 1985, year_max = current_year, ylab_text = "Shrub Cover (%)", fill_color = "firebrick") /
      plot_bar_discrete(pid, transects, "Grass", year_min = 1985, year_max = current_year, ylab_text = "Grass Cover (%)", fill_color = "darkgreen") /
      plot_dtw_discrete(pid, dtw_data, year_min = 1985, year_max = current_year) /
      plot_bar_discrete(pid, rs_data, "NDVI_SUR", year_min = 1985, year_max = current_year, ylab_text = "NDVI", fill_color = "darkgreen") /
      plot_bar_discrete(pid, rs_data, "PPT", year_min = 1985, year_max = current_year, ylab_text = "Precip (in)", fill_color = "darkblue") +
      plot_layout(ncol = 1) +
      plot_annotation(title = title, theme = theme(plot.title = element_text(hjust = 0.5)))

    print(plot_combo)
  }

  message("Saved PDF to: ", pdf_file)
}
