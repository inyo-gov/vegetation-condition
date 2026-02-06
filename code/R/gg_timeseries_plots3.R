# ggplot_timeseries_fixed_wrapping_corrected.R
library(ggplot2)
library(dplyr)
library(tidyr)
library(patchwork)

# Helper function to determine plot title based on attributes
get_title <- function(PID, attributes) {
  attr <- attributes %>% filter(Parcel == PID)
  type_label <- ifelse(attr$Type == "W", "Wellfield", "Control")
  title <- paste0(
    PID, " - ", type_label,
    " - Holland Type: ", attr$Holland,
    " - Green Book Type: ", attr$GB_TYPE,
    " - NRCS Ecological Site: ", attr$Ecologic_3
  )
  return(title)
}

# Function to test significance against baseline
baseline_test <- function(baseline_vec, new_vec) {
  baseline_vec <- na.omit(baseline_vec)
  new_vec <- na.omit(new_vec)

  if (length(baseline_vec) == 0 || length(new_vec) == 0) {
    return(NA_real_)
  }

  if (all(baseline_vec == 0)) {
    if (length(new_vec) > 1) {
      fit <- t.test(new_vec, mu = 0, alternative = "less")
      return(fit$p.value)
    } else {
      return(NA_real_)
    }
  }

  if (length(baseline_vec) <= 4) {
    mu.d1 <- mean(baseline_vec, na.rm = TRUE)
    if (length(new_vec) > 1) {
      fit <- t.test(new_vec, mu = mu.d1, alternative = "less")
      return(fit$p.value)
    } else {
      return(NA_real_)
    }
  }

  if (length(baseline_vec) > 1 && length(new_vec) > 1) {
    fit <- t.test(new_vec, baseline_vec, alternative = "less")
    return(fit$p.value)
  } else {
    return(NA_real_)
  }
}

# Function to create bar plots with asterisks for significance
plot_bar_discrete <- function(PID, df, column_name, baseline_years = 1984:1986, year_min = 1984, year_max = 2024, ylab_text = "Cover (%)", fill_color = "darkgreen", baseline_label = "Baseline") {
  df_sub <- df %>% filter(Parcel == PID)
  stats <- df_sub %>%
    group_by(Year) %>%
    summarise(
      Mean = mean(.data[[column_name]], na.rm = TRUE),
      SEM  = ifelse(length(na.omit(.data[[column_name]])) > 1, sqrt(var(.data[[column_name]], na.rm = TRUE) / length(na.omit(.data[[column_name]]))), NA_real_)
    ) %>%
    ungroup() %>%
    complete(Year = year_min:year_max)

  baseline_avg <- mean(stats$Mean[stats$Year %in% baseline_years], na.rm = TRUE)

  stats$pval <- sapply(stats$Year, function(y) {
    if (y %in% baseline_years) return(NA_real_)
    baseline_data <- df_sub[[column_name]][df_sub$Year %in% baseline_years]
    new_data <- df_sub[[column_name]][df_sub$Year == y]
    baseline_test(baseline_data, new_data)
  })

  stats$asterisk <- ifelse(stats$pval < 0.05, "**", ifelse(stats$pval < 0.1, "*", ""))

  g <- ggplot(stats, aes(x = as.integer(Year), y = Mean)) +
    geom_col(fill = fill_color, width = 0.6) +
    geom_errorbar(aes(ymin = Mean, ymax = Mean + 2*SEM), width = 0.2, na.rm = TRUE) +
    geom_hline(yintercept = baseline_avg, linetype = "dashed") +
    annotate("text", x = 1984.5, y = baseline_avg, label = baseline_label, hjust = 0, size = 3) +
    geom_text(aes(label = asterisk), vjust = -0.5, size = 3, color = "red", na.rm = TRUE) +
    scale_x_continuous(breaks = c(1985, seq(1985, year_max, by = 5), year_max), expand = expansion(mult = c(0.01, 0.01))) +
    labs(y = ylab_text, x = "Year") +
    theme_minimal(base_size = 12)

  return(g)
}

# Function for DTW line plot
plot_dtw_discrete <- function(PID, dtw_data, baseline_years = 1984:1986, year_min = 1984, year_max = 2024) {
  df_sub <- dtw_data %>% filter(Parcel == PID)
  stats <- df_sub %>%
    group_by(Year) %>%
    summarise(DTW = mean(DTW, na.rm = TRUE)) %>%
    ungroup() %>%
    complete(Year = year_min:year_max)

  baseline_avg <- mean(stats$DTW[stats$Year %in% baseline_years], na.rm = TRUE)

  g <- ggplot(stats, aes(x = as.integer(Year), y = DTW)) +
    geom_line(color = "blue") +
    geom_point(color = "blue") +
    geom_hline(yintercept = baseline_avg, linetype = "dashed", color = "blue") +
    annotate("text", x = 1984.5, y = baseline_avg, label = "Avg. 1984-86 DTW", hjust = 0, size = 3, color = "blue") +
    scale_y_reverse() +
    scale_x_continuous(breaks = c(1985, seq(1985, year_max, by = 5), year_max), expand = expansion(mult = c(0.01, 0.01))) +
    labs(y = "DTW (ft BGS)", x = "Year") +
    theme_minimal(base_size = 12)

  return(g)
}

# Main function to create plots and save to PDF
create_ts_plots_pdf_gg_discrete <- function(PIDs, pdf_file = "Parcel_TimeSeries_Wrapped.pdf", attributes, transects, dtw_data, rs_data, current_year = 2024) {
  pdf(pdf_file, width = 8.5, height = 11)
  on.exit(dev.off(), add = TRUE)

  for (pid in PIDs) {
    title <- get_title(pid, attributes)
    plot_combo <- plot_bar_discrete(pid, transects, "Cover", year_min = 1984, year_max = current_year, ylab_text = "Perennial Cover (%)", fill_color = "forestgreen", baseline_label = "Baseline") /
      plot_bar_discrete(pid, transects, "Shrub", year_min = 1984, year_max = current_year, ylab_text = "Shrub Cover (%)", fill_color = "firebrick", baseline_label = "Baseline") /
      plot_bar_discrete(pid, transects, "Grass", year_min = 1984, year_max = current_year, ylab_text = "Grass Cover (%)", fill_color = "darkgreen", baseline_label = "Baseline") /
      plot_dtw_discrete(pid, dtw_data, year_min = 1984, year_max = current_year) /
      plot_bar_discrete(pid, rs_data, "NDVI_SUR", year_min = 1984, year_max = current_year, ylab_text = "NDVI", fill_color = "darkgreen", baseline_label = "Avg. 1984-86 NDVI") /
      plot_bar_discrete(pid, rs_data, "PPT", year_min = 1984, year_max = current_year, ylab_text = "Precip (in)", fill_color = "darkblue", baseline_label = "Avg. 1984-86 Precip") +
      plot_layout(ncol = 1) +
      plot_annotation(
        title = paste0(title),
        theme = theme(
          plot.title = element_text(hjust = 0.5, size = 14, face = "bold", lineheight = 1.2)
        )
      )

    print(plot_combo)
  }

  message("Saved PDF to: ", pdf_file)
}
