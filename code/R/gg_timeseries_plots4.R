# ggplot_timeseries_final_fix.R
library(ggplot2)
library(dplyr)
library(tidyr)
library(patchwork)

# Helper function to determine plot title based on attributes
get_title <- function(PID, attributes) {
  attr <- attributes %>% filter(Parcel == PID)
  type_label <- ifelse(attr$Type == "W", "Wellfield", "Control")
  title <- paste0(
    PID, " - ", type_label, " - Holland Type: ", attr$Holland,
    " - Green Book Type: ", attr$GB_TYPE, " - NRCS Ecological Site: ", attr$Ecologic_3
  )

  # Wrap the title into multiple lines
  wrapped_title <- paste(strwrap(title, width = 60), collapse = "\n")
  return(wrapped_title)
}



# Function to calculate 95% CI
calculate_ci <- function(data_vec) {
  n <- length(na.omit(data_vec))
  if (n < 2) return(c(NA, NA))
  mean_val <- mean(data_vec, na.rm = TRUE)
  stderr_val <- sd(data_vec, na.rm = TRUE) / sqrt(n)
  t_critical <- qt(0.975, df = n - 1)
  ci_upper <- mean_val + t_critical * stderr_val
  ci_lower <- mean_val - t_critical * stderr_val
  return(c(ci_lower, ci_upper))
}

# Function to create bar plots with CI and significance
plot_bar_discrete <- function(PID, df, column_name, baseline_years = 1984:1986, year_min = 1984, year_max = 2024, ylab_text = "Cover (%)", fill_color = "darkgreen") {
  df_sub <- df %>% filter(Parcel == PID)
  stats <- df_sub %>%
    group_by(Year) %>%
    summarise(
      Mean = mean(.data[[column_name]], na.rm = TRUE),
      CI = list(calculate_ci(.data[[column_name]]))
    ) %>%
    ungroup() %>%
    mutate(
      CI_Lower = sapply(CI, `[`, 1),
      CI_Upper = sapply(CI, `[`, 2)
    ) %>%
    complete(Year = year_min:year_max)

  baseline_avg <- mean(stats$Mean[stats$Year %in% baseline_years], na.rm = TRUE)

  stats$Significant <- ifelse(
    !is.na(stats$CI_Lower) & stats$CI_Upper < baseline_avg, "**",
    ifelse(!is.na(stats$CI_Lower) & stats$CI_Lower > baseline_avg, "", NA)
  )

  g <- ggplot(stats, aes(x = as.integer(Year), y = Mean)) +
    geom_col(fill = fill_color, width = 0.6) +
    geom_errorbar(aes(ymin = CI_Lower, ymax = CI_Upper), width = 0.2, na.rm = TRUE) +
    geom_hline(yintercept = baseline_avg, linetype = "dashed") +
    annotate("text", x = 1984.5, y = baseline_avg, label = "Baseline", hjust = 0, size = 3) +
    geom_text(aes(label = Significant), vjust = -0.5, size = 3, color = "red", na.rm = TRUE) +
    scale_x_continuous(breaks = c(1985, seq(1985, year_max, by = 5), year_max), expand = expansion(mult = c(0.01, 0.01))) +
    labs(y = ylab_text, x = NULL) +  # Remove X-axis label
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
    labs(y = "DTW (ft BGS)", x = NULL) +  # Remove X-axis label
    theme_minimal(base_size = 12)

  return(g)
}

# Main function to create plots and save to PDF
create_ts_plots_pdf_gg_discrete <- function(PIDs, pdf_file = "Parcel_TimeSeries_Final.pdf", attributes, transects, dtw_data, rs_data, current_year = 2024) {
  pdf(pdf_file, width = 8.5, height = 11)
  on.exit(dev.off(), add = TRUE)

  for (pid in PIDs) {
    title <- get_title(pid, attributes)
    plot_combo <- plot_bar_discrete(pid, transects, "Cover", year_min = 1984, year_max = current_year, ylab_text = "Perennial Cover (%)", fill_color = "forestgreen") /
      plot_bar_discrete(pid, transects, "Shrub", year_min = 1984, year_max = current_year, ylab_text = "Shrub Cover (%)", fill_color = "firebrick") /
      plot_bar_discrete(pid, transects, "Grass", year_min = 1984, year_max = current_year, ylab_text = "Grass Cover (%)", fill_color = "darkgreen") /
      plot_dtw_discrete(pid, dtw_data, year_min = 1984, year_max = current_year) /
      plot_bar_discrete(pid, rs_data, "NDVI_SUR", year_min = 1984, year_max = current_year, ylab_text = "NDVI", fill_color = "darkgreen") /
      plot_bar_discrete(pid, rs_data, "PPT", year_min = 1984, year_max = current_year, ylab_text = "Precip (in)", fill_color = "darkblue") +
      plot_layout(ncol = 1) +
      plot_annotation(
        title = title,
        theme = theme(
          plot.title = element_text(hjust = 0.5, size = 14, face = "bold", lineheight = 1.2)
        )
      )

    print(plot_combo)
  }

  message("Saved PDF to: ", pdf_file)
}
