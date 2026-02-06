library(ggplot2)
library(dplyr)
library(tidyr)
library(patchwork)

########################################
## 0) Ensure Year is numeric upfront  ##
########################################
##  !!! IMPORTANT !!!
##  In your own code, do something like:
##
##   transects <- transects %>% mutate(Year = as.numeric(Year))
##   dtw_data  <- dtw_data  %>% mutate(Year = as.numeric(Year))
##   rs_data   <- rs_data   %>% mutate(Year = as.numeric(Year))
##
##  **before** calling create_ts_plots_pdf_gg_discrete().
##  Doing so usually fixes the “Discrete value supplied to continuous scale” error.
##

########################################
## 1) Title-Wrapping Helper Function   ##
########################################
get_title <- function(PID, attributes) {
  attr <- attributes %>% filter(Parcel == PID)
  type_label <- ifelse(attr$Type == "W", "Wellfield", "Control")
  title <- paste0(
    PID, " - ", type_label,
    " - Holland Type: ", attr$Holland,
    " - Green Book Type: ", attr$GB_TYPE,
    " - NRCS Ecological Site: ", attr$Ecologic_3
  )
  wrapped_title <- paste(strwrap(title, width = 60), collapse = "\n")
  return(wrapped_title)
}

########################################
## 2) 95% CI Calculation
########################################
calculate_ci <- function(x) {
  x <- na.omit(x)
  n <- length(x)
  if (n < 2) {
    return(c(NA, NA))
  }
  mean_x <- mean(x)
  sd_x   <- sd(x)
  se_x   <- sd_x / sqrt(n)
  t_crit <- qt(0.975, df = n - 1)  # 2-sided, 95% CI
  ci_lower <- mean_x - t_crit * se_x
  ci_upper <- mean_x + t_crit * se_x
  c(ci_lower, ci_upper)
}

########################################
## 3) T-Test Safeguard
########################################
baseline_test <- function(baseline_vec, new_vec) {
  baseline_vec <- na.omit(baseline_vec)
  new_vec      <- na.omit(new_vec)

  # Not enough data for a t-test
  if (length(baseline_vec) < 2 || length(new_vec) < 2) {
    return(NA_real_)
  }

  # Perform one-sided t-test (less)
  out <- tryCatch({
    t.test(new_vec, baseline_vec, alternative = "less")$p.value
  }, error = function(e) NA_real_)

  return(out)
}

########################################
## 4) The Bar Plot with CI & T-Test
########################################
plot_bar_discrete <- function(
    PID, df, column_name,
    baseline_years = 1984:1987,
    year_min = 1984,
    year_max = current_year,
    ylab_text = "Cover (%)",
    fill_color = "darkgreen",
    baseline_label = "Baseline"
) {
  # Subset data for this Parcel
  df_sub <- df %>% filter(Parcel == PID)

  # Summarize: Mean & 95% CI
  stats <- df_sub %>%
    group_by(Year) %>%
    summarise(
      Mean = mean(.data[[column_name]], na.rm = TRUE),
      CI   = list(calculate_ci(.data[[column_name]]))
    ) %>%
    ungroup() %>%
    mutate(
      CI_Lower = sapply(CI, `[`, 1),
      CI_Upper = sapply(CI, `[`, 2)
    ) %>%
    select(-CI) %>%
    ## Fill all possible years from year_min to year_max
    complete(Year = seq(year_min, year_max)) %>%
    ## If Year is not numeric, coerce it
    mutate(Year = as.numeric(Year)) %>%
    arrange(Year)

  # Compute the baseline's average
  baseline_avg <- mean(stats$Mean[stats$Year %in% baseline_years], na.rm = TRUE)

  # T-test p-values
  stats$pval <- sapply(stats$Year, function(y) {
    if (y %in% baseline_years) {
      return(NA_real_)
    }
    # baseline data + new_data
    baseline_data <- df_sub[[column_name]][df_sub$Year %in% baseline_years]
    new_data      <- df_sub[[column_name]][df_sub$Year == y]
    baseline_test(baseline_data, new_data)
  })

  # If p-value < 0.05 => "**", else if p < 0.1 => "*"
  stats$asterisk <- ifelse(stats$pval < 0.05, "**",
                           ifelse(stats$pval < 0.1, "*", ""))

  stats %>%
    complete(Year = seq(year_min, year_max)) %>%
    mutate(Year = as.numeric(Year
    arrange(Year) -> stats)) %>%

  cat("Year is: ", class(stats$Year), "\n")

  # Build the ggplot
  g <- ggplot(stats, aes(x = Year, y = Mean)) +
    geom_col(
      fill = fill_color,
      width = 0.6,
      position = position_identity()  # <--- add this
    ) +
    geom_errorbar(aes(ymin = CI_Lower, ymax = CI_Upper),
                  width = 0.2, na.rm = TRUE) +
    geom_hline(yintercept = baseline_avg, linetype = "dashed") +
    annotate("text",
             x = year_min + 0.5,
             y = baseline_avg,
             label = baseline_label,
             hjust = 0,
             size = 3) +
    geom_text(aes(label = asterisk),
              vjust = -0.5,
              size = 3,
              color = "red",
              na.rm = TRUE) +
    scale_x_continuous(
      breaks = seq(year_min, year_max, by = 5),
      expand = expansion(mult = c(0.01, 0.01))
    ) +
    labs(y = ylab_text, x = "Year") +
    theme_minimal(base_size = 12)

  return(g)
}

########################################
## 5) The DTW Line Plot
########################################
plot_dtw_discrete <- function(
    PID,
    dtw_data,
    baseline_years = 1984:1986,
    year_min = 1984,
    year_max = 2024
) {
  df_sub <- dtw_data %>% filter(Parcel == PID)

  stats <- df_sub %>%
    group_by(Year) %>%
    summarise(DTW = mean(DTW, na.rm = TRUE)) %>%
    ungroup() %>%
    complete(Year = seq(year_min, year_max)) %>%
    mutate(Year = as.numeric(Year)) %>%
    arrange(Year)

  baseline_avg <- mean(stats$DTW[stats$Year %in% baseline_years], na.rm = TRUE)

  g <- ggplot(stats, aes(x = Year, y = DTW)) +
    geom_line(color = "blue") +
    geom_point(color = "blue") +
    geom_hline(yintercept = baseline_avg, linetype = "dashed", color = "blue") +
    annotate("text",
             x = year_min + 0.5,
             y = baseline_avg,
             label = "Avg. 1984-86 DTW",
             hjust = 0,
             size = 3,
             color = "blue") +
    scale_y_reverse() +
    scale_x_continuous(
      breaks = seq(year_min, year_max, by = 5),
      expand = expansion(mult = c(0.01, 0.01))
    ) +
    labs(y = "DTW (ft BGS)", x = "Year") +
    theme_minimal(base_size = 12)

  return(g)
}

########################################
## 6) Master Function
########################################
create_ts_plots_pdf_gg_discrete <- function(
    PIDs,
    pdf_file,
    attributes,
    transects,
    dtw_data,
    rs_data,
    current_year
) {
  # In case user hasn't forcibly converted them yet
  transects <- transects %>% mutate(Year = as.numeric(Year))
  dtw_data  <- dtw_data  %>% mutate(Year = as.numeric(Year))
  rs_data   <- rs_data   %>% mutate(Year = as.numeric(Year))

  pdf(pdf_file, width = 8.5, height = 11)
  on.exit(dev.off(), add = TRUE)

  for (pid in PIDs) {
    title_text <- get_title(pid, attributes)

    # Build the stacked 6-plot layout
    plot_combo <-
      plot_bar_discrete(pid, transects, "Cover",
                        year_min=1984,
                        year_max=current_year,
                        ylab_text="Perennial Cover (%)",
                        fill_color="forestgreen",
                        baseline_label="Baseline"
      ) /
      plot_bar_discrete(pid, transects, "Shrub",
                        year_min=1984,
                        year_max=current_year,
                        ylab_text="Shrub Cover (%)",
                        fill_color="firebrick",
                        baseline_label="Baseline"
      ) /
      plot_bar_discrete(pid, transects, "Grass",
                        year_min=1984,
                        year_max=current_year,
                        ylab_text="Grass Cover (%)",
                        fill_color="darkgreen",
                        baseline_label="Baseline"
      ) /
      plot_dtw_discrete(pid, dtw_data,
                        year_min=1984,
                        year_max=current_year
      ) /
      plot_bar_discrete(pid, rs_data, "NDVI_SUR",
                        year_min=1984,
                        year_max=current_year,
                        ylab_text="NDVI",
                        fill_color="darkgreen",
                        baseline_label="Avg. 1984-86 NDVI"
      ) /
      plot_bar_discrete(pid, rs_data, "PPT",
                        year_min=1984,
                        year_max=current_year,
                        ylab_text="Precip (in)",
                        fill_color="darkblue",
                        baseline_label="Avg. 1984-86 Precip"
      ) +
      plot_layout(ncol=1) +
      plot_annotation(
        title=title_text,
        theme=theme(
          plot.title=element_text(
            hjust=0.5, size=14, face="bold", lineheight=1.2
          )
        )
      )

    print(plot_combo)
  }

  message("Saved PDF to: ", pdf_file)
}
