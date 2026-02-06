###############################################################################
## gg_timeseries_plots_discrete_years.R
##
## Demonstrates how to force all plots to have discrete x-axis from 1984 to the
## current year so they line up vertically. DTW is reversed (0 at top).
##
## Key differences from the previous approach:
##  - We create factor(Year) with levels = 1984:cYear, ensuring every year is present.
##  - We do "tidyr::complete" so missing years exist in the data with NA.
##  - We label x-axis at 5-year increments plus the last year if needed.
##  - No plot titles above each subplot (just a y-axis label).
###############################################################################

library(ggplot2)
library(dplyr)
library(tidyr)     # for tidyr::complete()
library(patchwork)

# A small helper to get the year breaks at 5-year increments plus the last year:
make_year_breaks <- function(year_min, year_max) {
  # e.g. seq(1985, 2024, by=5)
  brk <- seq(year_min, year_max, by=5)
  # ensure we also include year_max if it's not already in brk
  if (!(year_max %in% brk)) {
    brk <- c(brk, year_max)
  }
  brk
}
baseline_test <- function(baseline_vec, new_vec) {
  # Remove missing values
  baseline_vec <- na.omit(baseline_vec)
  new_vec <- na.omit(new_vec)

  # Ensure there are enough observations in new_vec
  if (length(new_vec) == 0) {
    return(NA_real_)  # No valid data, return NA for the p-value
  }

  # Determine which t-test to use based on baseline data
  if (length(baseline_vec) == 0) {
    return(NA_real_)  # No baseline data, return NA for the p-value
  } else if (all(baseline_vec == 0)) {
    # Baseline is all zeros, one-sample test vs. mu = 0
    fit <- t.test(new_vec, mu = 0, alternative = "less")
  } else if (length(baseline_vec) <= 4) {
    # Small baseline sample, use one-sample test with mean as mu
    mu.d1 <- mean(baseline_vec, na.rm = TRUE)
    fit <- t.test(new_vec, mu = mu.d1, alternative = "less")
  } else {
    # Two-sample t-test
    fit <- t.test(new_vec, baseline_vec, alternative = "less")
  }

  # Return the p-value
  fit$p.value
}
# Baseline test for "new_vec < baseline"
# baseline_test <- function(baseline_vec, new_vec) {
#   baseline_vec <- na.omit(baseline_vec)
#   new_vec      <- na.omit(new_vec)
#
#   if (length(baseline_vec) == 0) {
#     return(NA_real_)
#   } else if (all(baseline_vec == 0)) {
#     fit <- t.test(new_vec, mu=0, alternative="less")
#   } else if (length(baseline_vec) <= 4) {
#     mu.d1 <- mean(baseline_vec)
#     fit <- t.test(new_vec, mu=mu.d1, alternative="less")
#   } else {
#     fit <- t.test(new_vec, baseline_vec, alternative="less")
#   }
#   fit$p.value
# }

stderr <- function(x) sqrt(var(x, na.rm=TRUE) / length(na.omit(x)))

# A generic function to create a bar plot with missing years filled in
plot_bar_discrete <- function(
    PID,
    df,
    column_name,          # e.g. "Shrub"
    baseline_years = 1984:1988,
    year_min       = 1984,
    year_max       = 2024, # or current year
    ylab_text      = "Cover (%)",
    fill_color     = "darkgreen"
) {
  # Filter & summarize
  df_sub <- df %>% filter(Parcel == PID)
  if (nrow(df_sub) == 0) {
    return(ggplot() + theme_void())
  }
  # Summaries by year
  stats <- df_sub %>%
    group_by(Year) %>%
    summarise(
      Mean = mean(.data[[column_name]], na.rm=TRUE),
      SEM  = stderr(.data[[column_name]])
    ) %>%
    ungroup()

  # "Complete" the sequence 1984:year_max so missing years appear
  # with NA for Mean/SEM
  stats <- stats %>%
    complete(Year = year_min:year_max)

  # Calculate p-values
  stats$pval <- NA_real_
  for (i in seq_len(nrow(stats))) {
    y <- stats$Year[i]
    if (!is.na(y) && !(y %in% baseline_years)) {
      baseline_data <- df_sub[[column_name]][df_sub$Year %in% baseline_years]
      new_data      <- df_sub[[column_name]][df_sub$Year == y]
      stats$pval[i] <- baseline_test(baseline_data, new_data)
    }
  }

  # Baseline mean
  b_sub <- stats %>% filter(Year %in% baseline_years)
  baseline_mean <- if (nrow(b_sub) > 0) mean(b_sub$Mean, na.rm=TRUE) else NA

  # Convert Year to a factor with all levels 1984:year_max
  stats$YearF <- factor(stats$Year, levels=year_min:year_max)

  # Plot
  g <- ggplot(stats, aes(x=YearF, y=Mean)) +
    geom_col(
      fill = fill_color,
      na.rm=TRUE,
      width=0.9
    ) +
    geom_errorbar(
      aes(ymin=Mean - 2*SEM, ymax=Mean + 2*SEM),
      width=0.4,
      na.rm=TRUE
    ) +
    labs(x=NULL, y=ylab_text) +
    theme_minimal(base_size=11)

  # Dashed baseline
  if (is.finite(baseline_mean)) {
    g <- g +
      geom_hline(yintercept = baseline_mean, linetype="dashed") +
      annotate(
        "text", x=1, y=baseline_mean,
        label="Baseline", vjust=-0.5, size=3
      )
  }

  # Add "**" for p<0.05
  stats_sig <- stats %>% filter(!is.na(pval) & pval < 0.05)
  if (nrow(stats_sig) > 0) {
    offset <- 0.03 * max(stats$Mean, na.rm=TRUE)
    g <- g +
      geom_text(
        data=stats_sig,
        aes(x=YearF, y=Mean + 2*SEM + offset, label="**"),
        color="red",
        size=4,
        na.rm=TRUE
      )
  }

  # x-axis breaks: every 5 years + year_max if not multiple of 5
  brk <- make_year_breaks(year_min, year_max)
  g <- g + scale_x_discrete(breaks=factor(brk, levels=year_min:year_max))

  return(g)
}

# For DTW, we want a line plot + reversed y-scale
plot_dtw_discrete <- function(
    PID,
    dtw_data,
    baseline_years = 1984:1987,
    year_min=1984,
    year_max=2024
) {
  df_sub <- dtw_data %>% filter(Parcel == PID)
  if (nrow(df_sub) == 0) {
    return(ggplot() + theme_void())
  }

  # Summarize by year (or just keep raw, but let's do a mean if multiple rows/year)
  stats <- df_sub %>%
    group_by(Year) %>%
    summarise(DTW=mean(DTW, na.rm=TRUE)) %>%
    ungroup() %>%
    complete(Year=year_min:year_max)

  # Baseline
  b_sub <- stats %>% filter(Year %in% baseline_years)
  baseline_mean <- if(nrow(b_sub)>0) mean(b_sub$DTW, na.rm=TRUE) else NA

  stats$YearF <- factor(stats$Year, levels=year_min:year_max)

  g <- ggplot(stats, aes(x=YearF, y=DTW)) +
    geom_line(aes(group=1), color="blue", na.rm=TRUE) +
    geom_point(color="blue", na.rm=TRUE) +
    labs(x=NULL, y="DTW (ft BGS)") +
    scale_y_reverse() +          # flip so 0 is at top
    theme_minimal(base_size=11)

  if (is.finite(baseline_mean)) {
    g <- g +
      geom_hline(yintercept=baseline_mean, linetype="dashed", color="blue") +
      annotate(
        "text", x=1, y=baseline_mean,
        label="DTW Baseline", vjust=-0.5, color="blue", size=3
      )
  }

  # x-axis breaks
  brk <- make_year_breaks(year_min, year_max)
  g <- g + scale_x_discrete(breaks=factor(brk, levels=year_min:year_max))

  return(g)
}

# NDVI example: bar chart, no significance test
plot_ndvi_discrete <- function(
    PID, rs_data,
    baseline_year=1986,
    year_min=1984,
    year_max=2024
) {
  df_sub <- rs_data %>% filter(Parcel == PID)
  if (nrow(df_sub)==0) return(ggplot() + theme_void())

  stats <- df_sub %>%
    group_by(Year) %>%
    summarise(NDVI=mean(NDVI_SUR, na.rm=TRUE)) %>%
    ungroup() %>%
    complete(Year=year_min:year_max)

  b_sub <- stats %>% filter(Year == baseline_year)
  b_mean <- if(nrow(b_sub)==1) b_sub$NDVI else NA_real_

  stats$YearF <- factor(stats$Year, levels=year_min:year_max)

  g <- ggplot(stats, aes(x=YearF, y=NDVI)) +
    geom_col(fill="darkgreen", na.rm=TRUE, width=0.9) +
    labs(x=NULL, y="NDVI") +
    theme_minimal(base_size=11)

  if (is.finite(b_mean)) {
    g <- g +
      geom_hline(yintercept=b_mean, linetype="dashed", color="darkgreen") +
      annotate(
        "text", x=1, y=b_mean,
        label="86 NDVI", vjust=-0.5, color="darkgreen", size=3
      )
  }

  brk <- make_year_breaks(year_min, year_max)
  g <- g + scale_x_discrete(breaks=factor(brk, levels=year_min:year_max))

  return(g)
}

# PPT in inches
plot_ppt_discrete <- function(
    PID, rs_data,
    baseline_year=1986,
    year_min=1984,
    year_max=2024
) {
  df_sub <- rs_data %>% filter(Parcel==PID)
  if(nrow(df_sub)==0) return(ggplot() + theme_void())

  df_sub <- df_sub %>% mutate(PPT_in = PPT/25.4)
  stats <- df_sub %>%
    group_by(Year) %>%
    summarise(PPT_in=mean(PPT_in, na.rm=TRUE)) %>%
    ungroup() %>%
    complete(Year=year_min:year_max)

  b_sub <- stats %>% filter(Year==baseline_year)
  b_mean <- if(nrow(b_sub)==1) b_sub$PPT_in else NA_real_

  stats$YearF <- factor(stats$Year, levels=year_min:year_max)

  g <- ggplot(stats, aes(x=YearF, y=PPT_in)) +
    geom_col(fill="darkblue", na.rm=TRUE, width=0.9) +
    labs(x=NULL, y="Precip (in)") +
    theme_minimal(base_size=11)

  if(is.finite(b_mean)) {
    g <- g +
      geom_hline(yintercept=b_mean, linetype="dashed", color="blue") +
      annotate(
        "text", x=1, y=b_mean,
        label="86 PPT (in)",
        vjust=-0.5, color="blue", size=3
      )
  }

  brk <- make_year_breaks(year_min, year_max)
  g <- g + scale_x_discrete(breaks=factor(brk, levels=year_min:year_max))

  return(g)
}

# Now a function to assemble all 6 sub-plots with discrete x
plot_six_timeseries_gg_discrete <- function(
    PID,
    transects,
    dtw_data,
    rs_data,
    year_min=1984,
    year_max=2024
) {
  # Perennial
  p_cover <- plot_bar_discrete(
    PID, transects, column_name="Cover",
    baseline_years=1984:1988,
    year_min=year_min, year_max=year_max,
    ylab_text="Perennial Cover (%)",
    fill_color="forestgreen"
  )

  # Shrub
  p_shrub <- plot_bar_discrete(
    PID, transects, column_name="Shrub",
    baseline_years=1984:1988,
    year_min=year_min, year_max=year_max,
    ylab_text="Shrub Cover (%)",
    fill_color="firebrick"
  )

  # Grass
  p_grass <- plot_bar_discrete(
    PID, transects, column_name="Grass",
    baseline_years=1984:1988,
    year_min=year_min, year_max=year_max,
    ylab_text="Grass Cover (%)",
    fill_color="darkgreen"
  )

  # DTW
  p_dtw <- plot_dtw_discrete(
    PID, dtw_data,
    baseline_years=1984:1987,
    year_min=year_min, year_max=year_max
  )

  # NDVI
  p_ndvi <- plot_ndvi_discrete(
    PID, rs_data,
    baseline_year=1986,
    year_min=year_min, year_max=year_max
  )

  # PPT
  p_ppt <- plot_ppt_discrete(
    PID, rs_data,
    baseline_year=1986,
    year_min=year_min, year_max=year_max
  )

  # Stack them (patchwork)
  combo <- p_cover / p_shrub / p_grass / p_dtw / p_ndvi / p_ppt +
    plot_layout(ncol=1) +
    plot_annotation(title=PID, theme=theme(plot.title=element_text(hjust=0.5)))

  return(combo)
}

# Finally, a function to loop over PIDs and save one page per parcel
create_ts_plots_pdf_gg_discrete <- function(
    PIDs,
    pdf_file="Parcel_TimeSeries_DiscreteYears.pdf",
    transects,
    dtw_data,
    rs_data,
    current_year=2024  # or a dynamic variable if you prefer
) {
  pdf(pdf_file, width=8.5, height=11)
  on.exit(dev.off(), add=TRUE)

  for (pid in PIDs) {
    combo <- plot_six_timeseries_gg_discrete(
      PID       = pid,
      transects = transects,
      dtw_data  = dtw_data,
      rs_data   = rs_data,
      year_min  = 1984,
      year_max  = current_year
    )
    print(combo)
  }

  message("Saved PDF to: ", pdf_file)
}
