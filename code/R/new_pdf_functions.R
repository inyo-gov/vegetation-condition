#############################################
## 1) HELPER FUNCTIONS FOR PLOTTING & TESTS ##
#############################################

stderr <- function(x) {
  sqrt(var(x, na.rm=TRUE) / length(na.omit(x)))
}

# Baseline test: tests whether 'new_vec' is significantly LOWER than 'baseline_vec'.
baseline_test <- function(baseline_vec, new_vec) {
  baseline_vec <- na.omit(baseline_vec)
  new_vec      <- na.omit(new_vec)

  # If no baseline data, skip
  if (length(baseline_vec) == 0) {
    return(NA_real_)
  }

  if (all(baseline_vec == 0)) {
    # If all baseline = 0 => one‐sample test vs. mu=0
    fit <- t.test(new_vec, mu = 0, alternative = "less")
  } else if (length(baseline_vec) <= 4) {
    # If baseline is small => one‐sample test vs. mean(baseline)
    mu.d1 <- mean(baseline_vec)
    fit   <- t.test(new_vec, mu = mu.d1, alternative = "less")
  } else {
    # two‐sample test
    fit <- t.test(new_vec, baseline_vec, alternative = "less")
  }
  fit$p.value
}

# A utility to create a blank plot with standard axes/lines.
setup_blank_plot <- function(xlim, ylim, ylab = "") {
  plot(xlim, ylim, xlab = "", ylab = ylab,
       xlim = xlim, ylim = ylim,
       yaxs = "i", type = "n", axes = FALSE)
  axis(side = 1, at = seq(xlim[1], xlim[2]), labels = FALSE, tcl = -0.2)
  axis(side = 1, at = pretty(xlim))
  axis(side = 2, at = pretty(ylim), las = 2)
  abline(h = pretty(ylim), col = "white")
  box()
}

# A helper to draw bars + ±2*SEM error bars.
draw_bars_with_errors <- function(
    years, means, sems,
    bar.space   = 0.2,
    bar.color   = "darkgreen",
    error.mult  = 2
) {
  for (i in seq_along(years)) {
    bar.x <- c(
      rep(years[i] - bar.space, 2),
      rep(years[i] + bar.space, 2)
    )
    bar.y <- c(0, means[i], means[i], 0)
    polygon(bar.x, bar.y, col = bar.color)

    if (!is.na(sems[i])) {
      upper <- means[i] + error.mult * sems[i]
      lower <- means[i] - error.mult * sems[i]
      lines(c(years[i], years[i]), c(lower, upper))
      lines(c(years[i] - bar.space, years[i] + bar.space), rep(lower, 2))
      lines(c(years[i] - bar.space, years[i] + bar.space), rep(upper, 2))
    }
  }
}

# A small helper to draw a dashed baseline line and label it
# near the *mean* of the baseline years on the x‐axis.
draw_baseline <- function(baseline_years, df, col, label) {
  # baseline_years: numeric vector of baseline years
  # df: data frame with Year, Mean columns
  # col: color for abline
  # label: text to place near baseline line
  b_rows <- df[df$Year %in% baseline_years, ]
  if (nrow(b_rows) == 0) return(NULL)

  b_mean <- mean(b_rows$Mean, na.rm = TRUE)
  if (!is.finite(b_mean)) return(NULL)

  abline(h = b_mean, col = col, lty = 2)

  # Place the label in the middle of the baseline years:
  x_label <- mean(b_rows$Year, na.rm = TRUE)
  # pos=3 means "above" the line
  text(x_label, b_mean, label, pos = 3, cex = 0.8)
}

######################################################
## 2) INDIVIDUAL PLOT FUNCTIONS
##    (Now includes a "perennial cover" function)
######################################################

# A) Perennial Cover (assumes a 'Cover' column in your data)
plot_perennial_cover <- function(
    PID,
    transects,
    baseline_years = 1984:1988,
    xlim = c(1985, 2025),
    bar.space = 0.2,
    asterisk.offset = 2
) {
  df <- subset(transects, Parcel == PID)
  if (nrow(df) == 0) {
    plot.new()
    text(0.5, 0.5, "No data")
    return()
  }

  stats <- df %>%
    dplyr::group_by(Year) %>%
    dplyr::summarise(
      Mean = mean(Cover, na.rm = TRUE),
      SEM  = stderr(Cover)
    ) %>%
    dplyr::arrange(Year)

  y_min <- min(stats$Mean, na.rm=TRUE) - 2
  y_max <- max(stats$Mean, na.rm=TRUE) + 3
  if (!is.finite(y_min) || !is.finite(y_max)) {
    plot.new()
    text(0.5, 0.5, "No valid Perennial Cover data")
    return()
  }
  ylim <- c(min(0, y_min), y_max)

  setup_blank_plot(xlim, ylim, ylab = "Perennial Cover [%]")

  # Dashed baseline
  draw_baseline(baseline_years, stats, col="darkgreen", label="Baseline Cover")

  draw_bars_with_errors(
    years   = stats$Year,
    means   = stats$Mean,
    sems    = stats$SEM,
    bar.space = bar.space,
    bar.color = "darkgreen",
    error.mult = 2
  )

  # T‐test vs. baseline
  stats$pval <- NA_real_
  for (i in seq_len(nrow(stats))) {
    y <- stats$Year[i]
    if (y %in% baseline_years) next
    baseline_data <- df$Cover[df$Year %in% baseline_years]
    new_data      <- df$Cover[df$Year == y]
    stats$pval[i] <- baseline_test(baseline_data, new_data)
  }

  # Asterisks if significantly lower than baseline
  sig_rows <- subset(stats, !is.na(pval) & pval < 0.05)
  for (r in seq_len(nrow(sig_rows))) {
    yr    <- sig_rows$Year[r]
    m_val <- sig_rows$Mean[r]
    text(
      x     = yr,
      y     = m_val + asterisk.offset,
      labels= "**",
      col   = "red",
      adj   = 0.5
    )
  }
}


# B) Shrub Cover
plot_shrubcover <- function(
    PID,
    transects,
    baseline_years = 1984:1988,
    xlim = c(1985, 2025),
    bar.space = 0.2,
    asterisk.offset = 2
) {
  df <- subset(transects, Parcel == PID)
  if (nrow(df) == 0) {
    plot.new()
    text(0.5, 0.5, "No data")
    return()
  }

  stats <- df %>%
    dplyr::group_by(Year) %>%
    dplyr::summarise(
      Mean = mean(Shrub, na.rm = TRUE),
      SEM  = stderr(Shrub)
    ) %>%
    dplyr::arrange(Year)

  y_min <- min(stats$Mean, na.rm=TRUE) - 2
  y_max <- max(stats$Mean, na.rm=TRUE) + 3
  if (!is.finite(y_min) || !is.finite(y_max)) {
    plot.new()
    text(0.5, 0.5, "No valid Shrub data")
    return()
  }
  ylim <- c(min(0, y_min), y_max)

  setup_blank_plot(xlim, ylim, ylab = "Shrub Cover [%]")

  # Draw dashed baseline
  draw_baseline(baseline_years, stats, col="brown", label="Baseline Shrub")

  draw_bars_with_errors(
    years   = stats$Year,
    means   = stats$Mean,
    sems    = stats$SEM,
    bar.space = bar.space,
    bar.color = "brown",
    error.mult = 2
  )

  # T‐test vs. baseline
  stats$pval <- NA_real_
  for (i in seq_len(nrow(stats))) {
    y <- stats$Year[i]
    if (y %in% baseline_years) next
    baseline_data <- df$Shrub[df$Year %in% baseline_years]
    new_data      <- df$Shrub[df$Year == y]
    stats$pval[i] <- baseline_test(baseline_data, new_data)
  }

  # Asterisks for p < 0.05
  sig_rows <- subset(stats, !is.na(pval) & pval < 0.05)
  for (r in seq_len(nrow(sig_rows))) {
    yr    <- sig_rows$Year[r]
    m_val <- sig_rows$Mean[r]
    text(
      x     = yr,
      y     = m_val + asterisk.offset,
      labels= "**",
      col   = "red",
      adj   = 0.5
    )
  }
}


# C) Grass Cover
plot_grasscover <- function(
    PID,
    transects,
    baseline_years = 1984:1988,
    xlim = c(1985, 2025),
    bar.space = 0.2,
    asterisk.offset = 2
) {
  df <- subset(transects, Parcel == PID)

  stats <- df %>%
    dplyr::group_by(Year) %>%
    dplyr::summarise(
      Mean = mean(Grass, na.rm = TRUE),
      SEM  = stderr(Grass)
    ) %>%
    dplyr::arrange(Year)

  if (nrow(stats) == 0) {
    plot.new()
    text(0.5, 0.5, "No valid Grass data")
    return()
  }

  y_min <- min(stats$Mean, na.rm=TRUE) - 2
  y_max <- max(stats$Mean, na.rm=TRUE) + 3
  ylim  <- c(min(0, y_min), y_max)

  setup_blank_plot(xlim, ylim, ylab = "Grass Cover [%]")

  draw_baseline(baseline_years, stats, col="darkgreen", label="Baseline Grass")

  draw_bars_with_errors(
    years   = stats$Year,
    means   = stats$Mean,
    sems    = stats$SEM,
    bar.space = bar.space,
    bar.color = "darkgreen",
    error.mult = 2
  )

  # T‐test
  stats$pval <- NA_real_
  for (i in seq_len(nrow(stats))) {
    y <- stats$Year[i]
    if (y %in% baseline_years) next
    baseline_data <- df$Grass[df$Year %in% baseline_years]
    new_data      <- df$Grass[df$Year == y]
    stats$pval[i] <- baseline_test(baseline_data, new_data)
  }

  sig_rows <- subset(stats, !is.na(pval) & pval < 0.05)
  for (r in seq_len(nrow(sig_rows))) {
    yr    <- sig_rows$Year[r]
    m_val <- sig_rows$Mean[r]
    text(yr, m_val + asterisk.offset, "**", col="red", adj=0.5)
  }
}


# D) DTW (draw as a line chart, typically no t‐test)
plot_dtw <- function(
    PID,
    dtw_data,
    baseline_years = 1984:1987,
    xlim = c(1985, 2025)
) {
  df <- dtw_data[dtw_data$Parcel == PID, , drop=FALSE]
  if (nrow(df) == 0) {
    plot.new()
    text(0.5, 0.5, "No DTW data")
    return()
  }

  df <- df[order(df$Year), ]
  y_max <- max(df$DTW, na.rm=TRUE) + 1
  y_min <- min(df$DTW, na.rm=TRUE) - 1
  # Reverse scale if you prefer 0 at top
  ylim <- c(y_max, min(y_min, 0))

  setup_blank_plot(xlim, ylim, ylab = "DTW (ft BGS)")

  # Baseline line
  base_rows <- df[df$Year %in% baseline_years, ]
  if (nrow(base_rows) > 0) {
    b_mean <- mean(base_rows$DTW, na.rm=TRUE)
    if (is.finite(b_mean)) {
      abline(h = b_mean, col = "blue", lty=2)
      text(mean(base_rows$Year, na.rm=TRUE), b_mean,
           "DTW Baseline", pos=3, cex=0.8)
    }
  }

  lines(df$Year, df$DTW, type="o", pch=16, col="blue")
}


# E) NDVI (bar chart, no t‐test shown)
plot_ndvi <- function(
    PID,
    rs_data,
    baseline_years = 1986,
    xlim = c(1985, 2025),
    bar.space = 0.2
) {
  df <- rs_data[rs_data$Parcel == PID, , drop=FALSE]
  if (nrow(df) == 0) {
    plot.new()
    text(0.5, 0.5, "No NDVI data")
    return()
  }

  df <- df[order(df$Year), ]
  y_max <- max(df$NDVI_SUR, na.rm=TRUE) + 0.05
  y_min <- min(df$NDVI_SUR, na.rm=TRUE) - 0.05
  setup_blank_plot(xlim, c(y_min, y_max), ylab = "NDVI")

  # Baseline line
  b_rows <- df[df$Year %in% baseline_years, ]
  if (nrow(b_rows) > 0) {
    b_mean <- mean(b_rows$NDVI_SUR, na.rm=TRUE)
    if (is.finite(b_mean)) {
      abline(h = b_mean, col = "darkgreen", lty=2)
      text(mean(b_rows$Year, na.rm=TRUE), b_mean,
           "1986 NDVI", pos=3, cex=0.8)
    }
  }

  for (i in seq_len(nrow(df))) {
    bar.x <- c(
      rep(df$Year[i] - bar.space, 2),
      rep(df$Year[i] + bar.space, 2)
    )
    bar.y <- c(0, df$NDVI_SUR[i], df$NDVI_SUR[i], 0)
    polygon(bar.x, bar.y, col="darkgreen")
  }
}


# F) PPT (inches instead of mm)
plot_ppt_in <- function(
    PID,
    rs_data,
    baseline_years = 1986,
    xlim = c(1985, 2025),
    bar.space = 0.2
) {
  df <- rs_data[rs_data$Parcel == PID, , drop=FALSE]
  if (nrow(df) == 0) {
    plot.new()
    text(0.5, 0.5, "No PPT data")
    return()
  }

  df <- df[order(df$Year), ]
  # Convert from mm to inches
  df$PPT_in <- df$PPT / 25.4

  y_max <- max(df$PPT_in, na.rm=TRUE) + 0.5
  y_min <- min(df$PPT_in, na.rm=TRUE) - 0.5
  setup_blank_plot(xlim, c(min(0,y_min), y_max), ylab = "Precip (in)")

  # Baseline line
  b_rows <- df[df$Year %in% baseline_years, ]
  if (nrow(b_rows) > 0) {
    b_mean <- mean(b_rows$PPT_in, na.rm=TRUE)
    if (is.finite(b_mean)) {
      abline(h = b_mean, col = "darkblue", lty=2)
      text(mean(b_rows$Year, na.rm=TRUE), b_mean,
           "1986 PPT (in)", pos=3, cex=0.8)
    }
  }

  for (i in seq_len(nrow(df))) {
    bar.x <- c(
      rep(df$Year[i] - bar.space, 2),
      rep(df$Year[i] + bar.space, 2)
    )
    bar.y <- c(0, df$PPT_in[i], df$PPT_in[i], 0)
    polygon(bar.x, bar.y, col="darkblue")
  }
}


###############################################################
## 3) MAIN FUNCTION TO PRODUCE 6‐PANEL PLOT FOR ONE PARCEL
##    1) Perennial Cover
##    2) Shrub Cover
##    3) Grass Cover
##    4) DTW
##    5) NDVI
##    6) PPT (inches)
###############################################################
plot_six_timeseries <- function(
    PID,
    transects,
    dtw_data,
    rs_data,
    ...
) {
  par(mfrow = c(6,1), mar = c(3,4,2,2))

  # 1) Perennial Cover
  plot_perennial_cover(PID, transects, ...)

  # 2) Shrub
  plot_shrubcover(PID, transects, ...)

  # 3) Grass
  plot_grasscover(PID, transects, ...)

  # 4) DTW
  plot_dtw(PID, dtw_data, ...)

  # 5) NDVI
  plot_ndvi(PID, rs_data, ...)

  # 6) PPT (in)
  plot_ppt_in(PID, rs_data, ...)
}


#################################################################
## 4) FUNCTION TO LOOP OVER PIDs, CREATING A MULTI‐PAGE PDF
##    Title is folded into the figure caption, not a separate mtext.
#################################################################
create_ts_plots_pdf <- function(
    PIDs,
    pdf_file   = "Parcel_TimeSeries.pdf",
    attributes = NULL,  # optional: to add descriptor in figure caption
    transects,
    dtw_data,
    rs_data
) {
  pdf(pdf_file, width = 8.5, height = 11)
  on.exit(dev.off(), add=TRUE)  # ensure PDF closes even if error

  fig_num <- 1
  for (pid in PIDs) {
    # For each parcel, we do one page of 6 stacked plots
    plot_six_timeseries(
      PID       = pid,
      transects = transects,
      dtw_data  = dtw_data,
      rs_data   = rs_data
    )

    # Build a caption that includes the old "title" or descriptors
    # from your attributes table:
    if (!is.null(attributes)) {
      att <- dplyr::filter(attributes, Parcel == pid)
      # You can add as much detail as you want here:
      descriptor_line <- paste0(
        "(W/C): ", att$Type,
        " | Type: ", att$GB_TYPE,
        " | ", att$Holland,
        " | ESD: ", att$Ecologic_3,
        " | Geomorphic: ", att$geomdesc
      )

      # Then fold it into the figure caption:
      cap_text <- paste0(
        "Figure ", fig_num,
        ": Baseline vs. Reinventory (* p < 0.05). ",
        "Parcel = ", pid, ". ",
        descriptor_line
      )
    } else {
      cap_text <- paste(
        "Figure", fig_num,
        ": Baseline vs. Reinventory (* p < 0.05). Parcel =", pid
      )
    }
    mtext(cap_text, side=1, line=2, cex=0.8)

    fig_num <- fig_num + 1
  }

  message("PDF saved to: ", pdf_file)
}

############################################################################
## 5) EXAMPLE USAGE:
## create_ts_plots_pdf(
##   PIDs       = unique(AttributesPID$Parcel),
##   pdf_file   = "Parcel_TimeSeries.pdf",
##   attributes = attributes_pfix,
##   transects  = transects,
##   dtw_data   = dtw_pfix,
##   rs_data    = rs_pfix
## )
############################################################################
