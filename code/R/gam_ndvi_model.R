# Generalized Additive Model (GAM) for NDVI and DTW relationships
# Models:
# 1. NDVI as a function of PPT and DTW (flexible, no forced thresholds)
# 2. DTW change as a function of pumping, recharge (PPT_lag1), and previous DTW

library(mgcv)
library(dplyr)
library(ggplot2)
library(plotly)

# Function to create piecewise DTW terms
# Only creates segments for applicable thresholds
create_dtw_piecewise <- function(dtw, threshold1 = 6, threshold2 = 12, 
                                 use_threshold1 = TRUE, use_threshold2 = TRUE) {
  if (use_threshold1 && use_threshold2) {
    # Both thresholds: three segments
    data.frame(
      dtw_seg1 = pmax(0, pmin(threshold1, dtw)),  # 0 to threshold1
      dtw_seg2 = pmax(0, pmin(threshold2 - threshold1, pmax(0, dtw - threshold1))),  # threshold1 to threshold2
      dtw_seg3 = pmax(0, dtw - threshold2)  # > threshold2
    )
  } else if (use_threshold1 && !use_threshold2) {
    # Only first threshold: two segments
    data.frame(
      dtw_seg1 = pmax(0, pmin(threshold1, dtw)),  # 0 to threshold1
      dtw_seg2 = pmax(0, dtw - threshold1),  # > threshold1
      dtw_seg3 = 0  # Not used
    )
  } else {
    # No thresholds: single segment
    data.frame(
      dtw_seg1 = dtw,  # All data in one segment
      dtw_seg2 = 0,
      dtw_seg3 = 0
    )
  }
}

# Function to fit GAM model for NDVI
fit_gam_ndvi_model <- function(dtw_data, rs_data, pumping_data = NULL, 
                                threshold1 = 6, threshold2 = 12,
                                prefer_flexible = TRUE) {
  # Combine data
  combined <- dtw_data %>%
    select(Year, DTW) %>%
    full_join(
      rs_data %>% select(Year, NDVI_SUR, PPT),
      by = "Year"
    ) %>%
    filter(!is.na(DTW), !is.na(NDVI_SUR), !is.na(PPT)) %>%
    arrange(Year)
  
  if (nrow(combined) < 5) {
    return(NULL)
  }
  
  # Calculate DTW distribution statistics for adaptive thresholds
  dtw_min <- min(combined$DTW, na.rm = TRUE)
  dtw_max <- max(combined$DTW, na.rm = TRUE)
  dtw_q25 <- quantile(combined$DTW, 0.25, na.rm = TRUE)
  dtw_q75 <- quantile(combined$DTW, 0.75, na.rm = TRUE)
  dtw_range <- dtw_max - dtw_min
  
  # Determine which thresholds are needed based on data range
  # Thresholds represent ecological zones:
  # - Below ~6ft: Herbaceous plants (grasses) can access water
  # - 6-12ft: Transition zone, woody plants (shrubs) can access
  # - Above 12ft: Deep water table, water becomes limiting
  
  use_threshold1 <- FALSE
  use_threshold2 <- FALSE
  adaptive_threshold1 <- NULL
  adaptive_threshold2 <- NULL
  
  # If max DTW < 6ft: No thresholds needed (all in herbaceous zone, water not limiting)
  if (dtw_max < 6) {
    # All data is in shallow zone where water isn't limiting
    use_threshold1 <- FALSE
    use_threshold2 <- FALSE
  }
  # If max DTW < 12ft but min DTW < 6ft: Only first threshold at ~6ft
  else if (dtw_max < 12 && dtw_min < 6) {
    # Data spans herbaceous zone and transition, but not deep zone
    use_threshold1 <- TRUE
    use_threshold2 <- FALSE
    # Use adaptive threshold: 25th percentile or fixed 6ft, whichever is more appropriate
    adaptive_threshold1 <- max(dtw_q25, threshold1)
  }
  # If max DTW >= 12ft: Both thresholds needed
  else if (dtw_max >= 12) {
    # Data spans all zones
    use_threshold1 <- TRUE
    use_threshold2 <- TRUE
    # Use adaptive thresholds based on percentiles
    adaptive_threshold1 <- max(dtw_q25, threshold1)
    adaptive_threshold2 <- min(dtw_q75, threshold2)
    # Ensure threshold2 > threshold1
    if (adaptive_threshold2 <= adaptive_threshold1) {
      adaptive_threshold2 <- max(adaptive_threshold1 + 2, threshold2)
    }
  }
  # If min DTW >= 6ft but max < 12ft: Only first threshold might be useful
  else if (dtw_min >= 6 && dtw_max < 12) {
    # Data is in transition zone only
    use_threshold1 <- TRUE
    use_threshold2 <- FALSE
    adaptive_threshold1 <- max(dtw_q25, threshold1)
  }
  # If min DTW >= 12ft: No thresholds needed (all deep, water limiting throughout)
  else if (dtw_min >= 12) {
    # All data is in deep zone where water is limiting
    use_threshold1 <- FALSE
    use_threshold2 <- FALSE
  }
  
  # Store threshold information
  threshold_info <- list(
    use_threshold1 = use_threshold1,
    use_threshold2 = use_threshold2,
    adaptive_threshold1 = adaptive_threshold1,
    adaptive_threshold2 = adaptive_threshold2,
    fixed_threshold1 = threshold1,
    fixed_threshold2 = threshold2,
    dtw_min = dtw_min,
    dtw_max = dtw_max,
    dtw_range = dtw_range,
    dtw_q25 = dtw_q25,
    dtw_q75 = dtw_q75
  )
  
  # Create piecewise DTW terms only if thresholds are applicable
  if (use_threshold1 || use_threshold2) {
    # Use adaptive thresholds if available, otherwise use fixed
    thresh1 <- ifelse(use_threshold1, adaptive_threshold1, threshold1)
    thresh2 <- ifelse(use_threshold2, adaptive_threshold2, threshold2)
    dtw_piecewise <- create_dtw_piecewise(combined$DTW, thresh1, thresh2, 
                                         use_threshold1, use_threshold2)
    combined <- cbind(combined, dtw_piecewise)
  } else {
    # No thresholds needed - will use flexible model
    combined$dtw_seg1 <- combined$DTW
    combined$dtw_seg2 <- 0
    combined$dtw_seg3 <- 0
  }
  
  # Try different GAM model formulations
  models <- list()
  aics <- numeric()
  
  # Model 0: Flexible tensor product smooth (no forced thresholds)
  # This lets the data determine the shape of the relationship
  tryCatch({
    models$flexible <- gam(NDVI_SUR ~ te(PPT, DTW, k = c(4, 4)),
                          data = combined, method = "REML")
    aics["flexible"] <- AIC(models$flexible)
  }, error = function(e) NULL)
  
  # Model 0b: Alternative flexible model with separate smooths and interaction
  tryCatch({
    models$flexible_interaction <- gam(NDVI_SUR ~ s(PPT, k = 4) + 
                                      s(DTW, k = 4) + 
                                      ti(PPT, DTW, k = c(3, 3)),
                                      data = combined, method = "REML")
    aics["flexible_interaction"] <- AIC(models$flexible_interaction)
  }, error = function(e) NULL)
  
  # Model 1: Smooth terms for PPT and piecewise DTW (only if thresholds are used)
  if (use_threshold1 || use_threshold2) {
    tryCatch({
      if (use_threshold1 && use_threshold2) {
        models$smooth <- gam(NDVI_SUR ~ s(PPT, k = 4) + 
                             s(dtw_seg1, k = 3) + 
                             s(dtw_seg2, k = 3) + 
                             s(dtw_seg3, k = 3),
                             data = combined, method = "REML")
      } else if (use_threshold1) {
        models$smooth <- gam(NDVI_SUR ~ s(PPT, k = 4) + 
                             s(dtw_seg1, k = 3) + 
                             s(dtw_seg2, k = 3),
                             data = combined, method = "REML")
      }
      aics["smooth"] <- AIC(models$smooth)
    }, error = function(e) NULL)
  }
  
  # Model 2: Piecewise linear (only if thresholds are used)
  if (use_threshold1 || use_threshold2) {
    tryCatch({
      if (use_threshold1 && use_threshold2) {
        models$piecewise_linear <- gam(NDVI_SUR ~ PPT + 
                                       dtw_seg1 + dtw_seg2 + dtw_seg3,
                                       data = combined, method = "REML")
      } else if (use_threshold1) {
        models$piecewise_linear <- gam(NDVI_SUR ~ PPT + 
                                       dtw_seg1 + dtw_seg2,
                                       data = combined, method = "REML")
      }
      aics["piecewise_linear"] <- AIC(models$piecewise_linear)
    }, error = function(e) NULL)
  }
  
  # Model 3: Piecewise quadratic (only if thresholds are used)
  if (use_threshold1 || use_threshold2) {
    tryCatch({
      if (use_threshold1 && use_threshold2) {
        models$piecewise_quad <- gam(NDVI_SUR ~ PPT + 
                                    I(dtw_seg1^2) + dtw_seg1 +
                                    I(dtw_seg2^2) + dtw_seg2 +
                                    I(dtw_seg3^2) + dtw_seg3,
                                    data = combined, method = "REML")
      } else if (use_threshold1) {
        models$piecewise_quad <- gam(NDVI_SUR ~ PPT + 
                                    I(dtw_seg1^2) + dtw_seg1 +
                                    I(dtw_seg2^2) + dtw_seg2,
                                    data = combined, method = "REML")
      }
      aics["piecewise_quad"] <- AIC(models$piecewise_quad)
    }, error = function(e) NULL)
  }
  
  # Model 4: Piecewise smooth with segment indicators (only if thresholds are used)
  if (use_threshold1 || use_threshold2) {
    tryCatch({
      thresh1 <- ifelse(use_threshold1, adaptive_threshold1, threshold1)
      thresh2 <- ifelse(use_threshold2, adaptive_threshold2, threshold2)
      
      if (use_threshold1 && use_threshold2) {
        combined <- combined %>%
          mutate(
            in_seg1 = ifelse(DTW <= thresh1, 1, 0),
            in_seg2 = ifelse(DTW > thresh1 & DTW <= thresh2, 1, 0),
            in_seg3 = ifelse(DTW > thresh2, 1, 0)
          )
        
        models$piecewise_smooth <- gam(NDVI_SUR ~ s(PPT, k = 4) + 
                                      s(DTW, by = in_seg1, k = 3) +
                                      s(DTW, by = in_seg2, k = 3) +
                                      s(DTW, by = in_seg3, k = 3),
                                      data = combined, method = "REML")
      } else if (use_threshold1) {
        combined <- combined %>%
          mutate(
            in_seg1 = ifelse(DTW <= thresh1, 1, 0),
            in_seg2 = ifelse(DTW > thresh1, 1, 0),
            in_seg3 = 0
          )
        
        models$piecewise_smooth <- gam(NDVI_SUR ~ s(PPT, k = 4) + 
                                      s(DTW, by = in_seg1, k = 3) +
                                      s(DTW, by = in_seg2, k = 3),
                                      data = combined, method = "REML")
      }
      aics["piecewise_smooth"] <- AIC(models$piecewise_smooth)
    }, error = function(e) NULL)
  }
  
  # Select best model
  if (length(aics) == 0) {
    return(NULL)
  }
  
  # If prefer_flexible is TRUE, prefer flexible models to avoid forced thresholds
  # that can create artifacts in regions with no data
  if (prefer_flexible && any(c("flexible", "flexible_interaction") %in% names(aics))) {
    flexible_aics <- aics[names(aics) %in% c("flexible", "flexible_interaction")]
    best_model_name <- names(flexible_aics)[which.min(flexible_aics)]
    best_model <- models[[best_model_name]]
  } else {
    best_model_name <- names(aics)[which.min(aics)]
    best_model <- models[[best_model_name]]
  }
  
  return(list(
    model = best_model,
    model_name = best_model_name,
    models = models,
    aics = aics,
    data = combined,
    thresholds = c(ifelse(use_threshold1, adaptive_threshold1, threshold1),
                   ifelse(use_threshold2, adaptive_threshold2, threshold2)),
    threshold_info = threshold_info
  ))
}

# Function to predict NDVI from GAM model
predict_ndvi_gam <- function(gam_result) {
  if (is.null(gam_result) || is.null(gam_result$model)) {
    return(NULL)
  }
  
  data <- gam_result$data
  predictions <- predict(gam_result$model, newdata = data, type = "response")
  
  return(data.frame(
    Year = data$Year,
    NDVI_observed = data$NDVI_SUR,
    NDVI_predicted = predictions,
    PPT = data$PPT,
    DTW = data$DTW
  ))
}

# Function to extract GAM summary statistics
extract_gam_summary <- function(gam_result) {
  if (is.null(gam_result) || is.null(gam_result$model)) {
    return(NULL)
  }
  
  model <- gam_result$model
  summary_model <- summary(model)
  
  # Calculate R-squared
  y_obs <- gam_result$data$NDVI_SUR
  y_pred <- predict(model, type = "response")
  ss_res <- sum((y_obs - y_pred)^2, na.rm = TRUE)
  ss_tot <- sum((y_obs - mean(y_obs, na.rm = TRUE))^2, na.rm = TRUE)
  r_squared <- 1 - (ss_res / ss_tot)
  
  return(list(
    r_squared = r_squared,
    adj_r_squared = summary_model$r.sq,
    deviance_explained = summary_model$dev.expl,
    aic = AIC(model)
  ))
}

# Function to plot GAM results (2D plots)
plot_gam_ndvi_results <- function(gam_result) {
  if (is.null(gam_result) || is.null(gam_result$model)) {
    return(NULL)
  }
  
  data <- gam_result$data
  predictions <- predict(gam_result$model, newdata = data, type = "response")
  
  # Observed vs Predicted
  p1 <- ggplot(data.frame(Observed = data$NDVI_SUR, Predicted = predictions),
               aes(x = Observed, y = Predicted)) +
    geom_point(alpha = 0.6) +
    geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "red") +
    labs(title = "Observed vs Predicted NDVI",
         x = "Observed NDVI", y = "Predicted NDVI") +
    theme_minimal()
  
  # NDVI vs DTW
  p2 <- ggplot(data.frame(DTW = data$DTW, NDVI = data$NDVI_SUR, Predicted = predictions),
               aes(x = DTW, y = NDVI)) +
    geom_point(aes(color = "Observed"), alpha = 0.6) +
    geom_line(aes(y = Predicted, color = "Predicted"), linewidth = 1)
  
  # Add threshold lines only if they are applicable
  if (!is.null(gam_result$threshold_info)) {
    thresh_info <- gam_result$threshold_info
    if (thresh_info$use_threshold1) {
      p2 <- p2 + geom_vline(xintercept = gam_result$thresholds[1], 
                            linetype = "dashed", color = "green", alpha = 0.7)
    }
    if (thresh_info$use_threshold2) {
      p2 <- p2 + geom_vline(xintercept = gam_result$thresholds[2], 
                            linetype = "dashed", color = "orange", alpha = 0.7)
    }
  } else {
    # Fallback: use thresholds if they exist
    if (length(gam_result$thresholds) >= 1 && !is.na(gam_result$thresholds[1])) {
      p2 <- p2 + geom_vline(xintercept = gam_result$thresholds[1], 
                            linetype = "dashed", color = "green", alpha = 0.7)
    }
    if (length(gam_result$thresholds) >= 2 && !is.na(gam_result$thresholds[2])) {
      p2 <- p2 + geom_vline(xintercept = gam_result$thresholds[2], 
                            linetype = "dashed", color = "orange", alpha = 0.7)
    }
  }
  
  p2 <- p2 +
    labs(title = "NDVI vs Depth to Water (DTW)",
         x = "DTW (ft)", y = "NDVI",
         color = "Type") +
    scale_color_manual(values = c("Observed" = "black", "Predicted" = "blue")) +
    theme_minimal()
  
  # Time series
  p3 <- ggplot(data.frame(Year = data$Year, 
                         Observed = data$NDVI_SUR, 
                         Predicted = predictions),
               aes(x = Year)) +
    geom_line(aes(y = Observed, color = "Observed"), linewidth = 1) +
    geom_line(aes(y = Predicted, color = "Predicted"), linewidth = 1, linetype = "dashed") +
    geom_point(aes(y = Observed, color = "Observed"), size = 2) +
    labs(title = "NDVI Time Series",
         x = "Year", y = "NDVI",
         color = "Type") +
    scale_color_manual(values = c("Observed" = "black", "Predicted" = "blue")) +
    theme_minimal()
  
  return(list(
    observed_vs_predicted = p1,
    ndvi_vs_dtw = p2,
    time_series = p3
  ))
}

# Function to create 3D surface plot of GAM model
plot_gam_3d_surface <- function(gam_result, n_grid = 30) {
  if (is.null(gam_result) || is.null(gam_result$model)) {
    return(NULL)
  }

  data <- gam_result$data

  # Create grid of PPT and DTW values for surface
  ppt_range <- seq(min(data$PPT, na.rm = TRUE), max(data$PPT, na.rm = TRUE), length.out = n_grid)
  dtw_range <- seq(min(data$DTW, na.rm = TRUE), max(data$DTW, na.rm = TRUE), length.out = n_grid)

  # Create grid with PPT first, then DTW
  # expand.grid(PPT, DTW) creates: PPT[1]DTW[1], PPT[2]DTW[1], ..., PPT[n]DTW[1], PPT[1]DTW[2], ...
  # When filling matrix byrow=FALSE (column by column):
  # Column 1: PPT[1]DTW[1], PPT[2]DTW[1], ..., PPT[n]DTW[1] (all PPT at DTW[1])
  # Column 2: PPT[1]DTW[2], PPT[2]DTW[2], ..., PPT[n]DTW[2] (all PPT at DTW[2])
  # Result: rows = PPT, columns = DTW
  # For plotly: z[i][j] = value at (x[j], y[i]) where x=DTW, y=PPT
  # So we need: rows of z = PPT, columns of z = DTW ✓
  grid <- expand.grid(PPT = ppt_range, DTW = dtw_range)

  # Create piecewise terms for grid if needed
  if (!"dtw_seg1" %in% names(grid)) {
    dtw_piecewise <- create_dtw_piecewise(grid$DTW,
                                          gam_result$thresholds[1],
                                          gam_result$thresholds[2])
    grid <- cbind(grid, dtw_piecewise)
  }

  # Add segment indicators if needed for piecewise smooth model
  if (gam_result$model_name == "piecewise_smooth") {
    grid <- grid %>%
      mutate(
        in_seg1 = ifelse(DTW <= gam_result$thresholds[1], 1, 0),
        in_seg2 = ifelse(DTW > gam_result$thresholds[1] & DTW <= gam_result$thresholds[2], 1, 0),
        in_seg3 = ifelse(DTW > gam_result$thresholds[2], 1, 0)
      )
  }
  
  # For flexible models, we don't need piecewise terms
  if (gam_result$model_name %in% c("flexible", "flexible_interaction")) {
    # Remove piecewise terms if they exist
    grid <- grid %>% select(-any_of(c("dtw_seg1", "dtw_seg2", "dtw_seg3", 
                                      "in_seg1", "in_seg2", "in_seg3")))
  }

  # Predict NDVI for grid
  grid_pred <- predict(gam_result$model, newdata = grid, type = "response")
  grid$NDVI_predicted <- grid_pred

  # Create matrix for plotly surface plot
  # expand.grid(PPT, DTW) with byrow=FALSE gives:
  # rows = PPT, columns = DTW
  # For plotly: z[i][j] = value at (x[j], y[i]) where x=DTW, y=PPT
  # So z[i][j] = NDVI at DTW[j], PPT[i] ✓
  z_matrix <- matrix(grid$NDVI_predicted, nrow = n_grid, ncol = n_grid, byrow = FALSE)
  # Matrix has: rows = PPT, columns = DTW, so z[i][j] = NDVI at DTW[j], PPT[i] ✓

  # Get predictions at actual data points for verification
  data_preds <- predict(gam_result$model, newdata = data, type = "response")

  # Create 3D surface plot
  p_3d <- plot_ly() %>%
    add_surface(
      x = ~dtw_range,  # DTW on x-axis (columns of matrix)
      y = ~ppt_range,  # PPT on y-axis (rows of matrix)
      z = ~z_matrix,   # z[i][j] = NDVI at DTW[j], PPT[i]
      colorscale = "Viridis",
      showscale = TRUE,
      colorbar = list(title = "Predicted<br>NDVI"),
      name = "Predicted Surface",
      opacity = 0.7
    ) %>%
    # Add actual observed data points
    add_trace(
      type = "scatter3d",
      mode = "markers",
      x = data$DTW,  # Match x-axis (DTW)
      y = data$PPT,  # Match y-axis (PPT)
      z = data$NDVI_SUR,  # NDVI on z-axis
      marker = list(
        size = 6,
        color = "red",
        symbol = "circle",
        line = list(color = "darkred", width = 1),
        opacity = 0.9
      ),
      name = "Actual NDVI",
      text = paste("Year:", data$Year,
                   "<br>Observed NDVI:", round(data$NDVI_SUR, 3),
                   "<br>PPT:", round(data$PPT, 1),
                   "<br>DTW:", round(data$DTW, 1)),
      hoverinfo = "text"
    ) %>%
    # Add predicted values at actual data points
    add_trace(
      type = "scatter3d",
      mode = "markers",
      x = data$DTW,
      y = data$PPT,
      z = data_preds,  # Predicted values at actual data points
      marker = list(
        size = 4,
        color = "blue",
        symbol = "diamond",
        line = list(color = "darkblue", width = 1),
        opacity = 0.8
      ),
      name = "Predicted at Actual Points",
      text = paste("Year:", data$Year,
                   "<br>Predicted NDVI:", round(data_preds, 3),
                   "<br>PPT:", round(data$PPT, 1),
                   "<br>DTW:", round(data$DTW, 1)),
      hoverinfo = "text"
    ) %>%
    layout(
      title = list(
        text = "GAM Model: NDVI ~ DTW + PPT (3D Surface)",
        font = list(size = 16)
      ),
      scene = list(
        xaxis = list(title = "Depth to Water (DTW, ft)",
                    backgroundcolor = "rgb(230, 230, 230)",
                    gridcolor = "rgb(255, 255, 255)",
                    showbackground = TRUE,
                    zerolinecolor = "rgb(255, 255, 255)"),
        yaxis = list(title = "Precipitation (PPT)",
                    backgroundcolor = "rgb(230, 230, 230)",
                    gridcolor = "rgb(255, 255, 255)",
                    showbackground = TRUE,
                    zerolinecolor = "rgb(255, 255, 255)"),
        zaxis = list(title = "NDVI",
                    backgroundcolor = "rgb(230, 230, 230)",
                    gridcolor = "rgb(255, 255, 255)",
                    showbackground = TRUE,
                    zerolinecolor = "rgb(255, 255, 255)"),
        camera = list(
          eye = list(x = 1.5, y = 1.5, z = 1.2)
        ),
        aspectmode = "cube"
      ),
      margin = list(l = 0, r = 0, b = 0, t = 50)
    )

  return(p_3d)
}

# Function to fit GAM model for DTW change
# Models: DTW ~ DTW_lag1 + Recharge (PPT_lag1) + Pumping
# This corresponds to the first part of the SEM: how pumping, recharge, and previous DTW influence current DTW
fit_gam_dtw_model <- function(dtw_data, rs_data, pumping_data = NULL, 
                               prefer_flexible = TRUE) {
  # Prepare data similar to SEM
  combined <- dtw_data %>%
    select(Year, DTW) %>%
    full_join(
      rs_data %>% select(Year, PPT),
      by = "Year"
    ) %>%
    filter(!is.na(DTW), !is.na(PPT)) %>%
    arrange(Year)
  
  # Add pumping if available
  if (!is.null(pumping_data) && nrow(pumping_data) > 0) {
    combined <- combined %>%
      left_join(
        pumping_data %>% select(Year, total_pumping_AF),
        by = "Year"
      )
  } else {
    combined$total_pumping_AF <- NA
  }
  
  if (nrow(combined) < 5) {
    return(NULL)
  }
  
  # Create lagged variables
  combined <- combined %>%
    mutate(
      DTW_lag1 = lag(DTW, n = 1),
      PPT_lag1 = lag(PPT, n = 1)
    ) %>%
    filter(!is.na(DTW_lag1), !is.na(PPT_lag1))
  
  if (nrow(combined) < 3) {
    return(NULL)
  }
  
  # Check if pumping data is available
  has_pumping <- !is.null(pumping_data) && 
                 nrow(pumping_data) > 0 && 
                 "total_pumping_AF" %in% names(combined) &&
                 sum(!is.na(combined$total_pumping_AF)) > 0
  
  # Try different GAM model formulations
  models <- list()
  aics <- numeric()
  
  # Model 1: Flexible tensor product (if pumping available)
  if (has_pumping) {
    tryCatch({
      models$flexible <- gam(DTW ~ te(DTW_lag1, PPT_lag1, total_pumping_AF, k = c(3, 3, 3)),
                            data = combined, method = "REML")
      aics["flexible"] <- AIC(models$flexible)
    }, error = function(e) NULL)
    
    # Model 2: Separate smooths with interaction
    tryCatch({
      models$flexible_interaction <- gam(DTW ~ s(DTW_lag1, k = 4) + 
                                         s(PPT_lag1, k = 4) + 
                                         s(total_pumping_AF, k = 4) +
                                         ti(DTW_lag1, PPT_lag1, k = c(3, 3)) +
                                         ti(DTW_lag1, total_pumping_AF, k = c(3, 3)),
                                        data = combined, method = "REML")
      aics["flexible_interaction"] <- AIC(models$flexible_interaction)
    }, error = function(e) NULL)
    
    # Model 3: Linear with smooth terms
    tryCatch({
      models$smooth_linear <- gam(DTW ~ DTW_lag1 + s(PPT_lag1, k = 4) + s(total_pumping_AF, k = 4),
                                  data = combined, method = "REML")
      aics["smooth_linear"] <- AIC(models$smooth_linear)
    }, error = function(e) NULL)
  } else {
    # Without pumping
    tryCatch({
      models$flexible <- gam(DTW ~ te(DTW_lag1, PPT_lag1, k = c(4, 4)),
                            data = combined, method = "REML")
      aics["flexible"] <- AIC(models$flexible)
    }, error = function(e) NULL)
    
    tryCatch({
      models$flexible_interaction <- gam(DTW ~ s(DTW_lag1, k = 4) + 
                                         s(PPT_lag1, k = 4) + 
                                         ti(DTW_lag1, PPT_lag1, k = c(3, 3)),
                                        data = combined, method = "REML")
      aics["flexible_interaction"] <- AIC(models$flexible_interaction)
    }, error = function(e) NULL)
    
    tryCatch({
      models$smooth_linear <- gam(DTW ~ DTW_lag1 + s(PPT_lag1, k = 4),
                                  data = combined, method = "REML")
      aics["smooth_linear"] <- AIC(models$smooth_linear)
    }, error = function(e) NULL)
  }
  
  # Select best model
  if (length(aics) == 0) {
    return(NULL)
  }
  
  # Prefer flexible models to avoid artifacts
  if (prefer_flexible && any(c("flexible", "flexible_interaction") %in% names(aics))) {
    flexible_aics <- aics[names(aics) %in% c("flexible", "flexible_interaction")]
    best_model_name <- names(flexible_aics)[which.min(flexible_aics)]
    best_model <- models[[best_model_name]]
  } else {
    best_model_name <- names(aics)[which.min(aics)]
    best_model <- models[[best_model_name]]
  }
  
  return(list(
    model = best_model,
    model_name = best_model_name,
    models = models,
    aics = aics,
    data = combined,
    has_pumping = has_pumping
  ))
}

# Function to extract GAM summary for DTW model
extract_gam_dtw_summary <- function(gam_result) {
  if (is.null(gam_result) || is.null(gam_result$model)) {
    return(NULL)
  }
  
  model <- gam_result$model
  summary_model <- summary(model)
  
  # Calculate R-squared
  y_obs <- gam_result$data$DTW
  y_pred <- predict(model, type = "response")
  ss_res <- sum((y_obs - y_pred)^2, na.rm = TRUE)
  ss_tot <- sum((y_obs - mean(y_obs, na.rm = TRUE))^2, na.rm = TRUE)
  r_squared <- 1 - (ss_res / ss_tot)
  
  return(list(
    r_squared = r_squared,
    adj_r_squared = summary_model$r.sq,
    deviance_explained = summary_model$dev.expl,
    aic = AIC(model)
  ))
}

