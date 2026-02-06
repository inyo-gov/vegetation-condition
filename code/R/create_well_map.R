# Function to create a map of wells, monitoring sites, and parcel
# Shows spatial context with buffers for well inclusion

library(ggplot2)
library(sf)
library(dplyr)
library(ggspatial)

# Function to create well map for a parcel
create_well_map <- function(parcel_id, 
                           max_distance_m = 5000,
                           exempt_max_distance_m = 10000,
                           output_file = NULL) {
  
  source("code/R/spatial_pumping_join.R")
  
  # Load spatial data
  linkage_result <- create_parcel_pumping_linkage(
    n_nearest_wells = 10,
    max_distance_m = max_distance_m,
    exempt_max_distance_m = exempt_max_distance_m
  )
  
  if (is.null(linkage_result)) {
    return(ggplot() + theme_void() + labs(title = "Could not load spatial data"))
  }
  
  parcels_sf <- linkage_result$parcels_sf
  wells_sf <- linkage_result$wells_sf
  monsites_sf <- linkage_result$monsites_sf
  
  # Get parcel of interest
  parcel_sf <- parcels_sf %>% filter(Parcel == parcel_id)
  
  if (nrow(parcel_sf) == 0) {
    return(ggplot() + theme_void() + labs(title = paste("Parcel", parcel_id, "not found")))
  }
  
  # Get parcel center for buffer (handle potential multipolygon)
  if (nrow(parcel_sf) > 0) {
    parcel_center <- st_centroid(st_geometry(parcel_sf))
    # Ensure CRS is set
    if (is.na(st_crs(parcel_center))) {
      st_crs(parcel_center) <- st_crs(parcel_sf)
    }
  } else {
    return(ggplot() + theme_void() + labs(title = "Parcel geometry not available"))
  }
  
  # Create buffers (ensure same CRS)
  buffer_nearest <- st_buffer(parcel_center, dist = max_distance_m)
  buffer_exempt <- st_buffer(parcel_center, dist = exempt_max_distance_m)
  
  # Ensure all spatial objects have same CRS
  target_crs <- st_crs(parcel_sf)
  if (!is.na(target_crs)) {
    if (!is.null(wells_sf) && nrow(wells_sf) > 0 && !is.na(st_crs(wells_sf))) {
      if (st_crs(wells_sf) != target_crs) {
        wells_sf <- st_transform(wells_sf, target_crs)
      }
    }
    if (!is.null(monsites_sf) && nrow(monsites_sf) > 0 && !is.na(st_crs(monsites_sf))) {
      if (st_crs(monsites_sf) != target_crs) {
        monsites_sf <- st_transform(monsites_sf, target_crs)
      }
    }
  }
  
  # Get wells linked to this parcel
  parcel_well_linkage <- linkage_result$parcel_well_linkage
  
  # Get parcel index
  parcel_info <- parcels_sf %>%
    st_drop_geometry() %>%
    filter(Parcel == parcel_id) %>%
    select(parcel_index, Parcel)
  
  if (nrow(parcel_info) == 0) {
    return(ggplot() + theme_void() + labs(title = "Parcel not found"))
  }
  
  parcel_index <- parcel_info$parcel_index[1]
  
  # Filter wells for this parcel
  parcel_wells <- parcel_well_linkage %>%
    filter(parcel_index == !!parcel_index) %>%
    distinct(well_id, source) %>%
    mutate(well_type = case_when(
      source == "nearest" ~ "nearest",
      source == "monitoring_site" ~ "monitoring",
      source == "exempt" ~ "exempt",
      TRUE ~ "unknown"
    ))
  
  # Get well locations
  if (!is.null(wells_sf) && nrow(wells_sf) > 0 && nrow(parcel_wells) > 0) {
    # Ensure well_id is character
    wells_sf$well_id <- as.character(wells_sf$well_id)
    parcel_wells$well_id <- as.character(parcel_wells$well_id)
    
    wells_included <- wells_sf %>%
      filter(well_id %in% parcel_wells$well_id) %>%
      left_join(parcel_wells %>% select(well_id, well_type), by = "well_id")
  } else {
    wells_included <- data.frame()
  }
  
  # Get closest monitoring site
  if (!is.null(monsites_sf) && nrow(monsites_sf) > 0) {
    distances <- st_distance(monsites_sf, parcel_center)
    closest_idx <- which.min(distances)
    closest_site <- monsites_sf[closest_idx, ]
  } else {
    closest_site <- data.frame()
  }
  
  # Get bounding box for map extent - combine bboxes from all features
  bboxes <- list(st_bbox(parcel_sf), st_bbox(buffer_exempt))
  if (nrow(wells_included) > 0) {
    bboxes[[length(bboxes) + 1]] <- st_bbox(wells_included)
  }
  if (nrow(closest_site) > 0) {
    bboxes[[length(bboxes) + 1]] <- st_bbox(closest_site)
  }
  
  # Get combined bbox
  xmins <- sapply(bboxes, function(b) b[1])
  ymins <- sapply(bboxes, function(b) b[2])
  xmaxs <- sapply(bboxes, function(b) b[3])
  ymaxs <- sapply(bboxes, function(b) b[4])
  
  bbox <- c(
    xmin = min(xmins, na.rm = TRUE),
    ymin = min(ymins, na.rm = TRUE),
    xmax = max(xmaxs, na.rm = TRUE),
    ymax = max(ymaxs, na.rm = TRUE)
  )
  names(bbox) <- c("xmin", "ymin", "xmax", "ymax")
  class(bbox) <- "bbox"
  # Expand bbox by 10%
  bbox_expanded <- bbox
  bbox_expanded[1] <- bbox[1] - (bbox[3] - bbox[1]) * 0.1
  bbox_expanded[2] <- bbox[2] - (bbox[4] - bbox[2]) * 0.1
  bbox_expanded[3] <- bbox[3] + (bbox[3] - bbox[1]) * 0.1
  bbox_expanded[4] <- bbox[4] + (bbox[4] - bbox[2]) * 0.1
  
  # Create map
  p <- ggplot() +
    # Background (if we have base map, add it here)
    
    # Buffer zones
    geom_sf(data = buffer_exempt, fill = "lightyellow", color = "orange", 
            linewidth = 1, alpha = 0.3, linetype = "dashed") +
    geom_sf(data = buffer_nearest, fill = "lightgreen", color = "green", 
            linewidth = 1, alpha = 0.3, linetype = "dashed") +
    
    # Wells (if available)
    {if (nrow(wells_included) > 0) {
      geom_sf(data = wells_included, 
              aes(color = well_type, shape = well_type),
              size = 3, alpha = 0.8)
    }} +
    
    # Monitoring sites
    {if (nrow(closest_site) > 0) {
      geom_sf(data = closest_site, 
              color = "blue", shape = 17, size = 4, alpha = 0.8)
    }} +
    
    # Parcel of interest
    geom_sf(data = parcel_sf, fill = "red", color = "darkred", 
            linewidth = 2, alpha = 0.5) +
    
    # Scale bar
    annotation_scale(location = "bl", width_hint = 0.3) +
    
    # North arrow
    annotation_north_arrow(location = "tr", which_north = "true",
                          style = north_arrow_fancy_orienteering) +
    
    # Labels
    {if (nrow(closest_site) > 0) {
      geom_sf_label(data = closest_site, 
                   aes(label = ifelse("SITENAME" %in% names(closest_site), SITENAME,
                                     ifelse("site_name" %in% names(closest_site), site_name, ""))),
                   nudge_y = 200, size = 3)
    }} +
    
    # Theme and labels
    theme_minimal() +
    theme(
      panel.grid = element_line(color = "gray90", linewidth = 0.5),
      panel.background = element_rect(fill = "white", color = NA),
      legend.position = "right"
    ) +
    labs(
      title = paste("Well Map for Parcel:", parcel_id),
      subtitle = paste0("Green buffer: ", max_distance_m/1000, " km (nearest wells), ",
                       "Orange buffer: ", exempt_max_distance_m/1000, " km (exempt wells)"),
      color = "Well Type",
      shape = "Well Type",
      x = "Longitude",
      y = "Latitude"
    ) +
    scale_color_manual(
      values = c("nearest" = "green", "monitoring" = "blue", "exempt" = "orange"),
      labels = c("nearest" = "Nearest Wells", "monitoring" = "Monitoring Site Wells", "exempt" = "Exempt Wells")
    ) +
    scale_shape_manual(
      values = c("nearest" = 16, "monitoring" = 15, "exempt" = 18),
      labels = c("nearest" = "Nearest Wells", "monitoring" = "Monitoring Site Wells", "exempt" = "Exempt Wells")
    ) +
    coord_sf(xlim = c(bbox_expanded[1], bbox_expanded[3]),
             ylim = c(bbox_expanded[2], bbox_expanded[4]),
             crs = st_crs(parcel_sf))
  
  # Save if output file specified
  if (!is.null(output_file)) {
    ggsave(output_file, p, width = 12, height = 10, dpi = 300)
    return(output_file)
  }
  
  return(p)
}

