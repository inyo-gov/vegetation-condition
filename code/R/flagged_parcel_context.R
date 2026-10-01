# Flagged-parcel profile context panel (hillshade + photo + decade spp-rank).
# Assets live under www/flagged_parcel_maps/{PCL}/; graceful no-op if missing.
# Research / measurability framing only — not I.C.1.b attributability.
#
# Map path: static context-overlay PNG (boundary/adjoining/transects/photo burned in)
# plus optional lightweight Leaflet ImageOverlay when htmlwidgets print cleanly.
# Quarto parcel loop uses results='asis' — prefer cat(HTML) over print(htmlwidget).

suppressPackageStartupMessages({
  if (!requireNamespace("dplyr", quietly = TRUE)) stop("dplyr required")
  if (!requireNamespace("readr", quietly = TRUE)) stop("readr required")
  if (!requireNamespace("jsonlite", quietly = TRUE)) stop("jsonlite required")
  if (!requireNamespace("htmltools", quietly = TRUE)) stop("htmltools required")
})
if (!exists("%>%", mode = "function")) {
  `%>%` <- dplyr::`%>%`
}

flagged_parcel_maps_root <- function() {
  candidates <- c(
    here::here("www", "flagged_parcel_maps"),
    here::here("data", "flagged_parcel_maps")
  )
  for (p in candidates) {
    if (dir.exists(p)) return(p)
  }
  candidates[[1]]
}

flagged_parcel_asset_dir <- function(parcel_id) {
  file.path(flagged_parcel_maps_root(), parcel_id)
}

flagged_parcel_has_context <- function(parcel_id) {
  d <- flagged_parcel_asset_dir(parcel_id)
  bounds <- file.path(d, "hillshade_bounds.json")
  png_ok <- length(list.files(d, pattern = "_hillshade_(preview|context_overlay)\\.png$")) > 0
  dir.exists(d) && file.exists(bounds) && isTRUE(png_ok)
}

.fp_read_json <- function(path) {
  if (!file.exists(path)) return(NULL)
  jsonlite::fromJSON(path, simplifyVector = TRUE)
}

.fp_escape_html <- function(x) {
  x <- as.character(x)
  x <- gsub("&", "&amp;", x, fixed = TRUE)
  x <- gsub("<", "&lt;", x, fixed = TRUE)
  x <- gsub(">", "&gt;", x, fixed = TRUE)
  x <- gsub('"', "&quot;", x, fixed = TRUE)
  x
}

.fp_find_file <- function(d, patterns) {
  for (pat in patterns) {
    hits <- list.files(d, pattern = pat, full.names = TRUE, recursive = TRUE)
    if (length(hits)) return(hits[[1]])
  }
  NULL
}

.fp_find_photo <- function(d) {
  photo_dir <- file.path(d, "photo")
  meta <- .fp_read_json(file.path(photo_dir, "photo_point_meta.json"))
  if (!is.null(meta) && !is.null(meta$photo_agol)) {
    p <- file.path(photo_dir, meta$photo_agol)
    if (file.exists(p)) return(list(path = p, meta = meta))
  }
  hits <- list.files(photo_dir, pattern = "_agol\\.(jpg|jpeg|JPG|JPEG)$", full.names = TRUE)
  if (length(hits)) return(list(path = hits[[1]], meta = meta))
  hits2 <- list.files(photo_dir, pattern = "\\.(jpg|jpeg|JPG|JPEG)$", full.names = TRUE)
  if (length(hits2)) return(list(path = hits2[[1]], meta = meta))
  NULL
}

#' Prefer site URL under /www/flagged_parcel_maps/... when the file lives there
#' (avoids multi-MB base64 in published HTML). Fall back to knitr::image_uri.
.fp_raster_uri <- function(path) {
  if (is.null(path) || !file.exists(path)) return(NULL)
  path_norm <- normalizePath(path, winslash = "/", mustWork = TRUE)
  # Match .../www/flagged_parcel_maps/<rest>
  m <- regexpr("/www/flagged_parcel_maps/.*$", path_norm, perl = TRUE)
  if (m[1] > 0) {
    return(substr(path_norm, m[1], nchar(path_norm)))
  }
  # here()-relative fallback if cwd differs
  root <- tryCatch(normalizePath(flagged_parcel_maps_root(), winslash = "/", mustWork = FALSE), error = function(e) "")
  if (nzchar(root) && startsWith(path_norm, root)) {
    rel <- substring(path_norm, nchar(root) + 1L)
    return(paste0("/www/flagged_parcel_maps", rel))
  }
  knitr::image_uri(path)
}


#' Inline Leaflet ImageOverlay + GeoJSON as a self-contained HTML fragment (no htmlwidget).
#' BWMA-preview-style layer panel (checkboxes) so hillshade / CHM / height bins / SAM
#' can be toggled without hunting clicks. Reusable for any parcel with www/flagged_parcel_maps/{PCL}/.
.fp_inline_leaflet_html <- function(parcel_id, d, bounds, preview_png) {
  b <- bounds$bounds_wgs84
  if (is.null(b) || !all(c("west", "south", "east", "north") %in% names(b))) {
    return(NULL)
  }
  west <- as.numeric(b$west); south <- as.numeric(b$south)
  east <- as.numeric(b$east); north <- as.numeric(b$north)
  img_uri <- .fp_raster_uri(preview_png)
  if (is.null(img_uri)) return(NULL)

  read_gj <- function(name) {
    path <- file.path(d, "overlays", name)
    if (!file.exists(path)) return("null")
    raw <- paste(readLines(path, warn = FALSE, encoding = "UTF-8"), collapse = "\n")
    parsed <- tryCatch(jsonlite::fromJSON(raw, simplifyVector = FALSE), error = function(e) NULL)
    if (is.null(parsed)) return("null")
    jsonlite::toJSON(parsed, auto_unbox = TRUE, null = "null")
  }

  boundary <- read_gj("parcel_boundary_wgs84.geojson")
  adjoining <- read_gj("adjoining_parcels_wgs84.geojson")
  starts <- read_gj("transect_starts_All_2026_wgs84.geojson")
  photo_pt <- read_gj("representative_photo_point_wgs84.geojson")

  # Optional research overlay (SAM × LPT) — not TYPE / not I.C.1.b
  read_hetero <- function(name) {
    path <- file.path(d, "heterogeneity", name)
    if (!file.exists(path)) return("null")
    raw <- paste(readLines(path, warn = FALSE, encoding = "UTF-8"), collapse = "\n")
    parsed <- tryCatch(jsonlite::fromJSON(raw, simplifyVector = FALSE), error = function(e) NULL)
    if (is.null(parsed)) return("null")
    jsonlite::toJSON(parsed, auto_unbox = TRUE, null = "null")
  }
  sam_outlines <- read_hetero(paste0(parcel_id, "_sam_outlines_web_wgs84.geojson"))
  labeled_segs <- read_hetero(paste0(parcel_id, "_labeled_segments_web_wgs84.geojson"))
  hetero_meta <- .fp_read_json(file.path(d, "heterogeneity", "research_overlay_meta.json"))
  has_hetero <- !identical(sam_outlines, "null") || !identical(labeled_segs, "null")

  # Optional CHM / height-class PNG overlays (BWMA-consistent bins)
  height_dir <- file.path(d, "height")
  height_meta <- .fp_read_json(file.path(height_dir, "height_layers_meta.json"))
  uri_or_null <- function(path) {
    if (is.null(path) || !file.exists(path)) return("null")
    u <- .fp_raster_uri(path)
    if (is.null(u)) return("null")
    jsonlite::toJSON(u, auto_unbox = TRUE)
  }
  chm_strata_png <- .fp_find_file(
    height_dir,
    c(paste0("^", parcel_id, "_chm_height_strata_preview\\.png$"), "_chm_height_strata_preview\\.png$")
  )
  shrub_mask_png <- .fp_find_file(
    height_dir,
    c(paste0("^", parcel_id, "_small_shrub_mask\\.png$"), "_small_shrub_mask\\.png$")
  )
  tree_mask_png <- .fp_find_file(
    height_dir,
    c(paste0("^", parcel_id, "_tree_mask\\.png$"), "_tree_mask\\.png$")
  )
  chm_uri <- uri_or_null(chm_strata_png)
  shrub_uri <- uri_or_null(shrub_mask_png)
  tree_uri <- uri_or_null(tree_mask_png)
  has_height <- !identical(chm_uri, "null") || !identical(shrub_uri, "null") || !identical(tree_uri, "null")

  shrub_lo <- if (!is.null(height_meta$thresholds_m$small_shrub[[1]])) height_meta$thresholds_m$small_shrub[[1]] else 0.3
  shrub_hi <- if (!is.null(height_meta$thresholds_m$small_shrub[[2]])) height_meta$thresholds_m$small_shrub[[2]] else 3.0
  tree_lo <- if (!is.null(height_meta$thresholds_m$tree[[1]])) height_meta$thresholds_m$tree[[1]] else 3.0

  map_id <- paste0("fp-map-", gsub("[^A-Za-z0-9]", "", parcel_id), "-", as.integer(runif(1, 1e6, 9e6)))
  panel_id <- paste0(map_id, "-layers")

  leaflet_css_path <- system.file("htmlwidgets/lib/leaflet/leaflet.css", package = "leaflet")
  leaflet_js_path <- system.file("htmlwidgets/lib/leaflet/leaflet.js", package = "leaflet")
  if (!nzchar(leaflet_css_path) || !nzchar(leaflet_js_path) ||
      !file.exists(leaflet_css_path) || !file.exists(leaflet_js_path)) {
    return(NULL)
  }
  css <- paste(readLines(leaflet_css_path, warn = FALSE), collapse = "\n")
  css <- gsub("</", "<\\/", css, fixed = TRUE)
  js_lib <- paste(readLines(leaflet_js_path, warn = FALSE), collapse = "\n")
  js_lib <- gsub("</", "<\\/", js_lib, fixed = TRUE)

  img_json <- jsonlite::toJSON(img_uri, auto_unbox = TRUE)

  # BWMA-style checkbox rows (only include layers that exist)
  layer_row <- function(id_suffix, checked, swatch, label, hint = NULL) {
    paste0(
      "<div class=\"fp-layer\">",
      "<input type=\"checkbox\" id=\"", map_id, "-", id_suffix, "\"",
      if (isTRUE(checked)) " checked" else "", "/>",
      "<span class=\"fp-swatch\" style=\"background:", swatch, "\"></span>",
      "<label for=\"", map_id, "-", id_suffix, "\">", label,
      if (!is.null(hint) && nzchar(hint)) paste0("<span class=\"fp-hint\">", hint, "</span>") else "",
      "</label></div>\n"
    )
  }

  panel_rasters <- paste0(
    "<div class=\"fp-layer-group\">Rasters</div>\n",
    layer_row("lyr-hs", TRUE, "#9e9e9e", "Hillshade", "0.5 m Sierra 2022"),
    if (!identical(chm_uri, "null")) {
      layer_row("lyr-chm", FALSE, "linear-gradient(90deg,#2e7d32,#fbc02d,#fb8c00,#c62828)",
                "CHM height strata", "1&lt;0.5 · 2 0.5–1.5 · 3 1.5–3 · 4 &gt;3 m")
    } else "",
    if (!identical(shrub_uri, "null")) {
      layer_row("lyr-shrub", FALSE, "#fb8c00", "Small-shrub",
                paste0(shrub_lo, "–", shrub_hi, " m (BWMA liberal_v1)"))
    } else "",
    if (!identical(tree_uri, "null")) {
      layer_row("lyr-tree", FALSE, "#c62828", "Tree",
                paste0("&gt;", tree_lo, " m"))
    } else ""
  )

  panel_vectors <- paste0(
    "<div class=\"fp-layer-group\">Vectors</div>\n",
    layer_row("lyr-boundary", TRUE, "#ffcc00", "Parcel boundary", NULL),
    layer_row("lyr-adjoining", TRUE, "#4fc3f7", "Adjoining parcels", NULL),
    layer_row("lyr-starts", TRUE, "#e53935", "Transect starts", "All_2026"),
    layer_row("lyr-photo", TRUE, "#ff9800", "Photo point", NULL),
    if (!identical(sam_outlines, "null")) {
      layer_row("lyr-sam", FALSE, "#90a4ae", "SAM outlines",
                paste0("n=", if (!is.null(hetero_meta$n_sam_outlines)) hetero_meta$n_sam_outlines else "?",
                       " · research"))
    } else "",
    if (!identical(labeled_segs, "null")) {
      layer_row("lyr-labeled", TRUE, "#5d4037", "SAM labeled",
                paste0("n=", if (!is.null(hetero_meta$n_labeled)) hetero_meta$n_labeled else "?",
                       " · LPT lifeform"))
    } else ""
  )

  paste0(
    "<style type=\"text/css\">\n", css, "\n",
    "#", map_id, " { height: 520px; width: 100%; background:#1a1a1a; border-radius:6px; position:relative; }\n",
    "@media (min-width: 768px) { #", map_id, " { height: 640px; } }\n",
    "@media (min-width: 1200px) { #", map_id, " { height: 720px; } }\n",
    "#", map_id, " .fp-layer-panel {\n",
    "  position:absolute; top:10px; right:10px; z-index:1000; width:250px; max-height:calc(100% - 20px);\n",
    "  overflow:auto; background:rgba(15,20,25,0.92); color:#e8eef7; border:1px solid #2a3a4f;\n",
    "  border-radius:8px; padding:0.5rem 0.55rem 0.65rem; font: 12px/1.35 system-ui,-apple-system,sans-serif;\n",
    "  box-shadow:0 4px 16px rgba(0,0,0,0.35); transition:transform 0.25s ease, opacity 0.25s ease;\n",
    "}\n",
    "#", map_id, " .fp-layer-panel.fp-collapsed { transform:translateX(calc(100% + 10px)); opacity:0; pointer-events:none; }\n",
    "#", map_id, " .fp-panel-toggle {\n",
    "  position:absolute; top:10px; right:10px; z-index:1001; background:rgba(15,20,25,0.92);\n",
    "  color:#e8eef7; border:1px solid #2a3a4f; border-radius:4px; padding:6px 10px; cursor:pointer;\n",
    "  font:600 12px system-ui,-apple-system,sans-serif; box-shadow:0 2px 8px rgba(0,0,0,0.3);\n",
    "  transition:background 0.2s ease; user-select:none;\n",
    "}\n",
    "#", map_id, " .fp-panel-toggle:hover { background:rgba(25,30,35,0.95); }\n",
    "#", map_id, " .fp-expand-toggle {\n",
    "  position:absolute; top:10px; right:78px; z-index:1001; background:rgba(15,20,25,0.92);\n",
    "  color:#e8eef7; border:1px solid #2a3a4f; border-radius:4px; padding:6px 10px; cursor:pointer;\n",
    "  font:600 12px system-ui,-apple-system,sans-serif; box-shadow:0 2px 8px rgba(0,0,0,0.3);\n",
    "  transition:background 0.2s ease; user-select:none;\n",
    "}\n",
    "#", map_id, " .fp-expand-toggle:hover { background:rgba(25,30,35,0.95); }\n",
    "#", map_id, ".fp-map-expanded, #", map_id, ".fp-map-expanded.fp-leaflet-map {\n",
    "  position:fixed !important; inset:12px; z-index:10000; height:auto !important; width:auto !important;\n",
    "  min-height:0 !important; border-radius:10px; box-shadow:0 12px 40px rgba(0,0,0,0.45);\n",
    "}\n",
    "#", map_id, " .fp-layer-panel h3 { margin:0 0 0.35rem; font-size:0.72rem; text-transform:uppercase; letter-spacing:0.04em; color:#8b9bb4; }\n",
    "#", map_id, " .fp-layer-group { margin:0.55rem 0 0.25rem; font-size:0.68rem; text-transform:uppercase; letter-spacing:0.04em; color:#8b9bb4; }\n",
    "#", map_id, " .fp-layer { display:flex; gap:0.4rem; align-items:flex-start; padding:0.22rem 0.1rem; }\n",
    "#", map_id, " .fp-layer label { flex:1; cursor:pointer; font-size:0.78rem; color:#e8eef7; }\n",
    "#", map_id, " .fp-hint { display:block; color:#8b9bb4; font-size:0.65rem; margin-top:0.08rem; }\n",
    "#", map_id, " .fp-swatch { width:12px; height:12px; border-radius:2px; border:1px solid #445; flex-shrink:0; margin-top:2px; }\n",
    "#", map_id, " .fp-layer-panel input[type=checkbox] { margin-top:2px; accent-color:#3b9eff; }\n",
    "#", map_id, " .leaflet-control-layers { font-size:12px; }\n",
    "</style>\n",
    "<div id=\"", map_id, "\" class=\"fp-leaflet-map\" role=\"img\" aria-label=\"",
    .fp_escape_html(parcel_id), " hillshade with layer toggles\">\n",
    "<button id=\"", map_id, "-expand\" class=\"fp-expand-toggle\" type=\"button\" aria-label=\"Expand map\" aria-pressed=\"false\">Expand</button>\n",
    "<button id=\"", panel_id, "-toggle\" class=\"fp-panel-toggle\" type=\"button\" aria-label=\"Toggle layer panel\" aria-expanded=\"false\">Layers</button>\n",
    "<div id=\"", panel_id, "\" class=\"fp-layer-panel fp-collapsed\" aria-label=\"Map layers\">\n",
    "<h3>Layers</h3>\n",
    panel_rasters,
    panel_vectors,
    "</div>\n",
    "</div>\n",
    "<script type=\"text/javascript\">\n", js_lib, "\n",
    "(function(){\n",
    "  var el = document.getElementById('", map_id, "');\n",
    "  if (!el || typeof L === 'undefined') return;\n",
    "  var map = L.map(el, {scrollWheelZoom:false, zoomControl:true, attributionControl:true});\n",
    "  var bounds = L.latLngBounds([", south, ",", west, "],[", north, ",", east, "]);\n",
    "  var overlays = {};\n",
    "  function addImg(uri, key, opacity){\n",
    "    if (!uri) return null;\n",
    "    var lyr = L.imageOverlay(uri, bounds, {opacity:opacity, interactive:false});\n",
    "    overlays[key] = lyr;\n",
    "    return lyr;\n",
    "  }\n",
    "  function addGj(gj, key, style, pointToLayer, onEach, addNow){\n",
    "    if (!gj) return null;\n",
    "    var lyr = L.geoJSON(gj, {style:style, pointToLayer:pointToLayer, onEachFeature:onEach});\n",
    "    overlays[key] = lyr;\n",
    "    if (addNow) lyr.addTo(map);\n",
    "    return lyr;\n",
    "  }\n",
    "  addImg(", img_json, ", 'hs', 0.98).addTo(map);\n",
    if (!identical(chm_uri, "null")) paste0("  addImg(", chm_uri, ", 'chm', 0.82);\n") else "",
    if (!identical(shrub_uri, "null")) paste0("  addImg(", shrub_uri, ", 'shrub', 0.75);\n") else "",
    if (!identical(tree_uri, "null")) paste0("  addImg(", tree_uri, ", 'tree', 0.8);\n") else "",
    "  addGj(", sam_outlines, ", 'sam', function(f){\n",
    "    var origin = (f.properties && f.properties.mask_origin) || '';\n",
    "    return {color: origin === 'residual_matrix' ? '#78909c' : '#90a4ae', weight:0.7, fill:false, opacity:0.35, dashArray: origin === 'residual_matrix' ? '2 3' : null};\n",
    "  }, null, null, false);\n",
    "  addGj(", labeled_segs, ", 'labeled', function(f){\n",
    "    var origin = (f.properties && f.properties.mask_origin) || '';\n",
    "    var shrub = (f.properties && f.properties.shrub_abs != null) ? Number(f.properties.shrub_abs) : 0;\n",
    "    var fill = origin === 'residual_matrix' ? '#607d8b' : (shrub >= 18 ? '#5d4037' : (shrub >= 12 ? '#8D6E63' : '#a1887f'));\n",
    "    return {color: origin === 'residual_matrix' ? '#37474f' : '#3e2723', weight:2.2, fillColor:fill, fillOpacity: origin === 'residual_matrix' ? 0.22 : 0.48, opacity:0.95, dashArray: origin === 'residual_matrix' ? '5 4' : null};\n",
    "  }, null, function(f, layer){\n",
    "    var p = f.properties || {};\n",
    "    var sid = (p.segment_id != null) ? p.segment_id : '?';\n",
    "    var shrub = (p.shrub_abs != null) ? Number(p.shrub_abs).toFixed(1) + '%' : '—';\n",
    "    var tops = p.top_spp || '—';\n",
    "    var hits = (p.n_hits != null) ? p.n_hits : '—';\n",
    "    var origin = p.mask_origin || '';\n",
    "    var html = '<strong>Research segment ' + sid + '</strong><br/>shrub_abs: ' + shrub + '<br/>top spp: ' + tops + '<br/>n_hits: ' + hits + (origin ? ('<br/>' + origin) : '');\n",
    "    layer.bindPopup(html);\n",
    "    layer.bindTooltip('seg ' + sid + ' · shrub ' + shrub, {sticky:true, direction:'top'});\n",
    "  }, true);\n",
    "  addGj(", boundary, ", 'boundary', {color:'#ffcc00', weight:2.5, fill:false, opacity:1}, null, null, true);\n",
    "  addGj(", adjoining, ", 'adjoining', {color:'#4fc3f7', weight:1.2, fillColor:'#4fc3f7', fillOpacity:0.08, opacity:0.85}, null, function(f, layer){\n",
    "    var id = (f.properties && (f.properties.PCL || f.properties.Parcel || f.properties.parcel)) || '';\n",
    "    if (id) layer.bindTooltip(String(id), {sticky:true, direction:'top'});\n",
    "  }, true);\n",
    "  addGj(", starts, ", 'starts', null, function(f, ll){\n",
    "    return L.circleMarker(ll, {radius:5, color:'#fff', weight:1, fillColor:'#e53935', fillOpacity:0.95});\n",
    "  }, function(f, layer){\n",
    "    var lab = (f.properties && (f.properties.IDENT || f.properties.Tag)) || 'transect';\n",
    "    layer.bindTooltip(String(lab), {sticky:true});\n",
    "  }, true);\n",
    "  addGj(", photo_pt, ", 'photo', null, function(f, ll){\n",
    "    return L.circleMarker(ll, {radius:8, color:'#212121', weight:2, fillColor:'#ff9800', fillOpacity:1});\n",
    "  }, function(f, layer){\n",
    "    var lab = (f.properties && f.properties.IDENT) || 'photo';\n",
    "    layer.bindPopup('<strong>Photo point</strong><br/>' + String(lab));\n",
    "  }, true);\n",
    "  function bindToggle(suffix, key){\n",
    "    var cb = document.getElementById('", map_id, "-' + suffix);\n",
    "    if (!cb || !overlays[key]) return;\n",
    "    function sync(){\n",
    "      if (cb.checked) { if (!map.hasLayer(overlays[key])) overlays[key].addTo(map); }\n",
    "      else { if (map.hasLayer(overlays[key])) map.removeLayer(overlays[key]); }\n",
    "    }\n",
    "    cb.addEventListener('change', sync);\n",
    "    sync();\n",
    "  }\n",
    "  bindToggle('lyr-hs', 'hs');\n",
    "  bindToggle('lyr-chm', 'chm');\n",
    "  bindToggle('lyr-shrub', 'shrub');\n",
    "  bindToggle('lyr-tree', 'tree');\n",
    "  bindToggle('lyr-boundary', 'boundary');\n",
    "  bindToggle('lyr-adjoining', 'adjoining');\n",
    "  bindToggle('lyr-starts', 'starts');\n",
    "  bindToggle('lyr-photo', 'photo');\n",
    "  bindToggle('lyr-sam', 'sam');\n",
    "  bindToggle('lyr-labeled', 'labeled');\n",
    "  // Also expose a compact L.control.layers for keyboard / mobile fallback\n",
    "  var lcOverlays = {};\n",
    "  if (overlays.hs) lcOverlays['Hillshade'] = overlays.hs;\n",
    "  if (overlays.chm) lcOverlays['CHM height strata'] = overlays.chm;\n",
    "  if (overlays.shrub) lcOverlays['Small-shrub'] = overlays.shrub;\n",
    "  if (overlays.tree) lcOverlays['Tree'] = overlays.tree;\n",
    "  if (overlays.boundary) lcOverlays['Parcel boundary'] = overlays.boundary;\n",
    "  if (overlays.adjoining) lcOverlays['Adjoining'] = overlays.adjoining;\n",
    "  if (overlays.starts) lcOverlays['Transect starts'] = overlays.starts;\n",
    "  if (overlays.photo) lcOverlays['Photo point'] = overlays.photo;\n",
    "  if (overlays.sam) lcOverlays['SAM outlines'] = overlays.sam;\n",
    "  if (overlays.labeled) lcOverlays['SAM labeled'] = overlays.labeled;\n",
    "  L.control.layers(null, lcOverlays, {collapsed:true, position:'bottomleft'}).addTo(map);\n",
    "  // Keep panel from stealing map drag\n",
    "  var panel = document.getElementById('", panel_id, "');\n",
    "  if (panel) { L.DomEvent.disableClickPropagation(panel); L.DomEvent.disableScrollPropagation(panel); }\n",
    "  // Toggle button functionality\n",
    "  var toggleBtn = document.getElementById('", panel_id, "-toggle');\n",
    "  if (toggleBtn) {\n",
    "    L.DomEvent.disableClickPropagation(toggleBtn);\n",
    "    toggleBtn.addEventListener('click', function(e){\n",
    "      e.stopPropagation();\n",
    "      var isCollapsed = panel.classList.contains('fp-collapsed');\n",
    "      if (isCollapsed) {\n",
    "        panel.classList.remove('fp-collapsed');\n",
    "        toggleBtn.setAttribute('aria-expanded', 'true');\n",
    "        toggleBtn.textContent = '✕';\n",
    "      } else {\n",
    "        panel.classList.add('fp-collapsed');\n",
    "        toggleBtn.setAttribute('aria-expanded', 'false');\n",
    "        toggleBtn.textContent = 'Layers';\n",
    "      }\n",
    "    });\n",
    "  }\n",
    "  // Expand / shrink map (fixed overlay; invalidateSize on toggle)\n",
    "  var expandBtn = document.getElementById('", map_id, "-expand');\n",
    "  if (expandBtn) {\n",
    "    L.DomEvent.disableClickPropagation(expandBtn);\n",
    "    expandBtn.addEventListener('click', function(e){\n",
    "      e.stopPropagation();\n",
    "      var open = el.classList.toggle('fp-map-expanded');\n",
    "      expandBtn.setAttribute('aria-pressed', open ? 'true' : 'false');\n",
    "      expandBtn.textContent = open ? 'Close' : 'Expand';\n",
    "      setTimeout(function(){ map.invalidateSize(); map.fitBounds(bounds, {padding:[40,40]}); }, 50);\n",
    "    });\n",
    "  }\n",
    "  // Fit to hillshade extent with generous padding so map fills the view nicely\n",
    "  map.fitBounds(bounds, {padding:[40,40]});\n",
    "  setTimeout(function(){ map.invalidateSize(); }, 200);\n",
    "})();\n",
    "</script>\n",
    "<p class=\"flagged-map-legend\"><span class=\"lg-b\">Yellow</span> parcel · ",
    "<span class=\"lg-a\">Cyan</span> adjoining · ",
    "<span class=\"lg-t\">Red</span> starts · ",
    "<span class=\"lg-p\">Orange</span> photo",
    if (isTRUE(has_height)) {
      paste0(
        " · <span class=\"lg-chm\">CHM strata</span> / <span class=\"lg-shrub\">small-shrub ",
        shrub_lo, "–", shrub_hi, " m</span> / <span class=\"lg-tree\">tree &gt;", tree_lo, " m</span>",
        " (layer panel)"
      )
    } else {
      ""
    },
    if (isTRUE(has_hetero)) {
      paste0(
        " · <span class=\"lg-sam\">SAM outlines</span> · <span class=\"lg-lab\">SAM labeled</span>",
        " (optional toggles)"
      )
    } else {
      ""
    },
    "</p>\n",
    if (isTRUE(has_hetero)) {
      caveat <- if (!is.null(hetero_meta) && !is.null(hetero_meta$note) && nzchar(hetero_meta$note)) {
        .fp_escape_html(hetero_meta$note)
      } else {
        "Residual gap closed — 16/16 LPT starts in sam_amg; 12 labeled AMG segments (Eco denser-v2)"
      }
      paste0(
        "<div class=\"fp-research-overlay-note\">\n",
        "<p class=\"fp-research-badge\"><strong>Research overlay</strong> — NAIP 2022 SAM segments × LPT 2022 lifeform labels. ",
        "Not vegetation TYPE · not I.C.1.b attributability. Use the layer panel to toggle SAM / CHM / height bins.</p>\n",
        "<p class=\"fp-research-caveat\">", caveat,
        ". Popups show shrub_abs + top gated species (SPAI / ARTR2 / ATTO / ERNA10).</p>\n",
        "</div>\n"
      )
    } else if (isTRUE(has_height)) {
      paste0(
        "<div class=\"fp-research-overlay-note\">\n",
        "<p class=\"fp-research-badge\"><strong>Height layers</strong> — Sierra 2022 CHM clipped to hillshade frame. ",
        "Small-shrub ", shrub_lo, "–", shrub_hi, " m · tree &gt;", tree_lo, " m (BWMA liberal_v1 bins). ",
        "Not vegetation TYPE · not I.C.1.b.</p>\n",
        "</div>\n"
      )
    } else {
      ""
    }
  )
}


.fp_static_map_html <- function(parcel_id, d) {
  overlay <- .fp_find_file(d, c(paste0("^", parcel_id, "_hillshade_context_overlay\\.png$"),
                                "_hillshade_context_overlay\\.png$",
                                paste0("^", parcel_id, "_hillshade_preview\\.png$"),
                                "_hillshade_preview\\.png$"))
  if (is.null(overlay)) return("")
  paste0(
    '<figure class="flagged-static-map"><img src="',
    knitr::image_uri(overlay),
    '" alt="', .fp_escape_html(parcel_id),
    ' hillshade context (boundary, adjoining, transects, photo point)"/>',
    '<figcaption>Hillshade context overlay (parcel boundary, adjoining parcels, transect starts, photo point).</figcaption></figure>\n'
  )
}

.fp_decade_rank_table_html <- function(parcel_id, d) {
  ranks_path <- file.path(d, "spp_rank", "decade_ranks.csv")
  delta_path <- file.path(d, "spp_rank", "rank_delta.csv")
  if (!file.exists(ranks_path)) return(NULL)

  ranks <- readr::read_csv(ranks_path, show_col_types = FALSE)
  if (!nrow(ranks)) return(NULL)

  periods <- c("baseline", "1990s", "2000s", "2010s", "current")
  period_labels <- c(
    baseline = "Baseline", `1990s` = "1990s", `2000s` = "2000s",
    `2010s` = "2010s", current = "2026"
  )
  present <- unique(as.character(ranks$period))
  missing <- setdiff(periods, present)

  delta <- if (file.exists(delta_path)) {
    readr::read_csv(delta_path, show_col_types = FALSE)
  } else {
    tibble::tibble()
  }

  earliest <- NA_character_
  if (nrow(delta) && "first_year_topk_order_diff_parcel" %in% names(delta)) {
    yrs <- delta$first_year_topk_order_diff_parcel
    yrs <- yrs[is.finite(yrs)]
    if (length(yrs)) earliest <- as.character(min(yrs))
  }

  focus_codes <- ranks %>%
    dplyr::filter(.data$rank <= 5) %>%
    dplyr::distinct(.data$species_code) %>%
    dplyr::pull(.data$species_code)

  if (nrow(delta) && "species_code" %in% names(delta)) {
    ord <- unique(c(delta$species_code[delta$species_code %in% focus_codes], focus_codes))
  } else {
    ord <- focus_codes
  }

  # IMPORTANT: do not name the period arg `period` — dplyr would treat
  # `.data$period == period` as column==column (always TRUE) and slice(1)
  # would repeat the first period's ranks across every decade column.
  fmt_rank_cell <- function(code, period_key) {
    hit <- ranks[
      !is.na(ranks$species_code) & ranks$species_code == code &
        !is.na(ranks$period) & as.character(ranks$period) == as.character(period_key),
      ,
      drop = FALSE
    ]
    if (!nrow(hit)) return("<span class=\"spp-na\">—</span>")
    hit <- hit[1, , drop = FALSE]
    paste0(
      "<span class=\"spp-rank\">", hit$rank[[1]], "</span>",
      " <span class=\"spp-name\">", .fp_escape_html(dplyr::coalesce(hit$common[[1]], code)), "</span>",
      "<br/><span class=\"spp-abs\">", sprintf("%.2f%%", hit$cover_abs_mean[[1]]), "</span>",
      " <span class=\"spp-rel\">(", sprintf("%.3f", hit$cover_rel_perennial[[1]]), ")</span>"
    )
  }

  header_periods <- periods
  if ("1990s" %in% missing) header_periods <- setdiff(header_periods, "1990s")

  thead <- paste0(
    "<tr><th scope=\"col\">Species</th>",
    paste0(vapply(header_periods, function(p) {
      lab <- period_labels[[p]]
      if (identical(p, "baseline")) {
        yrs <- ranks$year_or_years[ranks$period == "baseline"][1]
        if (!is.na(yrs)) lab <- paste0(lab, " (", yrs, ")")
      }
      if (identical(p, "current")) lab <- "2026"
      paste0("<th scope=\"col\">", .fp_escape_html(lab), "</th>")
    }, character(1)), collapse = ""),
    "</tr>"
  )

  body_rows <- vapply(ord, function(code) {
    sci <- ranks$scientific[ranks$species_code == code][1]
    lf <- ranks$lifeform[ranks$species_code == code][1]
    lab <- paste0(
      "<strong>", .fp_escape_html(code), "</strong>",
      if (!is.na(sci)) paste0("<br/><span class=\"spp-sci\">", .fp_escape_html(sci), "</span>") else "",
      if (!is.na(lf)) paste0("<br/><span class=\"spp-lf\">", .fp_escape_html(lf), "</span>") else ""
    )
    cells <- vapply(header_periods, function(p) {
      paste0("<td>", fmt_rank_cell(code, p), "</td>")
    }, character(1))
    paste0("<tr><th scope=\"row\">", lab, "</th>", paste(cells, collapse = ""), "</tr>")
  }, character(1))

  missing_note <- if (length(missing)) {
    paste0(
      "<p class=\"spp-rank-note\"><strong>Note:</strong> No LPT visits for ",
      .fp_escape_html(paste(missing, collapse = ", ")),
      " — column omitted. Research framing (decade perennial relative ranks); not I.C.1.b.</p>"
    )
  } else {
    "<p class=\"spp-rank-note\">Research framing (decade perennial relative ranks; top-N=10 method). Not I.C.1.b attributability.</p>"
  }

  callout <- if (!is.na(earliest)) {
    paste0(
      "<p class=\"spp-rank-callout\"><strong>Earliest top-10 reorder vs baseline:</strong> ",
      .fp_escape_html(earliest),
      " (first dated order difference on this parcel).</p>"
    )
  } else ""

  paste0(
    "<div class=\"spp-rank-panel\">\n",
    "<h4>Decade species-rank comparison</h4>\n",
    callout,
    missing_note,
    "<div class=\"spp-rank-table-wrap\">\n",
    "<table class=\"spp-rank-table\">\n<thead>", thead, "</thead>\n<tbody>\n",
    paste(body_rows, collapse = "\n"),
    "\n</tbody></table>\n</div>\n",
    "<p class=\"spp-rank-caption\">Rank by perennial relative cover within period (mean of visits present). ",
    "Absolute = parcelMeanCover (%). Pre-2015 vs permanent-network (~2015+) sampling design can change transect counts — do not over-call design change as ecology.</p>\n",
    "</div>\n"
  )
}

#' Emit hillshade map + photo + spp-rank HTML for a parcel (asis). No-op if assets missing.
emit_flagged_parcel_context <- function(parcel_id) {
  if (!flagged_parcel_has_context(parcel_id)) {
    return(invisible(FALSE))
  }
  d <- flagged_parcel_asset_dir(parcel_id)
  bounds <- .fp_read_json(file.path(d, "hillshade_bounds.json"))
  if (is.null(bounds)) return(invisible(FALSE))

  epoch <- if (!is.null(bounds$dtm_epoch)) bounds$dtm_epoch else "LiDAR DTM"
  pad_note <- if (!is.null(bounds$pad_note)) bounds$pad_note else NULL
  n_starts <- bounds$n_all2026_starts

  preview_png <- .fp_find_file(
    d,
    c(paste0("^", parcel_id, "_hillshade_preview\\.png$"), "_hillshade_preview\\.png$")
  )

  cat('\n\n<div class="flagged-parcel-context fp-map-band">\n')
  cat("<h4>Topographic context + photo</h4>\n")
  cat(
    "<p class=\"flagged-context-lead\">Parcel-framed hillshade (",
    .fp_escape_html(epoch),
    ") with boundary, adjoining parcels, All_2026 transect starts",
    if (is.finite(n_starts)) paste0(" (n=", n_starts, ")") else "",
    ", and representative photo point. Measurability / landscape context only — not attributability.</p>\n",
    sep = ""
  )
  if (!is.null(pad_note) && nzchar(pad_note)) {
    cat("<p class=\"flagged-context-pad\"><em>", .fp_escape_html(pad_note), "</em></p>\n", sep = "")
  }

  # Full-width map band; photo stacked below (never starve map in a skinny column).
  # Expand control grows the map to a fixed overlay. Layers panel stays collapsible.
  photo <- .fp_find_photo(d)
  cat("<div class=\"flagged-context-stack flagged-context-two-col\">\n")

  cat("<div class=\"flagged-context-map flagged-context-map-full\">\n")
  cat("<p class=\"flagged-map-kicker\"><strong>LiDAR map</strong> — hillshade + optional CHM / height / SAM layers (Layers / Expand)</p>\n")
  map_html <- NULL
  if (!is.null(preview_png)) {
    map_html <- tryCatch(
      .fp_inline_leaflet_html(parcel_id, d, bounds, preview_png),
      error = function(e) {
        message("inline leaflet failed for ", parcel_id, ": ", e$message)
        NULL
      }
    )
  }
  if (is.null(map_html) || !nzchar(map_html)) {
    map_html <- .fp_static_map_html(parcel_id, d)
  }
  cat(map_html)
  cat("</div>\n") # map

  cat("<div class=\"flagged-context-photo\">\n")
  if (!is.null(photo)) {
    ident <- if (!is.null(photo$meta$IDENT)) photo$meta$IDENT else basename(photo$path)
    visit <- if (!is.null(photo$meta$visit_date_iso)) photo$meta$visit_date_iso else NULL
    photo_src <- .fp_raster_uri(photo$path)
    if (is.null(photo_src)) photo_src <- knitr::image_uri(photo$path)
    cat(
      '<figure class="flagged-photo"><img src="',
      photo_src,
      '" alt="', .fp_escape_html(paste(parcel_id, "representative photo", ident)),
      '"/>',
      "<figcaption><strong>", .fp_escape_html(ident), "</strong>",
      if (!is.null(visit)) paste0(" · ", .fp_escape_html(visit)) else "",
      " (AGOL / LPT start-point attachment)</figcaption></figure>\n",
      sep = ""
    )
  } else {
    cat("<p class=\"flagged-photo-missing\"><em>No representative photo asset for this parcel.</em></p>\n")
  }
  cat("</div>\n") # photo
  cat("</div>\n") # stack

  tbl <- tryCatch(.fp_decade_rank_table_html(parcel_id, d), error = function(e) {
    message("spp-rank table failed for ", parcel_id, ": ", e$message)
    NULL
  })
  if (!is.null(tbl) && nzchar(tbl)) cat(tbl)

  cat("</div>\n\n") # flagged-parcel-context
  invisible(TRUE)
}
