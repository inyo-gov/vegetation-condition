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

#' Inline Leaflet ImageOverlay + GeoJSON as a self-contained HTML fragment (no htmlwidget).
.fp_inline_leaflet_html <- function(parcel_id, d, bounds, preview_png) {
  b <- bounds$bounds_wgs84
  if (is.null(b) || !all(c("west", "south", "east", "north") %in% names(b))) {
    return(NULL)
  }
  west <- as.numeric(b$west); south <- as.numeric(b$south)
  east <- as.numeric(b$east); north <- as.numeric(b$north)
  img_uri <- knitr::image_uri(preview_png)

  read_gj <- function(name) {
    path <- file.path(d, "overlays", name)
    if (!file.exists(path)) return("null")
    raw <- paste(readLines(path, warn = FALSE, encoding = "UTF-8"), collapse = "\n")
    # Validate JSON then re-emit compact
    parsed <- tryCatch(jsonlite::fromJSON(raw, simplifyVector = FALSE), error = function(e) NULL)
    if (is.null(parsed)) return("null")
    jsonlite::toJSON(parsed, auto_unbox = TRUE, null = "null")
  }

  boundary <- read_gj("parcel_boundary_wgs84.geojson")
  adjoining <- read_gj("adjoining_parcels_wgs84.geojson")
  starts <- read_gj("transect_starts_All_2026_wgs84.geojson")
  photo_pt <- read_gj("representative_photo_point_wgs84.geojson")

  map_id <- paste0("fp-map-", gsub("[^A-Za-z0-9]", "", parcel_id), "-", as.integer(runif(1, 1e6, 9e6)))

  # Leaflet CSS/JS from the installed package (inlined for self-contained Quarto)
  leaflet_css_path <- system.file("htmlwidgets/lib/leaflet/leaflet.css", package = "leaflet")
  leaflet_js_path <- system.file("htmlwidgets/lib/leaflet/leaflet.js", package = "leaflet")
  if (!nzchar(leaflet_css_path) || !nzchar(leaflet_js_path) ||
      !file.exists(leaflet_css_path) || !file.exists(leaflet_js_path)) {
    return(NULL)
  }
  css <- paste(readLines(leaflet_css_path, warn = FALSE), collapse = "\n")
  # Escape </style> in CSS if any
  css <- gsub("</", "<\\/", css, fixed = TRUE)
  js_lib <- paste(readLines(leaflet_js_path, warn = FALSE), collapse = "\n")
  js_lib <- gsub("</", "<\\/", js_lib, fixed = TRUE)

  img_json <- jsonlite::toJSON(img_uri, auto_unbox = TRUE)

  paste0(
    "<style type=\"text/css\">\n", css, "\n",
    "#", map_id, " { height: 520px; width: 100%; background:#1a1a1a; border-radius:6px; }\n",
    "</style>\n",
    "<div id=\"", map_id, "\" class=\"fp-leaflet-map\" role=\"img\" aria-label=\"",
    .fp_escape_html(parcel_id), " hillshade with overlays\"></div>\n",
    "<script type=\"text/javascript\">\n", js_lib, "\n",
    "(function(){\n",
    "  var el = document.getElementById('", map_id, "');\n",
    "  if (!el || typeof L === 'undefined') return;\n",
    "  var map = L.map(el, {scrollWheelZoom:false, zoomControl:true, attributionControl:true});\n",
    "  var bounds = L.latLngBounds([", south, ",", west, "],[", north, ",", east, "]);\n",
    "  L.imageOverlay(", img_json, ", bounds, {opacity:0.98, interactive:false}).addTo(map);\n",
    "  function addGj(gj, style, pointToLayer, onEach){\n",
    "    if (!gj) return;\n",
    "    L.geoJSON(gj, {style:style, pointToLayer:pointToLayer, onEachFeature:onEach}).addTo(map);\n",
    "  }\n",
    "  addGj(", boundary, ", {color:'#ffcc00', weight:2.5, fill:false, opacity:1});\n",
    "  addGj(", adjoining, ", {color:'#4fc3f7', weight:1.2, fillColor:'#4fc3f7', fillOpacity:0.08, opacity:0.85}, null, function(f, layer){\n",
    "    var id = (f.properties && (f.properties.PCL || f.properties.Parcel || f.properties.parcel)) || '';\n",
    "    if (id) layer.bindTooltip(String(id), {sticky:true, direction:'top'});\n",
    "  });\n",
    "  addGj(", starts, ", null, function(f, ll){\n",
    "    return L.circleMarker(ll, {radius:5, color:'#fff', weight:1, fillColor:'#e53935', fillOpacity:0.95});\n",
    "  }, function(f, layer){\n",
    "    var lab = (f.properties && (f.properties.IDENT || f.properties.Tag)) || 'transect';\n",
    "    layer.bindTooltip(String(lab), {sticky:true});\n",
    "  });\n",
    "  addGj(", photo_pt, ", null, function(f, ll){\n",
    "    return L.circleMarker(ll, {radius:8, color:'#212121', weight:2, fillColor:'#ff9800', fillOpacity:1});\n",
    "  }, function(f, layer){\n",
    "    var lab = (f.properties && f.properties.IDENT) || 'photo';\n",
    "    layer.bindPopup('<strong>Photo point</strong><br/>' + String(lab));\n",
    "  });\n",
    "  map.fitBounds(bounds, {padding:[12,12]});\n",
    "  setTimeout(function(){ map.invalidateSize(); }, 200);\n",
    "})();\n",
    "</script>\n",
    "<p class=\"flagged-map-legend\"><span class=\"lg-b\">Yellow</span> parcel boundary · ",
    "<span class=\"lg-a\">Cyan</span> adjoining · ",
    "<span class=\"lg-t\">Red</span> transect starts · ",
    "<span class=\"lg-p\">Orange</span> photo point</p>\n"
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

  cat('\n\n<div class="flagged-parcel-context">\n')
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

  # Stacked one-column layout: photo first, then full-width hillshade
  # (no sidebar / no shared-width photo+map row). Mobile/iPad friendly.
  photo <- .fp_find_photo(d)
  cat("<div class=\"flagged-context-stack\">\n")

  cat("<div class=\"flagged-context-photo\">\n")
  if (!is.null(photo)) {
    ident <- if (!is.null(photo$meta$IDENT)) photo$meta$IDENT else basename(photo$path)
    visit <- if (!is.null(photo$meta$visit_date_iso)) photo$meta$visit_date_iso else NULL
    cat(
      '<figure class="flagged-photo"><img src="',
      knitr::image_uri(photo$path),
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

  cat("<div class=\"flagged-context-map flagged-context-map-full\">\n")
  cat("<p class=\"flagged-map-kicker\"><strong>LiDAR hillshade</strong> — parcel-framed topographic context</p>\n")
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
  cat("</div>\n") # stack

  tbl <- tryCatch(.fp_decade_rank_table_html(parcel_id, d), error = function(e) {
    message("spp-rank table failed for ", parcel_id, ": ", e$message)
    NULL
  })
  if (!is.null(tbl) && nzchar(tbl)) cat(tbl)

  cat("</div>\n\n") # flagged-parcel-context
  invisible(TRUE)
}
