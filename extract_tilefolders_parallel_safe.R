extract_tilefolders_parallel_safe <- function(
    tile_dirs,
    pts,
    x = "x", y = "y",
    pts_crs = NA,
    id_col = NULL,
    file_pattern = "\\.tif(f)?$",
    layers = NULL,
    layer_names = c("basename", "filename", "custom"),
    layer_name_fun = NULL,
    method = c("simple", "bilinear"),
    cores = max(1, parallel::detectCores(logical = TRUE) - 1),
    plan = c("multisession", "multicore"),
    progress = TRUE,
    ...
) {
  stopifnot(length(tile_dirs) > 0)
  if (!requireNamespace("terra", quietly = TRUE)) stop("Package 'terra' is required.")
  if (!requireNamespace("future", quietly = TRUE) ||
      !requireNamespace("future.apply", quietly = TRUE)) {
    stop("Packages 'future' and 'future.apply' are required.")
  }
  
  method <- match.arg(method)
  plan <- match.arg(plan)
  layer_names <- match.arg(layer_names)
  
  # ---------- 1) Normalize points to a plain data.frame of coordinates ----------
  pts_df <- NULL
  pts_sf <- NULL
  
  if (inherits(pts, "sf")) {
    pts_sf <- pts
  } else if (inherits(pts, "SpatVector")) {
    pts_sf <- sf::st_as_sf(pts)
  } else if (is.data.frame(pts)) {
    if (is.na(pts_crs)) stop("If pts is a data.frame, please supply pts_crs (e.g., 'EPSG:4326').")
    pts_sf <- sf::st_as_sf(pts, coords = c(x, y), crs = pts_crs)
  } else {
    stop("pts must be an sf object, terra SpatVector, or data.frame.")
  }
  
  if (!inherits(sf::st_geometry(pts_sf), "sfc_POINT")) {
    stop("pts must be POINT geometry.")
  }
  
  # Extract coordinates into a plain data.frame
  coords <- sf::st_coordinates(pts_sf)
  pts_df <- sf::st_drop_geometry(pts_sf)
  pts_df$.x <- coords[, 1]
  pts_df$.y <- coords[, 2]
  
  n_pts <- nrow(pts_df)
  if (n_pts == 0) stop("No points provided.")
  
  point_id <- seq_len(n_pts)
  if (!is.null(id_col) && id_col %in% names(pts_df)) {
    point_id <- pts_df[[id_col]]
  } else if (!is.null(id_col)) {
    warning("id_col not found on pts; using row order as point_id.")
  }
  
  pts_crs_sf <- sf::st_crs(pts_sf)$wkt
  if (is.na(pts_crs_sf) || pts_crs_sf == "") stop("Points CRS is missing; please set it.")
  
  # ---------- 2) Discover tile files per folder ----------
  tile_info <- lapply(tile_dirs, function(d) {
    files <- list.files(d, pattern = file_pattern, full.names = TRUE)
    if (!is.null(layers)) {
      bn <- basename(files)
      files <- files[bn %in% layers | files %in% layers]
    }
    files <- sort(files)
    list(tile_dir = d, files = files)
  })
  
  nonempty <- vapply(tile_info, function(z) length(z$files) > 0, logical(1))
  if (!any(nonempty)) stop("No tif files found in any tile folder (check file_pattern/layers).")
  if (any(!nonempty)) {
    warning("Some tile folders had no matching tif files and were skipped.")
    tile_info <- tile_info[nonempty]
  }
  tile_dirs <- vapply(tile_info, `[[`, character(1), "tile_dir")
  
  # ---------- 3) Tile extents & CRS from one representative file per tile ----------
  tile_meta <- lapply(tile_info, function(z) {
    r0 <- terra::rast(z$files[1])
    e <- terra::ext(r0)
    data.frame(
      tile_dir = z$tile_dir,
      rep_file = z$files[1],
      xmin = e[1], xmax = e[2], ymin = e[3], ymax = e[4],
      crs = terra::crs(r0, proj = TRUE),
      stringsAsFactors = FALSE
    )
  })
  tile_meta <- do.call(rbind, tile_meta)
  
  # Reproject coordinates to tile CRS if needed (do this ONCE, outside parallel)
  tile_crs <- tile_meta$crs[1]
  if (!is.na(tile_crs) && tile_crs != "" && !identical(tile_crs, pts_crs_sf)) {
    # project coordinates using sf (safe, no terra pointer export)
    pts_tmp <- sf::st_as_sf(
      cbind(pts_df, geometry = sf::st_sfc(lapply(seq_len(n_pts), function(i) sf::st_point(c(pts_df$.x[i], pts_df$.y[i]))),
                                          crs = sf::st_crs(pts_crs_sf))),
      sf_column_name = "geometry"
    )
    pts_tmp <- sf::st_transform(pts_tmp, sf::st_crs(tile_crs))
    cc <- sf::st_coordinates(pts_tmp)
    pts_df$.x <- cc[, 1]
    pts_df$.y <- cc[, 2]
    pts_crs_sf <- sf::st_crs(pts_tmp)$wkt
  }
  
  # ---------- 4) Assign points to tiles (overlaps OK: pick first match) ----------
  # Create a quick tile index by extent checks (fast, no polygon ops):
  tile_i_for_point <- rep(NA_integer_, n_pts)
  
  for (i in seq_len(nrow(tile_meta))) {
    inside <- pts_df$.x >= tile_meta$xmin[i] & pts_df$.x <= tile_meta$xmax[i] &
      pts_df$.y >= tile_meta$ymin[i] & pts_df$.y <= tile_meta$ymax[i]
    
    # only assign points not yet assigned (so "first tile wins" deterministically)
    to_set <- which(is.na(tile_i_for_point) & inside)
    if (length(to_set)) tile_i_for_point[to_set] <- i
  }
  
  # group point indices per tile
  pts_by_tile <- split(seq_len(n_pts), tile_i_for_point)
  pts_by_tile <- pts_by_tile[!is.na(names(pts_by_tile))]
  used_tiles <- as.integer(names(pts_by_tile))
  
  # ---------- 5) Layer naming (consistent columns) ----------
  make_names <- function(files) {
    if (layer_names == "filename") return(basename(files))
    if (layer_names == "basename") return(tools::file_path_sans_ext(basename(files)))
    if (layer_names == "custom") {
      if (is.null(layer_name_fun)) stop("layer_name_fun must be supplied when layer_names='custom'.")
      nm <- layer_name_fun(files)
      if (length(nm) != length(files)) stop("layer_name_fun must return same length as files.")
      return(nm)
    }
  }
  expected_names <- make_names(tile_info[[1]]$files)
  
  # ---------- 6) Parallel extraction: pass ONLY simple objects ----------
  old_plan <- future::plan()
  on.exit(future::plan(old_plan), add = TRUE)
  
  future::plan(if (plan == "multicore") future::multicore else future::multisession,
               workers = cores)
  
  # Avoid future trying to serialize huge globals
  options(future.globals.onReference = "ignore")
  
  res_list <- future.apply::future_lapply(
    used_tiles,
    function(ti, tile_info, pts_by_tile, pts_df, pts_crs_sf, expected_names, method, dots) {
      idx <- pts_by_tile[[as.character(ti)]]
      
      # Build terra points INSIDE the worker
      psub_df <- data.frame(x = pts_df$.x[idx], y = pts_df$.y[idx])
      psub <- terra::vect(psub_df, geom = c("x", "y"), crs = pts_crs_sf)
      
      files <- tile_info[[ti]]$files
      nm <- make_names(files)
      if (length(nm) == length(expected_names) && setequal(nm, expected_names)) {
        files <- files[match(expected_names, nm)]
        nm <- expected_names
      }
      
      r <- terra::rast(files)
      names(r) <- nm
      
      # call extract with ... captured via dots list
      v <- do.call(terra::extract, c(list(x = r, y = psub, method = method), dots))
      
      # first column is point index within psub (1..length(idx))
      pt_local <- v[[1]]
      point_index <- idx[pt_local]
      
      out <- cbind(
        data.frame(point_index = point_index, stringsAsFactors = FALSE),
        v[, -1, drop = FALSE]
      )
      out$tile_dir <- tile_info[[ti]]$tile_dir
      out
    },
    tile_info = tile_info,
    pts_by_tile = pts_by_tile,
    pts_df = pts_df,
    pts_crs_sf = pts_crs_sf,
    expected_names = expected_names,
    method = method,
    dots = list(...),
    future.seed = TRUE,
    future.globals = FALSE,
    future.packages = "terra"
  )
  
  extracted <- do.call(rbind, res_list)
  
  # ---------- 7) Rebuild full output in original point order ----------
  value_cols <- setdiff(names(extracted), c("point_index", "tile_dir"))
  
  out <- data.frame(point_index = seq_len(n_pts), stringsAsFactors = FALSE)
  if (!is.null(id_col)) out$point_id <- point_id
  for (nm in value_cols) out[[nm]] <- NA
  out$tile_dir <- NA_character_
  
  ord <- match(extracted$point_index, out$point_index)
  out[ord, value_cols] <- extracted[, value_cols, drop = FALSE]
  out$tile_dir[ord] <- extracted$tile_dir
  
  out
}
