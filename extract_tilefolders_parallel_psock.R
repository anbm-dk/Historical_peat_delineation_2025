extract_tilefolders_parallel_psock <- function(
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
    ...
) {
  stopifnot(length(tile_dirs) > 0)
  if (!requireNamespace("terra", quietly = TRUE)) stop("Package 'terra' is required.")
  if (!requireNamespace("sf", quietly = TRUE)) stop("Package 'sf' is required.")
  method <- match.arg(method)
  layer_names <- match.arg(layer_names)
  
  # ---- points -> sf -> plain coordinate df ----
  if (inherits(pts, "SpatVector")) pts <- sf::st_as_sf(pts)
  if (is.data.frame(pts)) {
    if (is.na(pts_crs)) stop("If pts is a data.frame, please supply pts_crs (e.g., 'EPSG:4326').")
    pts <- sf::st_as_sf(pts, coords = c(x, y), crs = pts_crs)
  }
  if (!inherits(pts, "sf")) stop("pts must be sf, terra SpatVector, or data.frame.")
  if (!inherits(sf::st_geometry(pts), "sfc_POINT")) stop("pts must be POINT geometry.")
  if (is.na(sf::st_crs(pts))) stop("Points CRS is missing; please set it.")
  
  coords <- sf::st_coordinates(pts)
  pts_df <- sf::st_drop_geometry(pts)
  pts_df$.x <- coords[, 1]
  pts_df$.y <- coords[, 2]
  n_pts <- nrow(pts_df)
  
  point_id <- seq_len(n_pts)
  if (!is.null(id_col) && id_col %in% names(pts_df)) point_id <- pts_df[[id_col]]
  
  # ---- discover files per tile folder ----
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
  
  # ---- tile extents + CRS from first file in each tile ----
  tile_meta <- lapply(tile_info, function(z) {
    r0 <- terra::rast(z$files[1])
    e <- terra::ext(r0)
    data.frame(
      tile_dir = z$tile_dir,
      xmin = e[1], xmax = e[2], ymin = e[3], ymax = e[4],
      crs = terra::crs(r0, proj = TRUE),
      stringsAsFactors = FALSE
    )
  })
  tile_meta <- do.call(rbind, tile_meta)
  tile_crs <- tile_meta$crs[1]
  
  # ---- reproject points once (sf) if needed ----
  pts_wkt <- sf::st_crs(pts)$wkt
  if (!is.na(tile_crs) && tile_crs != "" && !identical(tile_crs, pts_wkt)) {
    pts2 <- sf::st_transform(pts, sf::st_crs(tile_crs))
    cc <- sf::st_coordinates(pts2)
    pts_df$.x <- cc[, 1]
    pts_df$.y <- cc[, 2]
    pts_wkt <- sf::st_crs(pts2)$wkt
  }
  
  # ---- assign points to tiles using extent checks (overlaps OK: first wins) ----
  tile_i_for_point <- rep(NA_integer_, n_pts)
  for (i in seq_len(nrow(tile_meta))) {
    inside <- pts_df$.x >= tile_meta$xmin[i] & pts_df$.x <= tile_meta$xmax[i] &
      pts_df$.y >= tile_meta$ymin[i] & pts_df$.y <= tile_meta$ymax[i]
    to_set <- which(is.na(tile_i_for_point) & inside)
    if (length(to_set)) tile_i_for_point[to_set] <- i
  }
  pts_by_tile <- split(seq_len(n_pts), tile_i_for_point)
  pts_by_tile <- pts_by_tile[!is.na(names(pts_by_tile))]
  used_tiles <- as.integer(names(pts_by_tile))
  
  # ---- layer naming ----
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
  
  dots <- list(...)
  
  # ---- PSOCK cluster on Windows ----
  cl <- parallel::makeCluster(min(cores, length(used_tiles)))
  on.exit(parallel::stopCluster(cl), add = TRUE)
  
  parallel::clusterEvalQ(cl, {
    library(terra)
    NULL
  })
  
  # Export only *simple* objects + small helper(s)
  parallel::clusterExport(
    cl,
    varlist = c("tile_info", "pts_by_tile", "pts_df", "pts_wkt",
                "expected_names", "method", "dots", "make_names"),
    envir = environment()
  )
  
  # Worker function: builds terra objects *inside* worker
  worker_fun <- function(ti) {
    idx <- pts_by_tile[[as.character(ti)]]
    
    psub_df <- data.frame(x = pts_df$.x[idx], y = pts_df$.y[idx])
    psub <- terra::vect(psub_df, geom = c("x", "y"), crs = pts_wkt)
    
    files <- tile_info[[ti]]$files
    nm <- make_names(files)
    if (length(nm) == length(expected_names) && setequal(nm, expected_names)) {
      files <- files[match(expected_names, nm)]
      nm <- expected_names
    }
    
    r <- terra::rast(files)
    names(r) <- nm
    
    v <- do.call(terra::extract, c(list(x = r, y = psub, method = method), dots))
    
    pt_local <- v[[1]]              # point index within psub
    point_index <- idx[pt_local]    # point index in original data
    
    out <- cbind(
      data.frame(point_index = point_index, stringsAsFactors = FALSE),
      v[, -1, drop = FALSE]
    )
    out$tile_dir <- tile_info[[ti]]$tile_dir
    out
  }
  
  extracted_list <- parallel::parLapply(cl, used_tiles, worker_fun)
  extracted <- do.call(rbind, extracted_list)
  
  # ---- rebuild full output in original order ----
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
