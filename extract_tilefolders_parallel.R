#' Parallel extract from raster tiles stored as subfolders (one tif per layer)
#'
#' Folder structure:
#'   tiles_root/
#'     tile_001/ layerA.tif layerB.tif layerC.tif ...
#'     tile_002/ layerA.tif layerB.tif layerC.tif ...
#'
#' @param tile_dirs Character vector of tile folder paths (each folder = one tile).
#' @param pts Points as terra SpatVector, sf POINT, or data.frame with x/y.
#' @param x,y If pts is a data.frame, column names for x/y.
#' @param pts_crs If pts is a data.frame, CRS string (e.g. "EPSG:25832").
#' @param id_col Optional point id column to carry through.
#' @param file_pattern Regex for layer files inside each tile folder (default "\\.tif(f)?$").
#' @param layers Optional layer selection:
#'        - NULL = use all matching files in each folder
#'        - character = keep only files whose basenames are in `layers`
#' @param layer_names How to name output columns:
#'        - "basename" = file basename without extension (default)
#'        - "filename" = full filename (basename with extension)
#'        - "custom" = use the provided `layer_name_fun`
#' @param layer_name_fun Function(files) -> character vector of names (used if layer_names="custom")
#' @param method "simple" or "bilinear"
#' @param cores Number of workers.
#' @param plan "multisession" (Windows/macOS/Linux) or "multicore" (Linux/macOS).
#' @param progress TRUE/FALSE for future.apply progress.
#' @param ... Passed to terra::extract()
#'
#' @return data.frame in original point order with extracted values for each layer.
extract_tilefolders_parallel <- function(
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
  
  # ---- coerce points to terra SpatVector ----
  pts_sv <- NULL
  pts_df <- NULL
  
  if (inherits(pts, "SpatVector")) {
    pts_sv <- pts
  } else if (inherits(pts, "sf")) {
    pts_sv <- terra::vect(pts)
  } else if (is.data.frame(pts)) {
    if (is.na(pts_crs)) stop("If pts is a data.frame, please supply pts_crs (e.g., 'EPSG:4326').")
    if (!all(c(x, y) %in% names(pts))) stop("pts data.frame must contain columns: ", x, ", ", y)
    pts_df <- pts
    pts_sv <- terra::vect(pts, geom = c(x, y), crs = pts_crs)
  } else {
    stop("pts must be a terra SpatVector, an sf object, or a data.frame.")
  }
  
  n_pts <- terra::nrow(pts_sv)
  if (n_pts == 0) stop("No points provided.")
  
  # point_id handling
  point_id <- seq_len(n_pts)
  if (!is.null(id_col)) {
    at <- terra::values(pts_sv)
    if (!is.null(at) && id_col %in% names(at)) {
      point_id <- at[[id_col]]
    } else if (!is.null(pts_df) && id_col %in% names(pts_df)) {
      point_id <- pts_df[[id_col]]
    } else {
      warning("id_col not found on pts; using row order as point_id.")
    }
  }
  
  # ---- discover tile files per folder ----
  tile_info <- lapply(tile_dirs, function(d) {
    files <- list.files(d, pattern = file_pattern, full.names = TRUE)
    if (!is.null(layers)) {
      # match by basename OR by full name provided in layers
      bn <- basename(files)
      files <- files[bn %in% layers | files %in% layers]
    }
    files <- sort(files)
    list(tile_dir = d, files = files)
  })
  
  # drop empty tiles (no matching files)
  nonempty <- vapply(tile_info, function(z) length(z$files) > 0, logical(1))
  if (!any(nonempty)) stop("No tif files found in any tile folder (check tile_dirs/file_pattern/layers).")
  if (any(!nonempty)) {
    warning("Some tile folders had no matching tif files and were skipped.")
    tile_info <- tile_info[nonempty]
  }
  
  # ---- tile extents + CRS from a representative file per tile ----
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
  
  # Ensure points CRS matches tiles CRS (assume all tiles share CRS of first tile)
  tile_crs <- tile_meta$crs[1]
  if (!is.na(tile_crs) && tile_crs != "" && terra::crs(pts_sv, proj = TRUE) != tile_crs) {
    pts_sv <- terra::project(pts_sv, tile_crs)
  }
  
  # ---- assign each point to a tile via tile extent polygons ----
  tile_polys <- lapply(seq_len(nrow(tile_meta)), function(i) {
    e <- terra::ext(tile_meta$xmin[i], tile_meta$xmax[i], tile_meta$ymin[i], tile_meta$ymax[i])
    p <- terra::as.polygons(e)
    p$tile_i <- i
    p
  })
  tiles_v <- do.call(rbind, tile_polys)
  terra::crs(tiles_v) <- tile_crs
  
  tile_hit <- terra::extract(tiles_v["tile_i"], pts_sv)
  
  # terra's point index is the first column (often called "ID", but not always)
  pt_index <- tile_hit[[1]]
  tile_val <- tile_hit[["tile_i"]]
  
  # Collapse overlaps: pick first (or use min() if you prefer)
  tile_i_for_point <- rep(NA_integer_, n_pts)
  
  first_tile <- tapply(tile_val, pt_index, function(x) {
    x <- x[!is.na(x)]
    if (length(x) == 0) NA_integer_ else x[1]   # or min(x)
  })
  
  tile_i_for_point[as.integer(names(first_tile))] <- as.integer(first_tile)
  
  stopifnot(length(tile_i_for_point) == n_pts)
  
  pts_by_tile <- split(seq_len(n_pts), tile_i_for_point)
  pts_by_tile <- pts_by_tile[!is.na(names(pts_by_tile))]
  used_tiles <- as.integer(names(pts_by_tile))
  
  # ---- layer naming (must be consistent across tiles) ----
  # We'll infer names from the first non-empty tile's files (after filtering by `layers`)
  first_files <- tile_info[[1]]$files
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
  expected_names <- make_names(first_files)
  
  # ---- parallel extraction over tiles with points ----
  old_plan <- future::plan()
  on.exit(future::plan(old_plan), add = TRUE)
  future::plan(if (plan == "multicore") future::multicore else future::multisession,
               workers = cores)
  
  res_list <- future.apply::future_lapply(
    used_tiles,
    function(ti) {
      idx <- pts_by_tile[[as.character(ti)]]
      psub <- pts_sv[idx]
      
      # Build a multi-layer SpatRaster for this tile from its files
      files <- tile_info[[ti]]$files
      
      # Optional: enforce same layer count/order as the first tile selection
      nm <- make_names(files)
      # Reorder to match expected_names if same set (common case)
      if (length(nm) == length(expected_names) && setequal(nm, expected_names)) {
        files <- files[match(expected_names, nm)]
        nm <- expected_names
      }
      r <- terra::rast(files)
      names(r) <- nm
      
      v <- terra::extract(r, psub, method = method, ...)
      point_index <- idx[v$ID]
      
      out <- cbind(
        data.frame(point_index = point_index, stringsAsFactors = FALSE),
        v[, -1, drop = FALSE]
      )
      out$tile_dir <- tile_info[[ti]]$tile_dir
      out
    },
    future.seed = TRUE,
    future.packages = "terra",
    future.globals = FALSE
  )
  
  extracted <- do.call(rbind, res_list)
  
  # Identify extracted value columns
  value_cols <- setdiff(names(extracted), c("point_index", "tile_dir"))
  
  # ---- build full output in original point order ----
  out <- data.frame(point_index = seq_len(n_pts), stringsAsFactors = FALSE)
  if (!is.null(id_col)) out$point_id <- point_id
  
  for (nm in value_cols) out[[nm]] <- NA
  out$tile_dir <- NA_character_
  
  ord <- match(extracted$point_index, out$point_index)
  out[ord, value_cols] <- extracted[, value_cols, drop = FALSE]
  out$tile_dir[ord] <- extracted$tile_dir
  
  out
}
