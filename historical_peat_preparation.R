# Layers for historical peat delineation

library(terra)
library(magrittr)
library(dplyr)
library(stringr)
library(sf)
library(future)

dir_code <- getwd()
root <- dirname(dir_code)
dir_dat <- paste0(root, "/Historical_peat_data/")

tmpfolder <- paste0(dir_dat, "/Temp/") %T>% dir.create()

mycrs <- "EPSG:25832"

dir_cov <- root %>%
  paste0(., "/covariates/covariates_10m/")

cov_files <- dir_cov %>%
  list.files(
    pattern = "\\.tif$",
    full.names = TRUE
  )

terraOptions(tempdir = tmpfolder)

# Load points

Jupiter_pts <- paste0(
  dir_dat, 
  "/Input_points/Jupiter_pts_processed.rds"
  ) %>%
  readRDS() %>%
  vect(
    geom = c("x", "y"), 
    crs = mycrs, 
    keepgeom = TRUE
  )

ochre_pts <- paste0(
  dir_dat, 
  "/Input_points/ochre_pts_processed.rds"
) %>%
  readRDS() %>%
  vect(
    geom = c("x", "y"), 
    crs = mycrs, 
    keepgeom = TRUE
  )

LU_1700_pts <- paste0(
  root,
  "/covariates/LU_18thcentury_points/LU_points_18thCentury.shp"
  ) %>% 
  vect()

plot(Jupiter_pts, "is_peat")
plot(ochre_pts, "is_peat")
plot(LU_1700_pts, "LU_txt")

# Load covariates

cov_all <- dir_cov %>%
  list.files(
    pattern = "\\.tif$",
    full.names = TRUE
  ) %>%
  rast()

names(cov_all)

covnames_topo_clim <- c(
  "chelsa_bio01_1981_2010_10m",
  "chelsa_bio12_1981_2010_10m",
  "convergence_index",
  "cos_aspect_radians",
  "cross_sectional_curvature",
  "detrended_3_mean",
  "dhm2015_terraen_10m",
  "flooded_depth_10m_mean",
  "flow_accumulation",
  "hillyness",
  "longitudinal_curvature",
  "maximal_curvature",
  "mid_slope_positon",
  "minimal_curvature",
  "normalized_height",
  "positive_openness",
  "profile_curvature",
  "profile_curvature2", 
  "rvb_bios",
  "rvb_fot",
  "saga_wetness_index",
  "sin_aspect_radians",
  "slope",
  "slope_height",
  "standardized_height",
  "tangential_curvature",
  "total_curvature",
  "valley_depth",
  "vdtochn",                             
  "vdtochngt0"
)

cov_names_selected <- c(
  cov_all %>%
    names() %>%
    str_subset(pattern = "georeg", negate = FALSE),
  cov_all %>%
    names() %>%
    str_subset(pattern = "geology", negate = FALSE),
  cov_all %>%
    names() %>%
    str_subset(pattern = "landscape", negate = FALSE),
  cov_all %>%
    names() %>%
    str_subset(pattern = "ogc_", negate = FALSE),
  covnames_topo_clim
)

cov_selected <- cov_all %>%
  terra::subset(cov_names_selected)
cov_lu <- cov_all %>%
  terra::subset(
    cov_all %>%
      names() %>%
      str_subset(pattern = "gw_", negate = TRUE)
  )

# Extract covariates

n_bgsamples_Ochre <- 65482
n_bgsamples_Jup   <- 40892

# Extract all layers (all tif files inside each tile folder)

source("extract_tilefolders_parallel_psock.R")

# options(future.globals.onReference = "ignore")

tile_dirs <- list.dirs(
  paste0(root, "/tiles_591"), 
  recursive = FALSE, 
  full.names = TRUE
)

names(ochre_pts)[names(ochre_pts) == "ID"] <- "pt_id"

cov_extr_Ochre <- extract_tilefolders_parallel_psock(
  tile_dirs = tile_dirs,
  pts = sf::st_as_sf(ochre_pts),
  id_col = "pt_id",
  method = "simple",
  cores = 4,
  pts_crs = mycrs
)

head(cov_extr_Ochre)

names(Jupiter_pts)[names(Jupiter_pts) == "ID"] <- "pt_id"

cov_extr_Jup <- extract_tilefolders_parallel_psock(
  tile_dirs = tile_dirs,
  pts = sf::st_as_sf(Jupiter_pts),
  id_col = "pt_id",
  method = "simple",
  cores = 4,
  pts_crs = mycrs
)

names(Jupiter_pts)[names(Jupiter_pts) == "ID"] <- "pt_id"

cov_extr_Jup <- extract_tilefolders_parallel_psock(
  tile_dirs = tile_dirs,
  pts = sf::st_as_sf(Jupiter_pts),
  id_col = "pt_id",
  method = "simple",
  cores = 4,
  pts_crs = mycrs
)

LU_1700_pts$pt_id <- c(1:nrow(LU_1700_pts))

cov_extr_LU <- extract_tilefolders_parallel_psock(
  tile_dirs = tile_dirs,
  pts = sf::st_as_sf(LU_1700_pts),
  id_col = "pt_id",
  method = "simple",
  cores = 4,
  pts_crs = mycrs
)

# cov_extr_Ochre <- ochre_pts %>%
#   terra:: extract(
#     x = cov_selected,
#     y = ., 
#     ID = FALSE
#     )
# cov_extr_Jup <- Jupiter_pts %>%
#   terra:: extract(
#     x = cov_selected,
#     y = ., 
#     ID = FALSE
#   )
# cov_extr_LU <- LU_1700_pts %>%
#   terra:: extract(
#     x = cov_lu,
#     y = ., 
#     ID = FALSE
#   )

dem <- cov_all["^dhm"]

set.seed(31847)
bg_pts_Ochre <- terra::spatSample(
  dem,
  n_bgsamples_Ochre,
  values = FALSE,
  na.rm = TRUE,
  xy = TRUE,
  as.points = TRUE
)
set.seed(31847)
bg_pts_Jup <- terra::spatSample(
  dem,
  n_bgsamples_Jup,
  values = FALSE,
  na.rm = TRUE,
  xy = TRUE,
  as.points = TRUE
)

bg_pts_Ochre$pt_id <- c(1:nrow(bg_pts_Ochre))
bg_pts_Jup$pt_id <- c(1:nrow(bg_pts_Jup))

bg_extr_Ochre <- extract_tilefolders_parallel_psock(
  tile_dirs = tile_dirs,
  pts = sf::st_as_sf(bg_pts_Ochre),
  id_col = "pt_id",
  method = "simple",
  cores = 4,
  pts_crs = mycrs
) %>%
  bind_cols(
    values(bg_pts_Ochre), .
  )
bg_extr_Jup <- extract_tilefolders_parallel_psock(
  tile_dirs = tile_dirs,
  pts = sf::st_as_sf(bg_pts_Jup),
  id_col = "pt_id",
  method = "simple",
  cores = 4,
  pts_crs = mycrs
) %>% 
  bind_cols(
    values(bg_pts_Jup), .
  )

# bg_extr_Ochre <- bg_pts_Ochre %>%
#   terra:: extract(
#     x = cov_selected,
#     y = ., 
#     ID = FALSE
#   ) %>%
#   bind_cols(
#     values(bg_pts_Ochre), .
#   )
# bg_extr_Jup <- bg_pts_Jup %>%
#   terra:: extract(
#     x = cov_selected,
#     y = ., 
#     ID = FALSE
#   ) %>%
#   bind_cols(
#     values(bg_pts_Jup), .
#   )

saveRDS(cov_extr_Ochre, paste0(dir_dat, "cov_extr_Ochre.Rds"))
saveRDS(cov_extr_Jup,   paste0(dir_dat, "cov_extr_Jup.Rds"))
saveRDS(cov_extr_LU,    paste0(dir_dat, "cov_extr_LU.Rds"))
saveRDS(bg_extr_Ochre,  paste0(dir_dat, "bg_extr_Ochre.Rds"))
saveRDS(bg_extr_Jup,    paste0(dir_dat, "bg_extr_Jup.Rds"))

# Extract folds

folds_raster <- paste0(
  root, "/folds/folds_10_100m.tif"
) %>%
  rast()

names(folds_raster) <- "fold"

folds_extr_Ochre <- ochre_pts %>%
  terra:: extract(
    x = folds_raster,
    y = ., 
    ID = FALSE
  )
folds_extr_Jup <- Jupiter_pts %>%
  terra:: extract(
    x = folds_raster,
    y = ., 
    ID = FALSE
  )
folds_extr_LU <- LU_1700_pts %>%
  terra:: extract(
    x = folds_raster,
    y = ., 
    ID = FALSE
  )
bg_folds_Ochre <- bg_pts_Ochre %>%
  terra:: extract(
    x = folds_raster,
    y = ., 
    ID = FALSE
  )
bg_folds_Jup <- bg_pts_Jup %>%
  terra:: extract(
    x = folds_raster,
    y = ., 
    ID = FALSE
  )

saveRDS(folds_extr_Ochre, paste0(dir_dat, "folds_extr_Ochre.Rds"))
saveRDS(folds_extr_Jup,   paste0(dir_dat, "folds_extr_Jup.Rds"))
saveRDS(folds_extr_LU,    paste0(dir_dat, "folds_extr_LU.Rds"))
saveRDS(bg_folds_Ochre,   paste0(dir_dat, "bg_folds_Ochre.Rds"))
saveRDS(bg_folds_Jup,     paste0(dir_dat, "bg_folds_Jup.Rds"))

# Models
# Use probability random forest based on ranger
# Use extratrees
# covariate selection based on cummulative importance
# Optimize for AUC
# Save models to rds
# Plot covariate importance for each model

# Model for is_peat in Ochre pts
# Model for is_peat in Jupiter pts
# Model for relative sampling density for Ochre pts
# Model for relative sampling density for Jupiter pts
# Model for each 1700s land use class


# Predictions
# Use tiles
# Drop unused covariates
# Merge

# Map for is_peat in Ochre pts
# Map for is_peat in Jupiter pts
# Map for relative sampling density for Ochre pts
# Map for relative sampling density for Jupiter pts
# Map for each 1700s land use class
# Standardize probabilities for each 1700s land use class to a sum of 1
# Standardize per tile, then merge.




# END