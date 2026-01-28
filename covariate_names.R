# Covariate names

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

cov_names_LU <- cov_lu %>% names()

# END