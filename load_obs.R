
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

# Load extracts

cov_extr_Jupiter <-  readRDS(paste0(dir_dat, "cov_extr_Jup.Rds"))
cov_extr_ochre <-    readRDS(paste0(dir_dat, "cov_extr_ochre.Rds"))
cov_extr_LU <-     readRDS(paste0(dir_dat, "cov_extr_LU.Rds"))
bg_extr_Jupiter <-   readRDS(paste0(dir_dat, "bg_extr_Jup.Rds"))
bg_extr_Ochre <-     readRDS(paste0(dir_dat, "bg_extr_ochre.Rds"))

folds_extr_Jupiter <-  readRDS(paste0(dir_dat, "folds_extr_Jup.Rds"))
folds_extr_ochre <-    readRDS(paste0(dir_dat, "folds_extr_ochre.Rds"))
folds_extr_LU <-     readRDS(paste0(dir_dat, "folds_extr_LU.Rds"))
bg_folds_Jupiter <-    readRDS(paste0(dir_dat, "bg_folds_Jup.Rds"))
bg_folds_ochre <-      readRDS(paste0(dir_dat, "bg_folds_ochre.Rds"))


# Recreate background points

bg_Jupiter_pts <- terra::vect(
  bg_extr_Jupiter, 
  geom = c("ogc_pi000", "ogc_pi500"), 
  crs = mycrs, 
  keepgeom = TRUE
)

bg_Ochre_pts <- terra::vect(
  bg_extr_Ochre, 
  geom = c("ogc_pi000", "ogc_pi500"), 
  crs = mycrs, 
  keepgeom = TRUE
)

# Make training/test data for Jupiter

Jupiter_data_peat_all <- bind_cols(
  values(Jupiter_pts),
  cov_extr_Jupiter,
  folds_extr_Jupiter
) %>%
  mutate(
    is_peat = factor(is_peat, levels = c(0, 1), labels = c("No", "Yes"))
  )

Jupiter_data_presence_all <- bind_rows(
  Jupiter_data_peat_all %>%
    mutate(
      sampled = 1,
      point_id = as.character(point_id)
    ),
  bind_cols(
    bg_extr_Jupiter,
    bg_folds_Jupiter
  ) %>%
    mutate(
      sampled = 0,
      point_id = as.character(point_id)
    )
) %>%
  mutate(
    sampled = factor(sampled, levels = c(0, 1), labels = c("No", "Yes"))
  )

Jupiter_data_peat_train <- Jupiter_data_peat_all %>%
  filter(fold != 10)
Jupiter_data_peat_test <- Jupiter_data_peat_all %>%
  filter(fold == 10)
Jupiter_data_presence_train <- Jupiter_data_presence_all %>%
  filter(fold != 10)
Jupiter_data_presence_test <- Jupiter_data_presence_all %>%
  filter(fold == 10)

# Make training/test data for Ochre DB

ochre_data_peat_all <- bind_cols(
  values(ochre_pts),
  cov_extr_ochre,
  folds_extr_ochre
) %>%
  mutate(
    is_peat = factor(is_peat, levels = c(0, 1), labels = c("No", "Yes"))
  )

ochre_data_presence_all <- bind_rows(
  ochre_data_peat_all %>%
    mutate(
      sampled = 1,
      point_id = as.character(point_id)
    ),
  bind_cols(
    bg_extr_Ochre,
    bg_folds_ochre
  ) %>%
    mutate(
      sampled = 0,
      point_id = as.character(point_id)
    )
) %>%
  mutate(
    sampled = factor(sampled, levels = c(0, 1), labels = c("No", "Yes"))
  )

ochre_data_peat_train <- ochre_data_peat_all %>%
  filter(fold != 10)
ochre_data_peat_test <- ochre_data_peat_all %>%
  filter(fold == 10)
ochre_data_presence_train <- ochre_data_presence_all %>%
  filter(fold != 10)
ochre_data_presence_test <- ochre_data_presence_all %>%
  filter(fold == 10)

# Make training/test data for Ochre DB

LU1700_data_all <- bind_cols(
  values(LU_1700_pts),
  cov_extr_LU,
  folds_extr_LU
)

LU1700_data_train <- LU1700_data_all %>%
  filter(fold != 10)
LU1700_data_test <- LU1700_data_all %>%
  filter(fold == 10)

# END
