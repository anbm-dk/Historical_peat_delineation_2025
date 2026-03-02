# Models for historical peat

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


library(terra)
library(magrittr)
library(dplyr)
library(stringr)
library(sf)
library(future)
library(caret)
library(ranger)

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

source("covariate_names.R")

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

# Remove unwanted covariaes

cov_names_selected %<>%
  str_subset(pattern = "s1_2020", negate = TRUE) %>%
  str_subset(pattern = "cost_dist", negate = TRUE)
  
cov_names_LU %<>%
  str_subset(pattern = "s1_2020", negate = TRUE) %>%
  str_subset(pattern = "cost_dist", negate = TRUE) %>%
  generics::intersect(., colnames(LU1700_data_all))

# Remove NAs from training data (for now, omit later)

Jupiter_data_peat_train <- Jupiter_data_peat_train %>%
  select(any_of(cov_names_selected)) %>%
  apply(1, function(x) {sum(is.na(x))}) %>%
  data.frame(num_na = .) %>%
  bind_cols(Jupiter_data_peat_train) %>%
  filter(num_na == 0) %>%
  select(-num_na)

Jupiter_data_presence_train <- Jupiter_data_presence_train %>%
  select(any_of(cov_names_selected)) %>%
  apply(1, function(x) {sum(is.na(x))}) %>%
  data.frame(num_na = .) %>%
  bind_cols(Jupiter_data_presence_train) %>%
  filter(num_na == 0) %>%
  select(-num_na)

ochre_data_peat_train <- ochre_data_peat_train %>%
  select(any_of(cov_names_selected)) %>%
  apply(1, function(x) {sum(is.na(x))}) %>%
  data.frame(num_na = .) %>%
  bind_cols(ochre_data_peat_train) %>%
  filter(num_na == 0) %>%
  select(-num_na)

ochre_data_presence_train <- ochre_data_presence_train %>%
  select(any_of(cov_names_selected)) %>%
  apply(1, function(x) {sum(is.na(x))}) %>%
  data.frame(num_na = .) %>%
  bind_cols(ochre_data_presence_train) %>%
  filter(num_na == 0) %>%
  select(-num_na)

LU1700_data_train <- LU1700_data_train %>%
  select(any_of(cov_names_LU)) %>%
  apply(1, function(x) {sum(is.na(x))}) %>%
  data.frame(num_na = .) %>%
  bind_cols(LU1700_data_train) %>%
  filter(num_na == 0) %>%
  select(-num_na)

# Make indices for cross validation

make_indices_train <- function(x) {
  folds3 <- ceiling(x / 3)
  
  out <- lapply(
    unique(folds3),
    function(x2) {
      out2 <- folds3 %>%
        data.frame(fold = .) %>%
        mutate(
          is_j = fold != x2,
          rnum = row_number(),
          ind_j = is_j * rnum
        ) %>%
        filter(ind_j != 0) %>%
        dplyr::select(., ind_j) %>%
        unlist() %>%
        unname()
      return(out2)
    }
  )
  
  return(out)
}

tr_ind_Jupiter_peat <- make_indices_train(Jupiter_data_peat_train$fold)
tr_ind_Jupiter_presence <- make_indices_train(Jupiter_data_presence_train$fold)
tr_ind_ochre_peat <- make_indices_train(ochre_data_peat_train$fold)
tr_ind_ochre_presence <- make_indices_train(ochre_data_presence_train$fold)
tr_ind_LU <- make_indices_train(LU1700_data_train$fold)


# Preliminary models

n_cores <- 19
n_trees <- 100

source("optimize_ranger.R")

make_bounds_and_grid <- function(
    cov_names,
    data,
    rows_grid,
    seed = 1
    ) {
  bounds <- list(
    mtry = c(2, length(cov_names)),  
    sqrt_min.node.size = c(1, sqrt(nrow(data)/10)),  # NB
    maxcor = c(0.1, 1),
    maximp = c(0.1, 1),
    extratrees = c(1L, 2L)
  )
  
  set.seed(seed)
  
  grid <- lapply(
    bounds, 
    function(x) {
      out = x[1] + runif(3) %>%
        multiply_by(c(0.10, 0.5, 0.90)) %>%
        multiply_by(x[2] - x[1])
      return(out)
    }
  ) %>%
    bind_cols() %>%
    expand.grid() %>%
    distinct() %>%
    sample_n(rows_grid)
  
  out <- list()
  out$bounds <- bounds
  out$grid <- grid
  return(out)
}

# Jupiter model for peat

bounds_and_grid_i <- make_bounds_and_grid(
    cov_names = cov_names_selected,
    data = Jupiter_data_peat_train,
    rows_grid = n_cores*2,
    seed = 875982760
)

bounds_opt <- bounds_and_grid_i$bounds
ingrid <- bounds_and_grid_i$grid

model_Jupiter_peat <- optimize_ranger(
  data = Jupiter_data_peat_train,
  target = "is_peat",
  cov_names = cov_names_selected,
  bounds_bayes = bounds_opt, # named list with bounds for bayesian opt.
  initgrid = ingrid,
  folds = tr_ind_Jupiter_peat, # list with indices, folds for cross validation
  sumfun = twoClassSummary, # summary function for accuracy assessment
  metric = "ROC", # character, length 1, name of evaluation metric
  max_metric = TRUE, # logical, should the evaluation metric be maximized
  classprob = TRUE, # should class probabilities be calculated
  cores = n_cores, # number cores for parallelization
  seed = 875982760,  # Random seed for model training
  numtrees = n_trees
)

model_Jupiter_peat

varImp(model_Jupiter_peat$model)

saveRDS(
  model_Jupiter_peat,
  paste0(dir_dat, "model_Jupiter_peat.rds")
)

# Jupiter model for point distributions

bounds_and_grid_i <- make_bounds_and_grid(
  cov_names = cov_names_selected,
  data = Jupiter_data_presence_train,
  rows_grid = n_cores*2,
  seed = 875982760
)

bounds_opt <- bounds_and_grid_i$bounds
ingrid <- bounds_and_grid_i$grid

model_Jupiter_presence <- optimize_ranger(
  data = Jupiter_data_presence_train,  # NB
  target = "sampled", # NB
  cov_names = cov_names_selected,
  bounds_bayes = bounds_opt, # named list with bounds for bayesian opt.
  initgrid = ingrid,
  folds = tr_ind_Jupiter_presence,  # NB
  sumfun = twoClassSummary, 
  metric = "ROC", 
  max_metric = TRUE, 
  classprob = TRUE, 
  cores = n_cores, 
  seed = 875982760,  
  numtrees = n_trees
)

model_Jupiter_presence

varImp(model_Jupiter_presence$model)

saveRDS(
  model_Jupiter_presence,
  paste0(dir_dat, "model_Jupiter_presence.rds")
)

# Ochre model for peat

bounds_and_grid_i <- make_bounds_and_grid(
  cov_names = cov_names_selected,
  data = ochre_data_peat_train,
  rows_grid = n_cores*2,
  seed = 875982760
)

bounds_opt <- bounds_and_grid_i$bounds
ingrid <- bounds_and_grid_i$grid

model_ochre_peat <- optimize_ranger(
  data = ochre_data_peat_train,  # NB
  target = "is_peat", # NB
  cov_names = cov_names_selected,
  bounds_bayes = bounds_opt, # named list with bounds for Bayesian opt.
  initgrid = ingrid,
  folds = tr_ind_ochre_peat,  # NB
  sumfun = twoClassSummary, 
  metric = "ROC", 
  max_metric = TRUE, 
  classprob = TRUE, 
  cores = n_cores, 
  seed = 875982760,  
  numtrees = n_trees
)

model_ochre_peat

varImp(model_ochre_peat$model)

saveRDS(
  model_ochre_peat,
  paste0(dir_dat, "model_ochre_peat.rds")
)

# Ochre model for presence

bounds_and_grid_i <- make_bounds_and_grid(
  cov_names = cov_names_selected,
  data = ochre_data_presence_train,
  rows_grid = n_cores*2,
  seed = 875982760
)

bounds_opt <- bounds_and_grid_i$bounds
ingrid <- bounds_and_grid_i$grid

model_ochre_presence <- optimize_ranger(
  data = ochre_data_presence_train,  # NB
  target = "sampled", # NB
  cov_names = cov_names_selected,
  bounds_bayes = bounds_opt, # named list with bounds for Bayesian opt.
  initgrid = ingrid,
  folds = tr_ind_ochre_presence,  # NB
  sumfun = twoClassSummary, 
  metric = "ROC", 
  max_metric = TRUE, 
  classprob = TRUE, 
  cores = n_cores, 
  seed = 875982760,  
  numtrees = n_trees
)

model_ochre_presence

varImp(model_ochre_presence$model)

saveRDS(
  model_ochre_peat,
  paste0(dir_dat, "model_ochre_presence.rds")
)

# 1700s Land use models

LU_1700_summary <- LU_1700_pts %>%
  values() %>%
  group_by(
    LU_txt
  ) %>%
  summarise(
    lu18thcent = mean(lu18thcent),
    n = n()
  ) %>%
  arrange(
    lu18thcent
  ) %>%
  mutate(
    LU_txt_nospace = str_replace_all(LU_txt , " ", "_")
  )

LU_models <- list()

for (i in 1:nrow(LU_1700_summary)) {
  LU_train_i <- LU1700_data_train %>%
    mutate(
      is_lu = factor(
        as.numeric(lu18thcent == i),
        levels = c(0, 1),
        labels = c("No", "Yes")
        )
    )
  
  bounds_and_grid_i <- make_bounds_and_grid(
    cov_names = cov_names_LU,
    data = LU_train_i,
    rows_grid = n_cores*2,
    seed = 875982760
  )
  
  bounds_opt <- bounds_and_grid_i$bounds
  ingrid <- bounds_and_grid_i$grid
  
  LU_models[[i]] <- optimize_ranger(
    data = LU_train_i,  # NB
    target = "is_lu", # NB
    cov_names = cov_names_LU,
    bounds_bayes = bounds_opt, # named list with bounds for Bayesian opt.
    initgrid = ingrid,
    folds = tr_ind_LU,  # NB
    sumfun = twoClassSummary, 
    metric = "ROC", 
    max_metric = TRUE, 
    classprob = TRUE, 
    cores = n_cores, 
    seed = 875982760,  
    numtrees = n_trees
  )
  
  print(LU_1700_summary$LU_txt_nospace[i])
  print(LU_models[[i]])
  
  varImp(LU_models[[i]]$model)
  
  saveRDS(
    LU_models[[i]],
    paste0(
      dir_dat, "model_1700s_", str_pad(i, width = 2, pad = "0"),  "_", 
      LU_1700_summary$LU_txt_nospace[i],  ".rds"
    )
  )
}

names(LU_models) <- LU_1700_summary$LU_txt_nospace

# saveRDS(
#   LU_models,
#   paste0(dir_dat, "LU_models_all.rds")
# )

# Load covariate tile

tile_dirs <- list.dirs(
  paste0(root, "/tiles_591"), 
  recursive = FALSE, 
  full.names = TRUE
)

cov_tile_300 <- tile_dirs[300] %>%
  list.files(
    pattern = "\\.tif$",
    full.names = TRUE
  ) %>%
  rast()

# Try mapping peat based on jupiter

model_i <- model_Jupiter_peat$model

cov_tile_300_selected <- cov_tile_300 %>%
  terra::subset(names(model_i$finalModel$variable.importance))

pred_jup_peat <- terra::predict(
  cov_tile_300_selected,
  model_i$finalModel,
  na.rm = TRUE,
  fun = function(model, ...) predict(model, ...)$predictions,
  index = 2
)

plot(pred_jup_peat)

# Map point probability for jupiter

model_i <- model_Jupiter_presence$model

cov_tile_300_selected <- cov_tile_300 %>%
  terra::subset(names(model_i$finalModel$variable.importance))

pred_jup_presence <- terra::predict(
  cov_tile_300_selected,
  model_i$finalModel,
  na.rm = TRUE,
  fun = function(model, ...) predict(model, ...)$predictions,
  index = 2
)

plot(pred_jup_presence)

prob_presence_jup <- pred_jup_peat*pred_jup_presence
prob_absence_jup <- (1-pred_jup_peat)*pred_jup_presence
peat_unc_jup <- (1 - (prob_presence_jup - prob_absence_jup)^2)

plot(prob_presence_jup)
plot(prob_absence_jup)
plot(peat_unc_jup)

# Try mapping peat based on ochre db

model_i <- model_ochre_peat$model

cov_tile_300_selected <- cov_tile_300 %>%
  terra::subset(names(model_i$finalModel$variable.importance))

pred_ochre_peat <- terra::predict(
  cov_tile_300_selected,
  model_i$finalModel,
  na.rm = TRUE,
  fun = function(model, ...) predict(model, ...)$predictions,
  index = 2
)

plot(pred_ochre_peat)

# Map point probability for ochre db

model_i <- model_ochre_presence$model

cov_tile_300_selected <- cov_tile_300 %>%
  terra::subset(names(model_i$finalModel$variable.importance))

pred_ochre_presence <- terra::predict(
  cov_tile_300_selected,
  model_i$finalModel,
  na.rm = TRUE,
  fun = function(model, ...) predict(model, ...)$predictions,
  index = 2
)

plot(pred_ochre_presence)

prob_presence_ochre <- pred_ochre_peat*pred_ochre_presence
prob_absence_ochre <- (1-pred_ochre_peat)*pred_ochre_presence
peat_unc_ochre <- (1 - (prob_presence_ochre - prob_absence_ochre)^2)

plot(prob_presence_ochre)
plot(prob_absence_ochre)
plot(peat_unc_ochre)

# Map land use probability

lu_prob_pred <- list()

for (i in 1:length(LU_models)) {
  model_i <- LU_models[[i]]$model
  
  cov_tile_300_selected <- cov_tile_300 %>%
    terra::subset(names(model_i$finalModel$variable.importance))
  
  lu_prob_pred[[i]] <- terra::predict(
    cov_tile_300_selected,
    model_i$finalModel,
    na.rm = TRUE,
    fun = function(model, ...) predict(model, ...)$predictions,
    index = 2
  )
}

names(lu_prob_pred) <- LU_1700_summary$LU_txt_nospace

lu_prob_rast <- rast(lu_prob_pred)

plot(lu_prob_rast)

lu_prob_sum <- sum(lu_prob_rast)

plot(lu_prob_sum)

lu_prob_rast2 <- lu_prob_rast / lu_prob_sum

plot(lu_prob_rast2)

lu_pred <- which.max(lu_prob_rast2)
  
levels(lu_pred) <- select(LU_1700_summary, c(lu18thcent, LU_txt))

plot(lu_pred)

library(probably)

# Predict for all tiles

library(parallel)
library(caret)
library(terra)
library(magrittr)
library(dplyr)
library(foreach)
library(stringr)


# Tiles for model prediction

numCores <- detectCores()
numCores

dir_tiles <- root %>%
  paste0(., "/tiles_591/")

subdir_tiles <- dir_tiles %>%
  list.dirs() %>%
  .[-1]

dir_pred_all <- dir_dat %>%
  paste0(., "/predictions/") %T>%
  dir.create(showWarnings = FALSE, recursive = TRUE)

dir_pred_tiles <- dir_pred_all %>%
  paste0(., "/tiles/") %T>%
  dir.create(showWarnings = FALSE, recursive = TRUE)

# Function for tiled predictions

predict_tiles <- function(
    model = NULL,
    target = NULL,
    subdir_tiles = NULL,
    dir_pred_all = NULL,
    dir_pred_tiles = NULL,
    cores = NULL,
    digits = NULL,
    temp = NULL
) {
  cov_selected <- names(model$finalModel$variable.importance)
  
  dir_pred_tiles_target <- dir_pred_tiles %>%
    paste0(
      ., "/", target, "/"
    ) %T>%
    dir.create(showWarnings = FALSE, recursive = TRUE)
  
  showConnections()
  
  cl <- makeCluster(cores)
  
  clusterEvalQ(
    cl,
    {
      library(terra)
      library(caret)
      library(ranger)
      library(magrittr)
      library(dplyr)
      library(tools)
    }
  )
  
  clusterExport(
    cl,
    c(
      "model",
      "subdir_tiles",
      "dir_pred_tiles_target",
      "target",
      "cov_selected",
      "digits",
      "temp"
    ),
    envir = environment()
  )
  
  parSapplyLB(
    cl,
    1:length(subdir_tiles),
    function(x) {
      terraOptions(memfrac = 0.02, tempdir = temp)
      
      cov_x_files <- subdir_tiles[x] %>%
        list.files(full.names = TRUE)
      
      cov_x_names <- cov_x_files %>%
        basename() %>%
        file_path_sans_ext()
      
      cov_x <- cov_x_files %>% rast()
      
      names(cov_x) <- cov_x_names
      
      cov_x %<>% subset(cov_selected)
      
      tilename_x <- basename(subdir_tiles[x])
      
      outname_x <- dir_pred_tiles_target %>%
        paste0(
          ., "/", target, "_",
          tilename_x, ".tif"
        )
      
      pred_x <- terra::predict(
        cov_x,
        model = model$finalModel,
        na.rm = TRUE,
        fun = function(model, ...) round(predict(model, ...)$predictions, digits = digits),
        index = 2,
        cores = 1,
        filename = outname_x,
        overwrite = TRUE
      )
      
      # math(
      #   pred_x, 
      #   "round",
      #   digits = digits,
      #   filename = outname_x,
      #   overwrite = TRUE
      # )
      
      return(NULL)
    }
  )
  
  stopCluster(cl)
  foreach::registerDoSEQ()
  rm(cl)
  
  outtiles_target <- dir_pred_tiles_target %>%
    list.files(full.names = TRUE) %>%
    sprc()
  
  merge(
    outtiles_target,
    filename = paste0(
      dir_pred_all, target, ".tif"),
    overwrite = TRUE,
    gdal = "TILED=YES",
    names = target
  )
}

# Predict peat from Jupiter

predict_tiles(
    model = model_Jupiter_peat$model,
    target = "Jupiter_ispeat",
    subdir_tiles = subdir_tiles,
    dir_pred_all = dir_pred_all,
    dir_pred_tiles = dir_pred_tiles,
    cores = numCores,
    digits = 3,
    temp = tmpfolder
)

# unlink(
#   list.files(tmpfolder, full.names = TRUE),
#   recursive = TRUE,
#   force = TRUE
# )

# Predict relative sampling density from Jupiter

predict_tiles(
  model = model_Jupiter_presence$model,
  target = "Jupiter_rsd",
  subdir_tiles = subdir_tiles,
  dir_pred_all = dir_pred_all,
  dir_pred_tiles = dir_pred_tiles,
  cores = numCores,
  digits = 3,
  temp = tmpfolder
)

# Calculate presence and absence probabilities plus uncertainty before deleting
# the tiles.

unlink(
  list.files(tmpfolder, full.names = TRUE),
  recursive = TRUE,
  force = TRUE
)

# Predict peat from OchreDB

predict_tiles(
  model = model_ochre_peat$model,
  target = "OchreDB_ispeat",
  subdir_tiles = subdir_tiles,
  dir_pred_all = dir_pred_all,
  dir_pred_tiles = dir_pred_tiles,
  cores = numCores,
  digits = 3,
  temp = tmpfolder
)

# unlink(
#   list.files(tmpfolder, full.names = TRUE),
#   recursive = TRUE,
#   force = TRUE
# )

# Predict relative sampling density  from OchreDB

predict_tiles(
  model = model_ochre_presence$model,
  target = "OchreDB_rsd",
  subdir_tiles = subdir_tiles,
  dir_pred_all = dir_pred_all,
  dir_pred_tiles = dir_pred_tiles,
  cores = numCores,
  digits = 3,
  temp = tmpfolder
)

# Calculate presence and absence probabilities plus uncertainty before deleting
# the tiles.

unlink(
  list.files(tmpfolder, full.names = TRUE),
  recursive = TRUE,
  force = TRUE
)

# Predict LU

for (i in 1:length(LU_models)) {
  predict_tiles(
    model = LU_models[[i]]$model,
    target = paste0("LU1700_", LU_1700_summary$LU_txt_nospace[i]),
    subdir_tiles = subdir_tiles,
    dir_pred_all = dir_pred_all,
    dir_pred_tiles = dir_pred_tiles,
    cores = numCores,
    digits = 3,
    temp = tmpfolder
  )
}

# Normalize probabilities and calculate classes before deleting the tiles.

unlink(
  list.files(tmpfolder, full.names = TRUE),
  recursive = TRUE,
  force = TRUE
)

# END