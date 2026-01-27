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

# Jupiter model for peat

source("optimize_ranger.R")

set.seed(875982760)

model_Jupiter_peat <- optimize_ranger(
  data = Jupiter_data_peat_train,
  target = "is_peat",
  cov_names = cov_names_selected,
  bounds_bayes = list(
    mtry = c(2, length(cov_names_selected)),  
    sqrt_min.node.size = c(1, sqrt(nrow(Jupiter_data_peat_train))),
    maxcor = c(0.1, 1),
    maximp = c(0.1, 1),
    extratrees = c(1L, 2L)
  ), # named list with bounds for bayesian opt.
  folds = tr_ind_Jupiter_peat, # list with indices, folds for cross validation
  sumfun = twoClassSummary, # summary function for accuracy assessment
  metric = "ROC", # character, length 1, name of evaluation metric
  max_metric = TRUE, # logical, should the evaluation metric be maximized
  classprob = TRUE, # should class probabilities be calculated
  cores = 19, # number cores for parallelization
  seed = 875982760,  # Random seed for model training
  numtrees = 100
)

model_Jupiter_peat

varImp(model_Jupiter_peat)

# Jupiter model for point distibutions

set.seed(875982760)

model_Jupiter_presence <- optimize_ranger(
  data = Jupiter_data_presence_train,  # NB
  target = "sampled", # NB
  cov_names = cov_names_selected,
  bounds_bayes = list(
    mtry = c(2, length(cov_names_selected)),  
    sqrt_min.node.size = c(1, sqrt(nrow(Jupiter_data_presence_train))),  # NB
    maxcor = c(0.1, 1),
    maximp = c(0.1, 1),
    extratrees = c(1L, 2L)
  ), 
  folds = tr_ind_Jupiter_presence,  # NB
  sumfun = twoClassSummary, 
  metric = "ROC", 
  max_metric = TRUE, 
  classprob = TRUE, 
  cores = 19, 
  seed = 875982760,  
  numtrees = 100
)

model_Jupiter_presence

varImp(model_Jupiter_presence)

# Ochre model for peat

set.seed(875982760)

model_ochre_peat <- optimize_ranger(
  data = ochre_data_peat_train,  # NB
  target = "is_peat", # NB
  cov_names = cov_names_selected,
  bounds_bayes = list(
    mtry = c(2, length(cov_names_selected)),  
    sqrt_min.node.size = c(1, sqrt(nrow(ochre_data_peat_train))),  # NB
    maxcor = c(0.1, 1),
    maximp = c(0.1, 1),
    extratrees = c(1L, 2L)
  ), 
  folds = tr_ind_ochre_peat,  # NB
  sumfun = twoClassSummary, 
  metric = "ROC", 
  max_metric = TRUE, 
  classprob = TRUE, 
  cores = 19, 
  seed = 875982760,  
  numtrees = 100
)


model_ochre_peat

# Ochre model for presence

set.seed(875982760)

model_ochre_presence <- optimize_ranger(
  data = ochre_data_presence_train,  # NB
  target = "sampled", # NB
  cov_names = cov_names_selected,
  bounds_bayes = list(
    mtry = c(2, length(cov_names_selected)),  
    sqrt_min.node.size = c(1, sqrt(nrow(ochre_data_presence_train))),  # NB
    maxcor = c(0.1, 1),
    maximp = c(0.1, 1),
    extratrees = c(1L, 2L)
  ), 
  folds = tr_ind_ochre_presence,  # NB
  sumfun = twoClassSummary, 
  metric = "ROC", 
  max_metric = TRUE, 
  classprob = TRUE, 
  cores = 19, 
  seed = 875982760,  
  numtrees = 100
)

model_ochre_presence


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
  
  set.seed(875982760)
  
  LU_models[[i]] <- optimize_ranger(
    data = LU_train_i,  # NB
    target = "is_lu", # NB
    cov_names = cov_names_LU,
    bounds_bayes = list(
      mtry = c(2, length(cov_names_LU)),  
      sqrt_min.node.size = c(1, sqrt(nrow(LU_train_i))),  # NB
      maxcor = c(0.1, 1),
      maximp = c(0.1, 1),
      extratrees = c(1L, 2L)
    ), 
    folds = tr_ind_LU,  # NB
    sumfun = twoClassSummary, 
    metric = "ROC", 
    max_metric = TRUE, 
    classprob = TRUE, 
    cores = 19, 
    seed = 875982760,  
    numtrees = 100
  )
  
  print(LU_1700_summary$LU_txt_nospace[i])
  print(LU_models[[i]])
}

names(LU_models) <- LU_1700_summary$LU_txt_nospace



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



plot(rast(lu_prob_pred))

# END