# Summarise models

library(terra)
library(magrittr)
library(dplyr)
library(stringr)
library(sf)
library(future)
library(caret)
library(ranger)
library(tibble)
library(tidyr)
library(tidyterra)

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

# Load observations

source("load_obs.R")

# Load peat models

model_Jupiter_peat <- readRDS(
  paste0(dir_dat, "model_Jupiter_peat.rds")
)
model_Jupiter_presence <- readRDS(
  paste0(dir_dat, "model_Jupiter_presence.rds")
)
model_ochre_peat <- readRDS(
  paste0(dir_dat, "model_ochre_peat.rds")
)
model_ochre_presence <- readRDS(
  paste0(dir_dat, "model_ochre_presence.rds")
)


# Load LU1700 models

LU_1700_pts <- paste0(
  root,
  "/covariates/LU_18thcentury_points/LU_points_18thCentury.shp"
) %>% 
  vect()

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
  LU_models[[i]] <- readRDS(
    paste0(
      dir_dat, "model_1700s_", str_pad(i, width = 2, pad = "0"),  "_",
      LU_1700_summary$LU_txt_nospace[i],  ".rds"
    )
  )
}

names(LU_models) <- LU_1700_summary$LU_txt_nospace


# Summarise peat models

model_Jupiter_peat$model
model_Jupiter_peat$model$trainingData$.outcome %>% table
model_Jupiter_peat$model$finalModel$forest$child.nodeIDs %>%
  lapply(
    function(x) {
      out <- length(x[[1]])
      return(out)
    }
  ) %>%
  unlist() %>%
  mean()

model_Jupiter_presence$model
model_Jupiter_presence$model$trainingData$.outcome %>% table
model_Jupiter_presence$model$finalModel$forest$child.nodeIDs %>%
  lapply(
    function(x) {
      out <- length(x[[1]])
      return(out)
    }
  ) %>%
  unlist() %>%
  mean()

model_ochre_peat$model
model_ochre_peat$model$trainingData$.outcome %>% table
model_ochre_peat$best_scores

model_ochre_peat$model$finalModel$forest$child.nodeIDs %>%
  lapply(
    function(x) {
      out <- length(x[[1]])
      return(out)
    }
  ) %>%
  unlist() %>%
  mean()

model_ochre_presence$model
model_ochre_presence$model$trainingData$.outcome %>% table
model_ochre_presence$best_scores

model_ochre_presence$model$finalModel$forest$child.nodeIDs %>%
  lapply(
    function(x) {
      out <- length(x[[1]])
      return(out)
    }
  ) %>%
  unlist() %>%
  mean()

varImp(model_Jupiter_peat$model)

varImp(model_Jupiter_presence$model)

rownames(varImp(model_Jupiter_presence$model)$importance)

varImp(model_ochre_peat$model)

varImp(model_ochre_presence$model)


# Summarize LU1700 models

names(LU_models) %>% as.data.frame()

LU_1700_pts

LU_models %>%
  lapply(
    function(x) {
      out <- x$model$results
      return(out)
    }
  ) %>%
  bind_rows()

LU_models %>%
  lapply(
    function(x) {
      out <- x$best_scores
      return(out)
    }
  ) %>%
  bind_rows()

LU_models %>%
  lapply(
    function(x) {
      out <- x$model$finalModel$num.independent.variables
      return(out)
    }
  ) %>%
  unlist() %>%
  as.data.frame()

LU_models %>%
  lapply(
    function(x) {
      out <- x$model$finalModel$forest$child.nodeIDs %>%
        lapply(
          function(x) {
            out <- length(x[[1]])
            return(out)
          }
        ) %>%
        unlist() %>%
        mean()
      return(out)
    }
  ) %>%
  unlist() %>%
  as.data.frame()

sapply(
  c(1:length(LU_models)),
  function(x) {
    myimp <- varImp(LU_models[[x]]$model)
    
    out <- myimp$importance %>%
      rownames_to_column() %>%
      mutate(
        target = names(LU_models)[x]
      ) %>%
      arrange(-Overall) %>%
      rowid_to_column("rank") %>%
      slice_head(n = 20)
    
    return(out)
  },
  simplify = FALSE
) %>%
  bind_rows() %>%
  as.data.frame()

# Summarise locations

table(Jupiter_data_peat_all$is_peat)
table(Jupiter_data_peat_train$is_peat)
table(Jupiter_data_peat_test$is_peat)

table(Jupiter_data_presence_train$sampled)
table(Jupiter_data_presence_test$sampled)

table(ochre_data_peat_all$is_peat)
table(ochre_data_peat_train$is_peat)
table(ochre_data_peat_test$is_peat)

table(ochre_data_presence_train$sampled)
table(ochre_data_presence_test$sampled)

# Accuracy for the independent datasets

# Jupiter - peat

pred_test_Jupiter_peat <- predict(
  model_Jupiter_peat$model,
  newdata = Jupiter_data_peat_test,
  type = "prob"
) %>%
  mutate(
    obs = Jupiter_data_peat_test$is_peat,
    pred = predict(
      model_Jupiter_peat$model,
      newdata = Jupiter_data_peat_test
    )
  )

pred_test_Jupiter_peat

twoClassSummary(pred_test_Jupiter_peat, lev = c("No", "Yes"))

# Jupiter RSD

pred_test_Jupiter_presence <- predict(
  model_Jupiter_presence$model,
  newdata = Jupiter_data_presence_test,
  type = "prob"
) %>%
  mutate(
    obs = Jupiter_data_presence_test$sampled,
    pred = predict(
      model_Jupiter_presence$model,
      newdata = Jupiter_data_presence_test
    )
  )

pred_test_Jupiter_presence

twoClassSummary(pred_test_Jupiter_presence, lev = c("No", "Yes"))

# ochre - peat

pred_test_ochre_peat <- predict(
  model_ochre_peat$model,
  newdata = ochre_data_peat_test,
  type = "prob"
) %>%
  mutate(
    obs = ochre_data_peat_test$is_peat,
    pred = predict(
      model_ochre_peat$model,
      newdata = ochre_data_peat_test
    )
  )

pred_test_ochre_peat

twoClassSummary(pred_test_ochre_peat, lev = c("No", "Yes"))

# ochre RSD

pred_test_ochre_presence <- predict(
  model_ochre_presence$model,
  newdata = ochre_data_presence_test,
  type = "prob"
) %>%
  mutate(
    obs = ochre_data_presence_test$sampled,
    pred = predict(
      model_ochre_presence$model,
      newdata = ochre_data_presence_test
    )
  )

pred_test_ochre_presence

twoClassSummary(pred_test_ochre_presence, lev = c("No", "Yes"))


# Average importance for LU models

LU_imp_all <- sapply(
  c(1:length(LU_models)),
  function(x) {
    myimp <- varImp(LU_models[[x]]$model)
    
    out <- myimp$importance %>%
      rownames_to_column() %>%
      mutate(
        target = names(LU_models)[x]
      )
    
    return(out)
  },
  simplify = FALSE
) %>%
  bind_rows() %>%
  as.data.frame()

LU_imp_all %>%
  pivot_wider(
    values_from = Overall,
    names_from = target
  ) %>% 
  mutate(
    across(
      everything(), 
      ~replace_na(., 0)
    )
  ) %>%
  rowwise() %>%
  mutate(
    mean = mean(
      c(
        Forest, Thicket, Moor, Meadow_or_bog, Lake, Open_country, Marsh, 
        Market_town, Dunes, Sea
      )
    )
  ) %>%
  ungroup() %>%
  arrange(-mean) %>%
  select(rowname, mean) %>%
  slice_head(n = 20) %>%
  as.data.frame()


# Independent validation for LU models

LU1700_ROC_test <- sapply(
  c(1:length(LU_models)),
  function(x) {
    
    LU_data_test_i <- LU1700_data_test %>%
      mutate(
        is_lu = factor(
          as.numeric(lu18thcent == x),
          levels = c(0, 1),
          labels = c("No", "Yes")
        )
      )
    
    pred_i <- predict(
      LU_models[[x]]$model,
      newdata = LU_data_test_i,
      type = "prob"
    ) %>%
      mutate(
        obs = LU_data_test_i$is_lu,
        pred = predict(
          LU_models[[x]]$model,
          newdata = LU_data_test_i
        )
      )
    
    out <- twoClassSummary(pred_i, lev = c("No", "Yes"))
    
    return(out)
  },
  simplify = FALSE
) %>%
  bind_rows()

LU1700_ROC_test

# Accuracy for LU class predictions

rast_LU_pred <- paste0(
  dir_dat, "/predictions/LU1700_class.tif"
) %>%
  rast()

LU_test_pred_class <- LU_1700_pts %>%
  mutate(
    fold = LU1700_data_all$fold
  ) %>%
  filter(fold == 10) %>%
  terra::extract(
    x = rast_LU_pred,
    y = .,
    ID = FALSE,
    bind = TRUE
  )

LU_confusion <- LU_test_pred_class %>%
  values() %>%
  mutate(
    pred = factor(
      LU1700_class, 
      levels = c(1:10), 
      labels = LU_1700_summary$LU_txt_nospace
      ),
    obs = factor(
      lu18thcent, 
      levels = c(1:10), 
      labels = LU_1700_summary$LU_txt_nospace
    )
  ) %>%
  select(
    obs, pred
  ) %>%
  as.list() %>%
  table() %>%
  confusionMatrix()

LU_confusion

LU_confusion$byClass

LU_confusion$byClass %>% dimnames() %>% magrittr::extract2(2) %>% as.data.frame()


# Compare to uncertainty

rast_LU_unc <- paste0(
  dir_dat, "/predictions/LU1700_uncertainty.tif"
) %>%
  rast()

rast_LU_class_unc <- c(
  rast_LU_pred,
  rast_LU_unc
)

LU_test_class_unc <- LU_1700_pts %>%
  mutate(
    fold = LU1700_data_all$fold
  ) %>%
  filter(fold == 10) %>%
  terra::extract(
    x = rast_LU_class_unc,
    y = .,
    ID = FALSE,
    bind = TRUE
  ) %>%
  values() %>%
  mutate(
    correct = as.numeric(lu18thcent == LU1700_class),
    unc_class = cut(LU1700_uncertainty, seq(0, 1, 0.1), include.lowest = TRUE)
  )

LU_test_class_unc %>%
  group_by(lu18thcent) %>%
  summarise(mean_unc = mean(LU1700_uncertainty))

LU_test_class_unc %>%
  group_by(LU1700_class) %>%
  summarise(mean_unc = mean(LU1700_uncertainty))

LU_test_class_unc %>%
  group_by(unc_class) %>%
  summarise(
    perc_correct = mean(correct)*100,
    n = n()
    )

# END
