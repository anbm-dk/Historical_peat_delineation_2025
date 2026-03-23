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

# END