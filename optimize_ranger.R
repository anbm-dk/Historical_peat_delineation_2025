# Function to optimize a ranger model

optimize_ranger <- function(
    data = NULL, # data frame, input data
    target = NULL,  # character vector (length 1), target variable.
    cov_names = NULL,  # Character vector, covariate names,
    bounds_bayes = NULL, # named list with bounds for bayesian opt.
    folds = NULL, # list with indices, folds for cross validation
    weights = NULL, # numeric, weights for model training and evaluation
    sumfun = NULL, # summary function for accuracy assessment
    metric = NULL, # character, length 1, name of evaluation metric
    max_metric = NULL, # logical, should the evaluation metric be maximized
    classprob = FALSE, # should class probabilities be calculated
    cores = 19, # number cores for parallelization
    seed = NULL,  # Random seed for model training
    numtrees = 100,
    initgrid = NULL
) {
  require(ParBayesianOptimization)
  require(caret)
  require(ranger)
  require(magrittr)
  require(dplyr)
  require(parallel)
  require(tools)
  require(boot)
  source("select_vars.R")
  
  formula_0 <- cov_names %>%
    paste0(collapse = " + ") %>%
    paste0(target, " ~ ", .) %>%
    as.formula()
  
  cor_cov <- data %>%
    select(any_of(cov_names)) %>%
    cor() %>%
    raise_to_power(2)
  
  targ_num <- data %>%
    pull(any_of(target)) %>%
    is.numeric()

  if (targ_num) {
    splitrules <- c("variance", "extratrees")
  } else {
    splitrules <- c("gini", "extratrees")
  }
  
  # Scoring function for Bayesian optimization
  scoringFunction <- function(
    mtry,  
    sqrt_min.node.size,
    maxcor,
    maximp,
    extratrees
  ) {
    # Model with all covariates
    set.seed(seed)
    model_0 <- ranger::ranger(
      formula = formula_0,
      data = data,
      importance = "impurity",
      num.trees = numtrees,
      mtry = round(mtry),
      write.forest = FALSE,
      probability = FALSE,
      min.node.size = round(sqrt_min.node.size^2),
      splitrule = splitrules[extratrees],
      num.threads = 1,
      # num.threads = cores,
      verbose = FALSE,
      node.stats = FALSE,
      seed = seed,
      na.action = "na.omit"
    )
    
    my_imp <- sort(model_0$variable.importance) %>% 
      rev() %>%
      divide_by(sum(.))
    
    cov_selection <- select_vars(
      importance = my_imp,
      correlation = cor_cov,
      maxcor = maxcor,
      maximp = maximp,
      minvar = 3
    )
    
    cov_filtered <- cov_selection$vars_selected

    # Make formula
    formula_i <- cov_filtered %>%
      paste0(collapse = " + ") %>%
      paste0(target, " ~ ", .) %>%
      as.formula()

    # Train model
    set.seed(seed)
    
    model_out <- caret::train(
      form = formula_i,
      data = data,
      method = "ranger",
      trControl = trainControl(
        index = folds,
        summaryFunction = sumfun,
        classProbs = classprob,
        allowParallel = FALSE
      ),
      num.threads = 1,
      # num.threads = cores,
      importance = "none",
      tuneGrid = expand.grid(
        mtry = round(min(mtry, length(cov_filtered))),  
        min.node.size = round(sqrt_min.node.size^2),
        splitrule = splitrules[extratrees]
      ),
      num.trees = numtrees,
      metric = metric,
      na.action = na.omit
    )
    if (max_metric) {
      out_score <- model_out$results %>%
        dplyr::select(any_of(metric)) %>%
        max()
    } else {
      out_score <- model_out$results %>%
        dplyr::select(any_of(metric)) %>%
        min() %>%
        "*"(-1)
    }
    return(
      list(
        Score = out_score
      )
    )
  }
  # Bayesian optimization
  showConnections()
  cl <- parallel::makePSOCKcluster(cores, outfile = "log.txt")
  doParallel::registerDoParallel(cl)
  clusterEvalQ(
    cl,
    {
      require(caret)
      require(ranger)
      require(magrittr)
      require(dplyr)
      require(tools)
      require(boot)
      source("select_vars.R")
    }
  )
  clusterExport(
    cl,
    c(
      "target",
      "folds",
      "sumfun",
      "metric",
      "data",
      "weights",
      "seed",
      "cor_cov",
      "cov_names",
      "splitrules"
    ),
    envir = environment()
  )
  set.seed(seed)
  scoreresults <- ParBayesianOptimization::bayesOpt(
    FUN = scoringFunction,
    bounds = bounds_bayes,
    # initPoints = cores*2,
    # iters.n = cores*10,
    iters.k = cores,
    acq = "ucb",
    gsPoints = cores*10,
    parallel = TRUE,
    initGrid = initgrid,
    verbose = 0,
    acqThresh = 0.95
  )
  stopCluster(cl)
  foreach::registerDoSEQ()
  rm(cl)
  showConnections()
  bestscores <- scoreresults$scoreSummary %>%
    filter(Score == max(Score, na.rm = TRUE))
  best_pars <- getBestPars(scoreresults)
  # Zero model to select covariates for the final model
  set.seed(seed)
  model_0 <- ranger::ranger(
    formula = formula_0,
    data = data,
    importance = "impurity",
    num.trees = numtrees,
    mtry = round(best_pars$mtry),  
    write.forest = FALSE,
    probability = FALSE,
    min.node.size = round(best_pars$sqrt_min.node.size^2),
    splitrule = splitrules[best_pars$extratrees],
    num.threads = cores,
    verbose = FALSE,
    node.stats = FALSE,
    seed = seed,
    na.action = "na.omit"
  )
  
  my_imp <- sort(model_0$variable.importance) %>% 
    rev() %>%
    divide_by(sum(.))
  
  cov_selection <- select_vars(
    importance = my_imp,
    correlation = cor_cov,
    maxcor = best_pars$maxcor,
    maximp = best_pars$maximp,
    minvar = 3
  )
  
  cov_filtered <- cov_selection$vars_selected
  
  # Final model
  # Make formula
  formula_final <- cov_filtered %>%
    paste0(collapse = " + ") %>%
    paste0(target, " ~ ", .) %>%
    as.formula()

  set.seed(seed)
  model_final <- caret::train(
    form = formula_final,
    data =  data,
    method = "ranger",
    trControl = trainControl(
      index = folds,
      summaryFunction = sumfun,
      classProbs = classprob,
      allowParallel = FALSE
    ),
    importance = "impurity",
    tuneGrid = expand.grid(
      mtry = round(min(best_pars$mtry, length(cov_filtered))),  
      min.node.size = round(best_pars$sqrt_min.node.size^2),
      splitrule = splitrules[best_pars$extratrees]
    ),
    num.threads = cores,
    num.trees = numtrees,
    metric = metric,
    na.action = na.omit
  )

  return(
    list(
      model = model_final,
      bayes_results = scoreresults,
      best_scores = bestscores
    )
  )
}

# END