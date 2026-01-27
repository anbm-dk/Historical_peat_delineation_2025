# Function to select covariates

select_vars <- function(
    importance,
    correlation,
    maxcor = 0.5,
    maximp = 0.5,
    minvar = 3
) {
  importance <- sort(importance) %>% 
    rev() %>%
    divide_by(sum(.))
  
  covnames <- names(importance)
  
  diag(correlation) <- NA
  
  vars_selected <- covnames[1]
  
  maxcors_loop <- numeric()
  sumimps_loop <- numeric()
  vars_dropped <- character()
  included_loop <- logical()
  
  maxcors_loop[1] <- 0
  sumimps_loop[1] <- importance[1]
  included_loop[1] <- TRUE
  
  i <- 1
  
  stopnow <- FALSE
  
  while(!stopnow) {
    i %<>% add(1)
    
    var_i <- covnames[i]
    
    ind_i <- c(which(vars_selected %in% covnames), i)
    
    maxcors_loop[i] <- max(correlation[ind_i, ind_i], na.rm = TRUE)
    sumimps_loop[i] <- sum(importance[ind_i], na.rm = TRUE)
    
    if (maxcors_loop[i] < maxcor) {
      vars_selected <- c(vars_selected, var_i)
      included_loop[i] <- TRUE
    } else {
      included_loop[i] <- FALSE
    }
    
    if (length(vars_selected) >= minvar) {
      if (sumimps_loop[i] > maximp) {
        stopnow <- TRUE
      }
    }
    
    if (i == length(covnames)) {
      stopnow <- TRUE
    }
  }
  
  loop_results <- data.frame(
    covariate = covnames[1:i],
    maxcor = maxcors_loop,
    sumimp = sumimps_loop,
    included = included_loop
  )
  
  if (length(vars_selected) < minvar) {
    extra_vars <- loop_results %>%
      filter(!included) %>%
      mutate(
        rank1 = rank(maxcor),
        rank2 = row_number(),
        rank3 = (rank1 + rank2)/2
      ) %>%
      arrange(rank3) %>%
      pull(covariate) %>%
      magrittr::extract(
        c(1:(minvar - length(vars_selected)))
      )
    
    vars_selected <- c(vars_selected, extra_vars)
  }
  
  vars_dropped <- setdiff(covnames, vars_selected)
  
  out <- list()
  out$results <- loop_results
  out$vars_selected <- vars_selected
  out$vars_dropped <- vars_dropped
  out$maxcor <- maxcor
  out$maximp <- maximp
  out$minvar <- minvar
  
  return(out)
}

# END


# END