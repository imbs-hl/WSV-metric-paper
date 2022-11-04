calculate_decision_tree <- function(data, instance, ...){
  
  ## Exctract data from instance
  test_dat       <- instance[[1]]
  rf             <- instance[[2]]
  params         <- instance[[3]]
  val_dat        <- instance[[4]]
  effect_var_ids <- instance[[5]]
  noise_var_ids  <- instance[[6]]
  train_dat      <- instance[[7]]
  
  
  ## Train single decision tree
  rf_dec <- rpart(y ~ ., data = train_dat, minsplit = params$min_node_size)
  
  covered_effect_vars <- sum(effect_var_ids %in% rf_dec$frame$var)/length(effect_var_ids)
  covered_noise_vars  <- sum(noise_var_ids %in% rf_dec$frame$var)/length(noise_var_ids)
  effect_var_fdr      <- 1 - sum(rf_dec$frame$var %in% effect_var_ids)/(length(rf_dec$frame$var) - sum(rf_dec$frame$var == "<leaf>"))
  
  ## Prediction accuracy on validation data set
  mse_val_dat_rf <- 1/nrow(val_dat) * sum((val_dat$y - predict(rf, data = val_dat[,-1])$predictions)^2)
  mse_val_dat_rep_tree <- 1/nrow(val_dat) * sum((val_dat$y - predict(rf_dec, data = val_dat[,-1]))^2)
  
  ## Prediction accuracy on forest prediction
  mse_rf_pred <- 1/nrow(val_dat) * sum((predict(rf, data = val_dat[,-1])$predictions - predict(rf_dec, data = val_dat[,-1]))^2)
  
  return(data.frame(metric               = "decision tree",
                    method               = "decision tree",
                    mean_dist            = NA, 
                    sd_dist              = NA, 
                    min_dist             = NA, 
                    max_dist             = NA,
                    setting              = params$setting,
                    min_node_size        = params$min_node_size,
                    covered_effect_vars  = covered_effect_vars,
                    covered_noise_vars   = covered_noise_vars,
                    effect_var_fdr       = effect_var_fdr,
                    time                 = NA,
                    mse_val_dat_rf       = mse_val_dat_rf,
                    mse_val_dat_tree     = mse_val_dat_rep_tree,
                    mse_rf_pred          = mse_rf_pred
  )
  )
}
