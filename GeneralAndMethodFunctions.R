library(plyr) 
library(dplyr)
library(ggplot2)
library(caret)
library(e1071) 



GPS <- function(p) {
  if (any(p == 0)) {
    gps <-0  
  } else if (any(is.na(p))){
    gps <- NA
  } else {
    n <- length(p)
    num <- n * prod(p)
    
    denom <- sum(sapply(1:n, function(j) prod(p[-j])))
    gps <- num / denom
  }
  return(gps)
}


# GPS_df <- function(df) {
#   apply(df, 1, function(p) GPS(as.numeric(p)))  
# }


GPS_df_filas <- function(df) {
  GPS <- function(p) {
    p <- as.numeric(p)  # Ensure numeric values
    
    if (any(is.na(p))) {
      return(NA)  # If there is NA, return NA
    }
    
    if (any(p == 0)) {
      return(0)  # If there is 0, return 0
    }
    
    n <- length(p)
    num <- n * prod(p)
    denom <- sum(sapply(1:n, function(j) prod(p[-j])))
    
    return(num / denom)
  }
  
  # Apply GPS to each row of data.frame
  apply(df, 1, GPS)
}


GPS_from_CM <-function(CM,vector_GPS){
  k = dim(CM)[1]

  p = numeric(k)
  p_ = numeric(k)
  r = numeric(k)
  r_ = numeric(k)

  # Probabilities of each category
  for (h in 1:k) {
    p[h] = CM[h, 2] / sum(CM[h, ])
    p_[h] = CM[h, 1] / sum(CM[h, ])
    r[h] = CM[h, 2] / sum(CM[, 2])
    r_[h] = CM[h, 1] / sum(CM[, 1])
  }

  metrics <- c(p, p_, r, r_)
  names(metrics) <- c(
    paste0("p1", 1:k),  # p11, p12, ..., p1k
    paste0("p_1", 1:k), # p_11, p_12, ..., p_1k
    paste0("r", 1:k, "1"),  # r11, r12, ..., rk1
    paste0("r", 1:k, "_1")   # r_11, r_12, ..., r_k1
  )

  p_vector <- metrics[vector_GPS]
  gps = GPS(p_vector)
  return(gps)
}


preds_k_cuts <-function(probs_model,k,min_th=0.05,max_th=0.95,step_th=0.05){
  
  # All combinations of thresholds
  combo_ths = combn(seq(min_th,max_th,step_th),k)
  
  # Preds categorized according to all combinations of thresholds
  Y_pred_class <- matrix(nrow = ncol(combo_ths), ncol = nrow(probs_model))
  
  
  # Loop for all combination of thresholds
  for (j in 1:ncol(combo_ths)) {
    
    thresholds <- combo_ths[,j]
    breaks <- c(Inf, thresholds, -Inf)
    Y_pred_class[j,] <- cut(probs_model[,'1'], breaks = breaks, labels = 0:length(thresholds), right = FALSE)
    
  }
  
  results <- list('Y_pred_cat'=Y_pred_class,'combo_ths'=combo_ths)
  
  return(results)
}


barplot_CM <- function(CM,list_names_obs,list_names_pred){
  
  library(plyr)
  
  colnames(CM) <- list_names_obs
  rownames(CM) <- list_names_pred
  
  df_ggplot = as.data.frame(as.table(CM))
  df_ggplot = ddply(df_ggplot, "Class_predicted", transform, Porcentaje=Freq/sum(Freq))
  # Percentage to plot 
  df_ggplot$To_plot <- paste0(round(df_ggplot$Porcentaje*100,2),'%')
  df_ggplot$To_plot[df_ggplot$Class_observed == list_names_obs[1]] <- ''
  
  detach("package:plyr")
  max_y = df_ggplot %>%
    group_by(Class_predicted) %>%
    summarise(Total_Freq = sum(Freq))
  y_max = max(max_y$Total_Freq)
  
  # Barplot
  graph_bar <- df_ggplot %>% ggplot( aes(x = Class_predicted, y = Freq, fill = Class_observed)) +
    geom_bar(stat="identity", position = position_stack(reverse = TRUE))  +
    geom_text(aes(y=Freq,label = To_plot),
              position = position_stack(reverse = TRUE), vjust = -0.5, hjust = 0.4, size = 6) +
    scale_fill_manual(values = c("lightblue","orange")) +
    theme_minimal() +
    theme(axis.text  = element_text(size = 14),
          axis.title = element_text(size = 16),
          legend.position = "none") +
    labs(x = "Predicted class", y = "Frequency")+
    ylim(0, y_max*1.1)
  
  return(graph_bar)
  
}



Case_FixedThreshold <- function(df, probs_model_train_class1, ths, GPS_p_values){
  
  k = length(ths) + 1  # Number of classes desired by the user
  
  # Categorization of class according to thresholds ths
  thresholds <- sort(ths, decreasing=TRUE)
  
  breaks <- c(Inf, thresholds, -Inf)
  
  # Category assignment
  Y_pred_class <- cut(probs_model_train_class1, breaks = breaks, labels = 0:length(thresholds), right = FALSE)
  
  # Confusion matrix
  CM <- table(Class_predicted = Y_pred_class,Class_observed = df$class)
  
  p = numeric(k)
  p_ = numeric(k)
  r = numeric(k)
  r_ = numeric(k)
  
  # Probabilities of each category
  for (j in 1:k) {
    p[j] = CM[j, 2] / sum(CM[j, ])  
    p_[j] = CM[j, 1] / sum(CM[j, ]) 
    r[j] = CM[j, 2] / sum(CM[, 2])  
    r_[j] = CM[j, 1] / sum(CM[, 1]) 
  }
  
  metrics_probs=c(p,p_,r,r_)
  names(metrics_probs) <- c(
    paste0("p1", 1:k),  # p11, p12, ..., p1k
    paste0("p_1", 1:k), # p_11, p_12, ..., p_1k
    paste0("r", 1:k, "1"),  # r11, r12, ..., rk1
    paste0("r", 1:k, "_1")   # r_11, r_12, ..., r_k1
  )
  
  
  p_vector = metrics_probs[GPS_p_values]
  gps = GPS(p_vector)
  
  results <- list('GPS' = gps,'CM'= CM)
  
  return(results)
}




###### AFTER THE MODEL:  MAX GPS WITHOUT and WITH RESTRICTIONS ######

AfterTheModel <- function(df, probs_model_train,restriction,user_condition,vector_GPS,
                                     num_cuts,min_th=0.05,max_th=0.95,step_th=0.05){
  
  k = num_cuts + 1  # Number of classes desired by the user
  elementsCM = k*2 
  number_p_r = elementsCM*2
  
  cat_combo = preds_k_cuts(probs_model_train,num_cuts)
  Y_pred_class = cat_combo$Y_pred_cat
  
  matrix_CMs = matrix(nrow=nrow(Y_pred_class), ncol=elementsCM)
  metrics_probs = matrix(nrow=nrow(Y_pred_class), ncol=number_p_r)
  for (i in 1:nrow(Y_pred_class)) {
    CM=table(Y_pred_class[i,],df$class)
    if (dim(CM)[1] == k) {# if k classes are produced
      matrix_CMs[i,]=c(CM) 
      
      p = numeric(k)
      p_ = numeric(k)
      r = numeric(k)
      r_ = numeric(k)
      
      for (j in 1:k) {
        p[j] = CM[j, 2] / sum(CM[j, ])  
        p_[j] = CM[j, 1] / sum(CM[j, ])
        r[j] = CM[j, 2] / sum(CM[, 2])  
        r_[j] = CM[j, 1] / sum(CM[, 1]) 
      }
      
      if (restriction){
        # Constraint evaluation
        if (user_condition(p, p_, r, r_)) { 
          metrics_probs[i, ] <- c(p, p_, r, r_)
        }
      } else{ # no constraint case
        metrics_probs[i, ] <- c(p, p_, r, r_)
      }
      
      
    }
  }
  
  metrics_probs=data.frame(metrics_probs)
  colnames(metrics_probs) <- c(
    paste0("p1", 1:k),  # p11, p12, ..., p1k
    paste0("p_1", 1:k), # p_11, p_12, ..., p_1k
    paste0("r", 1:k, "1"),  # r11, r12, ..., rk1
    paste0("r", 1:k, "_1")   # r_11, r_12, ..., r_k1
  )
  p_vector <- metrics_probs[vector_GPS]
  metrics_probs$GPS = GPS_df_filas(p_vector)
  positions = which(metrics_probs$GPS == max(metrics_probs$GPS, na.rm = TRUE))
  if (length(positions)==1){
    pos = which.max(metrics_probs$GPS)
  }else{# ties
    GPS_options = metrics_probs[positions,]
    GPS_options_filtered <- GPS_options %>%
      select(where(~ n_distinct(.) > 1))
    pos = as.numeric(names(which.max(apply(GPS_options_filtered,1,min))))
  }
  GPS_value = metrics_probs$GPS[pos]
  thresholds = cat_combo$combo_ths[,pos]
  best_CM = matrix_CMs[pos,]
  
  list_predicted = seq(0,k-1,1)
  bestCM_format <- matrix(best_CM, nrow = k, ncol = dim(CM)[2], 
                          dimnames = list(Class_predicted = list_predicted, 
                                          Class_observed = c("0", "1")))
  
  
  results <- list('GPS' = GPS_value, 'thresholds'=thresholds, 'best_CM' = bestCM_format)
  
  return(results)
}






###### IN THE MODEL ######

get_p_metrics_from_CM <- function(CM){

  # k is number of desired classes
  k = dim(CM)[1]
  p = numeric(k)
  p_ = numeric(k)
  r = numeric(k)
  r_ = numeric(k)
  
  for (j in 1:k) {
    p[j] = CM[j, 2] / sum(CM[j, ]) 
    p_[j] = CM[j, 1] / sum(CM[j, ]) 
    r[j] = CM[j, 2] / sum(CM[, 2])  
    r_[j] = CM[j, 1] / sum(CM[, 1])
  }
  metrics_probs <- c(p, p_, r, r_)
  
  names(metrics_probs) <- c(
    paste0("p1", 1:k),  # p11, p12, ..., p1k
    paste0("p_1", 1:k), # p_11, p_12, ..., p_1k
    paste0("r", 1:k, "1"),  # r11, r12, ..., rk1
    paste0("r", 1:k, "_1")   # r_11, r_12, ..., r_k1
  )
  results <-list('metrics_probs_v'=metrics_probs,
                 'p'=p,'p_'=p_,'r'= r,'r_'= r_)
  return(results)
}



get_best_threshold <- function(df) {
  
  errors <- sapply(df, function(fold) fold$error_test)
  gps_test <- sapply(df, function(fold) fold$GPS_test)
  
  if (any(!is.na(errors))) {
    best_fold_idx <- which.min(errors[!is.na(errors)])
  } else {
    best_fold_idx <- which.max(gps_test)
  }
  
  best_threshold <- df[[best_fold_idx]]$ths
  
  best_values <-list('best_fold'=best_fold_idx,'best_threshold'=best_threshold)
  
  return(best_values)
}




error_function <- function(p_new, p_new_, r_new, r_new_,user_condition) {
  cond_info <- user_condition(p_new, p_new_, r_new, r_new_)
  conditions <- cond_info$conditions
  results <- unlist(cond_info$results)
  
  if (all(results)) {
    error_terms=0  
  } else {
    error_terms <- sapply(seq_along(conditions), function(i) {
      if (!results[i]) {  
        cond <- conditions[[i]]
        if (cond$op %in% c("<", "<=")) return(max(0, cond$value - cond$threshold)^2)
        if (cond$op %in% c(">", ">=")) return(max(0, cond$threshold - cond$value)^2)
      }
      return(0)  
    })
    
    return(sum(error_terms))
  }
}



InTheModel <- function(df,kernelSVM,num_cuts,cv_folds,vector_GPS,restriction,user_condition,
                       min_th=0.05,max_th=0.95, step_th=0.05,
                       list_parameters=c(0.001, 0.01, 0.1, 1, 5, 10, 20, 50, 100,150, 200)){
  
  k = num_cuts + 1  # Number of classes desired by the user
  elementsCM = k*2 
  number_p_r = k*4 # dimensions to save results
  
  set.seed(56)
  folds <- createFolds(df$class, k = cv_folds)
  
  # To save results
  tracking <- data.frame('Params_model'=list_parameters)
  tracking$GPS_train <- NA
  tracking$GPS_test <- NA
  tracking$error_test <- NA
  results_folds <- list()
  list_results_folds <-list()
  
  for (i in 1:length(list_parameters)){
    C_i <- list_parameters[i]
    #print(paste('C_i=',C_i))
    
    for (h in 1:length(folds)){
      f = folds[[h]]
      training_fold <- df[-f, ]
      test_fold <- df[f, ]
      # Model training
      test_no_target <- test_fold[, setdiff(names(test_fold), "class")]
      train_no_target <- training_fold[, setdiff(names(training_fold), "class")]
      set.seed(579)
      model_svm = svm(class ~ ., data=training_fold, kernel=kernelSVM, scale=FALSE, cost=C_i,probability=TRUE)
      # Prediction on train
      pred_svm_train = predict(model_svm, train_no_target, probability=TRUE)
      probs_model_fold_train <- attr(pred_svm_train,"probabilities")
      probs_model_fold_train <- probs_model_fold_train[,c("0", "1")]
      probs_model_fold_train_1 <- probs_model_fold_train[,"1"]
      # Prediction on test
      pred_svm_alg = predict(model_svm, test_no_target, probability=TRUE)
      probs_model_fold_test <- attr(pred_svm_alg,"probabilities")
      probs_model_fold_test <- probs_model_fold_test[,c("0", "1")]
      probs_model_fold_test_1 <- probs_model_fold_test[,"1"]
      
      cat_combo = preds_k_cuts(probs_model_fold_train,num_cuts,min_th,max_th,step_th)
      Y_pred_class = cat_combo$Y_pred_cat
      
      # Matrix to save p, r metrics according to each CM
      metrics_probs = matrix(nrow=nrow(Y_pred_class), ncol=number_p_r)
      
      for (j in 1:nrow(Y_pred_class)) { 
        CM = table(Y_pred_class[j,],training_fold$class)
        if (dim(CM)[1] == k) {# if k classes are produced

          pr_metrics_from_CM = get_p_metrics_from_CM(CM)
          p = pr_metrics_from_CM$p
          p_ = pr_metrics_from_CM$p_
          r = pr_metrics_from_CM$r
          r_ = pr_metrics_from_CM$r_
          
          if (restriction){
            if (user_condition(p, p_, r, r_)$total_cond) { 
              metrics_probs[j, ] <- c(p, p_, r, r_)
            }
          } else{ 
            metrics_probs[j, ] <- c(p, p_, r, r_)
          }
          
          
        }# end if k classes are produced
      }# en for
      
      metrics_probs=data.frame(metrics_probs)
      colnames(metrics_probs) <- c(
        paste0("p1", 1:k),  # p11, p12, ..., p1k
        paste0("p_1", 1:k), # p_11, p_12, ..., p_1k
        paste0("r", 1:k, "1"),  # r11, r12, ..., rk1
        paste0("r", 1:k, "_1")   # r_11, r_12, ..., r_k1
      )
      if (all(is.na(metrics_probs))){
        GPS_value = NA
        thresholds = NA
        GPS_test <- NA
        error_test <- NA
        CM_test <- NA # NUEVO
      } else{
        p_vector <- metrics_probs[vector_GPS]
        metrics_probs$GPS = GPS_df_filas(p_vector)
        pos = which.max(metrics_probs$GPS)
        GPS_value = metrics_probs$GPS[pos]
        thresholds = cat_combo$combo_ths[,pos]
        
        breaks <- c(Inf, thresholds, -Inf)
        Y_pred_class_test <- cut(probs_model_fold_test_1, breaks = breaks, labels = 0:length(thresholds), right = FALSE)
        unique_values <- unique(Y_pred_class_test) 
        Y_pred_class_test <- factor(Y_pred_class_test, levels = sort(unique_values))
        CM_test <- table(Class_predicted = Y_pred_class_test,Class_observed = test_fold$class)
        if (dim(CM_test)[1] == k){
          pr_metrics_test <- get_p_metrics_from_CM(CM_test)
          p_v <- as.vector(pr_metrics_test$metrics_probs_v[vector_GPS])
          GPS_test <- GPS(p_v)
          #print(GPS_test)
          pTest = pr_metrics_test$p
          p_Test = pr_metrics_test$p_
          rTest = pr_metrics_test$r
          r_Test = pr_metrics_test$r_
          if (restriction){
            error_test <- error_function(pTest, p_Test, rTest, r_Test,user_condition)
          } else{
            error_test <- NA
          }
        }else{
          GPS_test <- NA
          error_test <- NA
        }
      }
      
      name_fold = paste0('Fold_',h)
      results_folds[[name_fold]] <-list('training_fold'=training_fold,'test_fold'=test_fold,
                                        'prob_train'=probs_model_fold_train,
                                        'prob_test' = probs_model_fold_test,
                                        'GPS_train'=GPS_value,
                                        'ths' = thresholds,
                                        'CM_test'=CM_test,
                                        'GPS_test'=GPS_test,
                                        'error_test'=error_test,
                                        'model_trained'=model_svm)
    }
    
    # Save all results from folds
    name_c_i <- paste0('Param_',C_i)
    list_results_folds[[name_c_i]] = results_folds
    
    
    all_GPS_test <- as.data.frame(lapply(results_folds, function(x) x$GPS_test))
    all_GPS_train <- as.data.frame(lapply(results_folds, function(x) x$GPS_train))
    all_error_test <- as.data.frame(lapply(results_folds, function(x) x$error_test))
    
    tracking[i,]$GPS_train = mean(as.numeric(all_GPS_train))
    tracking[i,]$GPS_test = mean(as.numeric(all_GPS_test))
    tracking[i,]$error_test = mean(as.numeric(all_error_test))
    
  }
  tracking$error_test[is.na(tracking$error_test)] <- 0
  
  tracking$difference <- tracking$GPS_test - tracking$error_test
  best_param_model <- tracking[which.max(tracking$difference), 'Params_model']
  param_name <- paste0("Param_", best_param_model)
  results_best_param <- list_results_folds[[param_name]]
  
  best_values <- get_best_threshold(results_best_param)
  best_threshold <- best_values$best_threshold
  best_fold <- best_values$best_fold
  best_GPS_train = results_best_param[[best_fold]]$GPS_train
  best_GPS_test = results_best_param[[best_fold]]$GPS_test
  best_error_test = results_best_param[[best_fold]]$error_test
  best_CM_test = results_best_param[[best_fold]]$CM_test
  best_model_trained <- results_best_param[[best_fold]]$model_trained
  
  results_to_return <- list('thresholds'=best_threshold,'GPS_train'=best_GPS_train,'GPS_test'=best_GPS_test,'error'=best_error_test,
                            'best_CM_test'=best_CM_test,
                            'best_param_model'=best_param_model,'best_model_trained'=best_model_trained)
  return(results_to_return)
  
}




### Plot SVM hyperplanes functions

calculate_prob <- function(Z, probA, probB) {
  result = 1/(1+ exp(Z*probA + probB))
}



to_optim <- function(th, probA, probB) {
  fun <- function(Z) {
    result = calculate_prob(Z, probA, probB) - th
  }
  return(fun)
}





