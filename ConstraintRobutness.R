## Robutness by varying constraints (+- 0.05) and variability in performance or category assignments

# source('GeneralAndMethodFunctions.R')
# 
# library(plyr)
# library(dplyr)
# library(ggplot2)
# library(readr)
# library(tidyr)
# library(caret)
# library(e1071)
# library(pracma)
# library(CORElearn)


# ### Simulated data
# set.seed(2812)
# a1 = rnorm(300,1,0.25)
# a2 = rnorm(30,2,0.15)
# b1 = rnorm(300,1.5,0.25)
# 
# set.seed(12)
# a12 = rnorm(300,2.5,0.25)
# a22 = rnorm(30,1.5,0.15)
# b12 = rnorm(300,2.5,0.25)
# 
# a=c(a1,a2)
# a_2=c(a12,a22)
# 
# class_0 = data.frame("x1"=a, "x2"=a_2, "class"="0")
# class_1 = data.frame("x1"=b1, "x2"=b12, "class"="1")
# df = rbind(class_0, class_1)
# df[,-3] = scale(df[,-3])
# 
# df %>% ggplot(aes(x = x1, y = x2, colour = class)) + geom_point()
# 
# 
# set.seed(1)
# df$class <- as.factor(df$class) 
# svm_cv <- tune("svm", class ~ ., data = df,
#                kernel = 'linear', scale=FALSE,
#                ranges = list(cost = c(0.001, 0.01, 0.1, 1, 5, 10, 20, 50, 100,
#                                       150, 200)))
# summary(svm_cv)
# best_cost=svm_cv$best.model$cost
# 
# set.seed(1)
# df$class <- as.factor(df$class)
# df_no_target <- df[, setdiff(names(df), "class")]
# model_svm = svm(class ~ ., data=df, kernel="linear", scale=FALSE, probability=TRUE, type="C", cost=best_cost)
# pred_svm = predict(model_svm, df_no_target, probability=TRUE, decision.values = TRUE)
# #coef_3x2 = coef(model_svm_3x2) # se usa para los plots de la SVM
# 
# probs_model_train <- attr(pred_svm,"probabilities")
# probs_model_train <- probs_model_train[,c("0", "1")]
# probs_model_train_class1 <- probs_model_train[,2]
# 
# 
# ## Max GPS with restrictions
# restriction = TRUE
# user_condition <- function(p, p_, r, r_) {
#   (p[3] > 0.8) & (p_[1] > 0.9) & (r_[1]>0.4)
# }
# # 
# num_cuts = 2
# vector_GPS = c('p13', 'p_11','r31' ) 
# results_restric1 <- AfterTheModel(df, probs_model_train,restriction,user_condition,vector_GPS,num_cuts)
# results_restric1
# 
# 




# # Original thresholds
# original_thresholds <- c(0.8, 0.9, 0.4)
# n_thresholds <- length(original_thresholds)
# delta <- 0.05
# 
# # Función para crear una función `user_condition` con valores dados
# make_user_condition <- function(thr) {
#   function(p, p_, r, r_) {
#     (p[3] > thr[1]) & (p_[1] > thr[2]) & (r_[1] > thr[3])
#   }
# }
# 
# # Genera todas las combinaciones posibles cambiando 1, 2 o 3 umbrales
# generate_combinations <- function(thresholds, delta) {
#   n_thresholds <- length(thresholds)
#   base <- c(-delta, 0, delta)
#   combos <- expand.grid(rep(list(base), n_thresholds))
#   combos <- as.matrix(combos)
#   
#   # Aplica cambios
#   all_comb <- t(t(combos) + thresholds)
#   num_changes <- rowSums(t(t(all_comb)-thresholds != 0))
#   return(list('all_comb' = all_comb,'num_changes' = num_changes))
# }
# 
# # Crear combinaciones
# res_comb <- generate_combinations(original_thresholds, delta)
# threshold_set <- res_comb$all_comb
# num_changes <- res_comb$num_changes
# 
# # Ejecutar AfterTheModel con cada combinación
# results_list <- list()
# for (i in 1:nrow(threshold_set)) {
#   thr <- threshold_set[i, ]
#   user_condition_aux <- make_user_condition(thr)
#   
#   result <- AfterTheModel(df, probs_model_train, restriction = TRUE, 
#                           user_condition = user_condition_aux, 
#                           vector_GPS = vector_GPS, 
#                           num_cuts = num_cuts)
#   
#   results_list[[i]] <- list(thresholds = thr, result = result,num_changes = num_changes[i])
# }
# 
# 
# 
# original_case <- which(sapply(results_list, function(x) x$num_changes) == 0)
# original_GPS <- results_list[[original_case]]$result$GPS 
# original_CM <- results_list[[original_case]]$result$best_CM
# original_CM_prop <- prop.table(original_CM,margin = 2) # Proportion of observed classed in predicted classes
# 
# results_list <- results_list[-original_case] # remove original case from list
# 
# 
# results_comp <- lapply(results_list, function(x) {
#   dif_GPS <- abs(x$result$GPS - original_GPS)
#   CM_prop <- prop.table(x$result$best_CM, margin = 2)
#   dif_CM_prop <- abs(original_CM_prop - CM_prop)
#   
#   list(
#     dif_GPS = dif_GPS,
#     dif_CM_prop = dif_CM_prop,
#     num_changes = x$num_changes
#   )
# })
# 
# 
# grouped_results <- split(results_comp, sapply(results_comp, function(x) x$num_changes))
# grouped_results[1]
# mean_gps_by_group <- sapply(grouped_results, function(group) {
#   mean(sapply(group, function(x) x$dif_GPS))
# })
# 
# mean_CM_by_group <- sapply(grouped_results, function(group) { # average of all values in proportion
#   mean(sapply(group, function(x) x$dif_CM_prop))
# })
# 
# 
# summary_df <- data.frame(
#   num_changes = as.integer(names(grouped_results)),
#   mean_dif_GPS = mean_gps_by_group,
#   mean_CM_prop_diff = mean_CM_by_group
# )
# 
# # Add total summary
# group_sizes <- sapply(grouped_results, length)
# 
# # Weighted avg
# total_mean_dif_GPS <- sum(mean_gps_by_group * group_sizes) / sum(group_sizes)
# total_mean_CM_diff <- sum(mean_CM_by_group * group_sizes) / sum(group_sizes)
# 
# # Combining both df
# total_row <- data.frame(
#   num_changes = "TOTAL",
#   mean_dif_GPS = total_mean_dif_GPS,
#   mean_CM_prop_diff = total_mean_CM_diff
# )
# summary_df <- rbind(summary_df, total_row)
# 


generate_combinations <- function(thresholds, delta) {
  n_thresholds <- length(thresholds)
  base <- c(-delta, 0, delta)
  combos <- expand.grid(rep(list(base), n_thresholds))
  combos <- as.matrix(combos)

  # Aplica cambios
  all_comb <- t(t(combos) + thresholds)
  num_changes <- rowSums(t(t(all_comb)-thresholds != 0))
  return(list('all_comb' = all_comb,'num_changes' = num_changes))
}



ResultGroupAggregationAndOutput <- function(results_comp){
  grouped_results <- split(results_comp, sapply(results_comp, function(x) x$num_changes))
  mean_gps_by_group <- sapply(grouped_results, function(group) {
    mean(sapply(group, function(x) x$dif_GPS))
  })
  
  median_gps_by_group <- sapply(grouped_results, function(group) {
    median(sapply(group, function(x) x$dif_GPS))
  })
  
  sd_gps_by_group <- sapply(grouped_results, function(group) {
    sd(sapply(group, function(x) x$dif_GPS))
  })
  
  mean_CM_by_group <- sapply(grouped_results, function(group) { # average of all values in proportion
    mean(sapply(group, function(x) x$dif_CM_prop))
  })
  
  median_CM_by_group <- sapply(grouped_results, function(group) {
    median(sapply(group, function(x) x$dif_CM_prop))
  })
  
  sd_CM_by_group <- sapply(grouped_results, function(group) {
    sd(sapply(group, function(x) x$dif_CM_prop))
  })
  
  
  # Add total summary
  group_sizes <- sapply(grouped_results, length)
  
  summary_df <- data.frame(
    num_changes = as.integer(names(grouped_results)),
    mean_dif_GPS = mean_gps_by_group,
    median_dif_GPS = median_gps_by_group,
    sd_dif_GPS = sd_gps_by_group,
    mean_CM_prop_diff = mean_CM_by_group,
    median_CM_prop_diff = median_CM_by_group,
    sd_CM_prop_diff = sd_CM_by_group,
    changes_sizes = group_sizes
  )
  
  
  # Weighted avg
  total_mean_dif_GPS <- sum(mean_gps_by_group * group_sizes) / sum(group_sizes)
  total_mean_CM_diff <- sum(mean_CM_by_group * group_sizes) / sum(group_sizes)
  
  all_dif_GPS <- unlist(lapply(grouped_results, function(group) {sapply(group, function(x) x$dif_GPS)}))
  
  all_dif_CM_prop <- unlist(lapply(grouped_results, function(group) {sapply(group, function(x) x$dif_CM_prop)}))
  
  total_median_dif_GPS <- median(all_dif_GPS)
  total_sd_dif_GPS <- sd(all_dif_GPS)
  
  total_median_CM_diff <- median(all_dif_CM_prop)
  total_sd_CM_diff <- sd(all_dif_CM_prop)
  
  # Combining both df
  total_row <- data.frame(
    num_changes = "TOTAL",
    mean_dif_GPS = total_mean_dif_GPS,
    median_dif_GPS = total_median_dif_GPS,
    sd_dif_GPS = total_sd_dif_GPS,
    mean_CM_prop_diff = total_mean_CM_diff,
    median_CM_prop_diff = total_median_CM_diff,
    sd_CM_prop_diff = total_sd_CM_diff,
    changes_sizes = sum(group_sizes)
  )
  summary_df <- rbind(summary_df, total_row)
  return(summary_df = summary_df)
}



EvaluateConstraintRobustnessAfter <- function(thresholds, delta, make_user_condition,
                                              df,probs_model_train,vector_GPS,num_cuts){
  # Crear combinaciones
  res_comb <- generate_combinations(thresholds, delta)
  threshold_set <- res_comb$all_comb
  num_changes <- res_comb$num_changes
  
  # Ejecutar AfterTheModel con cada combinación
  results_list <- list()
  valid_id <- 1  # contador para índices válidos
  for (i in 1:nrow(threshold_set)) {
    thr <- threshold_set[i, ]
    user_condition_aux <- make_user_condition(thr)
    
    result <- AfterTheModel(df, probs_model_train, restriction = TRUE, 
                            user_condition = user_condition_aux, 
                            vector_GPS = vector_GPS, 
                            num_cuts = num_cuts)
    
    gps_check <- is.numeric(result$GPS) && length(result$GPS) == 1
    
    if (gps_check) {
      results_list[[valid_id]] <- list(thresholds = thr, result = result,num_changes = num_changes[i])
      valid_id <- valid_id + 1
    }
    
  }
  
  
  original_case <- which(sapply(results_list, function(x) x$num_changes) == 0)
  original_GPS <- results_list[[original_case]]$result$GPS 
  original_CM <- results_list[[original_case]]$result$best_CM
  original_CM_prop <- prop.table(original_CM) # Proportion of observed classed in predicted classes
  
  results_list <- results_list[-original_case] # remove original case from list
  
  
  results_comp <- lapply(results_list, function(x) {
    dif_GPS <- abs(x$result$GPS - original_GPS)
    CM_prop <- prop.table(x$result$best_CM) # margin?
    dif_CM_prop <- abs(original_CM_prop - CM_prop)
    
    list(
      dif_GPS = dif_GPS,
      dif_CM_prop = dif_CM_prop,
      num_changes = x$num_changes
    )
  })
  
  summary_df <- ResultGroupAggregationAndOutput(results_comp)

  
  return(summary_df)
}




# num_cuts = 2
# vector_GPS = c('p13', 'p_11','r31' ) 
# 
# # Original thresholds
# original_thresholds <- c(0.8, 0.9, 0.4)
# n_thresholds <- length(original_thresholds)
# delta <- 0.05
# 
# # Función para crear una función `user_condition` con valores dados
# make_user_condition <- function(thr) {
#   function(p, p_, r, r_) {
#     (p[3] > thr[1]) & (p_[1] > thr[2]) & (r_[1] > thr[3])
#   }
# }
# 
# 
# 
# summary_df_1 <- EvaluateConstraintRobustnessAfter(original_thresholds, delta, make_user_condition,
#                                                               df,probs_model_train,vector_GPS,num_cuts)





###### Version for train-test


EvaluateConstraintRobustnessAfterTrainTest <- function(thresholds, delta, make_user_condition,
                                              df,probs_model_train,vector_GPS,num_cuts,
                                              df_test,probs_model_test){
  probs_test_class1 <- probs_model_test[,'1'] 
  # Crear combinaciones
  res_comb <- generate_combinations(thresholds, delta)
  threshold_set <- res_comb$all_comb # constraint_set
  num_changes <- res_comb$num_changes
  
  # Ejecutar AfterTheModel con cada combinación
  results_list <- list()
  results_list_test <- list()
  valid_id <- 1  # contador para índices válidos
  for (i in 1:nrow(threshold_set)) {
    thr <- threshold_set[i, ]
    user_condition_aux <- make_user_condition(thr)
    
    result <- AfterTheModel(df, probs_model_train, restriction = TRUE, 
                            user_condition = user_condition_aux, 
                            vector_GPS = vector_GPS, 
                            num_cuts = num_cuts)
    
    gps_check <- is.numeric(result$GPS) && length(result$GPS) == 1
    
    if (gps_check) {
      results_list[[valid_id]] <- list(thresholds = thr, result = result,num_changes = num_changes[i])
      # Get results in test set
      prob_cat_thr <- result$thresholds
      prob_cat_thr_ord <- sort(prob_cat_thr, decreasing=TRUE)
      breaks <- c(Inf, prob_cat_thr_ord, -Inf)
      Y_pred_class_test <- cut(probs_test_class1, breaks = breaks, labels = 0:length(prob_cat_thr_ord), right = FALSE)
      
      CM_test <- table(Class_predicted = Y_pred_class_test,Class_observed = df_test$class)
      gps_test <- GPS_from_CM(CM_test,vector_GPS)
      
      results_list_test[[valid_id]] <- list(prob_cat_thr = prob_cat_thr,GPS = gps_test,
                                            CM = CM_test,num_changes = num_changes[i])
      valid_id <- valid_id + 1
    }
    
  }
  
  
  original_case <- which(sapply(results_list, function(x) x$num_changes) == 0)
  original_GPS <- results_list[[original_case]]$result$GPS 
  original_CM <- results_list[[original_case]]$result$best_CM
  original_CM_prop <- prop.table(original_CM) # Proportion of observed classed in predicted classes
  # Thresholds
  original_thresholds_for_test <- results_list[[original_case]]$result$thresholds
  original_GPS_test <- results_list_test[[original_case]]$GPS
  original_CM_test <- results_list_test[[original_case]]$CM
  original_CM_test_prop <- prop.table(original_CM_test) # margin?
  
  results_list <- results_list[-original_case] # remove original case from lists
  results_list_test <- results_list_test[-original_case]
  
  # Train comparison
  results_comp <- lapply(results_list, function(x) {
    dif_GPS <- abs(x$result$GPS - original_GPS)
    CM_prop <- prop.table(x$result$best_CM)
    dif_CM_prop <- abs(original_CM_prop - CM_prop)
    
    list(
      dif_GPS = dif_GPS,
      dif_CM_prop = dif_CM_prop,
      num_changes = x$num_changes
    )
  })
  
  summary_df <- ResultGroupAggregationAndOutput(results_comp)
  
  # Test comparison
  results_comp_test <- lapply(results_list_test, function(x) {
    dif_GPS <- abs(x$GPS - original_GPS_test)
    CM_prop <- prop.table(x$CM)
    dif_CM_prop <- abs(original_CM_test_prop - CM_prop)
    
    list(
      dif_GPS = dif_GPS,
      dif_CM_prop = dif_CM_prop,
      num_changes = x$num_changes
    )
  })
  
  summary_df_test <- ResultGroupAggregationAndOutput(results_comp_test)
  
  
  return(list(train_summary=summary_df, test_summary=summary_df_test))
}








### In the model

EvaluateConstraintRobustnessIn <- function(thresholds, delta, make_user_condition,
                                           df, df_no_target, vector_GPS,num_cuts,cv_folds,kernelSVM){
  
  # Crear combinaciones
  res_comb <- generate_combinations(thresholds, delta)
  threshold_set <- res_comb$all_comb
  num_changes <- res_comb$num_changes
  
  # Ejecutar inTheModel con cada combinación
  results_list <- list()
  valid_id <- 1  # contador para índices válidos
  for (i in 1:nrow(threshold_set)) {
    thr <- threshold_set[i, ]
    user_condition_aux <- make_user_condition_in(thr)
    
    result <- InTheModel(df, kernelSVM = kernelSVM, num_cuts = num_cuts, cv_folds = cv_folds,
                         vector_GPS = vector_GPS,restriction = TRUE, user_condition = user_condition_aux)
    
    gps_check <- is.numeric(result$GPS_test) && length(result$GPS_test) == 1
    
    if (gps_check){
      model_in <- result$best_model_trained
      probs_test_in <- predict(model_in,df_no_target, probability=TRUE, decision.values = TRUE)
      
      probs_model_train_in <- attr(probs_test_in,"probabilities")
      probs_model_train_in <- probs_model_train_in[,c("0", "1")]
      probs_model_train_class1_in <- probs_model_train_in[,2]
      
      
      ths_in = result$thresholds
      breaks <- c(Inf, ths_in, -Inf)
      Y_pred_class <- cut(probs_model_train_class1_in, breaks = breaks, labels = 0:length(ths_in), right = FALSE)
      unique_values <- unique(Y_pred_class) 
      Y_pred_class <- factor(Y_pred_class, levels = sort(unique_values))
      CM <- table(Class_predicted = Y_pred_class,Class_observed = df$class)
      gps <- GPS_from_CM(CM,vector_GPS)
      
      
      results_list[[valid_id]] <- list(thresholds = thr, CM = CM, GPS = gps,num_changes = num_changes[i])
      valid_id <- valid_id + 1
      
      
    }

  }
  
  
  original_case <- which(sapply(results_list, function(x) x$num_changes) == 0)
  original_GPS <- results_list[[original_case]]$GPS
  original_CM <- results_list[[original_case]]$CM
  original_CM_prop <- prop.table(original_CM,margin = 2) # Proportion of observed classed in predicted classes
  
  results_list <- results_list[-original_case] # remove original case from list
  
  
  results_comp <- lapply(results_list, function(x) {
    dif_GPS <- abs(x$GPS - original_GPS)
    CM_prop <- prop.table(x$CM, margin = 2)
    dif_CM_prop <- abs(original_CM_prop - CM_prop)
    
    list(
      dif_GPS = dif_GPS,
      dif_CM_prop = dif_CM_prop,
      num_changes = x$num_changes
    )
  })
  
  summary_df <- ResultGroupAggregationAndOutput(results_comp)
  
  return(summary_df)
}



# 
# kernelSVM = 'linear'
# num_cuts = 2
# vector_GPS = c('p13', 'p_11', 'r31')
# cv_folds = 3
# restriction = TRUE
# delta = 0.05
# thresholds = c(0.9,0.4,0.8)
# 
# make_user_condition_in <- function(thr) {
#   function(p, p_, r, r_) {
#     # Constraints list
#     conditions <- list(
#       list(value = p_[1], threshold = thr[1], op = ">"),  
#       list(value = r_[1], threshold = thr[2], op = ">"),   
#       list(value = p[3], threshold = thr[3], op = ">")    
#     )
#     
#     # Evaluation of constraints
#     results <- lapply(conditions, function(cond) {
#       if (cond$op == "<") return(cond$value < cond$threshold)
#       if (cond$op == ">") return(cond$value > cond$threshold)
#       if (cond$op == "<=") return(cond$value <= cond$threshold)
#       if (cond$op == ">=") return(cond$value >= cond$threshold)
#     })
#     
#     all_true = all(unlist(results))
#     if (all_true){
#       total_cond = TRUE
#     } else{
#       total_cond = FALSE
#     }
#     
#     return(list(conditions = conditions, results = results,total_cond=total_cond))
#   }
# }
# 
# 
# 
# 
# summary_df <- EvaluateConstraintRobustnessIn(thresholds, delta, make_user_condition_in,
#                                            df, df_no_target, vector_GPS,num_cuts,cv_folds,kernelSVM)
# 




### In the model TRAIN TEST

EvaluateConstraintRobustnessInTrainTest <- function(thresholds, delta, make_user_condition,
                                           df, df_no_target, vector_GPS,num_cuts,cv_folds,kernelSVM,
                                           df_test,df_test_no_target){
  
  # Crear combinaciones
  res_comb <- generate_combinations(thresholds, delta)
  threshold_set <- res_comb$all_comb
  num_changes <- res_comb$num_changes
  
  # Ejecutar inTheModel con cada combinación
  results_list <- list()
  results_list_test <- list()
  valid_id <- 1  # contador para índices válidos
  for (i in 1:nrow(threshold_set)) {
    thr <- threshold_set[i, ]
    user_condition_aux <- make_user_condition_in(thr)
    
    result <- InTheModel(df, kernelSVM = kernelSVM, num_cuts = num_cuts, cv_folds = cv_folds,
                         vector_GPS = vector_GPS,restriction = TRUE, user_condition = user_condition_aux)
    
    gps_check <- is.numeric(result$GPS_test) && length(result$GPS_test) == 1
    
    if (gps_check){

      model_in <- result$best_model_trained
      probs_train_in <- predict(model_in,df_no_target, probability=TRUE, decision.values = TRUE)
      probs_test_in <- predict(model_in,df_test_no_target, probability=TRUE, decision.values = TRUE)
      
      probs_model_train_in <- attr(probs_test_in,"probabilities")
      probs_model_train_in <- probs_model_train_in[,c("0", "1")]
      probs_model_train_class1_in <- probs_model_train_in[,2]
      
      probs_model_test_in <- attr(probs_test_in,"probabilities")
      probs_model_test_in <- probs_model_test_in[,c("0", "1")]
      probs_model_test_class1_in <- probs_model_test_in[,2]
      
      
      # Test
      ths_in = result$thresholds
      breaks <- c(Inf, ths_in, -Inf)
      Y_pred_class <- cut(probs_model_test_class1_in, breaks = breaks, labels = 0:length(ths_in), right = FALSE)
      unique_values <- unique(Y_pred_class) 
      Y_pred_class <- factor(Y_pred_class, levels = sort(unique_values))
      CM_test <- table(Class_predicted = Y_pred_class,Class_observed = dfTest$class)
      GPS_test <- GPS_from_CM(CM_test,vector_GPS)
      
      results_list_test[[valid_id]] <- list(thresholds = thr, CM = CM_test, GPS = GPS_test,num_changes = num_changes[i])
      
      # Fold
      CM_fold <- result$best_CM_test
      GPS_fold <- result$GPS_test
      results_list[[valid_id]] <- list(thresholds = thr, CM = CM_fold, GPS = GPS_fold,num_changes = num_changes[i])
      
      valid_id <- valid_id + 1
      
      
    }
    
  }
  
  
  original_case <- which(sapply(results_list, function(x) x$num_changes) == 0)
  original_GPS <- results_list[[original_case]]$GPS
  original_CM <- results_list[[original_case]]$CM
  original_CM_prop <- prop.table(original_CM) # Proportion of observed classed in predicted classes
  original_GPS_test <- results_list_test[[original_case]]$GPS
  original_CM_test <- results_list_test[[original_case]]$CM
  original_CM_test_prop <- prop.table(original_CM_test)
  results_list <- results_list[-original_case] # remove original case from lists
  results_list_test <- results_list_test[-original_case] 
  
  
  results_comp <- lapply(results_list, function(x) {
    dif_GPS <- abs(x$GPS - original_GPS)
    CM_prop <- prop.table(x$CM)
    dif_CM_prop <- abs(original_CM_prop - CM_prop)
    
    list(
      dif_GPS = dif_GPS,
      dif_CM_prop = dif_CM_prop,
      num_changes = x$num_changes
    )
  })
  
  summary_df <- ResultGroupAggregationAndOutput(results_comp)
  
  results_comp_test <- lapply(results_list_test, function(x) {
    dif_GPS <- abs(x$GPS - original_GPS_test)
    CM_prop <- prop.table(x$CM)
    dif_CM_prop <- abs(original_CM_test_prop - CM_prop)
    
    list(
      dif_GPS = dif_GPS,
      dif_CM_prop = dif_CM_prop,
      num_changes = x$num_changes
    )
  })
  
  summary_df_test <- ResultGroupAggregationAndOutput(results_comp_test)
  
  return(list(train_summary=summary_df, test_summary=summary_df_test))
}





