###########################################################
#############  VARIANCE ACROSS RANDOM SPLITS  #############
###########################################################

## 10 times execution of results for real datasets to evaluate the effect of random train-test splits in results

variance_summary <- function(list_E){
  
  E_comp <- lapply(list_E, function(x) {
    class_pos_prop_train <- prop.table(x$CM_train, margin = 1)[,2]
    class_pos_prop_test <- prop.table(x$CM_test, margin = 1)[,2]
    
    list(
      class_pos_prop_train = class_pos_prop_train,
      class_pos_prop_test = class_pos_prop_test
    )
  })
  
  E_to_df <- t(as.data.frame(E_comp))
  E_train <- E_to_df[grep("train", row.names(E_to_df)), ]
  E_test <- E_to_df[grep("test", row.names(E_to_df)), ]
  
  E_long_train <- gather(as.data.frame(E_train), factor_key=TRUE)
  E_long_test <- gather(as.data.frame(E_test), factor_key=TRUE)
  
  E_train_summary <- E_long_train%>% dplyr::group_by(key)%>%
    dplyr::summarise(mean= mean(value), sd= sd(value), median = median(value))
  
  E_test_summary <- E_long_test%>% dplyr::group_by(key)%>%
    dplyr::summarise(mean= mean(value), sd= sd(value), median = median(value))
  
  dof = length(list_E)
  E_train_summary$CI_inf <- E_train_summary$mean - qt(0.975,dof-1)*E_train_summary$sd
  E_train_summary$CI_sup <- E_train_summary$mean + qt(0.975,dof-1)*E_train_summary$sd
  E_test_summary$CI_inf <- E_test_summary$mean - qt(0.975,dof-1)*E_test_summary$sd
  E_test_summary$CI_sup <- E_test_summary$mean + qt(0.975,dof-1)*E_test_summary$sd
  
  CM_train_mean <- Reduce(`+`, lapply(list_E, `[[`, "CM_train")) / length(list_E)
  CM_test_mean  <- Reduce(`+`, lapply(list_E, `[[`, "CM_test"))  / length(list_E)
  
  CM_mean_train <- round(CM_train_mean)
  CM_mean_test <- round(CM_test_mean)
  
  return(list(E_train_summary=E_train_summary, E_test_summary = E_test_summary,
              CM_mean_train = CM_mean_train, CM_mean_test = CM_mean_test))
  
}




VarianceSplitsMammo <- function(df, seed_vector){
  
  n_seeds <- length(seed_vector)
  list_E1 <- list()
  list_E2 <- list()
  list_E3 <- list()
  list_E4 <- list()
  list_E5 <- list()
  list_E6 <- list()
  
  for (i in 1:n_seeds){
    set.seed(seed_vector[i])
    trainIndex <- createDataPartition(df$class,
                                      p = 0.7, # training contains 70% of data
                                      list = FALSE)
    dfTrain <- df[ trainIndex,]
    dfTest  <- df[-trainIndex,]
    
    ### Best parameters SVM
    set.seed(1)
    svm_cv <- tune("svm", class ~ ., data = dfTrain,
                   kernel = 'radial', scale=FALSE,
                   ranges = list(cost = c(0.001, 0.01, 0.1, 1, 5, 10, 20, 50, 100,
                                          150, 200)))
    summary(svm_cv)
    best_cost=svm_cv$best.model$cost
    
    #set.seed(1)
    df_test_no_target <- dfTest[, setdiff(names(df), "class")]
    df_train_no_target <- dfTrain[, setdiff(names(df), "class")]
    set.seed(1)
    model_svm_mammo = svm(class ~ ., data=dfTrain, kernel="radial", scale=FALSE, probability=TRUE, type="C", cost=best_cost)
    pred_svm_train = predict(model_svm_mammo, df_train_no_target, probability=TRUE, decision.values = TRUE)
    probs_model_train <- attr(pred_svm_train,"probabilities")
    probs_model_train <- probs_model_train[,c("0", "1")]
    probs_model_train_class1 <- probs_model_train[,'1']
    
    # test
    pred_svm_test = predict(model_svm_mammo, df_test_no_target, probability=TRUE, decision.values = TRUE)
    probs_model_test <- attr(pred_svm_test,"probabilities")
    probs_model_test <- probs_model_test[,c("0", "1")]
    probs_model_test_class1 <- probs_model_test[,'1']
    
    ## E1: Classic cut: 1/2
    ths = c(1/2)
    GPS_p_values = c('p_11','p12','r1_1','r21')
    results_binary_train <- Case_FixedThreshold(dfTrain, probs_model_train_class1, ths, GPS_p_values)
    results_binary_test <- Case_FixedThreshold(dfTest, probs_model_test_class1, ths, GPS_p_values)
    list_E1[[i]] <- list(GPS_train = results_binary_train$GPS, CM_train = results_binary_train$CM,
                         GPS_test = results_binary_test$GPS, CM_test = results_binary_test$CM)
    
    ## E2: 3 cuts: 1/4-2/4-3/4
    ths = c(1/4,2/4,3/4)
    GPS_p_values = c('p_11','p14','r1_1','r41')
    results_four_train <- Case_FixedThreshold(dfTrain, probs_model_train_class1, ths, GPS_p_values)
    results_four_test <- Case_FixedThreshold(dfTest, probs_model_test_class1, ths, GPS_p_values)
    
    list_E2[[i]] <- list(GPS_train = results_four_train$GPS, CM_train = results_four_train$CM,
                         GPS_test = results_four_test$GPS, CM_test = results_four_test$CM)
    
    ## E3: Max GPS(p14, p_11, r1_1) without constraints
    
    restriction = FALSE
    num_cuts = 3
    vector_GPS = c( 'p_11','p14', 'r1_1')
    user_condition <- NULL
    results_3cuts_MaxGPS_train <- AfterTheModel(dfTrain, probs_model_train,restriction,user_condition,vector_GPS,num_cuts)
    ths_for_test <- results_3cuts_MaxGPS_train$thresholds
    
    probs_test_class1 <- probs_model_test[,'1'] 
    thresholds <- sort(ths_for_test, decreasing=TRUE)
    breaks <- c(Inf, thresholds, -Inf)
    Y_pred_class_test <- cut(probs_test_class1, breaks = breaks, labels = 0:length(thresholds), right = FALSE)
    CM_test <- table(Class_predicted = Y_pred_class_test,Class_observed = dfTest$class)
    if (dim(CM_test[rowSums(CM_test[])>0,])[1] > num_cuts){
      gps_test <- GPS_from_CM(CM_test,vector_GPS)
    } else {
      gps_test <- NA
    }
    
    list_E3[[i]] <- list(GPS_train = results_3cuts_MaxGPS_train$GPS, CM_train = results_3cuts_MaxGPS_train$best_CM,
                         ths = ths_for_test,
                         GPS_test = gps_test, CM_test = CM_test)
    
    ## E4: Calibration: isotonic
    class_train <- dfTrain$class
    class_test <- dfTest$class
    
    probs_train <- probs_model_train
    probs_test <- probs_model_test
    
    class_prin <- 2
    calibration_model <- calibrate(class_train, probs_train[,class_prin], class1=class_prin, 
                                   method="isoReg",assumeProbabilities=TRUE)
    
    calibratedProbs_train <- applyCalibration(probs_train[,class_prin], calibration_model)
    calibratedProbs <- applyCalibration(probs_test[,class_prin], calibration_model)
    
    ths = c(1/4,2/4,3/4)
    GPS_p_values = c('p_11','p14','r1_1','r41')
    results_iso <- Case_FixedThreshold(dfTest, calibratedProbs, ths, GPS_p_values)
    results_iso_train <- Case_FixedThreshold(dfTrain, calibratedProbs_train, ths, GPS_p_values)
    
    list_E4[[i]] <- list(GPS_train = results_iso_train$GPS, CM_train = results_iso_train$CM,
                         GPS_test = results_iso$GPS, CM_test = results_iso$CM)
    
    ## Max GPS with restrictions: case 4x2. After the model
    restriction = TRUE
    user_condition <- function(p, p_, r, r_) {
      (r_[1] > 0.5) & (r_[2] > 0.1)   & (p[3]>0.7) & (p[3]<0.85) & (p[4]>0.85) & (p_[1]>0.8)
    }
    
    num_cuts = 3
    vector_GPS = c('p14', 'p_11', 'r1_1') 
    results_restric_four_train <- AfterTheModel(dfTrain, probs_model_train,restriction,user_condition,vector_GPS,num_cuts)
    if (!is.numeric(results_restric_four_train$GPS)){
      CM_test <- NA
      gps_test <- NA
      
    } else {
      ths_for_test <- results_restric_four_train$thresholds
      
      probs_test_class1 <- probs_model_test[,'1'] 
      thresholds <- sort(ths_for_test, decreasing=TRUE)
      breaks <- c(Inf, thresholds, -Inf)
      Y_pred_class_test <- cut(probs_test_class1, breaks = breaks, labels = 0:length(thresholds), right = FALSE)
      CM_test <- table(Class_predicted = Y_pred_class_test,Class_observed = dfTest$class)
      gps_test <- GPS_from_CM(CM_test,vector_GPS)
      if (dim(CM_test[rowSums(CM_test[])>0,])[1] > num_cuts){
        gps_test <- GPS_from_CM(CM_test,vector_GPS)
      } else {
        gps_test <- NA
      }
    }
    
    
    list_E5[[i]] <- list(GPS_train = results_restric_four_train$GPS, CM_train = results_restric_four_train$best_CM,
                         ths = ths_for_test,
                         GPS_test = gps_test, CM_test = CM_test)
    
    ## In the model Case 4x2 
    user_condition <- function(p, p_, r, r_) {
      conditions <- list(
        list(value = r_[1], threshold = 0.5, op = ">"), 
        list(value = p_[1], threshold = 0.8, op = ">"), 
        list(value = r_[2], threshold = 0.1, op = ">"),   
        list(value = p[3], threshold = 0.7, op = ">"),    
        list(value = p[3], threshold = 0.85, op = "<"),   
        list(value = p[4], threshold = 0.85, op = ">")   
      )
      
      results <- lapply(conditions, function(cond) {
        if (cond$op == "<") return(cond$value < cond$threshold)
        if (cond$op == ">") return(cond$value > cond$threshold)
        if (cond$op == "<=") return(cond$value <= cond$threshold)
        if (cond$op == ">=") return(cond$value >= cond$threshold)
      })
      
      all_true = all(unlist(results))
      if (all_true){
        total_cond = TRUE
      } else{
        total_cond = FALSE
      }
      
      return(list(conditions = conditions, results = results,total_cond=total_cond))
    }
    
    kernelSVM = 'radial'
    num_cuts = 3
    vector_GPS = c('p14', 'p_11', 'r1_1') 
    cv_folds = 3
    restriction = TRUE
    
    results_In_train_four <- InTheModel(dfTrain,kernelSVM,num_cuts,cv_folds,vector_GPS,restriction,user_condition)
    
    model_in <- results_In_train_four$best_model_trained
    probs_train_in <- predict(model_in,df_train_no_target, probability=TRUE, decision.values = TRUE)
    probs_test_in <- predict(model_in,df_test_no_target, probability=TRUE, decision.values = TRUE)
    
    probs_model_train_in <- attr(probs_test_in,"probabilities")
    probs_model_train_in <- probs_model_train_in[,c("0", "1")]
    probs_model_train_class1_in <- probs_model_train_in[,2]
    
    probs_model_test_in <- attr(probs_test_in,"probabilities")
    probs_model_test_in <- probs_model_test_in[,c("0", "1")]
    probs_model_test_class1_in <- probs_model_test_in[,2]
    
    # Test
    ths_in = results_In_train_four$thresholds
    breaks <- c(Inf, ths_in, -Inf)
    Y_pred_class <- cut(probs_model_test_class1_in, breaks = breaks, labels = 0:length(ths_in), right = FALSE)
    unique_values <- unique(Y_pred_class) 
    Y_pred_class <- factor(Y_pred_class, levels = sort(unique_values))
    CM <- table(Class_predicted = Y_pred_class,Class_observed = dfTest$class)
    gps <- GPS_from_CM(CM,vector_GPS)
    
    list_E6[[i]] <- list(GPS_train = results_In_train_four$GPS_test, CM_train = results_In_train_four$best_CM_test,
                         ths = ths_in,
                         GPS_test = gps, CM_test = CM)
    
    
  }
  
  E1_summary <- variance_summary(list_E1)
  E2_summary <- variance_summary(list_E2)
  E3_summary <- variance_summary(list_E3)
  E4_summary <- variance_summary(list_E4)
  E5_summary <- variance_summary(list_E5)
  E6_summary <- variance_summary(list_E6)
  
  return(list(E1_summary=E1_summary,E2_summary=E2_summary,E3_summary=E3_summary,
              E4_summary=E4_summary,E5_summary=E5_summary,E6_summary=E6_summary))
  
}





VarianceSplitsPima <- function(df, seed_vector){
  
  n_seeds <- length(seed_vector)
  list_E1 <- list()
  list_E2 <- list()
  list_E3 <- list()
  list_E4 <- list()
  list_E5 <- list()
  list_E6 <- list()
  
  for (i in 1:n_seeds){
    set.seed(seed_vector[i])
    trainIndex <- createDataPartition(df$class,
                                      p = 0.7, # training contains 70% of data
                                      list = FALSE)
    
    dfTrain <- df[ trainIndex,]
    dfTest  <- df[-trainIndex,]
    
    kernelSVM = 'radial'
    
    svm_cv <- tune("svm", class ~ ., data = dfTrain,
                   kernel = kernelSVM, scale=TRUE,
                   ranges = list(cost = c(0.001, 0.01, 0.1, 1, 5, 10, 20, 50, 100,
                                          150, 200)))
    summary(svm_cv)
    best_cost=svm_cv$best.model$cost 
    
    
    df_test_no_target <- dfTest[, setdiff(names(df), "class")]
    df_train_no_target <- dfTrain[, setdiff(names(df), "class")]
    model_svm_Pima = svm(class ~ ., data=dfTrain, kernel="radial", scale=FALSE, probability=TRUE, type="C", cost=best_cost)
    pred_svm_train = predict(model_svm_Pima, df_train_no_target, probability=TRUE, decision.values = TRUE)
    probs_model_train <- attr(pred_svm_train,"probabilities")
    probs_model_train <- probs_model_train[,c("0", "1")]
    probs_model_train_class1 <- probs_model_train[,'1']
    
    # test
    pred_svm_test = predict(model_svm_Pima, df_test_no_target, probability=TRUE, decision.values = TRUE)
    probs_model_test <- attr(pred_svm_test,"probabilities")
    probs_model_test <- probs_model_test[,c("0", "1")]
    probs_model_test_class1 <- probs_model_test[,'1']
    
    ## E1: Classic cut: 1/2
    ths = c(1/2)
    GPS_p_values = c('p_11','p12','r1_1','r21')
    results_binary_train <- Case_FixedThreshold(dfTrain, probs_model_train_class1, ths, GPS_p_values)
    results_binary_test <- Case_FixedThreshold(dfTest, probs_model_test_class1, ths, GPS_p_values)
    list_E1[[i]] <- list(GPS_train = results_binary_train$GPS, CM_train = results_binary_train$CM,
                         GPS_test = results_binary_test$GPS, CM_test = results_binary_test$CM)
    
    ## E2: 4 cuts: 1/5-2/5-3/5-4/5
    ths = c(1/5,2/5,3/5,4/5)
    GPS_p_values = c('p_11','p15','r1_1','r51')
    results_five_train <- Case_FixedThreshold(dfTrain, probs_model_train_class1, ths, GPS_p_values)
    results_five_test <- Case_FixedThreshold(dfTest, probs_model_test_class1, ths, GPS_p_values)
    
    list_E2[[i]] <- list(GPS_train = results_five_train$GPS, CM_train = results_five_train$CM,
                         GPS_test = results_five_test$GPS, CM_test = results_five_test$CM)
    
    ## E3: Calibration: isotonic
    class_train <- dfTrain$class
    class_test <- dfTest$class
    
    probs_train <- probs_model_train
    probs_test <- probs_model_test
    
    class_prin <- 2
    calibration_model <- calibrate(class_train, probs_train[,class_prin], class1=class_prin, 
                                   method="isoReg",assumeProbabilities=TRUE)
    
    # apply the calibration to the testing set
    calibratedProbs <- applyCalibration(probs_test[,class_prin], calibration_model)
    calibratedProbs_train <- applyCalibration(probs_train[,class_prin], calibration_model)
    
    ths = c(1/5,2/5,3/5,4/5)
    GPS_p_values = c('p_11','p15','r1_1','r51')
    results_iso5 <- Case_FixedThreshold(dfTest, calibratedProbs, ths, GPS_p_values)
    results_iso5_train <- Case_FixedThreshold(dfTrain, calibratedProbs_train, ths, GPS_p_values)
    
    
    list_E3[[i]] <- list(GPS_train = results_iso5_train$GPS, CM_train = results_iso5_train$CM,
                         GPS_test = results_iso5$GPS, CM_test = results_iso5$CM)
    
    ## E4: Max GPS(p_11,p15) without constraints
    
    restriction = FALSE
    num_cuts = 4
    vector_GPS =   c('p15', 'p_11') 
    user_condition <- NULL
    results_2cuts_MaxGPS_class_train <- AfterTheModel(dfTrain, probs_model_train,restriction,user_condition,vector_GPS,num_cuts)
    ths_for_test <- results_2cuts_MaxGPS_class_train$thresholds
    
    probs_test_class1 <- probs_model_test[,'1'] 
    thresholds <- sort(ths_for_test, decreasing=TRUE)
    breaks <- c(Inf, thresholds, -Inf)
    Y_pred_class_test <- cut(probs_test_class1, breaks = breaks, labels = 0:length(thresholds), right = FALSE)
    
    CM_test <- table(Class_predicted = Y_pred_class_test,Class_observed = dfTest$class)
    
    if (dim(CM_test[rowSums(CM_test[])>0,])[1] > num_cuts){
      gps_test <- GPS_from_CM(CM_test,vector_GPS)
    } else {
      gps_test <- NA
    }
    
    list_E4[[i]] <- list(GPS_train = results_2cuts_MaxGPS_class_train$GPS, CM_train = results_2cuts_MaxGPS_class_train$best_CM,
                         ths = ths_for_test,
                         GPS_test = gps_test, CM_test = CM_test)
    
    ## E5: Max GPS with restrictions: case 5x2 After the model
    restriction = TRUE
    user_condition <- function(p, p_, r, r_) {
      (r_[1]>0.3) & (p[4]>0.7) & (p[3]>0.4) & (p[5]>0.9)
    }
    
    num_cuts = 4
    vector_GPS = c('p15', 'p_11') 
    results_restric_four_train <- AfterTheModel(dfTrain, probs_model_train,restriction,user_condition,vector_GPS,num_cuts)
    
    if (!is.numeric(results_restric_four_train$GPS)){
      CM_test <- NA
      gps_test <- NA
      
    } else {
      ths_for_test <- results_restric_four_train$thresholds
      
      probs_test_class1 <- probs_model_test[,'1'] 
      thresholds <- sort(ths_for_test, decreasing=TRUE)
      breaks <- c(Inf, thresholds, -Inf)
      Y_pred_class_test <- cut(probs_test_class1, breaks = breaks, labels = 0:length(thresholds), right = FALSE)
      CM_test <- table(Class_predicted = Y_pred_class_test,Class_observed = dfTest$class)
      gps_test <- GPS_from_CM(CM_test,vector_GPS)
      if (dim(CM_test[rowSums(CM_test[])>0,])[1] > num_cuts){
        gps_test <- GPS_from_CM(CM_test,vector_GPS)
      } else {
        gps_test <- NA
      }
    }
    
    list_E5[[i]] <- list(GPS_train = results_restric_four_train$GPS, CM_train = results_restric_four_train$best_CM,
                         ths = ths_for_test,
                         GPS_test = gps_test, CM_test = CM_test)
    
    ## E6: In the model Case 5x2 
    user_condition <- function(p, p_, r, r_) {
      conditions <- list(
        list(value = r_[1], threshold = 0.3, op = ">"),
        list(value = p[4], threshold = 0.7, op = ">"),
        list(value = p[5], threshold = 0.9, op = ">"),
        list(value = p[3], threshold = 0.4, op = ">")
      )
      
      results <- lapply(conditions, function(cond) {
        if (cond$op == "<") return(cond$value < cond$threshold)
        if (cond$op == ">") return(cond$value > cond$threshold)
        if (cond$op == "<=") return(cond$value <= cond$threshold)
        if (cond$op == ">=") return(cond$value >= cond$threshold)
      })
      
      all_true = all(unlist(results))
      if (all_true){
        total_cond = TRUE
      } else{
        total_cond = FALSE
      }
      
      return(list(conditions = conditions, results = results,total_cond=total_cond))
    }
    
    kernelSVM = 'radial'
    num_cuts = 4
    vector_GPS = c('p15', 'p_11')
    cv_folds = 2
    restriction = TRUE
    
    results_In_train_five <- InTheModel(dfTrain,kernelSVM,num_cuts,cv_folds,vector_GPS,restriction,user_condition)
    
    model_in <- results_In_train_five$best_model_trained
    probs_train_in <- predict(model_in,df_train_no_target, probability=TRUE, decision.values = TRUE)
    probs_test_in <- predict(model_in,df_test_no_target, probability=TRUE, decision.values = TRUE)
    
    probs_model_train_in <- attr(probs_test_in,"probabilities")
    probs_model_train_in <- probs_model_train_in[,c("0", "1")]
    probs_model_train_class1_in <- probs_model_train_in[,2]
    
    probs_model_test_in <- attr(probs_test_in,"probabilities")
    probs_model_test_in <- probs_model_test_in[,c("0", "1")]
    probs_model_test_class1_in <- probs_model_test_in[,2]
    
    # Test
    ths_in = results_In_train_five$thresholds
    breaks <- c(Inf, ths_in, -Inf)
    Y_pred_class <- cut(probs_model_test_class1_in, breaks = breaks, labels = 0:length(ths_in), right = FALSE)
    unique_values <- unique(Y_pred_class) 
    Y_pred_class <- factor(Y_pred_class, levels = sort(unique_values))
    CM <- table(Class_predicted = Y_pred_class,Class_observed = dfTest$class)
    gps <- GPS_from_CM(CM,vector_GPS)
    
    list_E6[[i]] <- list(GPS_train = results_In_train_five$GPS_test, CM_train = results_In_train_five$best_CM_test,
                         ths = ths_in,
                         GPS_test = gps, CM_test = CM)
    
    
  }
  
  E1_summary <- variance_summary(list_E1)
  E2_summary <- variance_summary(list_E2)
  E3_summary <- variance_summary(list_E3)
  E4_summary <- variance_summary(list_E4)
  E5_summary <- variance_summary(list_E5)
  E6_summary <- variance_summary(list_E6)
  
  return(list(E1_summary=E1_summary,E2_summary=E2_summary,E3_summary=E3_summary,
              E4_summary=E4_summary,E5_summary=E5_summary,E6_summary=E6_summary))
  
}





barplot_CM_CI <- function(CM, list_names_obs, list_names_pred, labels_top, label_size = 6){
  
  colnames(CM) <- list_names_obs
  rownames(CM) <- list_names_pred
  
  df <- as.data.frame(as.table(CM))
  names(df) <- c("Class_predicted","Class_observed","Freq")
  
  if (is.null(names(labels_top))) names(labels_top) <- list_names_pred
  
  df$To_plot <- labels_top[as.character(df$Class_predicted)]
  
  df$To_plot[df$Class_observed == list_names_obs[1]] <- ""
  
  y_max <- max(rowSums(CM))
  
  g<- ggplot(df, aes(x = Class_predicted, y = Freq, fill = Class_observed)) +
    geom_bar(stat = "identity", position = position_stack(reverse = TRUE)) +
    geom_text(aes(label = To_plot),
              position = position_stack(vjust = 1.01, reverse = TRUE),
              vjust = -0.2, hjust = 0.5, size = label_size,
              lineheight = 1) +
    scale_fill_manual(values = c("lightblue","orange")) +
    theme_minimal() +
    theme(axis.text  = element_text(size = 14),
          axis.title = element_text(size = 16),
          legend.position = "none") +
    labs(x = "Predicted class", y = "Frequency") +
    ylim(0, y_max * 1.2)
  plot(g)
}



make_labels_top <- function(mean_CI, digits = 1) {
  mean_CI$CI_sup[mean_CI$CI_sup>1] = 1
  mean_CI$CI_inf[mean_CI$CI_inf<0] = 0
  scale <- if (max(mean_CI$mean, mean_CI$CI_inf, mean_CI$CI_sup, na.rm = TRUE) <= 1) 100 else 1
  setNames(sprintf(paste0("%.", digits, "f%%\n[%.", digits, "f, %.", digits, "f]"),
                   mean_CI$mean*scale, mean_CI$CI_inf*scale, mean_CI$CI_sup*scale),
           mean_CI$key)
}



