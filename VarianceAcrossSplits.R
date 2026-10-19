###########################################################
#############  VARIANCE ACROSS RANDOM SPLITS  #############
###########################################################

## Repeated 70/30 train/test splits for the real datasets in the revised paper.
## Includes Mammographic Mass, Pima, CTG, Phishing and Credit Card Fraud.
## Uses GeneralAndMethodFunctions.R from the repository root.

#############################################
#####               MAMMO               #####
#############################################

source('GeneralAndMethodFunctions.R')



# df <- read_csv("datasets/mammographic_masses.data", col_names = FALSE)
# 
# colnames(df) <- c("BI-RADS", "Age", "Shape", "Margin", "Density", "class")
# df=df %>% mutate_if(is.character, as.numeric)
# 
# df = drop_na(df)
# df$class <- as.factor(df$class)
# str(df)
# df[,-6] <- scale(df[,-6])


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
  
  E_long_train <- tidyr::gather(as.data.frame(E_train), factor_key=TRUE)
  E_long_test <- tidyr::gather(as.data.frame(E_test), factor_key=TRUE)
  
  E_train_summary <- E_long_train%>% dplyr::group_by(key)%>%
    dplyr::summarise(mean= mean(value), sd= sd(value), median = median(value))
  
  E_test_summary <- E_long_test%>% dplyr::group_by(key)%>%
    dplyr::summarise(mean= mean(value), sd= sd(value), median = median(value))
  
  dof = length(list_E)
  E_train_summary$CI_inf <- E_train_summary$mean - qt(0.975,dof-1)*E_train_summary$sd / sqrt(dof)
  E_train_summary$CI_sup <- E_train_summary$mean + qt(0.975,dof-1)*E_train_summary$sd / sqrt(dof)
  E_test_summary$CI_inf <- E_test_summary$mean - qt(0.975,dof-1)*E_test_summary$sd / sqrt(dof)
  E_test_summary$CI_sup <- E_test_summary$mean + qt(0.975,dof-1)*E_test_summary$sd / sqrt(dof)
  
  
  # Media aritmética (elemento a elemento) sin crear E_CM
  CM_train_mean <- Reduce(`+`, lapply(list_E, `[[`, "CM_train")) / length(list_E)
  CM_test_mean  <- Reduce(`+`, lapply(list_E, `[[`, "CM_test"))  / length(list_E)
  
  CM_mean_train <- round(CM_train_mean)
  CM_mean_test <- round(CM_test_mean)
  
  return(list(E_train_summary=E_train_summary, E_test_summary = E_test_summary,
              CM_mean_train = CM_mean_train, CM_mean_test = CM_mean_test))
  
}

################################################################################################
############################ EQUAL-FREQUENCY BINNING ###########################################
################################################################################################
EqualFrequencyThresholds <- function(probs_class1, K) {
  
  ths <- as.numeric(quantile(probs_class1,probs = (1:(K - 1)) / K,na.rm = TRUE,names = FALSE,type = 8))
  
  if (anyDuplicated(ths)) {
    stop("Repeated equal-frequency thresholds were obtained due to tied probabilities.")
  }
  
  return(ths)
}


################################################################################################
############################ OPTIMAL BINNING  ###########################################
################################################################################################
OptimalBinningThresholds <- function(probs_class1, class, K) {
  
  x <- as.numeric(probs_class1)
  y <- as.integer(as.character(class))
  
  if (!all(y %in% c(0L, 1L))) {
    stop("The target must contain only classes 0 and 1.")
  }
  
  optbinning <- reticulate::import("optbinning")
  
  optb <- optbinning$OptimalBinning(name = "model_probability",min_n_bins = as.integer(K),max_n_bins = as.integer(K))
  
  optb$fit(x, y)
  
  ths <- as.numeric(optb$splits)
  
  if (length(ths) != K - 1) {
    stop(paste0("OptimalBinning returned ", length(ths) + 1," bins instead of the requested ", K, "."))
  }
  
  return(list(thresholds = ths,status = optb$status))
}


# seed_vector <- c(1,runif(9))

VarianceSplitsMammo <- function(df, seed_vector){
  
  n_seeds <- length(seed_vector)
  list_E1 <- list()
  list_E2 <- list()
  list_E3 <- list()
  list_E4 <- list()
  list_E5 <- list()
  list_E6 <- list()
  list_E_equal_freq <- list()
  list_E_optimal <- list()
  
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
    
    ## E4: Isotonic calibration + equal-width binning
    class_train <- dfTrain$class
    class_test <- dfTest$class
    
    probs_train <- probs_model_train
    probs_test <- probs_model_test
    
    class_prin <- 2
    calibration_model <- CORElearn::calibrate(class_train, probs_train[,class_prin], class1=class_prin, 
                                   method="isoReg",assumeProbabilities=TRUE)
    
    calibratedProbs_train <- CORElearn::applyCalibration(probs_train[,class_prin], calibration_model)
    calibratedProbs <- CORElearn::applyCalibration(probs_test[,class_prin], calibration_model)
    
    ths = c(1/4,2/4,3/4)
    GPS_p_values = c('p_11','p14','r1_1','r41')
    results_iso <- Case_FixedThreshold(dfTest, calibratedProbs, ths, GPS_p_values)
    results_iso_train <- Case_FixedThreshold(dfTrain, calibratedProbs_train, ths, GPS_p_values)
    
    list_E4[[i]] <- list(GPS_train = results_iso_train$GPS, CM_train = results_iso_train$CM,
                         GPS_test = results_iso$GPS, CM_test = results_iso$CM)
    
    
    
    ## Equal-frequency binning
    
    K <- 4
    GPS_p_values <- c('p_11','p14','r1_1','r41')
    
    # Thresholds obtained ONLY from training probabilities
    ths_equal_freq <- EqualFrequencyThresholds(probs_class1 = probs_model_train_class1,K = K)
    
    # Training
    results_equal_freq_train <- Case_FixedThreshold(dfTrain,probs_model_train_class1,ths_equal_freq,GPS_p_values)
    
    # External test: same thresholds, no recalculation
    results_equal_freq_test <- Case_FixedThreshold(dfTest,probs_model_test_class1,ths_equal_freq,GPS_p_values)
    
    list_E_equal_freq[[i]] <- list(GPS_train = results_equal_freq_train$GPS,CM_train = results_equal_freq_train$CM,
      ths = ths_equal_freq,GPS_test = results_equal_freq_test$GPS,CM_test = results_equal_freq_test$CM)
    
    
    ## Optimal binning
    K <- 4
    GPS_p_values <- c('p_11','p14','r1_1','r41')
    
    # IMPORTANT:
    # Optimal binning is fitted ONLY on the training data
    optimal_binning <- OptimalBinningThresholds(probs_class1 = probs_model_train_class1,class = dfTrain$class,K = K)
    
    ths_optimal <- optimal_binning$thresholds
    
    # Training
    results_optimal_train <- Case_FixedThreshold(dfTrain,probs_model_train_class1,ths_optimal,GPS_p_values)
    
    # External test: use the SAME thresholds
    results_optimal_test <- Case_FixedThreshold(dfTest,probs_model_test_class1,ths_optimal,GPS_p_values)
    
    list_E_optimal[[i]] <- list(GPS_train = results_optimal_train$GPS,CM_train = results_optimal_train$CM,
      ths = ths_optimal,status = optimal_binning$status,GPS_test = results_optimal_test$GPS,
      CM_test = results_optimal_test$CM)
    
    
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
    
    # results_In_train_four <- InTheModel(dfTrain,kernelSVM,num_cuts,cv_folds,vector_GPS,restriction,user_condition)
    calibrate <- FALSE
    
    results_In_train_four <- InTheModel(
      df = dfTrain,
      kernelSVM = kernelSVM,
      num_cuts = num_cuts,
      cv_folds = cv_folds,
      calibrate = calibrate,
      vector_GPS = vector_GPS,
      restriction = restriction,
      user_condition = user_condition
    )
    
    model_in <- results_In_train_four$best_model_trained
    probs_train_in <- predict(model_in,df_train_no_target, probability=TRUE, decision.values = TRUE)
    probs_test_in <- predict(model_in,df_test_no_target, probability=TRUE, decision.values = TRUE)
    
    probs_model_train_in <- attr(probs_train_in,"probabilities")
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
  E_equal_freq_summary <- variance_summary(list_E_equal_freq)
  E_optimal_summary <- variance_summary(list_E_optimal)
  
  return(list(E1_summary=E1_summary,E2_summary=E2_summary,E3_summary=E3_summary,
              E4_summary=E4_summary, E_equal_freq_summary = E_equal_freq_summary,
              E5_summary=E5_summary,E6_summary=E6_summary, E_optimal_summary = E_optimal_summary))
  
}





VarianceSplitsPima <- function(df, seed_vector){
  
  n_seeds <- length(seed_vector)
  list_E1 <- list()
  list_E2 <- list()
  list_E3 <- list()
  list_E4 <- list()
  list_E5 <- list()
  list_E6 <- list()
  list_E_equal_freq <- list()
  list_E_optimal <- list()
  
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
    
    ## E3: Isotonic calibration + equal-width binning
    class_train <- dfTrain$class
    class_test <- dfTest$class
    
    probs_train <- probs_model_train
    probs_test <- probs_model_test
    
    class_prin <- 2
    calibration_model <- CORElearn::calibrate(class_train, probs_train[,class_prin], class1=class_prin, 
                                   method="isoReg",assumeProbabilities=TRUE)
    
    # apply the calibration to the testing set
    calibratedProbs <- CORElearn::applyCalibration(probs_test[,class_prin], calibration_model)
    calibratedProbs_train <- CORElearn::applyCalibration(probs_train[,class_prin], calibration_model)
    
    ths = c(1/5,2/5,3/5,4/5)
    GPS_p_values = c('p_11','p15','r1_1','r51')
    results_iso5 <- Case_FixedThreshold(dfTest, calibratedProbs, ths, GPS_p_values)
    results_iso5_train <- Case_FixedThreshold(dfTrain, calibratedProbs_train, ths, GPS_p_values)
    
    
    list_E3[[i]] <- list(GPS_train = results_iso5_train$GPS, CM_train = results_iso5_train$CM,
                         GPS_test = results_iso5$GPS, CM_test = results_iso5$CM)
    
    #################################################
    ## Equal-frequency binning
    
    K <- 5
    GPS_p_values <- c('p_11','p15','r1_1','r51')
    
    # Thresholds obtained ONLY from training probabilities
    ths_equal_freq <- EqualFrequencyThresholds(probs_class1 = probs_model_train_class1,K = K)
    
    # Training
    results_equal_freq_train <- Case_FixedThreshold(dfTrain,probs_model_train_class1,ths_equal_freq,GPS_p_values)
    
    # External test
    results_equal_freq_test <- Case_FixedThreshold(dfTest,probs_model_test_class1,ths_equal_freq,GPS_p_values)
    
    list_E_equal_freq[[i]] <- list(GPS_train = results_equal_freq_train$GPS,CM_train = results_equal_freq_train$CM,
      ths = ths_equal_freq,GPS_test = results_equal_freq_test$GPS,CM_test = results_equal_freq_test$CM)
    
    #########################################3
    ## Optimal binning
    K <- 5
    GPS_p_values <- c('p_11','p15','r1_1','r51')
    
    # Optimal binning is fitted ONLY on TRAIN
    optimal_binning <- OptimalBinningThresholds(probs_class1 = probs_model_train_class1,class = dfTrain$class,K = K)
    
    ths_optimal <- optimal_binning$thresholds
    
    # Training
    results_optimal_train <- Case_FixedThreshold(dfTrain,probs_model_train_class1,ths_optimal,GPS_p_values)
    
    # External test
    results_optimal_test <- Case_FixedThreshold(dfTest,probs_model_test_class1,ths_optimal,GPS_p_values)
    
    list_E_optimal[[i]] <- list(GPS_train = results_optimal_train$GPS,CM_train = results_optimal_train$CM,
      ths = ths_optimal,status = optimal_binning$status,GPS_test = results_optimal_test$GPS,
      CM_test = results_optimal_test$CM)
    
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
    
    # results_In_train_five <- InTheModel(dfTrain,kernelSVM,num_cuts,cv_folds,vector_GPS,restriction,user_condition)
    
    calibrate <- FALSE
    
    results_In_train_five <- InTheModel(
      df = dfTrain,
      kernelSVM = kernelSVM,
      num_cuts = num_cuts,
      cv_folds = cv_folds,
      calibrate = calibrate,
      vector_GPS = vector_GPS,
      restriction = restriction,
      user_condition = user_condition
    )
    
    model_in <- results_In_train_five$best_model_trained
    probs_train_in <- predict(model_in,df_train_no_target, probability=TRUE, decision.values = TRUE)
    probs_test_in <- predict(model_in,df_test_no_target, probability=TRUE, decision.values = TRUE)
    
    probs_model_train_in <- attr(probs_train_in,"probabilities")
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
  E_equal_freq_summary <- variance_summary(list_E_equal_freq)
  E_optimal_summary <- variance_summary(list_E_optimal)
  
  return(list(E1_summary=E1_summary,E2_summary=E2_summary,E3_summary=E3_summary,
              E4_summary=E4_summary,E_equal_freq_summary = E_equal_freq_summary,
              E5_summary=E5_summary,E6_summary=E6_summary,E_optimal_summary = E_optimal_summary))
  
}





barplot_CM_CI <- function(CM, list_names_obs, list_names_pred, labels_top, label_size = 6){
  
  
  # Asegurar nombres del CM
  colnames(CM) <- list_names_obs
  rownames(CM) <- list_names_pred
  
  # Data frame largo
  df <- as.data.frame(as.table(CM))
  names(df) <- c("Class_predicted","Class_observed","Freq")
  
  # Etiquetas: si no vienen nombradas, asumimos el orden de list_names_pred
  if (is.null(names(labels_top))) names(labels_top) <- list_names_pred
  
  # Asignar la misma etiqueta a todas las filas de cada barra
  df$To_plot <- labels_top[as.character(df$Class_predicted)]
  
  # Mostrar la etiqueta solo en el “bloque superior” (tu lógica original):
  # vaciamos en la primera clase observada
  df$To_plot[df$Class_observed == list_names_obs[1]] <- ""
  
  # Altura máxima de barras
  y_max <- max(rowSums(CM))
  
  # Plot (idéntico estilo al tuyo)
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
  
  mean_CI$CI_sup[mean_CI$CI_sup > 1] <- 1
  mean_CI$CI_inf[mean_CI$CI_inf < 0] <- 0
  
  labels <- rep(NA_character_, nrow(mean_CI))
  
  for (i in 1:nrow(mean_CI)) {
    if (is.na(mean_CI$mean[i]) || is.nan(mean_CI$mean[i])) {
      labels[i] <- "Empty"
    } else {
      labels[i] <- sprintf(paste0("%.", digits, "f%%\n[%.", digits, "f, %.", digits, "f]"),
        mean_CI$mean[i] * 100,mean_CI$CI_inf[i] * 100,mean_CI$CI_sup[i] * 100)
    }
  }
  setNames(labels, mean_CI$key)
}


#########################################################################################################################
#####--------------------------------------------- NEW DATASETS  ---------------------------------------------###########
#########################################################################################################################


################################################################################################
################################ CTG #################################################
################################################################################################

VarianceSplitsCTG <- function(df, seed_vector) {
  
  n_seeds <- length(seed_vector)
  
  list_binary <- list()
  list_equal_width <- list()
  list_iso_equal_width <- list()
  list_equal_freq <- list()
  list_optimal <- list()
  list_roc <- list()
  list_proposed <- list()
  
  
  for (i in 1:n_seeds) {
    
    print(paste("CTG split:", i))
    
    # =========================================================================
    # 1. OUTER TRAIN-TEST SPLIT
    # =========================================================================
    
    set.seed(seed_vector[i])
    
    trainIndex <- createDataPartition(df$class,p = 0.7,list = FALSE)
    
    dfTrain <- df[trainIndex, ]
    dfTest  <- df[-trainIndex, ]
    
    
    # =========================================================================
    # 2. RANDOM FOREST
    # =========================================================================
    
    # Class weights estimated only from training
    n_neg <- sum(dfTrain$class == "0")
    n_pos <- sum(dfTrain$class == "1")
    
    weight_pos <- n_neg / n_pos
    
    class_weights <- c("0" = 1,"1" = weight_pos )
    
    set.seed(1)
    
    model_rf <- ranger::ranger(class ~ .,data = dfTrain,probability = TRUE,num.trees = 500,
      class.weights = class_weights,seed = 1)
    
    
    # # Raw model probabilities: TRAIN
    # probs_model_train <- predict(model_rf,data = dfTrain)$predictions
    # probs_model_train_class1 <- probs_model_train[, "1"]
    
    # Raw model probabilities: TRAIN
    # Out-of-bag probabilities: each observation is predicted
    # using only trees in which it was not used for training
    probs_model_train <- model_rf$predictions
    
    probs_model_train <- probs_model_train[, c("0", "1")]
    
    probs_model_train_class1 <- probs_model_train[, "1"]
    
    # Raw model probabilities: TEST
    probs_model_test <- predict(model_rf,data = dfTest)$predictions
    
    probs_model_test_class1 <- probs_model_test[, "1"]
    
    
    # =========================================================================
    # E1. CLASSICAL BINARY: threshold = 0.5
    # =========================================================================
    
    ths <- 0.5
    
    GPS_p_values <- c("p_11", "p12","r1_1", "r21")
    
    results_binary_train <- Case_FixedThreshold(dfTrain,probs_model_train_class1,ths,GPS_p_values)
    results_binary_test <- Case_FixedThreshold(dfTest,probs_model_test_class1,ths,GPS_p_values)
    
    list_binary[[i]] <- list(GPS_train = results_binary_train$GPS,CM_train = results_binary_train$CM,
      GPS_test = results_binary_test$GPS,CM_test = results_binary_test$CM)
    
    
    # =========================================================================
    # E2. EQUAL-WIDTH BINNING: 3x2
    # =========================================================================
    
    K <- 3
    ths <- c(1/3, 2/3)
    GPS_p_values <- c("p_11", "p13","r1_1", "r31")
    
    results_equal_width_train <- Case_FixedThreshold(dfTrain,probs_model_train_class1,ths,GPS_p_values)
    
    results_equal_width_test <- Case_FixedThreshold(dfTest,probs_model_test_class1,ths,GPS_p_values)
    
    list_equal_width[[i]] <- list(GPS_train = results_equal_width_train$GPS,CM_train = results_equal_width_train$CM,
      GPS_test = results_equal_width_test$GPS,CM_test = results_equal_width_test$CM)
    
    
    # =========================================================================
    # E3. ISOTONIC CALIBRATION + EQUAL-WIDTH BINNING
    # =========================================================================
    
    class_train <- dfTrain$class
    
    class_prin <- 2
    
    calibration_model <- CORElearn::calibrate(class_train,probs_model_train[, class_prin],class1 = class_prin,
      method = "isoReg",assumeProbabilities = TRUE)
    
    calibratedProbs_train <- CORElearn::applyCalibration(probs_model_train[, class_prin],calibration_model)
    
    calibratedProbs_test <- CORElearn::applyCalibration(probs_model_test[, class_prin],calibration_model)
    
    ths <- c(1/3, 2/3)
    
    GPS_p_values <- c("p_11", "p13","r1_1", "r31")
    
    results_iso_train <- Case_FixedThreshold(dfTrain,calibratedProbs_train,ths,GPS_p_values)
    
    results_iso_test <- Case_FixedThreshold(dfTest,calibratedProbs_test,ths,GPS_p_values)
    
    list_iso_equal_width[[i]] <- list(GPS_train = results_iso_train$GPS,CM_train = results_iso_train$CM,
      GPS_test = results_iso_test$GPS,CM_test = results_iso_test$CM)
    
    
    # =========================================================================
    # E4. EQUAL-FREQUENCY BINNING
    # =========================================================================
    
    K <- 3
    
    ths_equal_freq <- EqualFrequencyThresholds(probs_class1 = probs_model_train_class1,K = K)
    
    GPS_p_values <- c("p_11", "p13","r1_1", "r31")
    
    results_equal_freq_train <- Case_FixedThreshold(dfTrain,probs_model_train_class1,ths_equal_freq,
      GPS_p_values)
    
    results_equal_freq_test <- Case_FixedThreshold(dfTest,probs_model_test_class1,
      ths_equal_freq,GPS_p_values)
    
    list_equal_freq[[i]] <- list(GPS_train = results_equal_freq_train$GPS,CM_train = results_equal_freq_train$CM,
      ths = ths_equal_freq,GPS_test = results_equal_freq_test$GPS,CM_test = results_equal_freq_test$CM)
    
    
    # =========================================================================
    # E5. OPTIMAL BINNING
    # =========================================================================
    
    K <- 3
    GPS_p_values <- c("p_11", "p13", "r1_1", "r31")
    
    optimal_binning <- tryCatch(
      
      OptimalBinningThresholds(probs_class1 = probs_model_train_class1,class = dfTrain$class,K = K),
      
      error = function(e) {
        message("Optimal Binning failed in CTG split ",i,": ",e$message)
        return(NULL)
      }
    )
    
    
    if (is.null(optimal_binning)) {
      
      # Keep the position corresponding to this split
      list_optimal[i] <- list(NULL)
      
    } else {
      
      ths_optimal <- optimal_binning$thresholds
      
      results_optimal_train <- Case_FixedThreshold(dfTrain,probs_model_train_class1,
        ths_optimal,GPS_p_values)
      
      results_optimal_test <- Case_FixedThreshold(dfTest,probs_model_test_class1,
        ths_optimal,GPS_p_values)
      
      list_optimal[[i]] <- list(GPS_train = results_optimal_train$GPS,CM_train = results_optimal_train$CM,
        ths = ths_optimal,status = optimal_binning$status,GPS_test = results_optimal_test$GPS,
        CM_test = results_optimal_test$CM)
    }
    
    
    # =========================================================================
    # E6. ROC - YOUDEN INDEX
    # Binary comparison
    # =========================================================================
    
    roc_obj <- pROC::roc(response = dfTrain$class,predictor = probs_model_train_class1,
      levels = c("0", "1"),direction = "<",quiet = TRUE)
    
    youden_threshold <- as.numeric(pROC::coords(roc_obj,x = "best",best.method = "youden",
        ret = "threshold",transpose = FALSE)[1])
    
    GPS_p_values_binary <- c("p_11", "p12","r1_1", "r21")
    
    results_roc_train <- Case_FixedThreshold(dfTrain,probs_model_train_class1,youden_threshold,GPS_p_values_binary)
    results_roc_test <- Case_FixedThreshold(dfTest,probs_model_test_class1,youden_threshold,GPS_p_values_binary)
    
    list_roc[[i]] <- list(GPS_train = results_roc_train$GPS,CM_train = results_roc_train$CM,ths = youden_threshold,
      GPS_test = results_roc_test$GPS,CM_test = results_roc_test$CM)
    
    
    # =========================================================================
    # E7. PROPOSED METHOD: AFTER-THE-MODEL
    # Monitoring must contain at least 10% of TRAIN
    # =========================================================================
    
    restriction <- TRUE
    num_cuts <- 2
    
    # Number of observations of each observed class in TRAIN
    n0 <- sum(dfTrain$class == "0")
    n1 <- sum(dfTrain$class == "1")
    n_train <- nrow(dfTrain)
    
    user_condition <- function(p, p_, r, r_) {
      # Total proportion of TRAIN observations assigned to Monitoring
      prop_monitoring <- (r_[2] * n0 +r[2]  * n1) / n_train
      prop_monitoring >= 0.10
    }
    
    vector_GPS <- c("p_11","p13","r1_1","r31")
    
    results_proposed_train <- AfterTheModel(dfTrain,probs_model_train,restriction,
      user_condition,vector_GPS,num_cuts)
    
    
    # Stop if no feasible solution exists in TRAIN
    if (!is.numeric(results_proposed_train$GPS)) {
      stop(paste("No feasible After-the-model solution in CTG split",i))
    }
    
    ths_proposed <- results_proposed_train$thresholds
    
    results_proposed_test <- Case_FixedThreshold(dfTest,probs_model_test_class1,
      ths_proposed,vector_GPS)
    
    list_proposed[[i]] <- list(GPS_train = results_proposed_train$GPS,CM_train = results_proposed_train$best_CM,
      ths = ths_proposed,GPS_test = results_proposed_test$GPS,CM_test = results_proposed_test$CM)
  }
  
  # ===========================================================================
  # SUMMARIES ACROSS RANDOM SPLITS
  # ===========================================================================
  
  binary_summary <- variance_summary(list_binary)
  
  equal_width_summary <- variance_summary(list_equal_width)
  iso_equal_width_summary <- variance_summary(list_iso_equal_width)
  equal_freq_summary <- variance_summary(list_equal_freq)
  #optimal_summary <- variance_summary(list_optimal)
  roc_summary <- variance_summary(list_roc)
  proposed_summary <- variance_summary(list_proposed)
  
  # Optimal Binning: summarize only feasible splits
  list_optimal_valid <- Filter(Negate(is.null),list_optimal)
  optimal_summary <- if (length(list_optimal_valid) > 0L) variance_summary(list_optimal_valid) else NULL
  
  return(
    list(binary_summary = binary_summary,
      equal_width_summary = equal_width_summary,
      iso_equal_width_summary = iso_equal_width_summary,
      equal_freq_summary = equal_freq_summary,
      optimal_summary = optimal_summary,
      roc_summary = roc_summary,
      proposed_summary = proposed_summary,
      
      optimal_n_valid = length(list_optimal_valid),
      optimal_n_failed = n_seeds - length(list_optimal_valid),
      
      raw = list(binary = list_binary,equal_width = list_equal_width,
        iso_equal_width = list_iso_equal_width,equal_freq = list_equal_freq,
        optimal = list_optimal,roc = list_roc,proposed = list_proposed)
    )
  )
}

# Para cuando alguna categoría queda vacía:
variance_summary_allow_empty <- function(list_E){
  
  # Prevalence of positive class within each predicted category
  get_props <- function(CM) {
    
    row_totals <- rowSums(CM)
    
    props <- CM[, 2] / row_totals
    
    # Empty predicted categories -> NA instead of NaN
    props[row_totals == 0] <- NA_real_
    
    return(props)
  }
  
  
  props_train <- do.call(
    rbind,
    lapply(list_E, function(x) get_props(x$CM_train))
  )
  
  props_test <- do.call(
    rbind,
    lapply(list_E, function(x) get_props(x$CM_test))
  )
  
  
  summarize_props <- function(mat) {
    
    data.frame(
      key = colnames(mat),
      
      mean = apply(mat, 2, mean, na.rm = TRUE),
      sd = apply(mat, 2, sd, na.rm = TRUE),
      median = apply(mat, 2, median, na.rm = TRUE),
      
      n_valid = apply(mat, 2, function(x) sum(!is.na(x)))
    )
  }
  
  
  E_train_summary <- summarize_props(props_train)
  E_test_summary  <- summarize_props(props_test)
  
  
  E_train_summary$CI_inf <- E_train_summary$mean - qt(0.975, E_train_summary$n_valid - 1) *
    E_train_summary$sd /sqrt(E_train_summary$n_valid)
  
  E_train_summary$CI_sup <-E_train_summary$mean +
    qt(0.975, E_train_summary$n_valid - 1) *
    E_train_summary$sd / sqrt(E_train_summary$n_valid)
  
  E_test_summary$CI_inf <-E_test_summary$mean -
    qt(0.975, E_test_summary$n_valid - 1) *
    E_test_summary$sd /sqrt(E_test_summary$n_valid)
  
  E_test_summary$CI_sup <-E_test_summary$mean +
    qt(0.975, E_test_summary$n_valid - 1) *
    E_test_summary$sd /sqrt(E_test_summary$n_valid)
  
  
  # Mean CM still uses ALL splits
  CM_train_mean <-Reduce(`+`, lapply(list_E, `[[`, "CM_train")) /length(list_E)
  
  CM_test_mean <-
    Reduce(`+`, lapply(list_E, `[[`, "CM_test")) /
    length(list_E)
  
  
  return(
    list(
      E_train_summary = E_train_summary,
      E_test_summary = E_test_summary,
      CM_mean_train = round(CM_train_mean),
      CM_mean_test = round(CM_test_mean)
    )
  )
}




################################################################################
######################## CTG - IN THE MODEL 10 SPLITS ###########################
################################################################################

VarianceSplitsCTG_InModel <- function(df, seed_vector) {
  
  n_seeds <- length(seed_vector)
  
  list_in_model <- vector("list", n_seeds)
  
  
  for (i in 1:n_seeds) {
    
    cat("\n====================================================\n")
    cat("CTG In-the-model split:", i, "\n")
    cat("====================================================\n")
    
    
    # =========================================================================
    # 1. OUTER TRAIN-TEST SPLIT
    # =========================================================================
    
    set.seed(seed_vector[i])
    
    trainIndex <- createDataPartition(
      df$class,
      p = 0.7,
      list = FALSE
    )
    
    dfTrain <- df[trainIndex, ]
    dfTest  <- df[-trainIndex, ]
    
    
    df_test_no_target <- dfTest[
      , setdiff(names(dfTest), "class"),
      drop = FALSE
    ]
    
    
    # =========================================================================
    # 2. IN-THE-MODEL SETTINGS
    # =========================================================================
    
    kernelSVM <- "radial"
    num_cuts <- 2
    cv_folds <- 3
    calibrate <- FALSE
    
    vector_GPS <- c(
      "p_11",
      "p13",
      "r1_1",
      "r31"
    )
    
    restriction <- TRUE
    
    
    # =========================================================================
    # 3. RESTRICTION:
    #    Monitoring category must contain at least 10% of observations
    # =========================================================================
    
    user_condition <- function(p, p_, r, r_) {
      
      # -------------------------------------------------------
      # Recover prevalence P(Y = 1)
      # -------------------------------------------------------
      
      denominator <- p * r_ + p_ * r
      
      valid_categories <- which(
        is.finite(denominator) &
          denominator > 0 &
          r > 0 &
          r_ > 0
      )
      
      if (length(valid_categories) == 0) {
        
        prop_monitoring <- 0
        
      } else {
        
        prevalence_values <-
          (p[valid_categories] * r_[valid_categories]) /
          denominator[valid_categories]
        
        prevalence_pos <- mean(prevalence_values)
        
        # Category 2 = Monitoring
        prop_monitoring <-
          r[2] * prevalence_pos +
          r_[2] * (1 - prevalence_pos)
      }
      
      
      # -------------------------------------------------------
      # Restrictions
      # -------------------------------------------------------
      
      conditions <- list(
        
        # Monitoring must contain at least 10% of observations
        list(
          value = prop_monitoring,
          threshold = 0.10,
          op = ">="
        ),
        
        # High-risk category must contain at least 95% positives
        list(
          value = p[3],
          threshold = 0.95,
          op = ">="
        )
      )
      
      
      results <- list(
        prop_monitoring >= 0.10,
        p[3] >= 0.95
      )
      
      
      total_cond <- all(unlist(results))
      
      
      return(
        list(
          conditions = conditions,
          results = results,
          total_cond = total_cond
        )
      )
    }
    
    
    # =========================================================================
    # 4. RUN IN-THE-MODEL ON OUTER TRAINING SET
    # =========================================================================
    
    results_in <- tryCatch(
      
      InTheModel(
        df = dfTrain,
        kernelSVM = kernelSVM,
        num_cuts = num_cuts,
        cv_folds = cv_folds,
        calibrate = calibrate,
        vector_GPS = vector_GPS,
        restriction = restriction,
        user_condition = user_condition,
        scale_svm = TRUE,
        balance_classes = TRUE
      ),
      
      error = function(e) {
        
        message(
          "In-the-model failed in CTG split ",
          i,
          ": ",
          e$message
        )
        
        return(NULL)
      }
    )
    
    
    # =========================================================================
    # 5. CHECK VALID SOLUTION
    # =========================================================================
    
    if (
      is.null(results_in) ||
      length(results_in$thresholds) != 2 ||
      any(is.na(results_in$thresholds)) ||
      is.null(results_in$best_model_trained)
    ) {
      
      list_in_model[[i]] <- NULL
      
      next
    }
    
    
    # =========================================================================
    # 6. EXTERNAL TEST
    # =========================================================================
    
    model_in <- results_in$best_model_trained
    
    pred_in_test <- predict(
      model_in,
      newdata = df_test_no_target,
      probability = TRUE
    )
    
    probs_in_test <- attr(
      pred_in_test,
      "probabilities"
    )
    
    probs_in_test <- probs_in_test[, c("0", "1")]
    
    probs_in_test_class1 <- probs_in_test[, "1"]
    
    
    # IMPORTANT:
    # thresholds selected using outer TRAIN only;
    # same frozen thresholds are applied to external TEST
    
    ths_in <- results_in$thresholds
    
    results_external_test <- Case_FixedThreshold(
      df = dfTest,
      probs_model_train_class1 = probs_in_test_class1,
      ths = ths_in,
      GPS_p_values = vector_GPS
    )
    
    
    # =========================================================================
    # 7. SAVE RESULTS
    # =========================================================================
    
    list_in_model[[i]] <- list(
      
      split = i,
      seed = seed_vector[i],
      
      thresholds = ths_in,
      best_C = results_in$best_param_model,
      
      GPS_internal_train = results_in$GPS_train,
      GPS_internal_validation = results_in$GPS_test,
      error_internal_validation = results_in$error,
      
      CM_internal_validation = results_in$best_CM_test,
      
      GPS_test = results_external_test$GPS,
      CM_test = results_external_test$CM
    )
  }
  
  
  # ===========================================================================
  # 8. KEEP VALID SPLITS
  # ===========================================================================
  
  list_in_model_valid <- Filter(
    Negate(is.null),
    list_in_model
  )
  
  
  n_valid <- length(list_in_model_valid)
  n_failed <- n_seeds - n_valid
  if (n_valid == 0L) {
    stop("No valid CTG In-the-model runs. Inspect the split diagnostics and constraints.")
  }
  
  
  # ===========================================================================
  # 9. TEST-SAMPLE SUMMARY
  # ===========================================================================
  
  CM_test_mean <- Reduce(
    `+`,
    lapply(
      list_in_model_valid,
      function(x) x$CM_test
    )
  ) / n_valid
  
  
  CM_mean_test <- round(CM_test_mean)
  
  
  # Pathological proportion within each predicted category
  E_test <- do.call(
    rbind,
    lapply(
      list_in_model_valid,
      function(x) {
        
        CM <- x$CM_test
        
        row_totals <- rowSums(CM)
        
        prop <- CM[, 2] / row_totals
        
        prop[row_totals == 0] <- NA_real_
        
        prop
      }
    )
  )
  
  
  E_test_summary <- data.frame(
    
    key = colnames(E_test),
    
    mean = apply(E_test, 2, mean, na.rm = TRUE),
    
    sd = apply(E_test, 2, sd, na.rm = TRUE),
    
    median = apply(E_test, 2, median, na.rm = TRUE),
    
    n_valid = apply(
      E_test,
      2,
      function(x) sum(!is.na(x))
    )
  )
  
  
  E_test_summary$CI_inf <-
    E_test_summary$mean -
    qt(
      0.975,
      E_test_summary$n_valid - 1
    ) *
    E_test_summary$sd /
    sqrt(E_test_summary$n_valid)
  
  E_test_summary$CI_sup <-
    E_test_summary$mean +
    qt(
      0.975,
      E_test_summary$n_valid - 1
    ) *
    E_test_summary$sd /
    sqrt(E_test_summary$n_valid)
  
  
  # ===========================================================================
  # 10. OTHER USEFUL SUMMARIES
  # ===========================================================================
  
  thresholds_matrix <- do.call(
    rbind,
    lapply(
      list_in_model_valid,
      function(x) x$thresholds
    )
  )
  
  colnames(thresholds_matrix) <- c(
    "threshold_1",
    "threshold_2"
  )
  
  
  best_C <- sapply(
    list_in_model_valid,
    function(x) x$best_C
  )
  
  
  GPS_test <- sapply(
    list_in_model_valid,
    function(x) x$GPS_test
  )
  
  
  return(
    list(
      
      test_summary = list(
        E_test_summary = E_test_summary,
        CM_mean_test = CM_mean_test
      ),
      
      thresholds = thresholds_matrix,
      
      best_C = best_C,
      
      GPS_test = GPS_test,
      
      GPS_test_mean = mean(GPS_test, na.rm = TRUE),
      GPS_test_sd = sd(GPS_test, na.rm = TRUE),
      
      n_valid = n_valid,
      n_failed = n_failed,
      
      raw = list_in_model
    )
  )
}



VarianceSplitsPhishing <- function(df, seed_vector) {
  
  n_seeds <- length(seed_vector)
  
  
  # ===========================================================================
  # STORAGE
  # ===========================================================================
  
  # Benchmark methods
  list_binary          <- vector("list", n_seeds)
  list_equal_width     <- vector("list", n_seeds)
  list_iso_equal_width <- vector("list", n_seeds)
  list_equal_freq      <- vector("list", n_seeds)
  list_optimal         <- vector("list", n_seeds)
  list_roc             <- vector("list", n_seeds)
  
  # Proposed method: unrestricted
  list_proposed <- vector("list", n_seeds)
  
  # Proposed method: five constrained perspectives
  list_option1 <- vector("list", n_seeds)
  list_option2 <- vector("list", n_seeds)
  list_option3 <- vector("list", n_seeds)
  list_option4 <- vector("list", n_seeds)
  list_option5 <- vector("list", n_seeds)
  
  # Keep track of possible optimal-binning failures
  optimal_errors <- vector("list", n_seeds)
  
  
  # ===========================================================================
  # RANDOM SPLITS
  # ===========================================================================
  
  for (i in seq_len(n_seeds)) {
    
    cat(
      "\n====================================================\n",
      "Phishing split:", i, "\n",
      "====================================================\n"
    )
    
    
    # =========================================================================
    # 1. OUTER TRAIN-TEST SPLIT
    # =========================================================================
    
    set.seed(seed_vector[i])
    
    trainIndex <- caret::createDataPartition(
      df$class,
      p = 0.7,
      list = FALSE
    )
    
    dfTrain <- df[trainIndex, ]
    dfTest  <- df[-trainIndex, ]
    
    
    # =========================================================================
    # 2. RANDOM FOREST
    # =========================================================================
    
    # Class weights estimated ONLY from TRAIN
    n_neg <- sum(dfTrain$class == "0")
    n_pos <- sum(dfTrain$class == "1")
    
    weight_pos <- n_neg / n_pos
    
    class_weights <- c(
      "0" = 1,
      "1" = weight_pos
    )
    
    
    set.seed(1)
    
    model_rf <- ranger::ranger(
      class ~ .,
      data = dfTrain,
      probability = TRUE,
      num.trees = 500,
      class.weights = class_weights,
      seed = 1
    )
    
    
    # -------------------------------------------------------------------------
    # TRAIN probabilities:
    # OOB probabilities from ranger
    # -------------------------------------------------------------------------
    
    probs_model_train <- model_rf$predictions
    probs_model_train <- probs_model_train[, c("0", "1")]
    
    probs_model_train_class1 <- probs_model_train[, "1"]
    
    
    # -------------------------------------------------------------------------
    # TEST probabilities:
    # external held-out observations
    # -------------------------------------------------------------------------
    
    probs_model_test <- predict(
      model_rf,
      data = dfTest
    )$predictions
    
    probs_model_test <- probs_model_test[, c("0", "1")]
    
    probs_model_test_class1 <- probs_model_test[, "1"]
    
    
    # =========================================================================
    # COMMON GPS FOR 3-CATEGORY METHODS
    # =========================================================================
    
    vector_GPS <- c(
      "p_11",
      "p13",
      "r1_1",
      "r31"
    )
    
    
    # =========================================================================
    # E1. CLASSICAL BINARY: threshold = 0.5
    # =========================================================================
    
    ths_binary <- 0.5
    
    GPS_binary <- c(
      "p_11",
      "p12",
      "r1_1",
      "r21"
    )
    
    results_binary_train <- Case_FixedThreshold(
      dfTrain,
      probs_model_train_class1,
      ths_binary,
      GPS_binary
    )
    
    results_binary_test <- Case_FixedThreshold(
      dfTest,
      probs_model_test_class1,
      ths_binary,
      GPS_binary
    )
    
    list_binary[[i]] <- list(
      GPS_train = results_binary_train$GPS,
      CM_train  = results_binary_train$CM,
      ths       = ths_binary,
      GPS_test  = results_binary_test$GPS,
      CM_test   = results_binary_test$CM
    )
    
    
    # =========================================================================
    # E2. EQUAL-WIDTH BINNING
    # =========================================================================
    
    ths_equal_width <- c(1/3, 2/3)
    
    results_equal_width_train <- Case_FixedThreshold(
      dfTrain,
      probs_model_train_class1,
      ths_equal_width,
      vector_GPS
    )
    
    results_equal_width_test <- Case_FixedThreshold(
      dfTest,
      probs_model_test_class1,
      ths_equal_width,
      vector_GPS
    )
    
    list_equal_width[[i]] <- list(
      GPS_train = results_equal_width_train$GPS,
      CM_train  = results_equal_width_train$CM,
      ths       = ths_equal_width,
      GPS_test  = results_equal_width_test$GPS,
      CM_test   = results_equal_width_test$CM
    )
    
    
    # =========================================================================
    # E3. ISOTONIC CALIBRATION + EQUAL-WIDTH
    # =========================================================================
    
    class_train <- dfTrain$class
    
    # Positive class is the second column ("1")
    class_prin <- 2
    
    calibration_model <- CORElearn::calibrate(
      class_train,
      probs_model_train[, class_prin],
      class1 = class_prin,
      method = "isoReg",
      assumeProbabilities = TRUE
    )
    
    calibratedProbs_train <- CORElearn::applyCalibration(
      probs_model_train[, class_prin],
      calibration_model
    )
    
    calibratedProbs_test <- CORElearn::applyCalibration(
      probs_model_test[, class_prin],
      calibration_model
    )
    
    ths_iso <- c(1/3, 2/3)
    
    results_iso_train <- Case_FixedThreshold(
      dfTrain,
      calibratedProbs_train,
      ths_iso,
      vector_GPS
    )
    
    results_iso_test <- Case_FixedThreshold(
      dfTest,
      calibratedProbs_test,
      ths_iso,
      vector_GPS
    )
    
    list_iso_equal_width[[i]] <- list(
      GPS_train = results_iso_train$GPS,
      CM_train  = results_iso_train$CM,
      ths       = ths_iso,
      GPS_test  = results_iso_test$GPS,
      CM_test   = results_iso_test$CM
    )
    
    
    # =========================================================================
    # E4. EQUAL-FREQUENCY BINNING
    # =========================================================================
    
    K <- 3
    
    ths_equal_freq <- EqualFrequencyThresholds(
      probs_class1 = probs_model_train_class1,
      K = K
    )
    
    results_equal_freq_train <- Case_FixedThreshold(
      dfTrain,
      probs_model_train_class1,
      ths_equal_freq,
      vector_GPS
    )
    
    results_equal_freq_test <- Case_FixedThreshold(
      dfTest,
      probs_model_test_class1,
      ths_equal_freq,
      vector_GPS
    )
    
    list_equal_freq[[i]] <- list(
      GPS_train = results_equal_freq_train$GPS,
      CM_train  = results_equal_freq_train$CM,
      ths       = ths_equal_freq,
      GPS_test  = results_equal_freq_test$GPS,
      CM_test   = results_equal_freq_test$CM
    )
    
    
    # =========================================================================
    # E5. OPTIMAL BINNING
    # =========================================================================
    
    optimal_binning <- tryCatch(
      
      OptimalBinningThresholds(
        probs_class1 = probs_model_train_class1,
        class = dfTrain$class,
        K = 3
      ),
      
      error = function(e) {
        
        optimal_errors[[i]] <<- conditionMessage(e)
        
        NULL
      }
    )
    
    
    if (!is.null(optimal_binning)) {
      
      ths_optimal <- optimal_binning$thresholds
      
      results_optimal_train <- Case_FixedThreshold(
        dfTrain,
        probs_model_train_class1,
        ths_optimal,
        vector_GPS
      )
      
      results_optimal_test <- Case_FixedThreshold(
        dfTest,
        probs_model_test_class1,
        ths_optimal,
        vector_GPS
      )
      
      list_optimal[[i]] <- list(
        GPS_train = results_optimal_train$GPS,
        CM_train  = results_optimal_train$CM,
        ths       = ths_optimal,
        status    = optimal_binning$status,
        GPS_test  = results_optimal_test$GPS,
        CM_test   = results_optimal_test$CM
      )
      
    }
    
    
    # =========================================================================
    # E6. ROC-YOUDEN
    # =========================================================================
    
    roc_obj <- pROC::roc(
      response = dfTrain$class,
      predictor = probs_model_train_class1,
      levels = c("0", "1"),
      direction = "<",
      quiet = TRUE
    )
    
    youden_threshold <- as.numeric(
      pROC::coords(
        roc_obj,
        x = "best",
        best.method = "youden",
        ret = "threshold",
        transpose = FALSE
      )[1]
    )
    
    results_roc_train <- Case_FixedThreshold(
      dfTrain,
      probs_model_train_class1,
      youden_threshold,
      GPS_binary
    )
    
    results_roc_test <- Case_FixedThreshold(
      dfTest,
      probs_model_test_class1,
      youden_threshold,
      GPS_binary
    )
    
    list_roc[[i]] <- list(
      GPS_train = results_roc_train$GPS,
      CM_train  = results_roc_train$CM,
      ths       = youden_threshold,
      GPS_test  = results_roc_test$GPS,
      CM_test   = results_roc_test$CM
    )
    
    
    # =========================================================================
    # E7. AFTER-THE-MODEL: UNRESTRICTED
    # =========================================================================
    
    results_proposed_train <- AfterTheModel(
      dfTrain,
      probs_model_train,
      restriction = FALSE,
      user_condition = NULL,
      vector_GPS = vector_GPS,
      num_cuts = 2
    )
    
    ths_proposed <- results_proposed_train$thresholds
    
    results_proposed_test <- Case_FixedThreshold(
      dfTest,
      probs_model_test_class1,
      ths_proposed,
      vector_GPS
    )
    
    list_proposed[[i]] <- list(
      GPS_train = results_proposed_train$GPS,
      CM_train  = results_proposed_train$best_CM,
      ths       = ths_proposed,
      GPS_test  = results_proposed_test$GPS,
      CM_test   = results_proposed_test$CM
    )
    
    
    # =========================================================================
    # AFTER-THE-MODEL: FIVE FIXED OPERATIONAL PERSPECTIVES
    # =========================================================================
    
    n0 <- sum(dfTrain$class == "0")
    n1 <- sum(dfTrain$class == "1")
    n_train <- nrow(dfTrain)
    
    
    # -------------------------------------------------------------------------
    # Internal helper
    #
    # All restrictions are learned ONLY on TRAIN.
    # The thresholds selected on TRAIN are frozen and evaluated on TEST.
    # -------------------------------------------------------------------------
    
    run_after_scenario <- function(condition_fun, scenario_name) {
      
      results_train <- AfterTheModel(
        dfTrain,
        probs_model_train,
        restriction = TRUE,
        user_condition = condition_fun,
        vector_GPS = vector_GPS,
        num_cuts = 2
      )
      
      
      # Explicit check for infeasibility
      if (
        !is.numeric(results_train$GPS) ||
        length(results_train$GPS) != 1 ||
        !is.finite(results_train$GPS) ||
        length(results_train$thresholds) != 2 ||
        any(!is.finite(results_train$thresholds))
      ) {
        
        stop(
          paste0(
            "No feasible solution for ",
            scenario_name,
            " in Phishing split ",
            i,
            "."
          )
        )
      }
      
      
      ths <- results_train$thresholds
      
      results_test <- Case_FixedThreshold(
        dfTest,
        probs_model_test_class1,
        ths,
        vector_GPS
      )
      
      
      return(
        list(
          GPS_train = results_train$GPS,
          CM_train  = results_train$best_CM,
          ths       = ths,
          GPS_test  = results_test$GPS,
          CM_test   = results_test$CM
        )
      )
    }
    
    
    # =========================================================================
    # OPTION 1
    # Minimum operational size of Suspicious
    #
    # P(Suspicious) >= 10%
    # =========================================================================
    
    condition_option1 <- function(p, p_, r, r_) {
      
      prop_suspicious <- (
        r_[2] * n0 +
          r[2]  * n1
      ) / n_train
      
      isTRUE(prop_suspicious >= 0.10)
    }
    
    list_option1[[i]] <- run_after_scenario(
      condition_option1,
      "Option 1: Suspicious >= 10%"
    )
    
    
    # =========================================================================
    # OPTION 2
    # High-confidence phishing category
    #
    # P(Suspicious) >= 10%
    # P(Phishing | High) >= 98%
    # =========================================================================
    
    condition_option2 <- function(p, p_, r, r_) {
      
      prop_suspicious <- (
        r_[2] * n0 +
          r[2]  * n1
      ) / n_train
      
      isTRUE(prop_suspicious >= 0.10) &&
        isTRUE(p[3] >= 0.98)
    }
    
    list_option2[[i]] <- run_after_scenario(
      condition_option2,
      "Option 2: Suspicious >= 10% and P(Phishing | High) >= 98%"
    )
    
    
    # =========================================================================
    # OPTION 3
    # High-confidence legitimate category
    #
    # P(Suspicious) >= 10%
    # P(Legitimate | Low) >= 98%
    # =========================================================================
    
    condition_option3 <- function(p, p_, r, r_) {
      
      prop_suspicious <- (
        r_[2] * n0 +
          r[2]  * n1
      ) / n_train
      
      isTRUE(prop_suspicious >= 0.10) &&
        isTRUE(p_[1] >= 0.98)
    }
    
    list_option3[[i]] <- run_after_scenario(
      condition_option3,
      "Option 3: Suspicious >= 10% and P(Legitimate | Low) >= 98%"
    )
    
    
    # =========================================================================
    # OPTION 4
    # High-confidence decisions at BOTH extremes
    #
    # P(Suspicious) >= 10%
    # P(Legitimate | Low) >= 98%
    # P(Phishing | High) >= 98%
    # =========================================================================
    
    condition_option4 <- function(p, p_, r, r_) {
      
      prop_suspicious <- (
        r_[2] * n0 +
          r[2]  * n1
      ) / n_train
      
      isTRUE(prop_suspicious >= 0.10) &&
        isTRUE(p_[1] >= 0.98) &&
        isTRUE(p[3]  >= 0.98)
    }
    
    list_option4[[i]] <- run_after_scenario(
      condition_option4,
      paste0(
        "Option 4: Suspicious >= 10%, ",
        "P(Legitimate | Low) >= 98%, ",
        "P(Phishing | High) >= 98%"
      )
    )
    
    
    # =========================================================================
    # OPTION 5
    # Coverage-oriented phishing detection
    #
    # P(Suspicious) >= 10%
    # P(High | Phishing) >= 90%
    #
    # r[3] = proportion of ALL phishing observations assigned to High
    # =========================================================================
    
    condition_option5 <- function(p, p_, r, r_) {
      
      prop_suspicious <- (
        r_[2] * n0 +
          r[2]  * n1
      ) / n_train
      
      isTRUE(prop_suspicious >= 0.10) &&
        isTRUE(r[3] >= 0.90)
    }
    
    list_option5[[i]] <- run_after_scenario(
      condition_option5,
      "Option 5: Suspicious >= 10% and P(High | Phishing) >= 90%"
    )
    
    
    # =========================================================================
    # PRINT SELECTED THRESHOLDS
    # =========================================================================
    
    cat("\nSelected After-the-model thresholds:\n")
    
    cat(
      "Unrestricted:",
      paste(round(list_proposed[[i]]$ths, 3), collapse = " - "),
      "\n"
    )
    
    cat(
      "Option 1    :",
      paste(round(list_option1[[i]]$ths, 3), collapse = " - "),
      "\n"
    )
    
    cat(
      "Option 2    :",
      paste(round(list_option2[[i]]$ths, 3), collapse = " - "),
      "\n"
    )
    
    cat(
      "Option 3    :",
      paste(round(list_option3[[i]]$ths, 3), collapse = " - "),
      "\n"
    )
    
    cat(
      "Option 4    :",
      paste(round(list_option4[[i]]$ths, 3), collapse = " - "),
      "\n"
    )
    
    cat(
      "Option 5    :",
      paste(round(list_option5[[i]]$ths, 3), collapse = " - "),
      "\n"
    )
    
  }
  
  
  # ===========================================================================
  # SUMMARIES ACROSS THE RANDOM TRAIN-TEST SPLITS
  # ===========================================================================
  
  binary_summary <- variance_summary(
    list_binary
  )
  
  equal_width_summary <- variance_summary(
    list_equal_width
  )
  
  iso_equal_width_summary <- variance_summary(
    list_iso_equal_width
  )
  
  equal_freq_summary <- variance_summary(
    list_equal_freq
  )
  
  
  # ---------------------------------------------------------------------------
  # Optimal binning:
  # summarize only valid splits
  # ---------------------------------------------------------------------------
  
  valid_optimal <- Filter(
    Negate(is.null),
    list_optimal
  )
  
  optimal_summary <- if (length(valid_optimal) > 0) {
    variance_summary(valid_optimal)
  } else {
    NULL
  }
  
  
  roc_summary <- variance_summary(
    list_roc
  )
  
  
  # ---------------------------------------------------------------------------
  # Proposed method: unrestricted
  # ---------------------------------------------------------------------------
  
  proposed_summary <- variance_summary(
    list_proposed
  )
  
  
  # ---------------------------------------------------------------------------
  # Proposed method: five perspectives
  # ---------------------------------------------------------------------------
  
  option1_summary <- variance_summary(
    list_option1
  )
  
  option2_summary <- variance_summary(
    list_option2
  )
  
  option3_summary <- variance_summary(
    list_option3
  )
  
  option4_summary <- variance_summary(
    list_option4
  )
  
  option5_summary <- variance_summary(
    list_option5
  )
  
  
  # ===========================================================================
  # RETURN
  # ===========================================================================
  
  return(
    list(
      
      # ---------------------------------------------------------------
      # Benchmarks
      # ---------------------------------------------------------------
      
      binary_summary = binary_summary,
      equal_width_summary = equal_width_summary,
      iso_equal_width_summary = iso_equal_width_summary,
      equal_freq_summary = equal_freq_summary,
      optimal_summary = optimal_summary,
      roc_summary = roc_summary,
      
      
      # ---------------------------------------------------------------
      # Proposed unrestricted
      # ---------------------------------------------------------------
      
      proposed_summary = proposed_summary,
      
      
      # ---------------------------------------------------------------
      # Proposed constrained scenarios
      # ---------------------------------------------------------------
      
      option1_summary = option1_summary,
      option2_summary = option2_summary,
      option3_summary = option3_summary,
      option4_summary = option4_summary,
      option5_summary = option5_summary,
      
      
      # ---------------------------------------------------------------
      # Optimal-binning diagnostics
      # ---------------------------------------------------------------
      
      optimal_n_valid = length(valid_optimal),
      
      optimal_failed_splits = which(
        vapply(
          list_optimal,
          is.null,
          logical(1)
        )
      ),
      
      optimal_errors = optimal_errors,
      
      
      # ---------------------------------------------------------------
      # Raw results from every split
      # ---------------------------------------------------------------
      
      raw = list(
        
        binary = list_binary,
        equal_width = list_equal_width,
        iso_equal_width = list_iso_equal_width,
        equal_freq = list_equal_freq,
        optimal = list_optimal,
        roc = list_roc,
        
        proposed = list_proposed,
        
        option1 = list_option1,
        option2 = list_option2,
        option3 = list_option3,
        option4 = list_option4,
        option5 = list_option5
      )
    )
  )
}



############################################################################
#### CREDITCARD

VarianceSplitsCreditSOTA <- function(df, seed_vector) {
  
  # ===========================================================================
  # PREPARATION
  # ===========================================================================
  
  # Ensure binary target is treated as a classification factor
  df$class <- factor(
    as.character(df$class),
    levels = c("0", "1")
  )
  
  n_seeds <- length(seed_vector)
  
  
  # ===========================================================================
  # STORAGE
  # ===========================================================================
  
  list_binary          <- vector("list", n_seeds)
  list_equal_width     <- vector("list", n_seeds)
  list_iso_equal_width <- vector("list", n_seeds)
  list_equal_freq      <- vector("list", n_seeds)
  list_optimal         <- vector("list", n_seeds)
  list_roc             <- vector("list", n_seeds)
  
  equal_freq_errors <- vector("list", n_seeds)
  optimal_errors    <- vector("list", n_seeds)
  
  
  # ===========================================================================
  # RANDOM TRAIN-TEST SPLITS
  # ===========================================================================
  
  for (i in seq_len(n_seeds)) {
    
    cat(
      "\n====================================================\n",
      "Credit-card split:", i, "\n",
      "====================================================\n"
    )
    
    
    # =========================================================================
    # 1. OUTER STRATIFIED 70/30 SPLIT
    # =========================================================================
    
    set.seed(seed_vector[i])
    
    trainIndex <- caret::createDataPartition(
      df$class,
      p = 0.70,
      list = FALSE
    )
    
    dfTrain <- df[trainIndex, ]
    dfTest  <- df[-trainIndex, ]
    
    
    # Keep identical factor levels
    dfTrain$class <- factor(
      dfTrain$class,
      levels = c("0", "1")
    )
    
    dfTest$class <- factor(
      dfTest$class,
      levels = c("0", "1")
    )
    
    
    # =========================================================================
    # CLASS DISTRIBUTION
    # =========================================================================
    
    n_neg <- sum(dfTrain$class == "0")
    n_pos <- sum(dfTrain$class == "1")
    
    cat(
      "TRAIN:",
      nrow(dfTrain),
      "| Legitimate:", n_neg,
      "| Fraud:", n_pos,
      "| Fraud prevalence:",
      round(100 * n_pos / nrow(dfTrain), 4),
      "%\n"
    )
    
    
    # =========================================================================
    # 2. RANDOM FOREST
    # =========================================================================
    
    # Class weights estimated ONLY from TRAIN
    weight_pos <- n_neg / n_pos
    
    class_weights <- setNames(
      c(1, weight_pos),
      c("0", "1")
    )
    
    cat(
      "Fraud class weight:",
      round(weight_pos, 2),
      "\n"
    )
    
    
    set.seed(1)
    
    model_rf <- ranger::ranger(
      class ~ .,
      data = dfTrain,
      probability = TRUE,
      num.trees = 500,
      class.weights = class_weights,
      seed = 1
    )
    
    
    # =========================================================================
    # TRAIN PROBABILITIES: OUT-OF-BAG
    # =========================================================================
    
    probs_model_train <- model_rf$predictions
    
    probs_model_train <- probs_model_train[
      ,
      c("0", "1")
    ]
    
    probs_model_train_class1 <-
      probs_model_train[, "1"]
    
    
    # =========================================================================
    # TEST PROBABILITIES: EXTERNAL HELD-OUT SET
    # =========================================================================
    
    probs_model_test <- predict(
      model_rf,
      data = dfTest
    )$predictions
    
    probs_model_test <- probs_model_test[
      ,
      c("0", "1")
    ]
    
    probs_model_test_class1 <-
      probs_model_test[, "1"]
    
    
    # =========================================================================
    # GPS DEFINITIONS
    # =========================================================================
    
    # Binary 2x2
    GPS_binary <- c(
      "p_11",
      "p12",
      "r1_1",
      "r21"
    )
    
    
    # Multicategory 3x2
    GPS_three <- c(
      "p_11",
      "p13",
      "r1_1",
      "r31"
    )
    
    
    # =========================================================================
    # E1. CLASSICAL BINARY CLASSIFICATION
    # threshold = 0.5
    # =========================================================================
    
    ths_binary <- 0.5
    
    results_binary_train <- Case_FixedThreshold(
      dfTrain,
      probs_model_train_class1,
      ths_binary,
      GPS_binary
    )
    
    results_binary_test <- Case_FixedThreshold(
      dfTest,
      probs_model_test_class1,
      ths_binary,
      GPS_binary
    )
    
    list_binary[[i]] <- list(
      GPS_train = results_binary_train$GPS,
      CM_train  = results_binary_train$CM,
      ths       = ths_binary,
      GPS_test  = results_binary_test$GPS,
      CM_test   = results_binary_test$CM
    )
    
    
    # =========================================================================
    # E2. EQUAL-WIDTH BINNING
    # K = 3
    # =========================================================================
    
    ths_equal_width <- c(
      1/3,
      2/3
    )
    
    results_equal_width_train <- Case_FixedThreshold(
      dfTrain,
      probs_model_train_class1,
      ths_equal_width,
      GPS_three
    )
    
    results_equal_width_test <- Case_FixedThreshold(
      dfTest,
      probs_model_test_class1,
      ths_equal_width,
      GPS_three
    )
    
    list_equal_width[[i]] <- list(
      GPS_train = results_equal_width_train$GPS,
      CM_train  = results_equal_width_train$CM,
      ths       = ths_equal_width,
      GPS_test  = results_equal_width_test$GPS,
      CM_test   = results_equal_width_test$CM
    )
    
    
    # =========================================================================
    # E3. ISOTONIC CALIBRATION + EQUAL-WIDTH BINNING
    #
    # Calibration fitted ONLY on TRAIN OOB probabilities
    # =========================================================================
    
    class_train <- dfTrain$class
    
    # Second probability column = class "1" = Fraud
    class_prin <- 2
    
    calibration_model <- CORElearn::calibrate(
      class_train,
      probs_model_train[, class_prin],
      class1 = class_prin,
      method = "isoReg",
      assumeProbabilities = TRUE
    )
    
    calibratedProbs_train <- CORElearn::applyCalibration(
      probs_model_train[, class_prin],
      calibration_model
    )
    
    calibratedProbs_test <- CORElearn::applyCalibration(
      probs_model_test[, class_prin],
      calibration_model
    )
    
    ths_iso <- c(
      1/3,
      2/3
    )
    
    results_iso_train <- Case_FixedThreshold(
      dfTrain,
      calibratedProbs_train,
      ths_iso,
      GPS_three
    )
    
    results_iso_test <- Case_FixedThreshold(
      dfTest,
      calibratedProbs_test,
      ths_iso,
      GPS_three
    )
    
    list_iso_equal_width[[i]] <- list(
      GPS_train = results_iso_train$GPS,
      CM_train  = results_iso_train$CM,
      ths       = ths_iso,
      GPS_test  = results_iso_test$GPS,
      CM_test   = results_iso_test$CM
    )
    
    
    # =========================================================================
    # E4. EQUAL-FREQUENCY BINNING
    #
    # Thresholds estimated ONLY from TRAIN probabilities.
    # May be infeasible because of tied probabilities.
    # =========================================================================
    
    K <- 3
    
    equal_freq_result <- tryCatch(
      
      {
        
        ths_equal_freq <- EqualFrequencyThresholds(
          probs_class1 = probs_model_train_class1,
          K = K
        )
        
        results_equal_freq_train <- Case_FixedThreshold(
          dfTrain,
          probs_model_train_class1,
          ths_equal_freq,
          GPS_three
        )
        
        results_equal_freq_test <- Case_FixedThreshold(
          dfTest,
          probs_model_test_class1,
          ths_equal_freq,
          GPS_three
        )
        
        list(
          GPS_train = results_equal_freq_train$GPS,
          CM_train  = results_equal_freq_train$CM,
          ths       = ths_equal_freq,
          GPS_test  = results_equal_freq_test$GPS,
          CM_test   = results_equal_freq_test$CM
        )
      },
      
      error = function(e) {
        
        equal_freq_errors[[i]] <<- conditionMessage(e)
        
        NULL
      }
    )
    
    
    # IMPORTANT:
    # Do NOT assign NULL with list_equal_freq[[i]] <- NULL,
    # because that removes the list element.
    
    if (!is.null(equal_freq_result)) {
      
      list_equal_freq[[i]] <- equal_freq_result
      
      cat(
        "Equal-frequency thresholds:",
        paste(
          round(equal_freq_result$ths, 5),
          collapse = " - "
        ),
        "\n"
      )
      
    } else {
      
      cat(
        "Equal-frequency: NOT FEASIBLE -",
        equal_freq_errors[[i]],
        "\n"
      )
    }
    
    
    # =========================================================================
    # E5. OPTIMAL BINNING
    #
    # Fitted ONLY on TRAIN
    # =========================================================================
    
    optimal_binning <- tryCatch(
      
      {
        
        OptimalBinningThresholds(
          probs_class1 = probs_model_train_class1,
          class = dfTrain$class,
          K = 3
        )
      },
      
      error = function(e) {
        
        optimal_errors[[i]] <<- conditionMessage(e)
        
        NULL
      }
    )
    
    
    if (!is.null(optimal_binning)) {
      
      ths_optimal <- optimal_binning$thresholds
      
      results_optimal_train <- Case_FixedThreshold(
        dfTrain,
        probs_model_train_class1,
        ths_optimal,
        GPS_three
      )
      
      results_optimal_test <- Case_FixedThreshold(
        dfTest,
        probs_model_test_class1,
        ths_optimal,
        GPS_three
      )
      
      list_optimal[[i]] <- list(
        GPS_train = results_optimal_train$GPS,
        CM_train  = results_optimal_train$CM,
        ths       = ths_optimal,
        status    = optimal_binning$status,
        GPS_test  = results_optimal_test$GPS,
        CM_test   = results_optimal_test$CM
      )
      
      cat(
        "Optimal-binning thresholds:",
        paste(
          round(ths_optimal, 5),
          collapse = " - "
        ),
        "\n"
      )
      
    } else {
      
      cat(
        "Optimal binning: NOT FEASIBLE",
        if (!is.null(optimal_errors[[i]])) {
          paste0(" - ", optimal_errors[[i]])
        } else {
          ""
        },
        "\n"
      )
    }
    
    
    # =========================================================================
    # E6. ROC-YOUDEN
    #
    # Threshold estimated ONLY from TRAIN
    # =========================================================================
    
    roc_obj <- pROC::roc(
      response = dfTrain$class,
      predictor = probs_model_train_class1,
      levels = c("0", "1"),
      direction = "<",
      quiet = TRUE
    )
    
    youden_threshold <- as.numeric(
      pROC::coords(
        roc_obj,
        x = "best",
        best.method = "youden",
        ret = "threshold",
        transpose = FALSE
      )[1]
    )
    
    
    results_roc_train <- Case_FixedThreshold(
      dfTrain,
      probs_model_train_class1,
      youden_threshold,
      GPS_binary
    )
    
    results_roc_test <- Case_FixedThreshold(
      dfTest,
      probs_model_test_class1,
      youden_threshold,
      GPS_binary
    )
    
    list_roc[[i]] <- list(
      GPS_train = results_roc_train$GPS,
      CM_train  = results_roc_train$CM,
      ths       = youden_threshold,
      GPS_test  = results_roc_test$GPS,
      CM_test   = results_roc_test$CM
    )
    
    
    cat(
      "ROC-Youden threshold:",
      round(youden_threshold, 5),
      "\n"
    )
    
    
    # =========================================================================
    # OPTIONAL DIAGNOSTIC INFORMATION
    # =========================================================================
    
    cat(
      "Fraction of OOB TRAIN probabilities equal to zero:",
      round(
        mean(probs_model_train_class1 == 0),
        4
      ),
      "\n"
    )
    
    cat(
      "Number of unique OOB TRAIN probabilities:",
      length(
        unique(probs_model_train_class1)
      ),
      "\n"
    )
    
    
  }   # END OF RANDOM-SPLIT LOOP
  
  
  # ===========================================================================
  # SUMMARIES ACROSS THE RANDOM TRAIN-TEST SPLITS
  # ===========================================================================
  
  binary_summary <- variance_summary(
    list_binary
  )
  
  
  equal_width_summary <- variance_summary(
    list_equal_width
  )
  
  
  iso_equal_width_summary <- variance_summary(
    list_iso_equal_width
  )
  
  
  # ===========================================================================
  # EQUAL-FREQUENCY
  # Summarize only valid splits
  # ===========================================================================
  
  valid_equal_freq <- Filter(
    Negate(is.null),
    list_equal_freq
  )
  
  equal_freq_summary <- if (
    length(valid_equal_freq) > 0
  ) {
    
    variance_summary(
      valid_equal_freq
    )
    
  } else {
    
    NULL
  }
  
  
  # ===========================================================================
  # OPTIMAL BINNING
  # Summarize only valid splits
  # ===========================================================================
  
  valid_optimal <- Filter(
    Negate(is.null),
    list_optimal
  )
  
  optimal_summary <- if (
    length(valid_optimal) > 0
  ) {
    
    variance_summary(
      valid_optimal
    )
    
  } else {
    
    NULL
  }
  
  
  # ===========================================================================
  # ROC-YOUDEN
  # ===========================================================================
  
  roc_summary <- variance_summary(
    list_roc
  )
  
  
  # ===========================================================================
  # RETURN
  # ===========================================================================
  
  return(
    list(
      
      # -----------------------------------------------------------------------
      # Benchmark summaries
      # -----------------------------------------------------------------------
      
      binary_summary =
        binary_summary,
      
      equal_width_summary =
        equal_width_summary,
      
      iso_equal_width_summary =
        iso_equal_width_summary,
      
      equal_freq_summary =
        equal_freq_summary,
      
      optimal_summary =
        optimal_summary,
      
      roc_summary =
        roc_summary,
      
      
      # -----------------------------------------------------------------------
      # Equal-frequency diagnostics
      # -----------------------------------------------------------------------
      
      equal_freq_n_valid =
        length(valid_equal_freq),
      
      equal_freq_failed_splits =
        which(
          vapply(
            list_equal_freq,
            is.null,
            logical(1)
          )
        ),
      
      equal_freq_errors =
        equal_freq_errors,
      
      
      # -----------------------------------------------------------------------
      # Optimal-binning diagnostics
      # -----------------------------------------------------------------------
      
      optimal_n_valid =
        length(valid_optimal),
      
      optimal_failed_splits =
        which(
          vapply(
            list_optimal,
            is.null,
            logical(1)
          )
        ),
      
      optimal_errors =
        optimal_errors,
      
      
      # -----------------------------------------------------------------------
      # Raw results
      # -----------------------------------------------------------------------
      
      raw = list(
        
        binary =
          list_binary,
        
        equal_width =
          list_equal_width,
        
        iso_equal_width =
          list_iso_equal_width,
        
        equal_freq =
          list_equal_freq,
        
        optimal =
          list_optimal,
        
        roc =
          list_roc
      )
    )
  )
}


VarianceSplitsCreditAfter <- function(df, seed_vector) {
  
  n_seeds <- length(seed_vector)
  
  
  # ========================================================================
  # STORAGE
  # ========================================================================
  
  list_option1 <- vector("list", n_seeds)
  list_option2 <- vector("list", n_seeds)
  list_option3 <- vector("list", n_seeds)
  list_option4 <- vector("list", n_seeds)
  list_option5 <- vector("list", n_seeds)
  list_clean_low <- vector("list", n_seeds)
  list_uncertainty <- vector("list", n_seeds)
  
  
  # ========================================================================
  # GPS DEFINITIONS
  # ========================================================================
  
  GPS_base <- c("p_11","p13","r1_1","r31")
  GPS_clean <- c("p12","p_12","r1_1","p13","r31")
  GPS_uncertainty <- c("p_11","p12","p_12","p13","r31")
  
  # ========================================================================
  # HELPER: valid After-the-model result?
  # ========================================================================
  
  valid_after <- function(x) {
    
    is.list(x) &&
      is.numeric(x$GPS) &&
      length(x$GPS) == 1 &&
      is.finite(x$GPS) &&
      length(x$thresholds) == 2 &&
      all(is.finite(x$thresholds))
  }
  
  
  # ========================================================================
  # HELPER: fit TRAIN and evaluate frozen thresholds on TEST
  # ========================================================================
  
  run_case <- function(dfTrain,
                       dfTest,
                       probs_train,
                       probs_test_class1,
                       vector_GPS,
                       user_condition) {
    
    result_train <- suppressWarnings(
      Case_MaxGPS_Restrictions(
        dfTrain,
        probs_train,
        restriction = TRUE,
        user_condition = user_condition,
        vector_GPS = vector_GPS,
        num_cuts = 2
      )
    )
    
    
    # Not feasible
    if (!valid_after(result_train)) {
      
      return(
        list(
          feasible = FALSE,
          GPS_train = NA_real_,
          CM_train = NULL,
          ths = c(NA_real_, NA_real_),
          GPS_test = NA_real_,
          CM_test = NULL
        )
      )
    }
    
    
    # --------------------------------------------------------
    # TEST: thresholds learned ONLY on TRAIN
    # --------------------------------------------------------
    
    result_test <- Case_FixedThreshold(
      dfTest,
      probs_test_class1,
      result_train$thresholds,
      vector_GPS
    )
    
    
    list(
      feasible = TRUE,
      GPS_train = result_train$GPS,
      CM_train = result_train$best_CM,
      ths = result_train$thresholds,
      GPS_test = result_test$GPS,
      CM_test = result_test$CM
    )
  }
  
  
  # ========================================================================
  # REPEATED SPLITS
  # ========================================================================
  
  for (i in seq_along(seed_vector)) {
    
    cat(
      "\n========================================\n",
      "Split", i, "of", n_seeds,
      "- seed =", seed_vector[i],
      "\n========================================\n"
    )
    
    
    # ----------------------------------------------------------------------
    # OUTER 70/30 STRATIFIED SPLIT
    # ----------------------------------------------------------------------
    
    set.seed(seed_vector[i])
    
    trainIndex <- caret::createDataPartition(
      df$class,
      p = 0.70,
      list = FALSE
    )
    
    dfTrain <- df[trainIndex, ]
    dfTest  <- df[-trainIndex, ]
    
    dfTrain$class <- factor(
      dfTrain$class,
      levels = c("0", "1")
    )
    
    dfTest$class <- factor(
      dfTest$class,
      levels = c("0", "1")
    )
    
    
    # ----------------------------------------------------------------------
    # CLASS COUNTS
    # ----------------------------------------------------------------------
    
    n0 <- sum(dfTrain$class == "0")
    n1 <- sum(dfTrain$class == "1")
    n_train <- nrow(dfTrain)
    
    
    # ----------------------------------------------------------------------
    # RANDOM FOREST
    # ----------------------------------------------------------------------
    
    class_weights <- c(
      "0" = 1,
      "1" = n0 / n1
    )
    
    set.seed(1)
    
    model_rf <- ranger::ranger(
      class ~ .,
      data = dfTrain,
      probability = TRUE,
      num.trees = 500,
      class.weights = class_weights,
      seed = 1
    )
    
    
    # TRAIN: OOB probabilities
    probs_train <- model_rf$predictions[
      ,
      c("0", "1")
    ]
    
    
    # TEST: external probabilities
    probs_test <- predict(
      model_rf,
      data = dfTest
    )$predictions
    
    probs_test <- probs_test[
      ,
      c("0", "1")
    ]
    
    probs_test_class1 <- probs_test[, "1"]
    
    
    # ======================================================================
    # OPTION 1
    # Suspicious >= 0.10%
    # ======================================================================
    
    condition1 <- function(p, p_, r, r_) {
      
      prop_suspicious <-
        (r_[2] * n0 + r[2] * n1) / n_train
      
      isTRUE(
        prop_suspicious >= 0.001
      )
    }
    
    list_option1[[i]] <- run_case(
      dfTrain,
      dfTest,
      probs_train,
      probs_test_class1,
      GPS_base,
      condition1
    )
    
    
    # ======================================================================
    # OPTION 2
    # High purity >= 95%
    # ======================================================================
    
    condition2 <- function(p, p_, r, r_) {
      
      isTRUE(
        p[3] >= 0.95
      )
    }
    
    list_option2[[i]] <- run_case(
      dfTrain,
      dfTest,
      probs_train,
      probs_test_class1,
      GPS_base,
      condition2
    )
    
    
    # ======================================================================
    # OPTION 3
    # High captures >= 85% of fraud
    # ======================================================================
    
    condition3 <- function(p, p_, r, r_) {
      
      isTRUE(
        r[3] >= 0.85
      )
    }
    
    list_option3[[i]] <- run_case(
      dfTrain,
      dfTest,
      probs_train,
      probs_test_class1,
      GPS_base,
      condition3
    )
    
    
    # ======================================================================
    # OPTION 4
    # High purity >= 90%
    # High fraud coverage >= 80%
    # ======================================================================
    
    condition4 <- function(p, p_, r, r_) {
      
      isTRUE(
        p[3] >= 0.90 &
          r[3] >= 0.80
      )
    }
    
    list_option4[[i]] <- run_case(
      dfTrain,
      dfTest,
      probs_train,
      probs_test_class1,
      GPS_base,
      condition4
    )
    
    
    # ======================================================================
    # OPTION 5
    # <= 15% of fraud may remain in Low
    # ======================================================================
    
    condition5 <- function(p, p_, r, r_) {
      
      isTRUE(
        r[1] <= 0.15
      )
    }
    
    list_option5[[i]] <- run_case(
      dfTrain,
      dfTest,
      probs_train,
      probs_test_class1,
      GPS_base,
      condition5
    )
    
    
    # ======================================================================
    # CLEAN LOW
    #
    # Low almost fraud-free
    # Suspicious genuinely mixed
    # Suspicious non-residual
    # High >= 95% fraud
    # High non-residual
    # ======================================================================
    
    condition_clean <- function(p, p_, r, r_) {
      
      prop_suspicious <-
        (r_[2] * n0 + r[2] * n1) / n_train
      
      prop_high <-
        (r_[3] * n0 + r[3] * n1) / n_train
      
      
      isTRUE(
        p[1] <= 0.0003 &
          
          p[2] >= 0.25 &
          p[2] <= 0.75 &
          
          prop_suspicious >= 0.001 &
          
          p[3] >= 0.95 &
          
          prop_high >= 0.0005
      )
    }
    
    list_clean_low[[i]] <- run_case(
      dfTrain,
      dfTest,
      probs_train,
      probs_test_class1,
      GPS_clean,
      condition_clean
    )
    
    
    # ======================================================================
    # UNCERTAINTY-ORIENTED GPS
    #
    # No direct constraint on p[2].
    # We let p12 + p_12 in the GPS create the uncertainty category.
    # ======================================================================
    
    condition_uncertainty <- function(p, p_, r, r_) {
      
      prop_suspicious <-
        (r_[2] * n0 + r[2] * n1) / n_train
      
      prop_high <-
        (r_[3] * n0 + r[3] * n1) / n_train
      
      
      isTRUE(
        p[1] <= 0.0003 &
          
          prop_suspicious >= 0.001 &
          
          p[3] >= 0.90 &
          
          prop_high >= 0.0005
      )
    }
    
    list_uncertainty[[i]] <- run_case(
      dfTrain,
      dfTest,
      probs_train,
      probs_test_class1,
      GPS_uncertainty,
      condition_uncertainty
    )
  }
  
  
  # ========================================================================
  # RETURN RAW RESULTS
  # ========================================================================
  
  list(
    option1 = list_option1,
    option2 = list_option2,
    option3 = list_option3,
    option4 = list_option4,
    option5 = list_option5,
    clean_low = list_clean_low,
    uncertainty = list_uncertainty
  )
}