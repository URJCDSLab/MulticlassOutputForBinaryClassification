################################################################################
# RunningTimes_After_vs_In.R
#
#
# Full study (expensive; original microbenchmark used 100 repetitions):
#   x <- run_running_times(repetitions = 100,
#                          output_csv = 'Results/running_times.csv')
################################################################################

source('GeneralAndMethodFunctions.R')

running_time_cases <- function() {
  c(
    simulated_3         = 'Simulated - 3 categories',
    simulated_2         = 'Simulated - 2 categories',
    mammographic        = 'Mammographic - 4 categories',
    pima                = 'Pima - 5 categories',
    ctg                 = 'CTG - 3 categories',
    phishing            = 'Phishing - 3 categories',
    credit_min_suspicious = 'Credit Card - Minimum suspicious',
    credit_high_purity    = 'Credit Card - High-purity High',
    credit_uncertainty    = 'Credit Card - Uncertainty'
  )
}

# Return the structured constraint object expected by InTheModel().
# Each condition is list(value = numeric, threshold = numeric, op = '>'/'<').
make_in_constraint <- function(condition_builder) {
  force(condition_builder)
  function(p, p_, r, r_) {
    conditions <- condition_builder(p, p_, r, r_)
    results <- lapply(conditions, function(cond) {
      if (!is.finite(cond$value)) return(FALSE)
      switch(cond$op,
             '>'  = cond$value >  cond$threshold,
             '>=' = cond$value >= cond$threshold,
             '<'  = cond$value <  cond$threshold,
             '<=' = cond$value <= cond$threshold,
             stop('Unsupported constraint operator: ', cond$op))
    })
    list(conditions = conditions,
         results = results,
         total_cond = all(unlist(results)))
  }
}

# Optimisation specifications are the same as Revision2.
# For CTG, Phishing and Credit Card Fraud only After-the-model was timed.
get_running_time_spec <- function(case, df_train) {
  if (case == 'simulated_3') {
    return(list(num_cuts = 2, gps = c('p13', 'p_11', 'r31'),
      after_condition = function(p, p_, r, r_) {
        isTRUE(p[3] > 0.8 && p_[1] > 0.9 && r_[1] > 0.4)
      },
      in_condition = make_in_constraint(function(p, p_, r, r_) list(
        list(value = p_[1], threshold = 0.9, op = '>'),
        list(value = r_[1], threshold = 0.4, op = '>'),
        list(value = p[3], threshold = 0.8, op = '>')
      ))))
  }

  if (case == 'simulated_2') {
    return(list(num_cuts = 1, gps = c('p12', 'p_11', 'r21', 'r1_1'),
      after_condition = function(p, p_, r, r_) isTRUE(p_[1] > 0.85),
      in_condition = make_in_constraint(function(p, p_, r, r_) list(
        list(value = p_[1], threshold = 0.85, op = '>')
      ))))
  }

  if (case == 'mammographic') {
    return(list(num_cuts = 3, gps = c('p14', 'p_11', 'r1_1'),
      after_condition = function(p, p_, r, r_) {
        isTRUE(r_[1] > 0.5 && r_[2] > 0.1 &&
               p[3] > 0.7 && p[3] < 0.85 &&
               p[4] > 0.85 && p_[1] > 0.8)
      },
      in_condition = make_in_constraint(function(p, p_, r, r_) list(
        list(value = r_[1], threshold = 0.5, op = '>'),
        list(value = p_[1], threshold = 0.8, op = '>'),
        list(value = r_[2], threshold = 0.1, op = '>'),
        list(value = p[3], threshold = 0.7, op = '>'),
        list(value = p[3], threshold = 0.85, op = '<'),
        list(value = p[4], threshold = 0.85, op = '>')
      ))))
  }

  if (case == 'pima') {
    return(list(num_cuts = 4, gps = c('p15', 'p_11'),
      after_condition = function(p, p_, r, r_) {
        isTRUE(r_[1] > 0.3 && p[4] > 0.7 &&
               p[3] > 0.4 && p[5] > 0.9)
      },
      in_condition = make_in_constraint(function(p, p_, r, r_) list(
        list(value = r_[1], threshold = 0.3, op = '>'),
        list(value = p[4], threshold = 0.7, op = '>'),
        list(value = p[5], threshold = 0.9, op = '>'),
        list(value = p[3], threshold = 0.4, op = '>')
      ))))
  }

  if (case == 'ctg' || case == 'phishing') {
    n0 <- sum(df_train$class == '0')
    n1 <- sum(df_train$class == '1')
    n  <- nrow(df_train)
    return(list(num_cuts = 2,
      gps = c('p_11', 'p13', 'r1_1', 'r31'),
      after_condition = function(p, p_, r, r_) {
        prop_monitoring <- (r_[2] * n0 + r[2] * n1) / n
        isTRUE(prop_monitoring >= 0.10)
      },
      in_condition = NULL))
  }

  if (startsWith(case, 'credit_')) {
    n0 <- sum(df_train$class == '0')
    n1 <- sum(df_train$class == '1')
    n  <- nrow(df_train)
    prop_category <- function(j, r, r_) (r_[j] * n0 + r[j] * n1) / n

    if (case == 'credit_min_suspicious') {
      return(list(num_cuts = 2, gps = c('p_11', 'p13', 'r1_1', 'r31'),
        after_condition = function(p, p_, r, r_) {
          isTRUE(prop_category(2, r, r_) >= 0.001)
        }, in_condition = NULL))
    }
    if (case == 'credit_high_purity') {
      return(list(num_cuts = 2, gps = c('p_11', 'p13', 'r1_1', 'r31'),
        after_condition = function(p, p_, r, r_) isTRUE(p[3] >= 0.95),
        in_condition = NULL))
    }
    if (case == 'credit_uncertainty') {
      return(list(num_cuts = 2,
        gps = c('p_11', 'p12', 'p_12', 'p13', 'r31'),
        after_condition = function(p, p_, r, r_) {
          isTRUE(p[1] <= 0.0003 &&
                 prop_category(2, r, r_) >= 0.001 &&
                 p[3] >= 0.90 &&
                 prop_category(3, r, r_) >= 0.0005)
        }, in_condition = NULL))
    }
  }
  stop('Unknown case: ', case)
}

# Preprocessing and initial model fitting take place OUTSIDE microbenchmark.
# A single preparation is reused for the three Credit Card conditions.
prepare_running_time_data <- function(dataset, data_dir = 'datasets') {
  file_in_data <- function(filename) {
    path <- file.path(data_dir, filename)
    if (!file.exists(path)) stop('Dataset not found: ', path)
    path
  }
  split_train <- function(df, seed) {
    set.seed(seed)
    ind <- caret::createDataPartition(df$class, p = 0.70, list = FALSE)
    df[ind, , drop = FALSE]
  }
  svm_train_probabilities <- function(df, kernel, seed = 1, scale_tuning = FALSE) {
    set.seed(seed)
    tuned <- e1071::tune('svm', class ~ ., data = df,
      kernel = kernel, scale = scale_tuning,
      ranges = list(cost = c(0.001, 0.01, 0.1, 1, 5, 10,
                             20, 50, 100, 150, 200)))
    cost <- tuned$best.model$cost
    set.seed(seed)
    model <- e1071::svm(class ~ ., data = df,
      kernel = kernel, scale = FALSE, probability = TRUE,
      type = 'C', cost = cost)
    pred <- predict(model, df[, setdiff(names(df), 'class'), drop = FALSE],
                    probability = TRUE)
    prob <- attr(pred, 'probabilities')[, c('0', '1'), drop = FALSE]
    list(probs = prob, kernel = kernel)
  }

  if (dataset == 'simulated') {
    set.seed(2812)
    a1 <- rnorm(300, 1, 0.25)
    a2 <- rnorm(30, 2, 0.15)
    b1 <- rnorm(300, 1.5, 0.25)
    set.seed(12)
    a12 <- rnorm(300, 2.5, 0.25)
    a22 <- rnorm(30, 1.5, 0.15)
    b12 <- rnorm(300, 2.5, 0.25)
    df <- rbind(
      data.frame(x1 = c(a1, a2), x2 = c(a12, a22), class = '0'),
      data.frame(x1 = b1, x2 = b12, class = '1')
    )
    df[, c('x1', 'x2')] <- scale(df[, c('x1', 'x2')])
    df$class <- factor(df$class, levels = c('0', '1'))
    fitted <- svm_train_probabilities(df, 'linear')
    return(list(df = df, probs = fitted$probs,
                in_model = TRUE, kernel = 'linear', cv_folds = 3))
  }

  if (dataset == 'mammographic') {
    df <- read.csv(file_in_data('mammographic_masses.data'), header = FALSE,
                   na.strings = '?')
    names(df) <- c('BI-RADS', 'Age', 'Shape', 'Margin', 'Density', 'class')
    df <- stats::na.omit(df)
    df$class <- factor(df$class, levels = c(0, 1))
    df[, -6] <- scale(df[, -6])
    train <- split_train(df, 1)
    fitted <- svm_train_probabilities(train, 'radial')
    return(list(df = train, probs = fitted$probs,
                in_model = TRUE, kernel = 'radial', cv_folds = 3))
  }

  if (dataset == 'pima') {
    df <- read.csv(file_in_data('pima.csv'))
    df$class <- factor(ifelse(df$class == 'tested_negative', 0, 1),
                       levels = c(0, 1))
    df[, setdiff(names(df), 'class')] <-
      scale(df[, setdiff(names(df), 'class')])
    train <- split_train(df, 5989)
    # Revision2 tuned Pima with scale=TRUE, fitted with scale=FALSE.
    fitted <- svm_train_probabilities(train, 'radial', scale_tuning = TRUE)
    return(list(df = train, probs = fitted$probs,
                in_model = TRUE, kernel = 'radial', cv_folds = 2))
  }

  if (dataset == 'ctg') {
    if (!requireNamespace('readxl', quietly = TRUE))
      stop('Install package readxl for CTG')
    df <- as.data.frame(readxl::read_excel(file_in_data('CTG.xls'),
                                           sheet = 'Raw Data'))
    cols <- c('LB', 'AC', 'FM', 'UC', 'DL', 'DS', 'DP',
              'ASTV', 'MSTV', 'ALTV', 'MLTV', 'Width', 'Min',
              'Max', 'Nmax', 'Nzeros', 'Mode', 'Mean', 'Median',
              'Variance', 'Tendency')
    df <- df[, c(cols, 'NSP'), drop = FALSE]
    df <- df[stats::complete.cases(df), , drop = FALSE]
    df$class <- factor(ifelse(df$NSP == 3, 1, 0), levels = c(0, 1))
    df$NSP <- NULL
    train <- split_train(df, 1)
    n0 <- sum(train$class == '0')
    n1 <- sum(train$class == '1')
    set.seed(1)
    model <- ranger::ranger(class ~ ., data = train, probability = TRUE,
       num.trees = 200, class.weights = c('0' = 1, '1' = n0/n1), seed = 1)
    probs <- model$predictions[, c('0', '1'), drop = FALSE]
    return(list(df = train, probs = probs, in_model = FALSE))
  }

  if (dataset == 'phishing') {
    df <- read.csv(file_in_data('phising.csv'))
    names(df)[names(df) == 'Type'] <- 'class'
    df$class <- factor(df$class, levels = c(0, 1))
    train <- split_train(df, 1)
    set.seed(1)
    model <- ranger::ranger(class ~ ., data = train, probability = TRUE,
                             num.trees = 200, seed = 1)
    probs <- model$predictions[, c('0', '1'), drop = FALSE]
    return(list(df = train, probs = probs, in_model = FALSE))
  }

  if (dataset == 'credit') {
    df <- read.csv(file_in_data('creditcard.csv'))
    names(df)[names(df) == 'Class'] <- 'class'
    df$class <- factor(df$class, levels = c(0, 1))
    train <- split_train(df, 1)
    n0 <- sum(train$class == '0')
    n1 <- sum(train$class == '1')
    set.seed(1)
    model <- ranger::ranger(class ~ ., data = train, probability = TRUE,
      num.trees = 500, class.weights = c('0' = 1, '1' = n0/n1), seed = 1)
    probs <- model$predictions[, c('0', '1'), drop = FALSE]
    return(list(df = train, probs = probs, in_model = FALSE))
  }
  stop('Unknown dataset: ', dataset)
}

run_running_times <- function(
    cases = names(running_time_cases()),
    repetitions = 100L,
    data_dir = 'datasets',
    output_csv = NULL) {

  if (!is.numeric(repetitions) || length(repetitions) != 1L ||
      is.na(repetitions) || repetitions < 1 || repetitions != as.integer(repetitions))
    stop('repetitions must be a positive integer')
  if (length(cases) < 1L || anyNA(cases) ||
      !all(cases %in% names(running_time_cases())))
    stop('Unknown case(s). Run names(running_time_cases()) to see options')
  if (!requireNamespace('microbenchmark', quietly = TRUE))
    stop('Install package microbenchmark to run the timings')
  if (any(cases %in% c('ctg', 'phishing', 'credit_min_suspicious',
                       'credit_high_purity', 'credit_uncertainty')) &&
      !requireNamespace('ranger', quietly = TRUE))
    stop('Install package ranger to run the RF cases')

  datasets <- c(simulated_3 = 'simulated', simulated_2 = 'simulated',
                mammographic = 'mammographic', pima = 'pima',
                ctg = 'ctg', phishing = 'phishing',
                credit_min_suspicious = 'credit',
                credit_high_purity = 'credit', credit_uncertainty = 'credit')
  prepared <- new.env(parent = emptyenv())
  out <- list()

  for (case in cases) {
    dataset <- unname(datasets[[case]])
    if (!exists(dataset, envir = prepared, inherits = FALSE)) {
      message('Preparing ', dataset, ' (outside timed region)...')
      assign(dataset, prepare_running_time_data(dataset, data_dir), envir = prepared)
    }
    base <- get(dataset, envir = prepared, inherits = FALSE)
    spec <- get_running_time_spec(case, base$df)
    message('Measuring ', case, ' with ', repetitions, ' repetitions...')

    # Both methods receive base$df, i.e. SAME real-data training partition.
    # After-the-model receives probabilities from the model fitted above.
    # For large RF datasets only After-the-model is part of Revision2 timing.
    if (base$in_model) {
      bench <- microbenchmark::microbenchmark(
        'After-the-model' = AfterTheModel(
          df = base$df, probs_model_train = base$probs,
          restriction = TRUE, user_condition = spec$after_condition,
          vector_GPS = spec$gps, num_cuts = spec$num_cuts),
        'In-the-model' = InTheModel(
          df = base$df, kernelSVM = base$kernel,
          num_cuts = spec$num_cuts, cv_folds = base$cv_folds,
          vector_GPS = spec$gps, restriction = TRUE,
          user_condition = spec$in_condition, calibrate = FALSE,
          scale_svm = FALSE, balance_classes = FALSE),
        times = as.integer(repetitions), unit = 's',
        control = list(warmup = 0L))
    } else {
      bench <- microbenchmark::microbenchmark(
        'After-the-model' = AfterTheModel(
          df = base$df, probs_model_train = base$probs,
          restriction = TRUE, user_condition = spec$after_condition,
          vector_GPS = spec$gps, num_cuts = spec$num_cuts),
        times = as.integer(repetitions), unit = 's',
        control = list(warmup = 0L))
    }

    out[[case]] <- do.call(rbind, lapply(unique(as.character(bench$expr)), function(method) {
      seconds <- bench$time[as.character(bench$expr) == method] / 1e9
      data.frame(case = case,
        dataset_case = unname(running_time_cases()[[case]]),
        method = method, repetitions = length(seconds),
        min_s = min(seconds), mean_s = mean(seconds),
        median_s = median(seconds), max_s = max(seconds),
        row.names = NULL)
    }))
  }
  result <- do.call(rbind, out)
  rownames(result) <- NULL
  if (!is.null(output_csv)) {
    dir.create(dirname(output_csv), recursive = TRUE, showWarnings = FALSE)
    write.csv(result, output_csv, row.names = FALSE)
    message('Saved: ', output_csv)
  }
  result
}
