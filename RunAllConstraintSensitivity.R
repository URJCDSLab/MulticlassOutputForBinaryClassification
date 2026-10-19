################################################################################
# RunAllConstraintSensitivity.R
# Sensitivity of the constraints used in the paper.
# Place in the repository root. Run from that directory:
#   source("RunAllConstraintSensitivity.R")
#   run_constraint_sensitivity(cases = "simulated_2_after")  # quick check
#   run_constraint_sensitivity()                              # all experiments
#
# Per-case checkpoints: Results/constraint_sensitivity_<case>.csv
# Combined output:      Results/constraint_sensitivity.csv
#                       Results/constraint_sensitivity_table.md
#
# The combined outputs are written only when ALL cases have finished.
# Existing checkpoints are reused unless overwrite = TRUE. If code, datasets,
# settings or the execution environment change, rerun with overwrite = TRUE.
################################################################################

if (!file.exists("GeneralAndMethodFunctions.R") ||
    !file.exists("ConstraintRobutness.R")) {
  stop("Run from the repository root (R working directory).")
}
source("GeneralAndMethodFunctions.R")
source("ConstraintRobutness.R")

suppressPackageStartupMessages({
  library(dplyr)
  library(caret)
  library(e1071)
})

sensitivity_cases <- function() {
  c(
    simulated_3_after = "Simulated - 3 categories / After-the-model",
    simulated_3_in    = "Simulated - 3 categories / In-the-model",
    simulated_2_after = "Simulated - 2 categories / After-the-model",
    simulated_2_in    = "Simulated - 2 categories / In-the-model",
    mammographic_after = "Mammographic / After-the-model",
    mammographic_in    = "Mammographic / In-the-model",
    pima_after = "Pima / After-the-model",
    pima_in    = "Pima / In-the-model",
    ctg_after  = "CTG / After-the-model",
    phishing_after = "Phishing / After-the-model",
    credit_min_suspicious = "Credit Card / Minimum suspicious",
    credit_high_purity    = "Credit Card / High-purity High",
    credit_uncertainty    = "Credit Card / Uncertainty"
  )
}

sensitivity_dataset <- function(case) {
  if (startsWith(case, "simulated_")) return("simulated")
  if (startsWith(case, "mammographic_")) return("mammographic")
  if (startsWith(case, "pima_")) return("pima")
  if (startsWith(case, "ctg_")) return("ctg")
  if (startsWith(case, "phishing_")) return("phishing")
  if (startsWith(case, "credit_")) return("credit")
  stop("Unknown sensitivity case: ", case)
}

# Preparation follows the experimental configurations in
# AnalysisConstraintRobutness.qmd, including the same random seeds.
prepare_sensitivity_data <- function(dataset, data_dir = "datasets") {
  data_file <- function(name) {
    path <- file.path(data_dir, name)
    if (!file.exists(path)) stop("Dataset not found: ", path)
    path
  }
  split_data <- function(df, seed) {
    set.seed(seed)
    idx <- caret::createDataPartition(df$class, p = 0.7, list = FALSE)
    list(train = df[idx, , drop = FALSE],
         test = df[-idx, , drop = FALSE])
  }
  svm_probabilities <- function(train, test = NULL, kernel,
                                tune_scale = FALSE) {
    set.seed(1)
    tuned <- e1071::tune("svm", class ~ ., data = train,
                         kernel = kernel, scale = tune_scale,
                         ranges = list(cost = c(0.001, 0.01, 0.1, 1, 5, 10,
                                                20, 50, 100, 150, 200)))
    set.seed(1)
    model <- e1071::svm(class ~ ., data = train, kernel = kernel,
                        scale = FALSE, probability = TRUE, type = "C",
                        cost = tuned$best.model$cost)
    get_probs <- function(df) {
      p <- stats::predict(model,
                df[, setdiff(names(df), "class"), drop = FALSE],
                probability = TRUE, decision.values = TRUE)
      attr(p, "probabilities")[, c("0", "1"), drop = FALSE]
    }
    list(train = train, test = test,
         probs_train = get_probs(train),
         probs_test = if (is.null(test)) NULL else get_probs(test),
         kernel = kernel)
  }
  rf_probabilities <- function(train, test, trees, weighted = FALSE) {
    weights <- NULL
    if (weighted) {
      n0 <- sum(train$class == "0")
      n1 <- sum(train$class == "1")
      weights <- c("0" = 1, "1" = n0 / n1)
    }
    set.seed(1)
    rf <- ranger::ranger(class ~ ., data = train, probability = TRUE,
                         num.trees = trees, class.weights = weights, seed = 1)
    # OOB predictions for training; predictions on an external held-out test set.
    list(train = train, test = test,
         probs_train = rf$predictions[, c("0", "1"), drop = FALSE],
         probs_test = stats::predict(rf, data = test)$predictions[
           , c("0", "1"), drop = FALSE])
  }

  if (dataset == "simulated") {
    set.seed(2812)
    a1 <- rnorm(300, 1, 0.25); a2 <- rnorm(30, 2, 0.15)
    b1 <- rnorm(300, 1.5, 0.25)
    set.seed(12)
    a12 <- rnorm(300, 2.5, 0.25); a22 <- rnorm(30, 1.5, 0.15)
    b12 <- rnorm(300, 2.5, 0.25)
    df <- rbind(data.frame(x1 = c(a1, a2), x2 = c(a12, a22), class = "0"),
                data.frame(x1 = b1, x2 = b12, class = "1"))
    df[, c("x1", "x2")] <- scale(df[, c("x1", "x2")])
    df$class <- factor(df$class, levels = c("0", "1"))
    return(svm_probabilities(df, kernel = "linear"))
  }

  if (dataset == "mammographic") {
    df <- utils::read.csv(data_file("mammographic_masses.data"),
                          header = FALSE, na.strings = "?")
    names(df) <- c("BI-RADS", "Age", "Shape", "Margin", "Density", "class")
    df <- stats::na.omit(df)
    df$class <- factor(df$class, levels = c(0, 1))
    df[, setdiff(names(df), "class")] <-
      scale(df[, setdiff(names(df), "class")])
    parts <- split_data(df, 1)
    return(svm_probabilities(parts$train, parts$test, kernel = "radial"))
  }

  if (dataset == "pima") {
    df <- utils::read.csv(data_file("pima.csv"))
    df$class <- factor(ifelse(df$class == "tested_negative", 0, 1),
                       levels = c(0, 1))
    df[, setdiff(names(df), "class")] <-
      scale(df[, setdiff(names(df), "class")])
    parts <- split_data(df, 5989)
    return(svm_probabilities(parts$train, parts$test, kernel = "radial",
                             tune_scale = TRUE))
  }

  if (dataset == "ctg") {
    if (!requireNamespace("readxl", quietly = TRUE)) {
      stop("Package 'readxl' is required for CTG")
    }
    df <- as.data.frame(readxl::read_excel(data_file("CTG.xls"),
                                            sheet = "Raw Data"))
    # Drop entirely empty Excel rows, as in the experimental notebook.
    df <- df[rowSums(!is.na(df)) > 0, , drop = FALSE]
    df$class <- factor(ifelse(df$NSP == 3, 1, 0), levels = c(0, 1))
    cols <- c("LB", "AC", "FM", "UC", "DL", "DS", "DP", "ASTV", "MSTV",
              "ALTV", "MLTV", "Width", "Min", "Max", "Nmax", "Nzeros",
              "Mode", "Mean", "Median", "Variance", "Tendency", "class")
    df <- df[, cols, drop = FALSE]
    parts <- split_data(df, 1)
    return(rf_probabilities(parts$train, parts$test, trees = 200,
                            weighted = TRUE))
  }

  if (dataset == "phishing") {
    df <- utils::read.csv(data_file("phising.csv"))
    names(df)[names(df) == "Type"] <- "class"
    df$class <- factor(df$class, levels = c(0, 1))
    parts <- split_data(df, 1)
    return(rf_probabilities(parts$train, parts$test, trees = 200))
  }

  if (dataset == "credit") {
    df <- utils::read.csv(data_file("creditcard.csv"))
    names(df)[names(df) == "Class"] <- "class"
    df$class <- factor(df$class, levels = c(0, 1))
    parts <- split_data(df, 1)
    return(rf_probabilities(parts$train, parts$test, trees = 500,
                            weighted = TRUE))
  }
  stop("Unknown dataset: ", dataset)
}

# Return the list of conditions expected by InTheModel().
make_in_condition <- function(values, ops, thresholds) {
  force(values); force(ops); force(thresholds)
  function(p, p_, r, r_) {
    v <- values(p, p_, r, r_)
    conditions <- Map(function(value, op, threshold)
      list(value = value, threshold = threshold, op = op),
      as.list(v), as.list(ops), as.list(thresholds))
    results <- lapply(conditions, function(x) {
      if (!is.finite(x$value)) return(FALSE)
      switch(x$op,
        ">"  = x$value > x$threshold,
        ">=" = x$value >= x$threshold,
        "<"  = x$value < x$threshold,
        "<=" = x$value <= x$threshold,
        stop("Unknown constraint operator"))
    })
    list(conditions = conditions, results = results,
         total_cond = all(unlist(results)))
  }
}

# Each case uses the same GPS, constraints and perturbations as the notebook.
sensitivity_spec <- function(case, train) {
  prop <- function(j, r, r_) {
    n0 <- sum(train$class == "0")
    n1 <- sum(train$class == "1")
    (r_[j] * n0 + r[j] * n1) / nrow(train)
  }
  in_case <- grepl("_in$", case)
  if (startsWith(case, "simulated_3")) {
    gps <- c("p13", "p_11", "r31")
    thr <- if (in_case) c(0.9, 0.4, 0.8) else c(0.8, 0.9, 0.4)
    after <- function(t) function(p, p_, r, r_)
      isTRUE(p[3] > t[1] && p_[1] > t[2] && r_[1] > t[3])
    inside <- function(t) make_in_condition(
      function(p, p_, r, r_) c(p_[1], r_[1], p[3]),
      c(">", ">", ">"), t)
    return(list(gps = gps, num_cuts = 2, cv_folds = 3, thresholds = thr,
                delta = 0.05, maker = if (in_case) inside else after))
  }
  if (startsWith(case, "simulated_2")) {
    after <- function(t) function(p, p_, r, r_) isTRUE(p_[1] > t[1])
    inside <- function(t) make_in_condition(
      function(p, p_, r, r_) c(p_[1]), ">", t)
    return(list(gps = c("p12", "p_11", "r21", "r1_1"), num_cuts = 1,
                cv_folds = 3, thresholds = c(0.85), delta = 0.05,
                maker = if (in_case) inside else after))
  }
  if (startsWith(case, "mammographic")) {
    thr <- if (in_case) c(0.5, 0.8, 0.1, 0.7, 0.85, 0.85) else
                       c(0.5, 0.1, 0.7, 0.85, 0.85, 0.8)
    after <- function(t) function(p, p_, r, r_)
      isTRUE(r_[1] > t[1] && r_[2] > t[2] && p[3] > t[3] &&
             p[3] < t[4] && p[4] > t[5] && p_[1] > t[6])
    inside <- function(t) make_in_condition(
      function(p, p_, r, r_) c(r_[1], p_[1], r_[2], p[3], p[3], p[4]),
      c(">", ">", ">", ">", "<", ">"), t)
    return(list(gps = c("p14", "p_11", "r1_1"), num_cuts = 3,
                cv_folds = 3, thresholds = thr, delta = 0.05,
                maker = if (in_case) inside else after))
  }
  if (startsWith(case, "pima")) {
    after <- function(t) function(p, p_, r, r_)
      isTRUE(r_[1] > t[1] && p[4] > t[2] && p[3] > t[3] && p[5] > t[4])
    inside <- function(t) make_in_condition(
      function(p, p_, r, r_) c(r_[1], p[4], p[3], p[5]),
      rep(">", 4), t)
    return(list(gps = c("p15", "p_11"), num_cuts = 4,
                cv_folds = 2, thresholds = c(0.3, 0.7, 0.4, 0.9),
                delta = 0.05, maker = if (in_case) inside else after))
  }
  if (startsWith(case, "ctg") || startsWith(case, "phishing")) {
    maker <- function(t) function(p, p_, r, r_)
      isTRUE(prop(2, r, r_) >= t[1])
    return(list(gps = c("p_11", "p13", "r1_1", "r31"),
                num_cuts = 2, thresholds = 0.10, delta = 0.05,
                maker = maker))
  }
  if (case == "credit_min_suspicious") {
    maker <- function(t) function(p, p_, r, r_)
      isTRUE(prop(2, r, r_) >= t[1])
    return(list(gps = c("p_11", "p13", "r1_1", "r31"),
                num_cuts = 2, thresholds = 0.001, delta = 0.0005,
                maker = maker))
  }
  if (case == "credit_high_purity") {
    maker <- function(t) function(p, p_, r, r_)
      isTRUE(p[3] >= t[1])
    return(list(gps = c("p_11", "p13", "r1_1", "r31"),
                num_cuts = 2, thresholds = 0.95, delta = 0.05,
                maker = maker))
  }
  if (case == "credit_uncertainty") {
    thresholds <- c(0.0003, 0.001, 0.90, 0.0005)
    maker <- function(t) function(p, p_, r, r_)
      isTRUE(p[1] <= t[1] && prop(2, r, r_) >= t[2] &&
             p[3] >= t[3] && prop(3, r, r_) >= t[4])
    return(list(gps = c("p_11", "p12", "p_12", "p13", "r31"),
                num_cuts = 2, thresholds = thresholds,
                delta = 0.05 * thresholds, maker = maker))
  }
  stop("No spec defined for case: ", case)
}

sensitivity_one_case <- function(case, prepared) {
  spec <- sensitivity_spec(case, prepared$train)
  after <- !grepl("_in$", case)
  simulated <- sensitivity_dataset(case) == "simulated"
  train <- prepared$train
  test <- prepared$test
  if (after) {
    if (simulated) {
      raw <- EvaluateConstraintRobustnessAfter(
        spec$thresholds, spec$delta, spec$maker, train,
        prepared$probs_train, spec$gps, spec$num_cuts)
    } else {
      raw <- EvaluateConstraintRobustnessAfterTrainTest(
        spec$thresholds, spec$delta, spec$maker, train,
        prepared$probs_train, spec$gps, spec$num_cuts,
        test, prepared$probs_test)
    }
  } else {
    train_x <- train[, setdiff(names(train), "class"), drop = FALSE]
    if (simulated) {
      raw <- EvaluateConstraintRobustnessIn(
        spec$thresholds, spec$delta, spec$maker,
        train, train_x, spec$gps, spec$num_cuts,
        spec$cv_folds, prepared$kernel)
    } else {
      test_x <- test[, setdiff(names(test), "class"), drop = FALSE]
      raw <- EvaluateConstraintRobustnessInTrainTest(
        spec$thresholds, spec$delta, spec$maker,
        train, train_x, spec$gps, spec$num_cuts,
        spec$cv_folds, prepared$kernel, test, test_x)
    }
  }
  partition_names <- if (simulated) "simulated" else c(
    if (after) "train" else "cv_validation", "test")
  summaries <- if (simulated) list(raw) else
    list(raw$train_summary, raw$test_summary)
  method <- if (after) "After-the-model" else "In-the-model"
  result <- do.call(rbind, Map(function(s, partition) {
    out <- as.data.frame(s, stringsAsFactors = FALSE)
    out$case <- case
    out$dataset_case <- unname(sensitivity_cases()[[case]])
    out$method <- method
    out$partition <- partition
    out[, c("case", "dataset_case", "method", "partition",
            "num_changes", "changes_sizes", "mean_dif_GPS",
            "median_dif_GPS", "sd_dif_GPS", "mean_CM_prop_diff",
            "median_CM_prop_diff", "sd_CM_prop_diff"), drop = FALSE]
  }, summaries, partition_names))
  rownames(result) <- NULL
  result
}

sensitivity_checkpoint_valid <- function(df, case) {
  required <- c("case", "dataset_case", "method", "partition", "num_changes",
                "changes_sizes", "mean_dif_GPS", "median_dif_GPS",
                "sd_dif_GPS", "mean_CM_prop_diff", "median_CM_prop_diff",
                "sd_CM_prop_diff")
  if (is.null(df) || !all(required %in% names(df))) return(FALSE)
  if (nrow(df) < 1 || anyNA(df$case) || !all(df$case == case)) return(FALSE)
  expected <- if (sensitivity_dataset(case) == "simulated") "simulated" else
    c(if (grepl("_in$", case)) "cv_validation" else "train", "test")
  identical(sort(unique(df$partition)), sort(expected)) &&
    all(vapply(split(df, df$partition), function(x)
      sum(x$num_changes == "TOTAL") == 1L, logical(1)))
}

run_constraint_sensitivity <- function(
    cases = names(sensitivity_cases()),
    data_dir = "datasets",
    results_dir = "Results",
    overwrite = FALSE) {
  known <- names(sensitivity_cases())
  if (length(cases) < 1L || anyNA(cases) ||
      anyDuplicated(cases) || !all(cases %in% known)) {
    stop("Unknown or duplicated cases. See names(sensitivity_cases()).")
  }
  dir.create(results_dir, showWarnings = FALSE, recursive = TRUE)
  prepared <- new.env(parent = emptyenv())
  outcome <- list()
  for (case in cases) {
    checkpoint <- file.path(results_dir,
                            paste0("constraint_sensitivity_", case, ".csv"))
    old <- if (!overwrite && file.exists(checkpoint))
      tryCatch(utils::read.csv(checkpoint, stringsAsFactors = FALSE),
               error = function(e) NULL) else NULL
    if (sensitivity_checkpoint_valid(old, case)) {
      message("Reusing checkpoint: ", checkpoint)
      outcome[[case]] <- old
      next
    }
    dataset <- sensitivity_dataset(case)
    if (!exists(dataset, envir = prepared, inherits = FALSE)) {
      message("Preparing dataset: ", dataset)
      assign(dataset, prepare_sensitivity_data(dataset, data_dir),
             envir = prepared)
    }
    message("Sensitivity: ", case)
    x <- sensitivity_one_case(case, get(dataset, envir = prepared,
                                         inherits = FALSE))
    utils::write.csv(x, checkpoint, row.names = FALSE)
    message("Saved: ", checkpoint)
    outcome[[case]] <- x
  }
  combined <- do.call(rbind, outcome)
  rownames(combined) <- NULL
  completed <- vapply(known, function(case) {
    path <- file.path(results_dir,
                       paste0("constraint_sensitivity_", case, ".csv"))
    if (!file.exists(path)) return(FALSE)
    x <- tryCatch(utils::read.csv(path, stringsAsFactors = FALSE),
                  error = function(e) NULL)
    sensitivity_checkpoint_valid(x, case)
  }, logical(1))
  if (all(completed)) {
    all_files <- file.path(results_dir,
                           paste0("constraint_sensitivity_", known, ".csv"))
    combined <- do.call(rbind, lapply(all_files, utils::read.csv,
                                     stringsAsFactors = FALSE))
    rownames(combined) <- NULL
    utils::write.csv(combined,
                     file.path(results_dir, "constraint_sensitivity.csv"),
                     row.names = FALSE)
    message("Saved: Results/constraint_sensitivity.csv")

    # Compact Markdown table: original paper report can link this generated file.
    total <- combined[combined$num_changes == "TOTAL" &
                        combined$partition %in% c("simulated", "test"), , drop = FALSE]
    fmt <- function(x) ifelse(is.finite(x), sprintf("%.5f", x), "NA")
    rows <- vapply(seq_len(nrow(total)), function(i) {
      x <- total[i, ]
      paste0("| ", x$dataset_case, " | ", x$method, " | ",
             fmt(x$mean_dif_GPS), " | ", fmt(x$mean_CM_prop_diff),
             " | ", x$changes_sizes, " |")
    }, character(1))
    writeLines(c(
      "# Constraint sensitivity — summary",
      "",
      "Average absolute changes relative to the unperturbed constraints.",
      "Real datasets: external test partitions. Simulated data: full dataset.",
      "For *In-the-model*, constraints are enforced in each training fold; retained runs have finite validation GPS, without requiring validation/test feasibility.",
      "Full results by number of changed constraints: [`constraint_sensitivity.csv`](constraint_sensitivity.csv).",
      "",
      "| Dataset / scenario | Method | Mean absolute GPS difference | Mean absolute CM difference | Retained perturbations |",
      "|:--|:--|--:|--:|--:|",
      rows, ""),
      file.path(results_dir, "constraint_sensitivity_table.md"))
    message("Saved: Results/constraint_sensitivity_table.md")
  } else {
    message("Checkpoint(s) saved. Combined files will be generated after all ",
            "13 cases have completed (", sum(completed), "/", length(known), ").")
  }
  print(combined)
  invisible(combined)
}
