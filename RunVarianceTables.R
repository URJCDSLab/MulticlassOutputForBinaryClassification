# Reproducible tables for the real-data experiments.
# Source this file from the repository root. Nothing is run on source().
# Numerical results are always computed by VarianceAcrossSplits.R.

variance_table_cases <- function() {
  c("pima", "ctg", "phishing", "credit_sota", "credit_after")
}

variance_table_seeds <- function(case) {
  if (identical(case, "pima")) {
    set.seed(1)
    return(c(5989, runif(9)))
  }
  1:10
}

variance_table_load <- function(case) {
  if (identical(case, "pima")) {
    df <- read.csv("datasets/pima.csv")
    df$class <- ifelse(df$class == "tested_negative", 0, 1)
    df <- dplyr::mutate_if(df, is.character, as.factor)
    df$class <- as.factor(df$class)
    df[, -9] <- scale(df[, -9])
    return(df)
  }
  if (identical(case, "ctg")) {
    df <- readxl::read_excel("datasets/CTG.xls", sheet = "Raw Data")
    # Same row selection and predictors as the CTG experiment.
    df <- dplyr::filter(df, dplyr::if_any(dplyr::everything(), ~ !is.na(.)))
    df$class <- factor(ifelse(df$NSP == 3, 1, 0), levels = c(0, 1))
    vars <- c("LB", "AC", "FM", "UC", "DL", "DS", "DP", "ASTV",
              "MSTV", "ALTV", "MLTV", "Width", "Min", "Max", "Nmax",
              "Nzeros", "Mode", "Mean", "Median", "Variance", "Tendency")
    return(as.data.frame(df[, c(vars, "class")]))
  }
  if (identical(case, "phishing")) {
    df <- read.csv("datasets/phising.csv")
    names(df)[names(df) == "Type"] <- "class"
    df$class <- factor(df$class, levels = c(0, 1))
    return(df)
  }
  if (case %in% c("credit_sota", "credit_after")) {
    df <- read.csv("datasets/creditcard.csv")
    names(df)[names(df) == "Class"] <- "class"
    df$class <- factor(df$class, levels = c(0, 1), labels = c("0", "1"))
    return(df)
  }
  stop("Unknown case: ", case)
}

variance_table_require_packages <- function() {
  required <- c("plyr", "dplyr", "tidyr", "caret", "e1071", "CORElearn",
                "pracma", "pROC", "ranger", "readxl", "reticulate")
  missing <- required[!vapply(required, requireNamespace, logical(1), quietly = TRUE)]
  if (length(missing)) stop("Missing R packages: ", paste(missing, collapse = ", "))
  suppressPackageStartupMessages({
    library(plyr)
    library(dplyr)
    library(tidyr)
    library(caret)
    library(e1071)
    library(CORElearn)
    library(pracma)
    library(pROC)
    library(ranger)
  })
  # Required by OptimalBinningThresholds() in VarianceAcrossSplits.R.
  reticulate::py_require("optbinning==0.21.0")
  source("VarianceAcrossSplits.R")
}

# Category statistics are computed split-by-split, before averaging.
# A split is counted as valid only if all requested categories are nonempty.
variance_table_statistics <- function(runs, n_categories, labels) {
  stopifnot(length(labels) == n_categories)
  total <- length(runs)
  matrices <- lapply(runs, function(x) {
    if (is.null(x) || !is.list(x) || is.null(x$CM_test) ||
        ("feasible" %in% names(x) && !isTRUE(x$feasible))) return(NULL)
    cm <- as.matrix(x$CM_test)
    if (!all(dim(cm) == c(n_categories, 2L)) || any(!is.finite(cm))) return(NULL)
    cm
  })
  matrices <- Filter(Negate(is.null), matrices)
  if (!length(matrices)) {
    return(data.frame(category = labels, mean = NA_real_, low = NA_real_,
                      high = NA_real_, size = NA_real_, n_nonempty = 0L,
                      valid_splits = 0L, total_splits = total))
  }
  proportions <- do.call(rbind, lapply(matrices, function(cm) {
    row_n <- rowSums(cm)
    v <- cm[, 2] / row_n
    v[row_n == 0] <- NA_real_
    v
  }))
  sizes <- do.call(rbind, lapply(matrices, function(cm) 100 * rowSums(cm) / sum(cm)))
  if (is.null(dim(proportions))) proportions <- matrix(proportions, ncol = n_categories)
  if (is.null(dim(sizes))) sizes <- matrix(sizes, ncol = n_categories)
  n_nonempty <- colSums(!is.na(proportions))
  average <- apply(proportions, 2, function(z) {
    if (all(is.na(z))) return(NA_real_)
    mean(z, na.rm = TRUE)
  })
  sd_value <- apply(proportions, 2, function(z) stats::sd(z, na.rm = TRUE))
  ci_halfwidth <- ifelse(n_nonempty > 1,
                        stats::qt(0.975, pmax(1, n_nonempty - 1)) *
                          sd_value / sqrt(pmax(1, n_nonempty)), NA_real_)
  data.frame(category = labels, mean = 100 * average,
             low = pmax(0, 100 * (average - ci_halfwidth)),
             high = pmin(100, 100 * (average + ci_halfwidth)),
             size = colMeans(sizes), n_nonempty = n_nonempty,
             valid_splits = sum(rowSums(sizes > 0) == n_categories),
             total_splits = total)
}

variance_table_definitions <- function() {
  list(
    pima = list(
      labels = c("Very low", "Low", "Medium", "High", "Very high"),
      methods = c("Equal-frequency" = "E_equal_freq", "Optimal binning" = "E_optimal",
                  "After-the-model, constrained" = "E5", "In-the-model, constrained" = "E6")),
    ctg = list(
      labels = c("Low risk", "Monitoring", "High risk"),
      methods = c("Equal-width" = "equal_width", "Equal-frequency" = "equal_freq",
                  "Optimal binning" = "optimal", "After-the-model, constrained" = "proposed")),
    phishing = list(
      labels = c("Low phishing risk", "Suspicious", "High phishing risk"),
      methods = c("Equal-frequency" = "equal_freq", "Optimal binning" = "optimal",
                  "After-the-model, minimum Suspicious" = "option1")),
    credit_sota = list(
      labels = c("Low fraud risk", "Suspicious", "High fraud risk"),
      methods = c("Classical (threshold 0.5)" = "binary", "Equal-width" = "equal_width",
                  "Isotonic + equal-width" = "iso_equal_width", "ROC-Youden" = "roc")),
    credit_after = list(
      labels = c("Low fraud risk", "Suspicious", "High fraud risk"),
      methods = c("After-the-model, Minimum Suspicious" = "option1",
                  "After-the-model, High-purity High" = "option2",
                  "After-the-model, Uncertainty" = "uncertainty"))
  )
}

# Produce one row per method and predicted category; the CSV keeps full precision.
variance_table_extract <- function(case, result) {
  spec <- variance_table_definitions()[[case]]
  rows <- list()
  for (method in names(spec$methods)) {
    key <- unname(spec$methods[[method]])
    # Pima returns summary objects. For the exact category sizes the corresponding
    # raw confusion matrices are included in VarianceAcrossSplits.R.
    if (identical(case, "pima")) {
      if (is.null(result$raw)) {
        stop("Pima raw split results not available. Replace VarianceAcrossSplits.R with the supplied version.")
      }
      runs <- result$raw[[key]]
    } else if (identical(case, "credit_after")) {
      runs <- result[[key]]
    } else {
      runs <- result$raw[[key]]
    }
    if (is.null(runs)) stop("Missing raw results for ", case, " / ", key)
    k <- if (case == "credit_sota" && key %in% c("binary", "roc")) 2L else length(spec$labels)
    label <- if (k == 2L) spec$labels[c(1, 3)] else spec$labels
    x <- variance_table_statistics(runs, k, label)
    x$case <- case
    x$method <- method
    x$order <- seq_len(nrow(x))
    rows[[method]] <- x
  }
  do.call(rbind, rows)
}

variance_table_format_vector <- function(x, digits = 1) {
  paste0("[", paste(ifelse(is.na(x), "empty", sprintf(paste0("%.", digits, "f"), x)),
                      collapse = ", "), "]")
}

variance_table_as_md <- function(x, case) {
  methods <- unique(x$method)
  output <- character()
  if (case == "credit") {
    output <- c("| Method / configuration | Low fraud risk (%) | Suspicious (%) | High fraud risk (%) | Category size (%) |",
                "|:--|--:|--:|--:|:--|")
  } else {
    output <- c("| Method / configuration | Valid splits | Positive-class prevalence (%) | Category size (%) |",
                "|:--|:--:|:--|:--|")
  }
  for (method in methods) {
    z <- x[x$method == method, , drop = FALSE]
    z <- z[order(z$order), , drop = FALSE]
    if (case == "credit") {
      vals <- vapply(seq_len(nrow(z)), function(i) {
        if (!is.finite(z$mean[i])) return("—")
        d <- if (z$mean[i] < 0.1) 3 else 1
        f <- function(v) sprintf(paste0("%.", d, "f"), v)
        paste0(f(z$mean[i]), " [", f(z$low[i]), ", ", f(z$high[i]), "]")
      }, character(1))
      if (length(vals) == 2) {
        vals <- append(vals, "—", after = 1)
        sizes <- append(z$size, NA_real_, after = 1)
      } else sizes <- z$size
      size_str <- paste0("[", paste(ifelse(is.na(sizes), "—", sprintf("%.2f", sizes)), collapse = ", "), "]")
      output <- c(output, paste0("| ", method, " | ", paste(vals, collapse = " | "), " | ", size_str, " |"))
    } else {
      prevalence <- variance_table_format_vector(z$mean, if (case == "ctg" && grepl("After", method)) 2 else 1)
      size <- variance_table_format_vector(z$size, 1)
      valid <- sprintf("%s/%s", z$valid_splits[1], z$total_splits[1])
      output <- c(output, paste0("| ", method, " | ", valid, " | ", prevalence, " | ", size, " |"))
    }
  }
  output
}

variance_table_markdown <- function(rows) {
  ordered <- c("pima", "ctg", "phishing", "credit")
  titles <- c(pima = "Pima", ctg = "Cardiotocography (CTG)",
              phishing = "Phishing", credit = "Credit Card Fraud")
  out <- c("# Variance across splits — tables", "",
           "Tables are computed from the external-test confusion matrices of the repeated 70/30 splits.",
           "A valid split contains all requested output categories.", "")
  for (case in ordered) {
    data <- if (case == "credit") rows[rows$case %in% c("credit_sota", "credit_after"), , drop = FALSE] else rows[rows$case == case, , drop = FALSE]
    if (nrow(data) == 0L) next
    out <- c(out, paste0("## ", titles[[case]]), "",
             variance_table_as_md(data, case), "")
  }
  out
}

# Execute any requested cases, save RDS checkpoints, and generate tables.
# Default: reuses existing checkpoint only when its seeds exactly match.
run_variance_tables <- function(cases = variance_table_cases(), overwrite = FALSE,
                                output_dir = "Results") {
  stopifnot(is.character(cases), length(cases) > 0L)
  if (!all(cases %in% variance_table_cases())) stop("Unknown case(s): ", paste(setdiff(cases, variance_table_cases()), collapse = ", "))
  dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
  variance_table_require_packages()
  for (case in cases) {
    seeds <- variance_table_seeds(case)
    path <- file.path(output_dir, paste0("variance_", case, ".rds"))
    if (file.exists(path) && !isTRUE(overwrite)) {
      stored <- readRDS(path)
      if (!identical(stored$seeds, seeds)) stop("Checkpoint seeds mismatch: ", path)
      message("Reusing ", path)
    } else {
      message("Running ", case, " (", length(seeds), " splits)")
      df <- variance_table_load(case)
      result <- switch(case,
                       pima = VarianceSplitsPima(df, seeds),
                       ctg = VarianceSplitsCTG(df, seeds),
                       phishing = VarianceSplitsPhishing(df, seeds),
                       credit_sota = VarianceSplitsCreditSOTA(df, seeds),
                       credit_after = VarianceSplitsCreditAfter(df, seeds))
      stored <- list(seeds = seeds, result = result)
      saveRDS(stored, path)
      message("Saved ", path)
    }
  }
  completed <- character()
  rows <- list()
  for (case in variance_table_cases()) {
    path <- file.path(output_dir, paste0("variance_", case, ".rds"))
    if (!file.exists(path)) next
    stored <- readRDS(path)
    if (!identical(stored$seeds, variance_table_seeds(case))) {
      warning("Skipping checkpoint with different seeds: ", path)
      next
    }
    rows[[case]] <- variance_table_extract(case, stored$result)
    completed <- c(completed, case)
  }
  if (length(rows)) {
    out <- do.call(rbind, rows)
    rownames(out) <- NULL
    write.csv(out, file.path(output_dir, "variance_tables.csv"), row.names = FALSE)
    writeLines(variance_table_markdown(out), file.path(output_dir, "variance_tables.md"), useBytes = TRUE)
    message("Updated Results/variance_tables.csv and Results/variance_tables.md (", length(completed), "/5 groups)")
    return(invisible(out))
  }
  invisible(NULL)
}

# Merge computed tables into the existing Markdown while keeping every plot.
# Works with either the original Markdown or one that already has dataset tables.
# Creates a separate output file and never reruns the experiments.
update_variance_analysis_md <- function(
    original = "Results/AnalysisVarianceAcrossSplits.md",
    output = "Results/AnalysisVarianceAcrossSplits_with_tables.md",
    csv_path = "Results/variance_tables.csv") {
  if (!file.exists(original)) stop("Missing ", original)
  if (!file.exists(csv_path)) stop("Missing generated table data: ", csv_path)
  if (normalizePath(original, mustWork = TRUE) ==
      normalizePath(output, mustWork = FALSE)) {
    stop("Please use a different output path to protect the original Markdown.")
  }

  rows <- read.csv(csv_path, stringsAsFactors = FALSE, check.names = FALSE)
  if (!all(variance_table_cases() %in% unique(rows$case))) {
    stop("Complete all five experiment groups before merging the tables.")
  }
  lines <- readLines(original, warn = FALSE, encoding = "UTF-8")
  image_references <- grep("!\\[[^]]*\\]\\(", lines, value = TRUE)

  # Replace an inclusive range; to < from inserts without deleting anything.
  splice_lines <- function(x, from, to, replacement) {
    before <- if (from > 1L) x[seq_len(from - 1L)] else character()
    after <- if (to < length(x)) x[seq.int(to + 1L, length(x))] else character()
    c(before, replacement, after)
  }

  merge_case <- function(doc, case, title, new_table) {
    headings <- which(trimws(doc) == paste0("# ", title))
    if (length(headings) > 1L) stop("Duplicate top-level section: ", title)
    if (length(headings) == 0L) {
      doc <- c(doc, "", paste0("# ", title), "")
      headings <- which(trimws(doc) == paste0("# ", title))
    }
    section_start <- headings[1L]
    subsequent <- which(grepl("^# [^#]", doc) & seq_along(doc) > section_start)
    section_end <- if (length(subsequent)) subsequent[1L] - 1L else length(doc)
    section <- seq.int(section_start, section_end)

    begin_marker <- paste0("<!-- BEGIN GENERATED VARIANCE TABLE: ", case, " -->")
    end_marker <- paste0("<!-- END GENERATED VARIANCE TABLE: ", case, " -->")
    risk_labels <- if (case == "credit") {
      variance_table_definitions()$credit_after$labels
    } else {
      variance_table_definitions()[[case]]$labels
    }
    explanation <- if (case == "credit") {
      "Mean fraud prevalence (%) and 95% confidence intervals for each category; category sizes are percentages of test observations."
    } else {
      "Mean positive-class prevalence (%) and category size (%) across the external-test splits; a valid split has all requested categories nonempty."
    }
    block <- c("", begin_marker, "## External-test summary table", "",
               paste0("Categories: ", paste(risk_labels, collapse = ", "), "."),
               explanation, "", new_table, "", end_marker, "")

    # If this tool generated the table before, replace precisely the marked block.
    beg <- section[trimws(doc[section]) == begin_marker]
    end <- section[trimws(doc[section]) == end_marker]
    if (length(beg) || length(end)) {
      if (length(beg) != 1L || length(end) != 1L || end[1L] <= beg[1L]) {
        stop("Malformed generated-table markers in ", title)
      }
      return(splice_lines(doc, beg[1L], end[1L], block[-1L]))
    }

    # Handle pre-existing pasted tables without removing prose or figures below.
    summary_heads <- section[grepl(
      "^#{2,4} (External-test results|External-test summary table|Summary of external-test results)\\s*$",
      trimws(doc[section]))]
    if (length(summary_heads)) {
      start <- summary_heads[1L]
      # Only treat a heading as an existing generated/pasted table when it has
      # a 'Method / configuration' table beneath it, before the next heading.
      next_head <- section[section > start & grepl("^#{1,6} ", doc[section])]
      search_end <- if (length(next_head)) min(next_head) - 1L else section_end
      search_range <- if (start < search_end) seq.int(start + 1L, search_end) else integer()
      table_heads <- search_range[grepl("^\\|[[:space:]]*Method / configuration[[:space:]]*\\|", doc[search_range])]
      if (length(table_heads)) {
        last <- table_heads[1L]
        while (last < search_end && grepl("^\\|", trimws(doc[last + 1L]))) {
          last <- last + 1L
        }
        return(splice_lines(doc, start, last, block[-1L]))
      }
    }

    # No pre-existing summary table: insert immediately below dataset heading.
    splice_lines(doc, section_start + 1L, section_start, block)
  }

  labels <- c(pima = "Pima", ctg = "Cardiotocography (CTG)",
              phishing = "Phishing", credit = "Credit Card Fraud")
  if (!any(trimws(lines) == "# Pima")) {
    stop("Cannot locate the Pima section in ", original)
  }
  for (case in names(labels)) {
    data <- if (case == "credit") {
      rows[rows$case %in% c("credit_sota", "credit_after"), , drop = FALSE]
    } else {
      rows[rows$case == case, , drop = FALSE]
    }
    lines <- merge_case(lines, case, labels[[case]], variance_table_as_md(data, case))
  }
  if (!identical(image_references,
                 grep("!\\[[^]]*\\]\\(", lines, value = TRUE))) {
    stop("Image references changed unexpectedly; output not written.")
  }
  dir.create(dirname(output), recursive = TRUE, showWarnings = FALSE)
  writeLines(lines, output, useBytes = TRUE)
  message("Created ", output, ". Existing figures are preserved.")
  invisible(output)
}
