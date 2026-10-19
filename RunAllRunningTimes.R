################################################################################
# RunAllRunningTimes.R
#
# LOCATION: repository root (next to RunningTimes_After_vs_In.R).
# RUN FROM THE REPOSITORY ROOT:
#   source("RunAllRunningTimes.R")
#
# OUTPUTS:
#   Results/running_times_<group>.csv       # One checkpoint per group
#   Results/running_times.csv               # All nine scenarios together
#   Results/running_times_table.md          # Markdown table for the report
#   Results/running_times_sessionInfo.txt   # R / package / OS information
#
#
# Timing scope inherited from RunningTimes_After_vs_In.R:
#   After-the-model: threshold optimization (model probabilities precomputed).
#   In-the-model: SVM training/CV plus threshold optimization.
#
# To repeat the full study from scratch, change OVERWRITE <- TRUE.
# With OVERWRITE <- FALSE, completed groups with the same repetition count
# are reused. Use a consistent computer/R environment for all groups.
################################################################################

# ------------------------- Settings ------------------------------------------
REPETITIONS <- 100L
OVERWRITE <- FALSE
RESULTS_DIR <- "Results"

if (!file.exists("RunningTimes_After_vs_In.R")) {
  stop("Run this script from the repository root, where ",
       "RunningTimes_After_vs_In.R is located.")
}
source("RunningTimes_After_vs_In.R")
dir.create(RESULTS_DIR, recursive = TRUE, showWarnings = FALSE)

# There are nine scenarios but six preparation groups. In particular, all three
# Credit Card scenarios reuse the same trained Random Forest and probabilities.
CASE_GROUPS <- list(
  simulated = c("simulated_2", "simulated_3"),
  mammographic = "mammographic",
  pima = "pima",
  ctg = "ctg",
  phishing = "phishing",
  credit = c("credit_min_suspicious", "credit_high_purity",
             "credit_uncertainty")
)

# Validate a checkpoint before reusing it, so e.g. a 2-repetition pilot
# cannot accidentally be included in the 100-repetition final results.
expected_pairs <- function(cases) {
  in_model_cases <- c("simulated_2", "simulated_3", "mammographic", "pima")
  unlist(lapply(cases, function(case) {
    methods <- c("After-the-model",
                 if (case %in% in_model_cases) "In-the-model")
    paste(case, methods, sep = "::")
  }), use.names = FALSE)
}

checkpoint_is_valid <- function(data, cases, repetitions) {
  required <- c("case", "dataset_case", "method", "repetitions",
                "min_s", "mean_s", "median_s", "max_s")
  if (is.null(data) || !all(required %in% names(data))) return(FALSE)
  observed <- paste(data$case, data$method, sep = "::")
  isTRUE(identical(sort(observed), sort(expected_pairs(cases)))) &&
    all(!is.na(data$repetitions) & data$repetitions == repetitions) &&
    all(is.finite(as.matrix(data[c("min_s", "mean_s",
                                   "median_s", "max_s")])))
}

run_group <- function(group_name, cases) {
  path <- file.path(RESULTS_DIR,
                    paste0("running_times_", group_name, ".csv"))
  if (!OVERWRITE && file.exists(path)) {
    old <- tryCatch(utils::read.csv(path, stringsAsFactors = FALSE),
                    error = function(e) NULL)
    if (checkpoint_is_valid(old, cases, REPETITIONS)) {
      message("Reusing completed group: ", path)
      return(old)
    }
    message("Existing checkpoint is incomplete or has different repetitions: ",
            path, ". Re-running group.")
  }

  message("\n--- Running group: ", group_name, " ---")
  # The timing function saves a checkpoint as soon as this group finishes.
  run_running_times(cases = cases,
                    repetitions = REPETITIONS,
                    output_csv = path)
}

# ------------------------- Execute all cases ---------------------------------
# Each group is saved immediately on completion. An interruption does not
# invalidate completed groups, and the script can be sourced again to resume.
res_simulated   <- run_group("simulated", CASE_GROUPS$simulated)
res_mammographic <- run_group("mammographic", CASE_GROUPS$mammographic)
res_pima        <- run_group("pima", CASE_GROUPS$pima)
res_ctg         <- run_group("ctg", CASE_GROUPS$ctg)
res_phishing    <- run_group("phishing", CASE_GROUPS$phishing)
res_credit      <- run_group("credit", CASE_GROUPS$credit)

# ------------------------- Merge / save --------------------------------------
all_results <- do.call(rbind, list(
  res_simulated, res_mammographic, res_pima,
  res_ctg, res_phishing, res_credit
))
rownames(all_results) <- NULL

case_order <- names(running_time_cases())
method_order <- c("After-the-model", "In-the-model")
all_results <- all_results[
  order(match(all_results$case, case_order),
        match(all_results$method, method_order)),
  , drop = FALSE
]
rownames(all_results) <- NULL

stopifnot(checkpoint_is_valid(all_results, case_order, REPETITIONS))

combined_path <- file.path(RESULTS_DIR, "running_times.csv")
utils::write.csv(all_results, combined_path, row.names = FALSE)
message("\nCombined results saved: ", combined_path)

# Generate a Markdown table from the CSV (rather than entering values manually).
fmt_time <- function(x) sprintf("%.4f", x)
header <- c(
  "# Running-time results",
  "",
  paste0("Repetitions per method/scenario: **", REPETITIONS, "**."),
  "",
  "**What is timed:** After-the-model measures threshold optimization using",
  "previously computed probabilities; In-the-model includes SVM training /",
  "cross-validation and threshold optimization. Data preparation, train/test",
  "splits, and initial fitting for After-the-model are excluded. These timings",
  "therefore cover different computational workloads.",
  "",
  "For real datasets where both methods are benchmarked, both use the same",
  "training partition (70% of the data). Measurements depend on the machine",
  "and installed software versions.",
  "",
  "Reproduce from the repository root with `source(\"RunAllRunningTimes.R\")`.",
  "The underlying CSV is [`running_times.csv`](running_times.csv).",
  "",
  "| Scenario | Method | Repetitions | Min (s) | Mean (s) | Median (s) | Max (s) |",
  "|:--|:--|--:|--:|--:|--:|--:|"
)
rows <- vapply(seq_len(nrow(all_results)), function(i) {
  x <- all_results[i, ]
  paste0("| ", x$dataset_case, " | ", x$method, " | ", x$repetitions,
         " | ", fmt_time(x$min_s), " | ", fmt_time(x$mean_s),
         " | ", fmt_time(x$median_s), " | ", fmt_time(x$max_s), " |")
}, character(1))

report_path <- file.path(RESULTS_DIR, "running_times_table.md")
writeLines(c(header, rows, ""), report_path, useBytes = TRUE)
message("Markdown table saved: ", report_path)

# Capture software environment. Hardware and system load can affect timings.
info_path <- file.path(RESULTS_DIR, "running_times_sessionInfo.txt")
writeLines(c(
  paste("Generated:", format(Sys.time(), "%Y-%m-%d %H:%M:%S %Z")),
  paste("Repetitions:", REPETITIONS),
  paste("System:", paste(Sys.info(), collapse = " | ")),
  "",
  capture.output(sessionInfo())
), info_path)
message("Environment information saved: ", info_path)

print(all_results)
################################################################################
