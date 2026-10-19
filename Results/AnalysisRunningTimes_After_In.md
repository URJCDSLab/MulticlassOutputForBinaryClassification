# Analysis Running Times

This document reports the running-time experiments for the paper **From Binary Probabilities to Ordinal Decision Categories: An Optimization-Based Risk-Stratification Framework**.

## Reproducibility

Run the following command from the repository root in R:

```r
source("RunAllRunningTimes.R")
```

- [`../RunningTimes_After_vs_In.R`](../RunningTimes_After_vs_In.R) defines the datasets, model preparation, GPS objectives, constraints, and individual benchmarks.
- [`../RunAllRunningTimes.R`](../RunAllRunningTimes.R) executes all scenarios using 100 repetitions per method/scenario, caches completed dataset groups, and saves the combined results.
- [`running_times.csv`](running_times.csv) contains the combined summary statistics. Separate `running_times_<group>.csv` files are generated as checkpoints, [`running_times_table.md`](running_times_table.md) is generated automatically from the combined results.
- [`running_times_sessionInfo.txt`](running_times_sessionInfo.txt) records the software environment for the reported run.

`RunAllRunningTimes.R` reuses complete group checkpoints by default. To rerun the whole study, set `OVERWRITE <- TRUE` in that script before sourcing it. Existing checkpoints are validated for case/method coverage and the number of repetitions, not for changes to source code or hardware. Do not mix group files produced with different software, benchmark definitions, or machines.

## Timing protocol

- **After-the-model:** the measured expression optimizes decision thresholds on already computed class-probability scores. Preparing the dataset, training/tuning the underlying classifier and obtaining these probabilities are excluded from this elapsed time.
- **In-the-model:** the measured expression includes SVM model fitting/cross-validation and the joint hyperparameter/threshold selection performed inside `InTheModel()`.
- In the simulated study, the full generated dataset is used for these illustrative benchmarks. In the real-data cases, the same 70% training partition is provided to both methods when both are benchmarked. 
- CTG, Phishing and Credit Card Fraud include After-the-model only in this benchmark, their initial Random Forest fitting is performed outside the measured region.
- Each result summarizes 100 timed evaluations using `microbenchmark`, times are expressed in seconds.

## Results

| Dataset / scenario | Method | Repetitions | Min (s) | Mean (s) | Median (s) | Max (s) |
|:--|:--|--:|--:|--:|--:|--:|
| Simulated - 3 categories | After-the-model | 100 | 0.0315 | 0.0386 | 0.0320 | 0.0846 |
| Simulated - 3 categories | In-the-model | 100 | 2.5847 | 2.6342 | 2.6374 | 2.8336 |
| Simulated - 2 categories | After-the-model | 100 | 0.0033 | 0.0047 | 0.0035 | 0.0530 |
| Simulated - 2 categories | In-the-model | 100 | 1.4443 | 1.4891 | 1.4685 | 2.1150 |
| Mammographic - 4 categories | After-the-model | 100 | 0.1199 | 0.1641 | 0.1596 | 0.3104 |
| Mammographic - 4 categories | In-the-model | 100 | 5.4900 | 6.3262 | 5.6012 | 9.1867 |
| Pima - 5 categories | After-the-model | 100 | 0.5173 | 0.5458 | 0.5352 | 0.7201 |
| Pima - 5 categories | In-the-model | 100 | 14.0561 | 14.2917 | 14.2797 | 14.6060 |
| CTG - 3 categories | After-the-model | 100 | 0.0233 | 0.0282 | 0.0236 | 0.0691 |
| Phishing - 3 categories | After-the-model | 100 | 1.8690 | 1.9358 | 1.9318 | 2.1686 |
| Credit Card - Minimum suspicious | After-the-model | 100 | 1.6437 | 1.7801 | 1.7952 | 1.9907 |
| Credit Card - High-purity High | After-the-model | 100 | 1.6004 | 1.8519 | 1.8446 | 1.9529 |
| Credit Card - Uncertainty | After-the-model | 100 | 1.6340 | 1.9086 | 1.8896 | 2.0480 |


## Execution environment and interpretation

These measurements were produced on 2026-10-19 using R 4.5.3, Ubuntu 24.04.4 LTS (x86_64) and microbenchmark 1.5.0. Versions of additional R packages are recorded in [`running_times_sessionInfo.txt`](running_times_sessionInfo.txt). CPU model, background workload and power settings were not captured, and absolute times can vary across machines and runs.

The main observed pattern is that the threshold-only step is short relative to the joint training/validation/threshold procedure in the cases where both are measured. 
