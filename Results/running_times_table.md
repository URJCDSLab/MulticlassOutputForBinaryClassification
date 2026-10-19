# Running-time results

Repetitions per method/scenario: **100**.

**What is timed:** After-the-model measures threshold optimization using
previously computed probabilities; In-the-model includes SVM training /
cross-validation and threshold optimization. Data preparation, train/test
splits, and initial fitting for After-the-model are excluded. These timings
therefore cover different computational workloads.

For real datasets where both methods are benchmarked, both use the same
training partition (70% of the data). Measurements depend on the machine
and installed software versions.

Reproduce from the repository root with `source("RunAllRunningTimes.R")`.
The underlying CSV is [`running_times.csv`](running_times.csv).

| Scenario | Method | Repetitions | Min (s) | Mean (s) | Median (s) | Max (s) |
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

