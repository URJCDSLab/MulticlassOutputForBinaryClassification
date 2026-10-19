# Analysis Constraint Robutness

Constraint-sensitivity experiments for *From Binary Probabilities to Ordinal Decision Categories: An Optimization-Based Risk-Stratification Framework*.

## Reproducibility

Run from the repository root:

```r
source("RunAllConstraintSensitivity.R")
run_constraint_sensitivity()
```

[`../ConstraintRobutness.R`](../ConstraintRobutness.R) contains the sensitivity functions and [`../RunAllConstraintSensitivity.R`](../RunAllConstraintSensitivity.R) runs all scenarios and saves the results. Completed cases are reused from their per-case CSV checkpoints, use `overwrite = TRUE` to recompute them.

All numerical results, including the number of constraints modified, are available in [`constraint_sensitivity.csv`](constraint_sensitivity.csv). [`constraint_sensitivity_table.md`](constraint_sensitivity_table.md) provides a compact test-set summary.

## Results

Entries report mean (standard deviation) of absolute changes relative to the unperturbed configuration. GPS and confusion-matrix (CM) proportion changes are reported separately. For *After-the-model*, the summaries include only perturbations with feasible thresholds on the training data. For *In-the-model*, thresholds must satisfy the constraints within their training folds, runs are retained when validation GPS is finite, regardless of constraint satisfaction on validation or test data. `–` denotes an undefined standard deviation. Simulated cases use the complete dataset, real-data results are shown on the external test partition unless otherwise indicated.

| Dataset / case | Method | Partition | GPS difference | CM-proportion difference | Retained perturbations |
|:--|:--|:--|--:|--:|--:|
| Simulated – 3 categories | After-the-model | Full dataset | 0.097 (0.107) | 0.035 (0.049) | 17 |
| Simulated – 3 categories | In-the-model | Full dataset | 0.084 (0.080) | 0.073 (0.081) | 17 |
| Simulated – 2 categories | After-the-model | Full dataset | 0.047 (0.047) | 0.044 (0.035) | 2 |
| Simulated – 2 categories | In-the-model | Full dataset | 0.016 (0.023) | 0.032 (0.039) | 2 |
| Mammographic | After-the-model | Train | 0.008 (0.007) | 0.020 (0.048) | 341 |
| Mammographic | After-the-model | Test | 0.011 (0.007) | 0.020 (0.047) | 341 |
| Mammographic | In-the-model | CV validation | 0.030 (0.018) | 0.037 (0.036) | 728 |
| Mammographic | In-the-model | Test | 0.025 (0.024) | 0.035 (0.038) | 728 |
| Pima | After-the-model | Test | 0.004 (0.006) | 0.016 (0.026) | 53 |
| Pima | In-the-model | Test | 0.009 (0.007) | 0.022 (0.032) | 80 |
| CTG | After-the-model | Test | 0.119 (–) | 0.014 (0.013) | 1 |
| Phishing | After-the-model | Test | 0.018 (0.006) | 0.018 (0.016) | 2 |
| Credit Card Fraud / Minimum suspicious | After-the-model | Test | 0.074 (0.104) | 9.17e-05 (0.000193) | 2 |
| Credit Card Fraud / High-purity High | After-the-model | Test | 0.005 (–) | 2.73e-05 (1.91e-05) | 1 |
| Credit Card Fraud / Uncertainty | After-the-model | Test | 0.000 (0.000) | 0.000 (0.000) | 80 |

## Results by number of perturbed constraints

The following tables provide means, medians, standard deviations and counts for each number of simultaneously modified constraints. `TOTAL` aggregates all retained perturbations in that case and partition.

### Simulated – 3 categories / After-the-model

| Partition | Constraints changed | Retained perturbations | Mean GPS diff. | Median GPS diff. | SD GPS diff. | Mean CM diff. | Median CM diff. | SD CM diff. |
|:--|--:|--:|--:|--:|--:|--:|--:|--:|
| Full dataset | 1 | 5 | 0.054875 | 0.000000 | 0.102395 | 0.020106 | 0.000000 | 0.041725 |
| Full dataset | 2 | 8 | 0.102891 | 0.038815 | 0.111030 | 0.037698 | 0.017460 | 0.050451 |
| Full dataset | 3 | 4 | 0.137188 | 0.137188 | 0.113591 | 0.050265 | 0.043651 | 0.052995 |
| Full dataset | TOTAL | 17 | 0.096839 | 0.038815 | 0.106815 | 0.035481 | 0.000000 | 0.049481 |

### Simulated – 3 categories / In-the-model

| Partition | Constraints changed | Retained perturbations | Mean GPS diff. | Median GPS diff. | SD GPS diff. | Mean CM diff. | Median CM diff. | SD CM diff. |
|:--|--:|--:|--:|--:|--:|--:|--:|--:|
| Full dataset | 1 | 5 | 0.048671 | 0.005815 | 0.079209 | 0.043879 | 0.001515 | 0.070685 |
| Full dataset | 2 | 8 | 0.089478 | 0.052838 | 0.082324 | 0.077740 | 0.066667 | 0.081554 |
| Full dataset | 3 | 4 | 0.118117 | 0.118770 | 0.079045 | 0.100631 | 0.081818 | 0.082618 |
| Full dataset | TOTAL | 17 | 0.084214 | 0.052838 | 0.080007 | 0.073167 | 0.063636 | 0.080796 |

### Simulated – 2 categories / After-the-model

| Partition | Constraints changed | Retained perturbations | Mean GPS diff. | Median GPS diff. | SD GPS diff. | Mean CM diff. | Median CM diff. | SD CM diff. |
|:--|--:|--:|--:|--:|--:|--:|--:|--:|
| Full dataset | 1 | 2 | 0.047269 | 0.047269 | 0.046670 | 0.043651 | 0.031746 | 0.035136 |
| Full dataset | TOTAL | 2 | 0.047269 | 0.047269 | 0.046670 | 0.043651 | 0.031746 | 0.035136 |

### Simulated – 2 categories / In-the-model

| Partition | Constraints changed | Retained perturbations | Mean GPS diff. | Median GPS diff. | SD GPS diff. | Mean CM diff. | Median CM diff. | SD CM diff. |
|:--|--:|--:|--:|--:|--:|--:|--:|--:|
| Full dataset | 1 | 2 | 0.016017 | 0.016017 | 0.022651 | 0.031970 | 0.020000 | 0.038672 |
| Full dataset | TOTAL | 2 | 0.016017 | 0.016017 | 0.022651 | 0.031970 | 0.020000 | 0.038672 |

### Mammographic / After-the-model

| Partition | Constraints changed | Retained perturbations | Mean GPS diff. | Median GPS diff. | SD GPS diff. | Mean CM diff. | Median CM diff. | SD CM diff. |
|:--|--:|--:|--:|--:|--:|--:|--:|--:|
| Train | 1 | 9 | 0.002702 | 0.000000 | 0.005419 | 0.002339 | 0.000000 | 0.006691 |
| Train | 2 | 36 | 0.005613 | 0.00070042 | 0.006394 | 0.007863 | 0.000000 | 0.026533 |
| Train | 3 | 82 | 0.007764 | 0.010572 | 0.006408 | 0.015003 | 0.000000 | 0.040841 |
| Train | 4 | 110 | 0.008541 | 0.010572 | 0.006383 | 0.021548 | 0.008591 | 0.050365 |
| Train | 5 | 80 | 0.007992 | 0.006702 | 0.006718 | 0.026160 | 0.013746 | 0.055704 |
| Train | 6 | 24 | 0.006612 | 0.002218 | 0.007114 | 0.028780 | 0.018900 | 0.058218 |
| Train | TOTAL | 341 | 0.007626 | 0.010369 | 0.006565 | 0.019613 | 0.006873 | 0.047987 |
| Test | 1 | 9 | 0.002324 | 0.000000 | 0.005525 | 0.002240 | 0.000000 | 0.007127 |
| Test | 2 | 36 | 0.005921 | 0.002186 | 0.007554 | 0.007672 | 0.000000 | 0.026293 |
| Test | 3 | 82 | 0.009647 | 0.013203 | 0.007703 | 0.014863 | 0.000000 | 0.040267 |
| Test | 4 | 110 | 0.012332 | 0.013203 | 0.006354 | 0.021692 | 0.012097 | 0.049500 |
| Test | 5 | 80 | 0.013514 | 0.013203 | 0.004391 | 0.026815 | 0.018145 | 0.054562 |
| Test | 6 | 24 | 0.013476 | 0.013203 | 0.002265 | 0.030074 | 0.020161 | 0.056815 |
| Test | TOTAL | 341 | 0.011103 | 0.013203 | 0.006793 | 0.019848 | 0.008065 | 0.047188 |

### Mammographic / In-the-model

| Partition | Constraints changed | Retained perturbations | Mean GPS diff. | Median GPS diff. | SD GPS diff. | Mean CM diff. | Median CM diff. | SD CM diff. |
|:--|--:|--:|--:|--:|--:|--:|--:|--:|
| CV validation | 1 | 12 | 0.014788 | 0.012013 | 0.015854 | 0.026654 | 0.013060 | 0.035680 |
| CV validation | 2 | 60 | 0.024017 | 0.022483 | 0.017019 | 0.036767 | 0.025773 | 0.038583 |
| CV validation | 3 | 160 | 0.029166 | 0.024309 | 0.017606 | 0.038692 | 0.030558 | 0.037939 |
| CV validation | 4 | 240 | 0.031423 | 0.030090 | 0.018119 | 0.037754 | 0.030346 | 0.035928 |
| CV validation | 5 | 192 | 0.031531 | 0.030090 | 0.018525 | 0.036823 | 0.026201 | 0.034544 |
| CV validation | 6 | 64 | 0.029656 | 0.030090 | 0.019241 | 0.036877 | 0.026132 | 0.034182 |
| CV validation | TOTAL | 728 | 0.029915 | 0.030090 | 0.018262 | 0.037373 | 0.028866 | 0.036116 |
| Test | 1 | 12 | 0.016248 | 0.00062063 | 0.024669 | 0.028142 | 0.014113 | 0.037790 |
| Test | 2 | 60 | 0.022316 | 0.009207 | 0.025103 | 0.035887 | 0.024194 | 0.038395 |
| Test | 3 | 160 | 0.024389 | 0.011267 | 0.025191 | 0.035919 | 0.024194 | 0.037417 |
| Test | 4 | 240 | 0.025057 | 0.013977 | 0.024853 | 0.034753 | 0.020161 | 0.036917 |
| Test | 5 | 192 | 0.025112 | 0.013977 | 0.023504 | 0.034605 | 0.020161 | 0.037764 |
| Test | 6 | 64 | 0.025343 | 0.016258 | 0.021839 | 0.035251 | 0.024194 | 0.039697 |
| Test | TOTAL | 728 | 0.024579 | 0.013977 | 0.024299 | 0.034998 | 0.020161 | 0.037637 |

### Pima / After-the-model

| Partition | Constraints changed | Retained perturbations | Mean GPS diff. | Median GPS diff. | SD GPS diff. | Mean CM diff. | Median CM diff. | SD CM diff. |
|:--|--:|--:|--:|--:|--:|--:|--:|--:|
| Train | 1 | 7 | 0.00053825 | 0.000000 | 0.001424 | 0.008975 | 0.000000 | 0.025304 |
| Train | 2 | 18 | 0.001047 | 0.000000 | 0.001737 | 0.015221 | 0.000000 | 0.032683 |
| Train | 3 | 20 | 0.001507 | 0.000000 | 0.001894 | 0.019498 | 0.003717 | 0.037048 |
| Train | 4 | 8 | 0.001884 | 0.001884 | 0.002014 | 0.022351 | 0.005576 | 0.039957 |
| Train | TOTAL | 53 | 0.001280 | 0.000000 | 0.001801 | 0.017086 | 0.001859 | 0.034871 |
| Test | 1 | 7 | 0.001688 | 0.000000 | 0.004467 | 0.009689 | 0.000000 | 0.020436 |
| Test | 2 | 18 | 0.003283 | 0.000000 | 0.005447 | 0.015169 | 0.000000 | 0.025445 |
| Test | 3 | 20 | 0.004727 | 0.000000 | 0.005940 | 0.017957 | 0.004348 | 0.027409 |
| Test | 4 | 8 | 0.005909 | 0.005909 | 0.006317 | 0.019239 | 0.010870 | 0.028116 |
| Test | TOTAL | 53 | 0.004014 | 0.000000 | 0.005651 | 0.016112 | 0.004348 | 0.026129 |

### Pima / In-the-model

| Partition | Constraints changed | Retained perturbations | Mean GPS diff. | Median GPS diff. | SD GPS diff. | Mean CM diff. | Median CM diff. | SD CM diff. |
|:--|--:|--:|--:|--:|--:|--:|--:|--:|
| CV validation | 1 | 8 | 0.002329 | 0.000000 | 0.005557 | 0.007621 | 0.000000 | 0.015559 |
| CV validation | 2 | 24 | 0.004658 | 0.001380 | 0.006713 | 0.014002 | 0.005576 | 0.020380 |
| CV validation | 3 | 32 | 0.006988 | 0.002761 | 0.007077 | 0.019145 | 0.011152 | 0.023936 |
| CV validation | 4 | 16 | 0.009317 | 0.009317 | 0.006771 | 0.023048 | 0.014870 | 0.027469 |
| CV validation | TOTAL | 80 | 0.006289 | 0.002761 | 0.006976 | 0.017230 | 0.011152 | 0.023403 |
| Test | 1 | 8 | 0.003454 | 0.000000 | 0.006802 | 0.009674 | 0.000000 | 0.020475 |
| Test | 2 | 24 | 0.006908 | 0.004741 | 0.007719 | 0.017971 | 0.004348 | 0.027301 |
| Test | 3 | 32 | 0.010362 | 0.009483 | 0.007175 | 0.024891 | 0.013043 | 0.032507 |
| Test | 4 | 16 | 0.013816 | 0.013816 | 0.004475 | 0.030435 | 0.013043 | 0.037595 |
| Test | TOTAL | 80 | 0.009326 | 0.009483 | 0.007432 | 0.022402 | 0.008696 | 0.031703 |

### CTG / After-the-model

| Partition | Constraints changed | Retained perturbations | Mean GPS diff. | Median GPS diff. | SD GPS diff. | Mean CM diff. | Median CM diff. | SD CM diff. |
|:--|--:|--:|--:|--:|--:|--:|--:|--:|
| Train | 1 | 1 | 0.075695 | 0.075695 | – | 0.011641 | 0.009402 | 0.011544 |
| Train | TOTAL | 1 | 0.075695 | 0.075695 | – | 0.011641 | 0.009402 | 0.011544 |
| Test | 1 | 1 | 0.118869 | 0.118869 | – | 0.013605 | 0.010989 | 0.013492 |
| Test | TOTAL | 1 | 0.118869 | 0.118869 | – | 0.013605 | 0.010989 | 0.013492 |

### Phishing / After-the-model

| Partition | Constraints changed | Retained perturbations | Mean GPS diff. | Median GPS diff. | SD GPS diff. | Mean CM diff. | Median CM diff. | SD CM diff. |
|:--|--:|--:|--:|--:|--:|--:|--:|--:|
| Train | 1 | 2 | 0.017900 | 0.017900 | 0.005821 | 0.017562 | 0.010907 | 0.015875 |
| Train | TOTAL | 2 | 0.017900 | 0.017900 | 0.005821 | 0.017562 | 0.010907 | 0.015875 |
| Test | 1 | 2 | 0.018287 | 0.018287 | 0.006183 | 0.017665 | 0.011004 | 0.016141 |
| Test | TOTAL | 2 | 0.018287 | 0.018287 | 0.006183 | 0.017665 | 0.011004 | 0.016141 |

### Credit Card Fraud / Minimum suspicious

| Partition | Constraints changed | Retained perturbations | Mean GPS diff. | Median GPS diff. | SD GPS diff. | Mean CM diff. | Median CM diff. | SD CM diff. |
|:--|--:|--:|--:|--:|--:|--:|--:|--:|
| Train | 1 | 2 | 0.082343 | 0.082343 | 0.116450 | 0.00011369 | 0.000000 | 0.00022803 |
| Train | TOTAL | 2 | 0.082343 | 0.082343 | 0.116450 | 0.00011369 | 0.000000 | 0.00022803 |
| Test | 1 | 2 | 0.073566 | 0.073566 | 0.104038 | 9.1681e-05 | 0.000000 | 0.00019308 |
| Test | TOTAL | 2 | 0.073566 | 0.073566 | 0.104038 | 9.1681e-05 | 0.000000 | 0.00019308 |

### Credit Card Fraud / High-purity High

| Partition | Constraints changed | Retained perturbations | Mean GPS diff. | Median GPS diff. | SD GPS diff. | Mean CM diff. | Median CM diff. | SD CM diff. |
|:--|--:|--:|--:|--:|--:|--:|--:|--:|
| Train | 1 | 1 | 0.00029763 | 0.00029763 | – | 1.8392e-05 | 2.0064e-05 | 1.2149e-05 |
| Train | TOTAL | 1 | 0.00029763 | 0.00029763 | – | 1.8392e-05 | 2.0064e-05 | 1.2149e-05 |
| Test | 1 | 1 | 0.005363 | 0.005363 | – | 2.7309e-05 | 2.926e-05 | 1.9113e-05 |
| Test | TOTAL | 1 | 0.005363 | 0.005363 | – | 2.7309e-05 | 2.926e-05 | 1.9113e-05 |

### Credit Card Fraud / Uncertainty

| Partition | Constraints changed | Retained perturbations | Mean GPS diff. | Median GPS diff. | SD GPS diff. | Mean CM diff. | Median CM diff. | SD CM diff. |
|:--|--:|--:|--:|--:|--:|--:|--:|--:|
| Train | 1 | 8 | 0.000000 | 0.000000 | 0.000000 | 0.000000 | 0.000000 | 0.000000 |
| Train | 2 | 24 | 0.000000 | 0.000000 | 0.000000 | 0.000000 | 0.000000 | 0.000000 |
| Train | 3 | 32 | 0.000000 | 0.000000 | 0.000000 | 0.000000 | 0.000000 | 0.000000 |
| Train | 4 | 16 | 0.000000 | 0.000000 | 0.000000 | 0.000000 | 0.000000 | 0.000000 |
| Train | TOTAL | 80 | 0.000000 | 0.000000 | 0.000000 | 0.000000 | 0.000000 | 0.000000 |
| Test | 1 | 8 | 0.000000 | 0.000000 | 0.000000 | 0.000000 | 0.000000 | 0.000000 |
| Test | 2 | 24 | 0.000000 | 0.000000 | 0.000000 | 0.000000 | 0.000000 | 0.000000 |
| Test | 3 | 32 | 0.000000 | 0.000000 | 0.000000 | 0.000000 | 0.000000 | 0.000000 |
| Test | 4 | 16 | 0.000000 | 0.000000 | 0.000000 | 0.000000 | 0.000000 | 0.000000 |
| Test | TOTAL | 80 | 0.000000 | 0.000000 | 0.000000 | 0.000000 | 0.000000 | 0.000000 |

