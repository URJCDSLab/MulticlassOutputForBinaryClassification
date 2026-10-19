# Constraint sensitivity — summary

Average absolute changes relative to the unperturbed constraints.
Real datasets: external test partitions. Simulated data: full dataset.
For *In-the-model*, constraints are enforced in each training fold; retained runs have finite validation GPS, without requiring validation/test feasibility.
Full results by number of changed constraints: [`constraint_sensitivity.csv`](constraint_sensitivity.csv).

| Dataset / scenario | Method | Mean absolute GPS difference | Mean absolute CM difference | Retained perturbations |
|:--|:--|--:|--:|--:|
| Simulated - 3 categories / After-the-model | After-the-model | 0.09684 | 0.03548 | 17 |
| Simulated - 3 categories / In-the-model | In-the-model | 0.08421 | 0.07317 | 17 |
| Simulated - 2 categories / After-the-model | After-the-model | 0.04727 | 0.04365 | 2 |
| Simulated - 2 categories / In-the-model | In-the-model | 0.01602 | 0.03197 | 2 |
| Mammographic / After-the-model | After-the-model | 0.01110 | 0.01985 | 341 |
| Mammographic / In-the-model | In-the-model | 0.02458 | 0.03500 | 728 |
| Pima / After-the-model | After-the-model | 0.00401 | 0.01611 | 53 |
| Pima / In-the-model | In-the-model | 0.00933 | 0.02240 | 80 |
| CTG / After-the-model | After-the-model | 0.11887 | 0.01361 | 1 |
| Phishing / After-the-model | After-the-model | 0.01829 | 0.01767 | 2 |
| Credit Card / Minimum suspicious | After-the-model | 0.07357 | 0.00009 | 2 |
| Credit Card / High-purity High | After-the-model | 0.00536 | 0.00003 | 1 |
| Credit Card / Uncertainty | After-the-model | 0.00000 | 0.00000 | 80 |

