# Variance across splits — tables

Tables are computed from the external-test confusion matrices of the repeated 70/30 splits.
A valid split contains all requested output categories.

## Pima

| Method / configuration | Valid splits | Positive-class prevalence (%) | Category size (%) |
|:--|:--:|:--|:--|
| Equal-frequency | 10/10 | [4.0, 18.6, 25.1, 48.8, 77.8] | [23.1, 15.7, 16.8, 27.3, 17.2] |
| Optimal binning | 10/10 | [4.0, 20.2, 38.9, 68.3, 85.0] | [23.0, 26.1, 25.8, 16.6, 8.5] |
| After-the-model, constrained | 10/10 | [7.0, 23.6, 37.9, 65.2, 99.1] | [29.9, 23.4, 14.3, 31.1, 1.3] |
| In-the-model, constrained | 10/10 | [4.3, 4.9, 46.3, 79.7, 100.0] | [21.5, 11.7, 60.0, 6.3, 0.5] |

## Cardiotocography (CTG)

| Method / configuration | Valid splits | Positive-class prevalence (%) | Category size (%) |
|:--|:--:|:--|:--|
| Equal-width | 10/10 | [0.7, 53.8, 97.4] | [91.3, 2.0, 6.7] |
| Equal-frequency | 0/10 | [empty, 0.0, 25.2] | [0.0, 67.5, 32.5] |
| Optimal binning | 9/10 | [0.1, 11.8, 91.4] | [84.3, 7.6, 8.0] |
| After-the-model, constrained | 10/10 | [0.13, 29.15, 99.51] | [84.9, 10.0, 5.1] |

## Phishing

| Method / configuration | Valid splits | Positive-class prevalence (%) | Category size (%) |
|:--|:--:|:--|:--|
| Equal-frequency | 10/10 | [1.6, 43.2, 99.7] | [33.3, 33.4, 33.3] |
| Optimal binning | 10/10 | [4.9, 77.9, 100.0] | [47.7, 29.2, 23.0] |
| After-the-model, minimum Suspicious | 10/10 | [4.4, 47.1, 97.7] | [47.2, 10.9, 41.9] |

## Credit Card Fraud

| Method / configuration | Low fraud risk (%) | Suspicious (%) | High fraud risk (%) | Category size (%) |
|:--|--:|--:|--:|:--|
| Classical (threshold 0.5) | 0.037 [0.033, 0.041] | — | 94.6 [92.5, 96.6] | [99.86, —, 0.14] |
| Equal-width | 0.030 [0.026, 0.034] | 59.9 [52.5, 67.3] | 97.0 [96.0, 98.0] | [99.84, 0.03, 0.13] |
| Isotonic + equal-width | 0.031 [0.027, 0.035] | 47.9 [34.3, 61.6] | 95.2 [93.0, 97.4] | [99.84, 0.02, 0.14] |
| ROC-Youden | 0.018 [0.015, 0.022] | — | 13.1 [11.1, 15.2] | [98.78, —, 1.22] |
| After-the-model, Minimum Suspicious | 0.025 [0.021, 0.029] | 10.3 [7.5, 13.1] | 92.9 [90.4, 95.3] | [99.76, 0.09, 0.15] |
| After-the-model, High-purity High | 0.036 [0.032, 0.039] | 63.8 [48.1, 79.6] | 95.9 [93.8, 98.0] | [99.85, 0.01, 0.14] |
| After-the-model, Uncertainty | 0.025 [0.022, 0.029] | 48.3 [38.6, 58.1] | 97.9 [97.1, 98.7] | [99.79, 0.11, 0.10] |

