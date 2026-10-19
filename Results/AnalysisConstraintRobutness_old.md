# Analysis Constraint Robutness
DSLAB

To evaluate the sensitivity of the proposed frameword to small changes
in the constraints, we conducted an experiment in which each constraint
was perturbed by $\pm 0.05$. For a problem with multiple constraints, we
considered all possible combinations of changes: first modifying one
constraint at a time, then all combinations of two constraints, and so
on, up to all constraints simultaneously.

For each combination, we recomputed the results and summarized the
impact on model performance as well as on the confusion matrix
proportions. The results are grouped according to the number of
constraints changed, providing insights into how performance and
prediction distributions vary as more constraints are perturbed.

In the different tables, num_changes indicates the number of constraints
modified, mean_dif_GPS and median_dif_GPS summarize the changes in
performance with regard to the results without altering the constraints,
and the other columns describe the corresponding differences in
confusion matrix proportions.

``` r
source('../GeneralAndMethodFunctions.R')
source('../ConstraintRobutness.R')

library(plyr)
library(dplyr)
library(ggplot2)
library(readr)
library(tidyr)
library(caret)
library(e1071)
library(pracma)
library(CORElearn)
```

# Simulated data with 3 categories

Generation of simulated data ans selection of best parameter for SVM.

``` r
set.seed(2812)
a1 = rnorm(300,1,0.25)
a2 = rnorm(30,2,0.15)
b1 = rnorm(300,1.5,0.25)

set.seed(12)
a12 = rnorm(300,2.5,0.25)
a22 = rnorm(30,1.5,0.15)
b12 = rnorm(300,2.5,0.25)

a=c(a1,a2)
a_2=c(a12,a22)

class_0 = data.frame("x1"=a, "x2"=a_2, "class"="0")
class_1 = data.frame("x1"=b1, "x2"=b12, "class"="1")
df = rbind(class_0, class_1)
df[,-3] = scale(df[,-3])

df %>% ggplot(aes(x = x1, y = x2, colour = class)) + geom_point()
```

![](AnalysisConstraintRobutness_files/figure-commonmark/unnamed-chunk-3-1.png)

``` r
set.seed(1)
df$class <- as.factor(df$class) 
svm_cv <- tune("svm", class ~ ., data = df,
               kernel = 'linear', scale=FALSE,
               ranges = list(cost = c(0.001, 0.01, 0.1, 1, 5, 10, 20, 50, 100,
                                      150, 200)))
summary(svm_cv)
```


    Parameter tuning of 'svm':

    - sampling method: 10-fold cross validation 

    - best parameters:
     cost
        1

    - best performance: 0.2380952 

    - Detailed performance results:
          cost     error dispersion
    1    0.001 0.4761905 0.03509656
    2    0.010 0.2460317 0.06661205
    3    0.100 0.2396825 0.06585123
    4    1.000 0.2380952 0.06260402
    5    5.000 0.2396825 0.06412821
    6   10.000 0.2396825 0.06412821
    7   20.000 0.2396825 0.06412821
    8   50.000 0.2396825 0.06412821
    9  100.000 0.2396825 0.06412821
    10 150.000 0.2396825 0.06412821
    11 200.000 0.2396825 0.06412821

``` r
best_cost=svm_cv$best.model$cost

set.seed(1)
df$class <- as.factor(df$class)
df_no_target <- df[, setdiff(names(df), "class")]
model_svm = svm(class ~ ., data=df, kernel="linear", scale=FALSE, probability=TRUE, type="C", cost=best_cost)
pred_svm = predict(model_svm, df_no_target, probability=TRUE, decision.values = TRUE)
#coef_3x2 = coef(model_svm_3x2) # se usa para los plots de la SVM

probs_model_train <- attr(pred_svm,"probabilities")
probs_model_train <- probs_model_train[,c("0", "1")]
probs_model_train_class1 <- probs_model_train[,2]
```

## Max GPS with constraints. After the model

``` r
num_cuts = 2
vector_GPS = c('p13', 'p_11','r31' )

# Original thresholds
original_thresholds <- c(0.8, 0.9, 0.4)
n_thresholds <- length(original_thresholds)
delta <- 0.05


make_user_condition <- function(thr) {
  function(p, p_, r, r_) {
    (p[3] > thr[1]) & (p_[1] > thr[2]) & (r_[1] > thr[3])
  }
}



summary_df_sa1 <- EvaluateConstraintRobustnessAfter(original_thresholds, delta, make_user_condition,
                                                              df,probs_model_train,vector_GPS,num_cuts)

summary_df_sa1
```

       num_changes mean_dif_GPS median_dif_GPS sd_dif_GPS mean_CM_prop_diff
    1            1   0.05487519      0.0000000  0.1023951        0.02010582
    2            2   0.10289098      0.0388153  0.1110302        0.03769841
    3            3   0.13718798      0.1371880  0.1135910        0.05026455
    11       TOTAL   0.09683857      0.0388153  0.1068152        0.03548086
       median_CM_prop_diff sd_CM_prop_diff changes_sizes
    1           0.00000000      0.04172486             5
    2           0.01746032      0.05045108             8
    3           0.04365079      0.05299464             4
    11          0.00000000      0.04948094            17

## Max GPS with constraints. In the model

``` r
summary_df_si1
```

       num_changes mean_dif_GPS median_dif_GPS sd_dif_GPS mean_CM_prop_diff
    1            1   0.04867078    0.005814582 0.07920864        0.04387879
    2            2   0.08947790    0.052837939 0.08232430        0.07773990
    3            3   0.11811732    0.118769666 0.07904498        0.10063131
    11       TOTAL   0.08421449    0.052837939 0.08000680        0.07316696
       median_CM_prop_diff sd_CM_prop_diff changes_sizes
    1          0.001515152      0.07068467             5
    2          0.066666667      0.08155389             8
    3          0.081818182      0.08261820             4
    11         0.063636364      0.08079588            17

# Simulated data with 2 categories

Max GPS with restrictions:

``` r
num_cuts = 1
vector_GPS = c('p12', 'p_11', 'r21','r1_1')


# Original thresholds
original_thresholds <- c(0.85)
n_thresholds <- length(original_thresholds)
delta <- 0.05


make_user_condition <- function(thr) {
  function(p, p_, r, r_) {
    (p_[1] > thr[1])
  }
}


summary_df_sa2 <- EvaluateConstraintRobustnessAfter(original_thresholds, delta, make_user_condition,
                                                              df,probs_model_train,vector_GPS,num_cuts)
summary_df_sa2
```

       num_changes mean_dif_GPS median_dif_GPS sd_dif_GPS mean_CM_prop_diff
    1            1   0.04726891     0.04726891 0.04666988        0.04365079
    11       TOTAL   0.04726891     0.04726891 0.04666988        0.04365079
       median_CM_prop_diff sd_CM_prop_diff changes_sizes
    1           0.03174603      0.03513642             2
    11          0.03174603      0.03513642             2

In the model

``` r
summary_df_si2
```

       num_changes mean_dif_GPS median_dif_GPS sd_dif_GPS mean_CM_prop_diff
    1            1   0.01601655     0.01601655 0.02265083         0.0319697
    11       TOTAL   0.01601655     0.01601655 0.02265083         0.0319697
       median_CM_prop_diff sd_CM_prop_diff changes_sizes
    1                 0.02      0.03867238             2
    11                0.02      0.03867238             2

# Mammographic dataset

## Max GPS with restrictions: case 4x2. After the model.

``` r
num_cuts = 3
vector_GPS = c('p14', 'p_11', 'r1_1') 

# Original thresholds
original_thresholds <- c(0.5, 0.1, 0.7, 0.85, 0.85, 0.8)
n_thresholds <- length(original_thresholds)
delta <- 0.05


make_user_condition <- function(thr) {
  function(p, p_, r, r_) {
    (r_[1] > thr[1]) & (r_[2] > thr[2])   & (p[3]>thr[3]) & (p[3]<thr[4]) & (p[4]>thr[5]) & (p_[1]>thr[6])
  }
}



summary_df_mammoa1 <- EvaluateConstraintRobustnessAfterTrainTest(original_thresholds, delta,
                                                              make_user_condition,dfTrain,probs_model_train,
                                                              vector_GPS,num_cuts,
                                                             dfTest,probs_model_test)

summary_df_mammoa1_train <- summary_df_mammoa1$train_summary
summary_df_mammoa1_test <- summary_df_mammoa1$test_summary
```

### Train

``` r
summary_df_mammoa1_train
```

       num_changes mean_dif_GPS median_dif_GPS  sd_dif_GPS mean_CM_prop_diff
    1            1  0.002701565   0.0000000000 0.005418993       0.002338679
    2            2  0.005612806   0.0007004223 0.006394112       0.007863211
    3            3  0.007763860   0.0105723416 0.006407609       0.015002934
    4            4  0.008541235   0.0105723416 0.006382727       0.021547954
    5            5  0.007991628   0.0067018821 0.006718031       0.026159794
    6            6  0.006611832   0.0022178640 0.007114220       0.028780069
    11       TOTAL  0.007626280   0.0103688810 0.006565250       0.019613326
       median_CM_prop_diff sd_CM_prop_diff changes_sizes
    1          0.000000000     0.006690788             9
    2          0.000000000     0.026533350            36
    3          0.000000000     0.040840674            82
    4          0.008591065     0.050364714           110
    5          0.013745704     0.055704408            80
    6          0.018900344     0.058217812            24
    11         0.006872852     0.047987105           341

### Test

``` r
summary_df_mammoa1_train
```

       num_changes mean_dif_GPS median_dif_GPS  sd_dif_GPS mean_CM_prop_diff
    1            1  0.002701565   0.0000000000 0.005418993       0.002338679
    2            2  0.005612806   0.0007004223 0.006394112       0.007863211
    3            3  0.007763860   0.0105723416 0.006407609       0.015002934
    4            4  0.008541235   0.0105723416 0.006382727       0.021547954
    5            5  0.007991628   0.0067018821 0.006718031       0.026159794
    6            6  0.006611832   0.0022178640 0.007114220       0.028780069
    11       TOTAL  0.007626280   0.0103688810 0.006565250       0.019613326
       median_CM_prop_diff sd_CM_prop_diff changes_sizes
    1          0.000000000     0.006690788             9
    2          0.000000000     0.026533350            36
    3          0.000000000     0.040840674            82
    4          0.008591065     0.050364714           110
    5          0.013745704     0.055704408            80
    6          0.018900344     0.058217812            24
    11         0.006872852     0.047987105           341

## Max GPS with restrictions: case 4x2. In the model.

### Train

``` r
summary_df_mammoi2_train
```

       num_changes mean_dif_GPS median_dif_GPS sd_dif_GPS mean_CM_prop_diff
    1            1   0.01697076     0.01201324 0.01881165        0.02633106
    2            2   0.02621986     0.02248289 0.01858357        0.03650926
    3            3   0.03067044     0.03009046 0.01826513        0.03857155
    4            4   0.03232123     0.03009046 0.01841468        0.03771231
    5            5   0.03208975     0.03009046 0.01870461        0.03679711
    6            6   0.02965556     0.03009046 0.01924103        0.03687654
    11       TOTAL   0.03090714     0.03009046 0.01865535        0.03729955
       median_CM_prop_diff sd_CM_prop_diff changes_sizes
    1           0.01095691      0.03548413            12
    2           0.02593180      0.03849666            60
    3           0.03055776      0.03789413           160
    4           0.03055776      0.03590095           240
    5           0.02675126      0.03452589           192
    6           0.02613225      0.03418237            64
    11          0.02916511      0.03608199           728

### Test

``` r
summary_df_mammoi2_test
```

       num_changes mean_dif_GPS median_dif_GPS sd_dif_GPS mean_CM_prop_diff
    1            1   0.01617972   0.0003922979 0.02471637        0.02738575
    2            2   0.02224761   0.0092066723 0.02516295        0.03553427
    3            3   0.02434248   0.0112666495 0.02523492        0.03601310
    4            4   0.02502940   0.0139770124 0.02488032        0.03504284
    5            5   0.02509506   0.0139770124 0.02352225        0.03488848
    6            6   0.02534298   0.0162583324 0.02183914        0.03525076
    11       TOTAL   0.02454817   0.0139770124 0.02432930        0.03514794
       median_CM_prop_diff sd_CM_prop_diff changes_sizes
    1           0.01209677      0.03792046            12
    2           0.02217742      0.03862034            60
    3           0.02419355      0.03752325           160
    4           0.02016129      0.03695399           240
    5           0.02419355      0.03777807           192
    6           0.02419355      0.03969670            64
    11          0.02419355      0.03769818           728

# Pima

## Max GPS with restrictions: case 5x2. After the model.

Sale una diferencia de 0 en GPS porque se calcula con p15 y p_11 y las
categorías 1, 4 y 5 permanecen constantes en todos los casos. Los únicos
valores que cambian son los relativos a las categorías 2 y 3 mientras
que esos se mantienen iguales.

``` r
num_cuts = 4
vector_GPS = c('p15', 'p_11') 

# Original thresholds
original_thresholds <- c(0.3, 0.7, 0.4, 0.9)
n_thresholds <- length(original_thresholds)
delta <- 0.05


make_user_condition <- function(thr) {
  function(p, p_, r, r_) {
    (r_[1] > thr[1]) & (p[4] > thr[2])   & (p[3]>thr[3]) & (p[5]>thr[4])
  }
}



summary_df_pimaa1 <- EvaluateConstraintRobustnessAfterTrainTest(original_thresholds, delta,
                                                              make_user_condition,dfTrain,probs_model_train,
                                                              vector_GPS,num_cuts,
                                                             dfTest,probs_model_test)

summary_df_pimaa1_train <- summary_df_pimaa1$train_summary
summary_df_pimaa1_test <- summary_df_pimaa1$test_summary
```

### Test

``` r
summary_df_pimaa1_test
```

       num_changes mean_dif_GPS median_dif_GPS sd_dif_GPS mean_CM_prop_diff
    1            1            0              0          0        0.00757764
    2            2            0              0          0        0.01207729
    3            3            0              0          0        0.01404348
    4            4            0              0          0        0.01456522
    11       TOTAL            0              0          0        0.01260049
       median_CM_prop_diff sd_CM_prop_diff changes_sizes
    1          0.000000000      0.02098484             7
    2          0.000000000      0.02510312            18
    3          0.000000000      0.02622754            20
    4          0.002173913      0.02631618             8
    11         0.000000000      0.02525204            53

## In the model Case 5x2

### Test

``` r
summary_df_pimai2_test
```

       num_changes mean_dif_GPS median_dif_GPS  sd_dif_GPS mean_CM_prop_diff
    1            1  0.003453947    0.000000000 0.006801936       0.009673913
    2            2  0.006907895    0.004741379 0.007719422       0.017971014
    3            3  0.010361842    0.009482759 0.007174911       0.024891304
    4            4  0.013815789    0.013815789 0.004475135       0.030434783
    11       TOTAL  0.009325658    0.009482759 0.007432168       0.022402174
       median_CM_prop_diff sd_CM_prop_diff changes_sizes
    1          0.000000000      0.02047479             8
    2          0.004347826      0.02730139            24
    3          0.013043478      0.03250681            32
    4          0.013043478      0.03759482            16
    11         0.008695652      0.03170284            80
