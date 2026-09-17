# Compute permutation-based predictor importance

`importance_perm()` computes model-agnostic variable importance scores
by permuting individual predictors (one at a time) and measuring how
worse model performance becomes.

## Usage

``` r
importance_perm(
  wflow,
  data,
  metrics = NULL,
  type = "original",
  size = 500,
  times = 10,
  eval_time = NULL,
  event_level = "first"
)
```

## Arguments

- wflow:

  A fitted
  [`workflows::workflow()`](https://workflows.tidymodels.org/reference/workflow.html).

- data:

  A data frame of the data passed to
  [`workflows::fit.workflow()`](https://workflows.tidymodels.org/reference/fit-workflow.html),
  including the outcome and case weights (if any).

- metrics:

  A
  [`yardstick::metric_set()`](https://yardstick.tidymodels.org/reference/metric_set.html)
  or `NULL`.

- type:

  A character string for which *level* of predictors to compute. A value
  of `"original"` (default) will return values in the same
  representation of `data`. Using `"derived"` will compute them for any
  derived features/predictors, such as dummy indicator columns, etc.

- size:

  How many data points to predict for each permutation iteration.

- times:

  How many iterations to repeat the calculations.

- eval_time:

  For censored regression models, a vector of time points at which the
  survival probability is estimated. This is only needed if a dynamic
  metric is used, such as the Brier score or the area under the ROC
  curve.

- event_level:

  A single string. Either `"first"` or `"second"` to specify which level
  of `truth` to consider as the "event". This argument is only
  applicable when `estimator = "binary"`.

## Value

A tibble with extra classes `"importance_perm"` and either
"`original_importance_perm"` or "`derived_importance_perm"`. The columns
are:

- `.metric` the name of the performance metric:

- `predictor`: the predictor

- `n`: the number of usable results (should be the same as `times`)

- `mean`: the average of the differences in performance. For each
  metric, larger values indicate worse performance (i.e., higher
  importance).

- `std_err`: the standard error of the differences.

- `importance`: the mean divided by the standard error.

- For censored regression models, an additional `.eval_time` column may
  also be included (depending on the metric requested).

## Details

The function can compute importance at two different levels.

- The "original" predictors are the unaltered columns in the source data
  set. For example, for a categorical predictor used with linear
  regression, the original predictor is the factor column.

- "Derived" predictors are the final versions given to the model. For
  the categorical predictor example, the derived versions are the binary
  indicator variables produced from the factor version.

This can make a difference when pre-processing/feature engineering is
used. This can help us understand *how* a predictor can be important

Importance scores are computed for each predictor (at the specified
level) and each performance metric. If no metric is specified, defaults
are used:

- Classification:
  [`yardstick::brier_class()`](https://yardstick.tidymodels.org/reference/brier_class.html),
  [`yardstick::roc_auc()`](https://yardstick.tidymodels.org/reference/roc_auc.html),
  and
  [`yardstick::accuracy()`](https://yardstick.tidymodels.org/reference/accuracy.html).

- Regression:
  [`yardstick::rmse()`](https://yardstick.tidymodels.org/reference/rmse.html)
  and
  [`yardstick::rsq()`](https://yardstick.tidymodels.org/reference/rsq.html).

- Censored regression:
  [`yardstick::brier_survival()`](https://yardstick.tidymodels.org/reference/brier_survival.html)

For censored data, importance is computed for each evaluation time (when
a dynamic metric is specified).

By default, no parallelism is used to process models in tune; you have
to opt-in.

### Using future to parallel process

You should install the package and choose your flavor of parallelism
using the [plan](https://future.futureverse.org/reference/plan.html)
function. This allows you to specify the number of worker processes and
the specific technology to use.

For example, you can use:

       library(future)
       plan(multisession, workers = 4)

and work will be conducted simultaneously (unless there is an exception;
see the section below).

See
[`future::plan()`](https://future.futureverse.org/reference/plan.html)
for possible options other than `multisession`.

### Using mirai to parallel process

To configure parallel processing with mirai, use the
[`mirai::daemons()`](https://mirai.r-lib.org/reference/daemons.html)
function. The first argument, `n`, determines the number of parallel
workers. Using `daemons(0)` reverts to sequential processing.

The arguments `url` and `remote` are used to set up and launch parallel
processes over the network for distributed computing. See
[`mirai::daemons()`](https://mirai.r-lib.org/reference/daemons.html)
documentation for more details.

## Examples

``` r
if (rlang::is_installed(c("modeldata", "recipes", "workflows", "parsnip"))) {
  library(modeldata)
  library(recipes)
  library(workflows)
  library(dplyr)
  library(parsnip)

  set.seed(12)
  dat_tr <-
    sim_logistic(250, ~ .1 + 2 * A - 3 * B + 1 * A *B, corr = .7) |>
    dplyr::bind_cols(sim_noise(250, num_vars = 10))

  rec <-
    recipe(class ~ ., data = dat_tr) |>
    step_interact(~ A:B) |>
    step_normalize(all_numeric_predictors()) |>
    step_pca(contains("noise"), num_comp = 5)

  lr_wflow <- workflow(rec, logistic_reg())
  lr_fit <- fit(lr_wflow, dat_tr)

  set.seed(39)
  orig_res <- importance_perm(lr_fit, data = dat_tr, type = "original",
                              size = 100, times = 3)
  orig_res

  set.seed(39)
  deriv_res <- importance_perm(lr_fit, data = dat_tr, type = "derived",
                               size = 100, times = 3)
  deriv_res
}
#> 
#> Attaching package: ‘modeldata’
#> The following object is masked from ‘package:datasets’:
#> 
#>     penguins
#> # A tibble: 24 × 6
#>    .metric     predictor     n    mean std_err importance
#>    <chr>       <chr>     <int>   <dbl>   <dbl>      <dbl>
#>  1 accuracy    B             3  0.207  0.00333     62.0  
#>  2 brier_class B             3  0.165  0.0132      12.5  
#>  3 roc_auc     B             3  0.229  0.0191      12.0  
#>  4 brier_class A             3  0.127  0.0226       5.64 
#>  5 roc_auc     A             3  0.153  0.0402       3.81 
#>  6 accuracy    A             3  0.16   0.0451       3.55 
#>  7 brier_class A_x_B         3  0.0328 0.0128       2.56 
#>  8 roc_auc     A_x_B         3  0.0653 0.0287       2.28 
#>  9 accuracy    A_x_B         3  0.07   0.0351       1.99 
#> 10 accuracy    PC1           3 -0.03   0.0436      -0.688
#> # ℹ 14 more rows
```
