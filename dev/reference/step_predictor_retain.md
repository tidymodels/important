# Supervised Feature Selection via A Single Filter

`step_predictor_retain()` creates a *specification* of a recipe step
that uses a logical statement that includes one or more scoring
functions to measure how much each predictor is related to the outcome
value. This step retains the predictors that pass the logical statement.

## Usage

``` r
step_predictor_retain(
  recipe,
  ...,
  score,
  role = NA,
  trained = FALSE,
  results = NULL,
  removals = NULL,
  skip = FALSE,
  id = rand_id("predictor_retain")
)
```

## Arguments

- recipe:

  A recipe object. The step will be added to the sequence of operations
  for this recipe.

- ...:

  One or more selector functions to choose variables for this step. See
  [`selections()`](https://recipes.tidymodels.org/reference/selections.html)
  for more details.

- score:

  A valid R expression that produces a logical result. The equation can
  contain the names of one or more score functions from the filtro
  package, such as
  [`filtro::score_imp_rf()`](https://filtro.tidymodels.org/reference/score_imp_rf.html),
  [`filtro:: score_roc_auc()`](https://filtro.tidymodels.org/reference/score_roc_auc.html).
  See the Details and Examples sections below. This argument *should be
  named* when used.

- role:

  Not used by this step since no new variables are created.

- trained:

  A logical to indicate if the quantities for preprocessing have been
  estimated.

- results:

  A data frame of score and desirability values for each predictor
  evaluated. These values are not determined until
  [`recipes::prep()`](https://recipes.tidymodels.org/reference/prep.html)
  is called.

- removals:

  A character string that contains the names of predictors that should
  be removed. These values are not determined until
  [`recipes::prep()`](https://recipes.tidymodels.org/reference/prep.html)
  is called.

- skip:

  A logical. Should the step be skipped when the recipe is baked by
  [`bake()`](https://recipes.tidymodels.org/reference/bake.html)? While
  all operations are baked when
  [`prep()`](https://recipes.tidymodels.org/reference/prep.html) is run,
  some operations may not be able to be conducted on new data (e.g.
  processing the outcome variable(s)). Care should be taken when using
  `skip = TRUE` as it may affect the computations for subsequent
  operations.

- id:

  A character string that is unique to this step to identify it.

## Value

An updated version of `recipe` with the new step added to the sequence
of any existing operations. When you
[`tidy()`](https://recipes.tidymodels.org/reference/tidy.recipe.html)
this step, a tibble::tibble is returned with columns `terms` and `id`:

- terms:

  character, the selectors or variables selected to be removed

- id:

  character, id of this step

Once trained, additional columns are included (see Details section).

## Details

The `score` should be valid R syntax that produces a logical result and
should not use external data. The list of variables that can be used is
in the section below.

### Scoring Functions

As of version 0.2.0 of the filtro package, the following score functions
are available:

- `aov_fstat`
  ([`documentation`](https://filtro.tidymodels.org/reference/score_aov_pval.html))

- `aov_pval`
  ([`documentation`](https://filtro.tidymodels.org/reference/score_aov_pval.html))

- `cor_pearson`
  ([`documentation`](https://filtro.tidymodels.org/reference/score_cor_pearson.html))

- `cor_spearman`
  ([`documentation`](https://filtro.tidymodels.org/reference/score_cor_pearson.html))

- `gain_ratio`
  ([`documentation`](https://filtro.tidymodels.org/reference/score_info_gain.html))

- `imp_rf`
  ([`documentation`](https://filtro.tidymodels.org/reference/score_imp_rf.html))

- `imp_rf_conditional`
  ([`documentation`](https://filtro.tidymodels.org/reference/score_imp_rf.html))

- `imp_rf_oblique`
  ([`documentation`](https://filtro.tidymodels.org/reference/score_imp_rf.html))

- `info_gain`
  ([`documentation`](https://filtro.tidymodels.org/reference/score_info_gain.html))

- `roc_auc`
  ([`documentation`](https://filtro.tidymodels.org/reference/score_roc_auc.html))

- `sym_uncert`
  ([`documentation`](https://filtro.tidymodels.org/reference/score_info_gain.html))

- `xtab_pval_chisq`
  ([`documentation`](https://filtro.tidymodels.org/reference/score_xtab_pval_chisq.html))

- `xtab_pval_fisher`
  ([`documentation`](https://filtro.tidymodels.org/reference/score_xtab_pval_chisq.html))

Some important notes:

- Scores that are p-values are automatically transformed by filtro to be
  in the format `-log10(pvalue)` so that a p-value of 0.1 is converted
  to 1.0. For these, use the
  [`maximize()`](https://desirability2.tidymodels.org/reference/aliases.html)
  goal.

- Other scores are also transformed in the data. For example, the
  correlation scores given to the recipe step are in absolute value
  format. See the filtro documentation for each score.

- You can use some in-line functions using base R functions. For
  example, `maximize(max(score_cor_spearman))`.

- If a predictor cannot be computed for all scores, it is given a
  "fallback value" that will prevent it from being excluded for this
  reason.

This step can potentially remove columns from the data set. This may
cause issues for subsequent steps in your recipe if the missing columns
are specifically referenced by name. To avoid this, see the advice in
the *Tips for saving recipes and filtering columns* section of
[recipes::selections](https://recipes.tidymodels.org/reference/selections.html).

### Case Weights

Case weights can be used by some scoring functions. To learn more, load
the filtro package and check the `case_weights` property of the score
object (see Examples below). For a recipe, use one of the tidymodels
case weight functions such as
[`hardhat::importance_weights()`](https://hardhat.tidymodels.org/reference/importance_weights.html)
or
[hardhat::frequency_weights](https://hardhat.tidymodels.org/reference/frequency_weights.html),
to assign the correct data type to the vector of case weights. A recipe
will then interpret that class to be a case weight (and no other role).
A full example is below.

### Tidy method

For a trained recipe, the
[`tidy()`](https://generics.r-lib.org/reference/tidy.html) method will
return a tibble with columns `terms` (the predictor names), `id`, and
columns for the estimated scores. The score columns are the raw values,
before being filled with "safe values" or transformed.

There is an additional local column called `removed` that notes whether
the predictor failed the filter and was removed after this step is
executed.

## Examples

``` r
library(recipes)

rec <- recipe(mpg ~ ., data = mtcars) |>
  step_predictor_retain(
    all_predictors(),
    score = cor_pearson >= 0.75 | cor_spearman >= 0.75
  )

prepped <- prep(rec)

bake(prepped, mtcars)
#> # A tibble: 32 × 5
#>      cyl  disp    hp    wt   mpg
#>    <dbl> <dbl> <dbl> <dbl> <dbl>
#>  1     6  160    110  2.62  21  
#>  2     6  160    110  2.88  21  
#>  3     4  108     93  2.32  22.8
#>  4     6  258    110  3.22  21.4
#>  5     8  360    175  3.44  18.7
#>  6     6  225    105  3.46  18.1
#>  7     8  360    245  3.57  14.3
#>  8     4  147.    62  3.19  24.4
#>  9     4  141.    95  3.15  22.8
#> 10     6  168.   123  3.44  19.2
#> # ℹ 22 more rows

tidy(prepped, 1)
#> # A tibble: 10 × 5
#>    terms removed cor_pearson cor_spearman id                    
#>    <chr> <lgl>         <dbl>        <dbl> <chr>                 
#>  1 cyl   FALSE        -0.852       -0.911 predictor_retain_xL0Ac
#>  2 disp  FALSE        -0.848       -0.909 predictor_retain_xL0Ac
#>  3 hp    FALSE        -0.776       -0.895 predictor_retain_xL0Ac
#>  4 drat  TRUE          0.681        0.651 predictor_retain_xL0Ac
#>  5 wt    FALSE        -0.868       -0.886 predictor_retain_xL0Ac
#>  6 qsec  TRUE          0.419        0.467 predictor_retain_xL0Ac
#>  7 vs    TRUE          0.664        0.707 predictor_retain_xL0Ac
#>  8 am    TRUE          0.600        0.562 predictor_retain_xL0Ac
#>  9 gear  TRUE          0.480        0.543 predictor_retain_xL0Ac
#> 10 carb  TRUE         -0.551       -0.657 predictor_retain_xL0Ac
```
