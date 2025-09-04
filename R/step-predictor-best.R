#' Supervised Feature Selection via Choosing the Top Predictors
#'
#' `step_predictor_best()` creates a *specification* of a recipe step that uses
#' a single scoring function to measure how much each predictor is related to
#' the outcome value. This step retains a proportion of the most important
#' predictors, and this proportion can be tuned.
#'
#' @inheritParams step_predictor_desirability
#'
#' @param score The name of a single score function from the \pkg{filtro}
#' package, such as `"imp_rf"` (for [filtro::score_imp_rf()]), etc.
#' See the Details and Examples sections below. This argument *should be named*
#' when used.
#'
#' @export
#'
#' @details
#'
#' ```{r child = "man/rmd/filtro-scores.Rmd"}
#' ```
#'
#' Some important notes:
#'
#' - Scores that are p-values are automatically transformed by \pkg{filtro} to
#'   be in the format `-log10(pvalue)` so that a p-value of 0.1 is converted to
#'   1.0. For these, use the `maximize()` goal.
#'
#' - Other scores are also transformed in the data. For example, the correlation
#'   scores given to the recipe step are in absolute value format. See the
#'   \pkg{filtro} documentation for each score.
#'
#'  - You can use some in-line functions using base R functions. For example,
#'    `maximize(max(score_cor_spearman))`.
#'
#' - If a predictor cannot be computed for all scores, it is given a "fallback
#'   value" that will prevent it from being excluded for this reason.
#'
#' This step can potentially remove columns from the data set. This may cause
#' issues for subsequent steps in your recipe if the missing columns are
#' specifically referenced by name. To avoid this, see the advice in the _Tips
#' for saving recipes and filtering columns_ section of [recipes::selections].
#'
#'
#' ## Ties
#'
#' Note that [dplyr::slice_max()] with the argument `with_ties = TRUE ` is used
#' to select predictors. If there are many ties in overall desirability, the
#' proportion selected can be larger than the value given to `prep_terms()`.
#'
#' ## Case Weights
#'
#' Case weights can be used by some scoring functions. To learn more, load the
#' \pkg{filtro} package and check the `case_weights` property of the score object
#' (see Examples below). For a recipe, use one of the tidymodels case weight
#' functions such as [hardhat::importance_weights()] or
#' [hardhat::frequency_weights], to assign the correct data type to the vector of case
#' weights. A recipe will then interpret that class to be a case weight (and no
#' other role). A full example is below.
#'
#' ## Tidy method
#'
#' For a trained recipe, the `tidy()` method will return a tibble with columns
#' `terms` (the predictor names), `id`, and columns for the estimated scores.
#' The score columns are the raw values, before being filled with "safe values"
#' or transformed.
#'
#' There is an additional local column called `removed` that notes whether the
#' predictor failed the filter and was removed after this step is executed.
#'
#' @return An updated version of `recipe` with the new step added to the
#'  sequence of any existing operations. When you
#'  [`tidy()`][recipes::tidy.recipe] this step, a tibble::tibble is returned
#'  with columns `terms` and `id`:
#'
#' \describe{
#'   \item{terms}{character, the selectors or variables selected to be removed}
#'   \item{id}{character, id of this step}
#' }
#' Once trained, additional columns are included (see Details section).
#' @examples
#' library(recipes)
#'
#' rec <- recipe(mpg ~ ., data = mtcars) |>
#'   step_predictor_best(
#'     all_predictors(),
#'     score = "cor_spearman"
#'   )
#'
#' prepped <- prep(rec)
#'
#' bake(prepped, mtcars)
#'
#' tidy(prepped, 1)
step_predictor_best <- function(
  recipe,
  ...,
  score,
  role = NA,
  trained = FALSE,
  prop_terms = 0.5,
  update_prop = TRUE,
  results = NULL,
  removals = NULL,
  skip = FALSE,
  id = rand_id("predictor_best")
) {
  add_step(
    recipe,
    step_predictor_best_new(
      terms = enquos(...),
      score = score,
      role = role,
      trained = trained,
      prop_terms = prop_terms,
      update_prop = update_prop,
      results = results,
      removals = removals,
      skip = skip,
      id = id,
      case_weights = NULL
    )
  )
}

step_predictor_best_new <-
  function(
    terms,
    score,
    role,
    trained,
    prop_terms,
    update_prop = update_prop,
    results,
    removals,
    skip,
    id,
    case_weights
  ) {
    step(
      subclass = "predictor_best",
      terms = terms,
      score = score,
      role = role,
      trained = trained,
      prop_terms = prop_terms,
      update_prop = update_prop,
      results = results,
      removals = removals,
      skip = skip,
      id = id,
      case_weights = case_weights
    )
  }

#' @export
prep.step_predictor_best <- function(x, training, info = NULL, ...) {
  col_names <- recipes_eval_select(x$terms, training, info)
  check_type(training[, col_names], types = c("double", "integer", "factor"))

  bottom <- ifelse(x$update_prop, .Machine$double.eps, 0.0)
  check_number_decimal(
    x$prop_terms,
    min = bottom,
    max = 1,
    arg = "prop_terms"
  )

  if (x$update_prop) {
    x$prop_terms <- update_prop(length(col_names), x$prop_terms)
  }

  # First we check the _type_ of weight to see if it is used. Later, in
  # `compute_score()`, we check to see if the score supports case weights.
  wts <- get_case_weights(info, training)
  were_weights_used <- are_weights_used(wts, unsupervised = FALSE)
  if (isFALSE(were_weights_used)) {
    wts <- NULL
  }

  outcome_name <- pull_outcome_column_name(info)

  if (length(col_names) > 1) {
    filter_res <- calculate_predictor_best(
      score = x$score,
      prop_terms = x$prop_terms,
      outcome = outcome_name,
      data = training[, c(outcome_name, col_names)],
      weights = wts
    )
  } else {
    filter_res <- list(
      tibble::tibble(
        outcome = character(0),
        predictor = character(0),
        score = double(0),
        removed = logical(0)
      ),
      removals = character(0)
    )
  }

  step_predictor_best_new(
    terms = x$terms,
    score = x$score,
    role = x$role,
    trained = TRUE,
    results = filter_res$raw,
    prop_terms = x$prop_terms,
    update_prop = x$update_prop,
    removals = filter_res$removals,
    skip = x$skip,
    id = x$id,
    case_weights = were_weights_used
  )
}

calculate_predictor_best <- function(
  score,
  prop_terms,
  outcome = character(0),
  data,
  weights
) {
  score_function <- paste0("score_", score)

  fm <- stats::as.formula(paste(outcome, "~ ."))

  score_res <- compute_score(
    score_function,
    args = list(),
    form = fm,
    data = data,
    weights = weights
  )

  # ------------------------------------------------------------------------------
  # Fill in missings

  # The current filtro::fill_safe_value() only applies to class_score, not df nor tibble.

  score_df <-
    score_res |>
    filtro::fill_safe_value(return_results = TRUE, transform = TRUE)

  # ------------------------------------------------------------------------------
  # filter predictors

  if (score_res@direction == "maximize") {
    keepers <- score_df |>
      dplyr::slice_max(score, prop = prop_terms, with_ties = TRUE)
  } else {
    keepers <- score_df |>
      dplyr::slice_min(score, prop = prop_terms, with_ties = TRUE)
  }
  keepers <- keepers |> dplyr::pull(predictor)

  removals <- setdiff(score_df$predictor, keepers)

  raw_res <- score_res@results |> dplyr::select(outcome, predictor, score)
  raw_res$removed <- raw_res$predictor %in% removals

  list(
    raw = raw_res,
    removals = removals
  )
}

#' @export
bake.step_predictor_best <- function(object, new_data, ...) {
  new_data <- recipes_remove_cols(new_data, object)
  new_data
}

#' @export
print.step_predictor_best <- function(
  x,
  width = max(20, options()$width - 36),
  ...
) {
  if (identical(x$score, rlang::enexpr())) {
    title <- cli::format_inline("Feature selection on")
  } else {
    scores <- unique(x$score)
    title <- cli::format_inline("Feature selection via {.code {scores}} on")
  }

  print_step(
    x$removals,
    x$terms,
    x$trained,
    title,
    width,
    case_weights = x$case_weights
  )
  invisible(x)
}

#' @usage NULL
#' @export
tidy.step_predictor_best <- tidy_filtro_rec

#' @export
tunable.step_predictor_best <- function(x, ...) {
  tibble::tibble(
    name = "prop_terms",
    call_info = list(
      list(pkg = "dials", fun = "prop_terms")
    ),
    source = "recipe",
    component = "step_predictor_best",
    component_id = x$id
  )
}

#' @rdname required_pkgs.important
#' @export
required_pkgs.step_predictor_best <- function(x, ...) {
  c("important", "filtro")
}
