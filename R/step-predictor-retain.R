#' Supervised Feature Selection via A Single Filter
#'
#' `step_predictor_retain()` creates a *specification* of a recipe step that
#' uses a logical statement that includes one or more scoring functions to
#' measure how much each predictor is related to the outcome value. This step
#' retains the predictors that pass the logical statement.
#'
#' @inheritParams step_predictor_desirability
#' @param score A valid R expression that produces a logical result. The
#' equation can contain the names of one or more score functions from the
#' \pkg{filtro} package, such as [filtro::score_imp_rf()],
#' [filtro:: score_roc_auc()]. See the Details and Examples sections below.
#' This argument *should be named* when used.
#'
#' @export
#'
#' @details
#'
#' The `score` should be valid R syntax that produces a logical result and
#' should not use external data. The list of variables that can be used is in
#' the section below.
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
#'
#' @examples
#' library(recipes)
#'
#' rec <- recipe(mpg ~ ., data = mtcars) |>
#'   step_predictor_retain(
#'     all_predictors(),
#'     score = cor_pearson >= 0.75 | cor_spearman >= 0.75
#'   )
#'
#' prepped <- prep(rec)
#'
#' bake(prepped, mtcars)
#'
#' tidy(prepped, 1)
step_predictor_retain <- function(
  recipe,
  ...,
  score,
  role = NA,
  trained = FALSE,
  results = NULL,
  removals = NULL,
  skip = FALSE,
  id = rand_id("predictor_retain")
) {
  add_step(
    recipe,
    step_predictor_retain_new(
      terms = enquos(...),
      role = role,
      trained = trained,
      score = rlang::enexpr(score),
      results = results,
      removals = removals,
      skip = skip,
      id = id,
      case_weights = NULL
    )
  )
}

step_predictor_retain_new <-
  function(
    terms,
    role,
    trained,
    score,
    results,
    removals,
    skip,
    id,
    case_weights
  ) {
    step(
      subclass = "predictor_retain",
      terms = terms,
      role = role,
      trained = trained,
      score = score,
      results = results,
      removals = removals,
      skip = skip,
      id = id,
      case_weights = case_weights
    )
  }

#' @export
prep.step_predictor_retain <- function(x, training, info = NULL, ...) {
  col_names <- recipes_eval_select(x$terms, training, info)
  check_type(training[, col_names], types = c("double", "integer"))

  # First we check the _type_ of weight to see if it is used. Later, in
  # `compute_score()`, we check to see if the score supports case weights.
  wts <- get_case_weights(info, training)
  were_weights_used <- are_weights_used(wts, unsupervised = FALSE)
  if (isFALSE(were_weights_used)) {
    wts <- NULL
  }

  outcome_name <- pull_outcome_column_name(info)

  if (length(col_names) > 1) {
    filter_res <- calculate_predictor_retain(
      xpr = x$score,
      outcome = outcome_name,
      data = training[, c(outcome_name, col_names)],
      weights = wts
    )
  } else {
    filter_res <- list(
      raw = tibble::tibble(
        outcome = character(0),
        predictor = character(0),
        removed = logical(0)
      ),
      removals = character(0)
    )
  }

  step_predictor_retain_new(
    terms = x$terms,
    role = x$role,
    trained = TRUE,
    score = x$score,
    results = filter_res$raw,
    removals = filter_res$removals,
    skip = x$skip,
    id = x$id,
    case_weights = were_weights_used
  )
}

# TODO: how to pass opts and or tune objects? Internal score set?

# Assumes `data` has all predictors and the outcome columns (only)
calculate_predictor_retain <- function(
  xpr,
  outcome = character(0),
  data,
  weights,
  opts = list()
) {
  all_scores <- unique(all.vars(xpr))
  all_score_functions <- paste0("score_", all_scores)

  # ------------------------------------------------------------------------------
  # Find all known class_{score} and check against list

  # ------------------------------------------------------------------------------
  # Process any options

  opts <- make_opt_list(opts, all_scores)

  # ------------------------------------------------------------------------------

  fm <- as.formula(paste(outcome, "~ ."))

  # ------------------------------------------------------------------------------

  # Get list of args for each scoring method and use map2()
  score_res <- purrr::map2(
    all_score_functions,
    opts,
    compute_score,
    form = fm,
    data = data,
    weights = weights
  )
  names(score_res) <- all_scores

  # ------------------------------------------------------------------------------
  # Fill in missings

  score_df <- # save for tidy method
    score_res |>
    filtro::fill_safe_values(transform = TRUE)

  # ------------------------------------------------------------------------------
  # filter predictors

  keepers <- score_df |> dplyr::filter(!!xpr) |> dplyr::pull(predictor)
  removals <- setdiff(score_df$predictor, keepers)

  raw_res <- filtro::bind_scores(score_res)
  raw_res$removed <- raw_res$predictor %in% removals

  list(
    raw = raw_res,
    removals = removals
  )
}

make_opt_list <- function(opts, scores) {
  res <- purrr::map(scores, ~ list())
  names(res) <- scores
  score_opts <- intersect(scores, names(opts))
  for (i in score_opts) {
    res[[i]] <- opts[[i]]
  }
  res
}

#' @export
bake.step_predictor_retain <- function(object, new_data, ...) {
  new_data <- recipes_remove_cols(new_data, object)
  new_data
}

#' @export
print.step_predictor_retain <- function(
  x,
  width = max(20, options()$width - 36),
  ...
) {
  scores <- unique(all.vars(x$score))

  word <- ifelse(x$trained, "removing", "for")

  title <- cli::format_inline(
    "Feature selection using {.and {.code {scores}}} {word}"
  )
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
tidy.step_predictor_retain <- tidy_filtro_rec

#' @rdname required_pkgs.important
#' @export
required_pkgs.step_predictor_desirability <- function(x, ...) {
  c("important", "filtro")
}
