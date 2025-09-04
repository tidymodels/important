#' Supervised Multivariate Feature Selection via Desirability Functions
#'
#' `step_predictor_desirability()` creates a *specification* of a recipe step
#' that uses one or more "score" functions to measure how much each predictor
#' is related to the outcome value. These scores are combined into a composite
#' value using user-specified _desirability_ functions and a proportion of the
#' most desirable predictors are retained.
#'
#' @inheritParams recipes::step_center
#' @param score An object produced by [desirability2::desirability()] that uses
#'   one or more score functions from the \pkg{filtro} package. See the Details
#'   and Examples sections below. This argument *should be named* when used.
#' @param prop_terms The proportion of predictors that should be retained when
#'   ordered by overall desirability. A value of [hardhat::tune()] can also be
#'   used.
#' @param update_prop A logical: should `prop_terms` be updated so that at least
#'   one predictor will be retained?
#' @param removals A character string that contains the names of predictors that
#'   should be removed. These values are not determined until [recipes::prep()]
#'   is called.
#' @param results A data frame of score and desirability values for each
#'   predictor evaluated. These values are not determined until [recipes::prep()]
#'   is called.
#' @export
#'
#' @details
#'
#' This recipe step can compute one or more scores and conduct a simultaneous
#' selection of the top predictors using _desirability functions_. These are
#' functions that, for some type of goal, translate the score's values to a
#' scale of `[0, 1]`,  where 1.0 is the best result and 0.0 is unacceptable.
#' Once we have these for each score, the overall desirability is computed
#' using the geometric mean of the individual desirabilities. See the examples
#' in [desirability2::d_overall()] and [desirability2::d_max()].
#'
#' To define desirabilities, use [desirability2::desirability()] function to
#' define _goals_ for each score and pass that to the recipe in the `score`
#' argument.
#'
#' ```{r child = "man/rmd/filtro-scores.Rmd"}
#' ```
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
#'    `maximize(max(cor_spearman))`.
#'
#' - If a predictor cannot be computed for all scores, it is given a "fallback
#'   value" that will prevent it from being excluded for this reason.
#'
#' This step can potentially remove columns from the data set. This may cause
#' issues for subsequent steps in your recipe if the missing columns are
#' specifically referenced by name. To avoid this, see the advice in the _Tips
#' for saving recipes and filtering columns_ section of [recipes::selections].
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
#' `terms` (the predictor names), `id`, columns for the estimated scores, and
#' the desirability results. The score columns are the raw values, before being
#' filled with "safe values" or transformed.
#'
#' The desirability columns will have the same name as the scores with an
#' additional prefix of `.d_`. The overall desirability column is called
#' `.d_overall`.
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
#' @seealso [desirability2::desirability()]
#' @references Derringer, G. and Suich, R. (1980), Simultaneous Optimization of
#'  Several Response Variables. _Journal of Quality Technology_, 12, 214-219.
#'
#' [https://desirability2.tidymodels.org/reference/inline_desirability.html](https://desirability2.tidymodels.org/reference/inline_desirability.html)
#' @examples
#' library(recipes)
#' library(desirability2)
#'
#' if (rlang::is_installed("modeldata")) {
#' 	# The `ad_data` has a binary outcome column ("Class") and mostly numeric
#' 	# predictors. For these, we score the predictors using an analysis of
#' 	# variance model where the predicor is the outcome and the outcome class
#' 	# defines the groups.
#' 	# There is also a single factor predictor ("Genotype") and we'll use
#' 	# Fisher's Exact test to score it. NOTE that for scores using hypothesis
#' 	# tests, the -log10(pvalue) is returned so that larger values are more
#' 	# important.
#'
#' 	# The score_* objects here are from the filtro package. See Details above.
#' 	goals <-
#' 		desirability(
#' 			maximize(xtab_pval_fisher),
#' 			maximize(aov_pval)
#' 		)
#'
#' 	example_data <- modeldata::ad_data
#' 	rec <-
#' 		recipe(Class ~ ., data = example_data) |>
#' 		step_predictor_desirability(
#' 			all_predictors(),
#' 			score = goals,
#' 			prop_terms = 1 / 2
#' 		)
#' 	rec
#'
#' 	# Now evaluate the predictors and rank them via desirability:
#' 	prepped <- prep(rec)
#' 	prepped
#'
#' 	# Use the tidy() method to get the results:
#' 	predictor_scores <- tidy(prepped, number = 1)
#' 	mean(predictor_scores$removed)
#' 	predictor_scores
#'
#' 	# --------------------------------------------------------------------------
#'
#' 	# Case-weight example: use the hardhat package to create the appropriate type
#' 	# of case weights. Here, we'll increase the weights for the minority class and
#' 	# add them to the data frame.
#'
#' 	library(hardhat)
#'
#' 	example_weights <- example_data
#' 	weights <- ifelse(example_data$Class == "Impaired", 5, 1)
#' 	example_weights$weights <- importance_weights(weights)
#'
#' 	# To see if the scores can use case weights, load the filtro package and
#' 	# check the `case_weights` property:
#'
#' 	library(filtro)
#'
#' 	score_xtab_pval_fisher@case_weights
#' 	score_aov_pval@case_weights
#'
#' 	# The recipe will automatically find the case weights and will
#' 	# not treat them as predictors.
#' 	rec_wts <-
#' 		recipe(Class ~ ., data = example_weights) |>
#' 		step_predictor_desirability(
#' 			all_predictors(),
#' 			score = goals,
#' 			prop_terms = 1 / 2
#' 		) |>
#' 		prep()
#' 	rec_wts
#'
#' 	predictor_scores_wts <-
#' 		tidy(rec_wts, number = 1) |>
#' 		select(terms, .d_overall_weighted = .d_overall)
#'
#' 	library(dplyr)
#' 	library(ggplot2)
#'
#' 	# The selection did not substantially change with these case weights
#' 	full_join(predictor_scores, predictor_scores_wts, by = "terms") |>
#' 		ggplot(aes(.d_overall, .d_overall_weighted)) +
#' 		geom_abline(col = "darkgreen", lty = 2) +
#' 		geom_point(alpha = 1 / 2) +
#' 		coord_fixed(ratio = 1) +
#' 		labs(x = "Unweighted", y = "Class Weighted")
#' }
#'
step_predictor_desirability <- function(
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
  id = rand_id("predictor_desirability")
) {
  S7::check_is_S7(score)
  if (getRversion() >= "4.3.0") {
    if (!inherits(score, "desirability2::desirability_set")) {
      cli::cli_abort(
        "Please use the {.fn desirability} function in the {.pkg desirability2}
		   package to create an object to pass to {.arg score}."
      )
    }
  }

  add_step(
    recipe,
    step_predictor_desirability_new(
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

step_predictor_desirability_new <-
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
      subclass = "predictor_desirability",
      terms = terms,
      score = score,
      role = role,
      trained = trained,
      results = results,
      prop_terms = prop_terms,
      update_prop = update_prop,
      removals = removals,
      skip = skip,
      id = id,
      case_weights = case_weights
    )
  }

#' @export
prep.step_predictor_desirability <- function(x, training, info = NULL, ...) {
  rlang::check_installed("desirability2")
  col_names <- recipes_eval_select(x$terms, training, info)
  check_type(training[, col_names], types = c("double", "integer", "factor"))

  bottom <- ifelse(x$update_prop, .Machine$double.eps, 0.0)
  check_number_decimal(
    x$prop_terms,
    min = bottom,
    max = 1,
    arg = "prop_terms"
  )

  if (length(col_names) < 2) {
    res <-
      step_predictor_desirability_new(
        terms = x$terms,
        score = score,
        role = x$role,
        trained = TRUE,
        prop_terms = x$prop_terms,
        update_prop = x$update_prop,
        results = NULL,
        removals = character(0),
        skip = x$skip,
        id = x$id,
        case_weights = were_weights_used
      )
    return(res)
  }

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

  var_names <- unique(unlist(x$score@variables))
  score_names <- paste0("score_", var_names)
  score_names <- check_score_names(score_names)
  outcome_name <- pull_outcome_column_name(info)
  fm <- paste(outcome_name, "~ .")
  fm <- stats::as.formula(fm)

  score_objs <-
    purrr::map(
      score_names,
      ~ compute_score(.x, list(), fm, training[c(outcome_name, col_names)], wts)
    )
  raw_scores <- filtro::bind_scores(score_objs)

  score_df <-
    score_objs |>
    filtro::fill_safe_values(transform = TRUE)

  if (all_scores_missing(raw_scores)) {
    cli::cli_warn("All score computations failed; skipping feature selection.")
    keep_list <- score_df
  } else {
    # make desirability expression/eval quosure
    score_df <- desirability2::make_desirability_cols(x$score, score_df)

    bad_news <- purrr::map_lgl(score_df$.d_overall, ~ identical(.x, 0.0))
    if (all(bad_news)) {
      keep_list <- score_df[0, ]
    } else {
      keep_list <-
        score_df |>
        dplyr::slice_max(.d_overall, prop = x$prop_terms, with_ties = TRUE)
    }
  }

  rm_list <-
    dplyr::anti_join(score_df, keep_list[, "predictor"], by = "predictor") |>
    dplyr::pull(predictor)

  score_df$removed <- score_df$predictor %in% rm_list

  score_df <- score_df |>
    dplyr::select(outcome, predictor, removed, dplyr::starts_with(".d_")) |>
    dplyr::full_join(raw_scores, by = c("outcome", "predictor")) |>
    dplyr::relocate(removed, .after = "predictor") |>
    dplyr::relocate(dplyr::starts_with(".d_"), .after = dplyr::everything())

  step_predictor_desirability_new(
    terms = x$terms,
    score = x$score,
    role = x$role,
    trained = TRUE,
    results = score_df,
    prop_terms = x$prop_terms,
    update_prop = x$update_prop,
    removals = rm_list,
    skip = x$skip,
    id = x$id,
    case_weights = were_weights_used
  )
}

#' @export
bake.step_predictor_desirability <- function(object, new_data, ...) {
  new_data <- recipes_remove_cols(new_data, object)
  new_data
}

#' @export
print.step_predictor_desirability <- function(
  x,
  width = max(20, options()$width - 36),
  ...
) {
  scores <- unique(x$score@variables)

  title <- cli::format_inline(
    "Feature selection via desirability functions ({.code {scores}}) on"
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
tidy.step_predictor_desirability <- tidy_filtro_rec

#' @export
tunable.step_predictor_desirability <- function(x, ...) {
  tibble::tibble(
    name = "prop_terms",
    call_info = list(
      list(pkg = "dials", fun = "prop_terms")
    ),
    source = "recipe",
    component = "step_predictor_desirability",
    component_id = x$id
  )
}

#' S3 methods for tracking which additional packages are needed for steps.
#'
#' Recipe-adjacent packages always list themselves as a required package so that
#' the steps can function properly within parallel processing schemes.
#' @param x A recipe step
#' @return A character vector
#' @name required_pkgs.important
#' @keywords internal
#' @export
required_pkgs.step_predictor_desirability <- function(x, ...) {
  c("important", "filtro", "desirability2")
}
