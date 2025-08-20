#' Feature Selection
#'
#' `step_predictor_desirability()` creates a *specification* of a recipe step
#' that uses one or more "score" functions to measure how how much each
#' predictor is related to the outcome value. These scores are combined into
#' a composite value using user-specified _desirability_ functions and a
#' proportion of the most desirable predictors are retained.
#'
#' @inheritParams recipes::step_center
#' @param score An object produced by [desirability2::desirability()] that uses
#'   one or more score functions from the \pkg{filtro} package. See the Details
#'   and Examples sections below. This argument *should be named* when used.
#' @param prop_terms The proportion of predictors that should be retained when
#'   ordered by overall desirability.
#' @param update_prop A logical: should `prop_terms` be updated so that at least
#'   one predictor will be retained?
#' @param removals A character string that contains the names of predictors that
#'   should be removed. These values are not determined until [recipes::prep()]
#'   is called.
#' @param results A data frame of score and desirability values for each
#'   predictor evaluated. These values are not determined until
#'   [recipes::prep()] is called.
#' @return An updated version of `recipe` with the new step added to the
#'   sequence of any existing operations.
#' @export
#'
#' @details
#'
#' This step ...
#'
#' This step can potentially remove columns from the data set. This may
#' cause issues for subsequent steps in your recipe if the missing columns are
#' specifically referenced by name. To avoid this, see the advice in the
#' _Tips for saving recipes and filtering columns_ section of
#' [recipes::selections].
#'
#' # Tidying
#'
#' When you [`tidy()`][recipes::tidy.recipe] this step, a tibble::tibble is
#' returned with columns `terms` and `id`:
#'
#' \describe{
#'   \item{terms}{character, the selectors or variables selected to be removed}
#'   \item{id}{character, id of this step}
#' }
#'
#' ```{r, echo = FALSE, results="asis"}
#' step <- "step_predictor_desirability"
#' result <- knitr::knit_child("man/rmd/tunable-args.Rmd")
#' cat(result)
#' ```
#'
#' The underlying operation does not allow for case weights.
#'
#' @seealso [desirability2::desirability()]
#' @references Derringer, G. and Suich, R. (1980), Simultaneous Optimization of
#' Several Response Variables. _Journal of Quality Technology_, 12, 214-219.
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
#' 			maximize(score_xtab_pval_fisher),
#' 			maximize(score_aov_pval)
#' 		)
#'
#' 	rec <-
#' 		recipe(Class ~ ., data = modeldata::ad_data) |>
#' 		step_predictor_desirability(
#' 			all_predictors(),
#' 			score = goals,
#' 			prop_terms = 1/2
#' 		)
#' 	rec
#'
#' 	# Now evaluate the predictors and rank them via desirability:
#' 	rec_trained <- prep(rec)
#' 	rec_trained
#'
#' 	# Use the tidy() method to get the results:
#' 	predictor_scores <- tidy(rec, number = 1)
#' 	mean(predictor_scores$retained)
#' 	predictor_scores
#' }
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
	if (!inherits(score, "desirability2::desirability_set")) {
		cli::cli_abort(
			"Please use the {.fn desirability} function in the {.pkg desirability2}
		   package to create an object to pass to {.arg score}."
		)
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
  col_names <- recipes_eval_select(x$terms, training, info)
  check_type(training[, col_names], types = c("double", "integer", "factor"))
  check_number_decimal(x$prop_terms, min = .Machine$double.eps, max = 1, arg = "prop_terms")

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

  wts <- get_case_weights(info, training)
  were_weights_used <- are_weights_used(wts, unsupervised = TRUE)
  if (isFALSE(were_weights_used)) {
    wts <- NULL
  }

  score_names <- check_score_names(unlist(x$score@variables))
  outcome_name <- pull_outcome_column_name(info)
  fm <- paste(outcome_name, "~ .")
  fm <- stats::as.formula(fm)

  # TODO ooof... how to handle case weights?
  score_objs <-
  	purrr::map(
  		score_names,
  		~ compute_score(.x, list(), fm, training[ c(outcome_name, col_names)])
  	) |>
  	filtro::fill_safe_values() # and then transform?

  # The score names include "score_" but the column names don't
  rm_vec <- gsub("^score_", "", score_names)
  names(rm_vec) <- score_names
  score_objs <- dplyr::rename(score_objs, rm_vec)

  # make desirability expression/eval quosre
  score_objs <- desirability2::make_desirability_cols(x$score, score_objs)

  keep_list <-
  	score_objs |>
  	dplyr::slice_max(.d_overall, prop = x$prop_terms, with_ties = TRUE)
  rm_list <-
  	dplyr::anti_join(score_objs, keep_list[, "predictor"], by = "predictor") |>
  	purrr::pluck("predictor")

  step_predictor_desirability_new(
    terms = x$terms,
    score = x$score,
    role = x$role,
    trained = TRUE,
    results = score_objs,
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
print.step_predictor_desirability <- function(x, width = max(20, options()$width - 36), ...) {
  title <- "Feature selection via desirability functions on"
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
tidy.step_predictor_desirability <- function(x, ...) {
  if (is_trained(x)) {
    res <-
    	x$results |>
    	dplyr::select(-outcome, terms = predictor)
    res$retained <- !(res$terms %in% x$removals)
  } else {
    term_names <- sel2char(x$terms)
    res <- tibble::tibble(terms = term_names)
  }
  res$id <- x$id
  res
}

# TODO make a new dials parameter for prop_terms

#' @export
tunable.step_predictor_desirability <- function(x, ...) {
  tibble::tibble(
    name = "prop_terms",
    call_info = list(
      list(pkg = "dials", fun = "threshold")
    ),
    source = "recipe",
    component = "step_predictor_desirability",
    component_id = x$id
  )
}
