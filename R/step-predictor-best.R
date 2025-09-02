#' Supervised Feature Selection via Choosing the Top Predictors
#'
#' `step_predictor_best()` creates a *specification* of a recipe step that will
#' perform feature selection by ...
#'
#' @inheritParams step_predictor_desirability
#'
#' @param score The name of a single score function from the \pkg{filtro}
#' package, such as [filtro::score_imp_rf()], [filtro:: score_roc_auc()], etc.
#' See the Details and Examples sections below. This argument *should be named*
#' when used.
#'
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
step_predictor_best <- function(
  recipe,
  ...,
  score,
  role = NA,
  trained = FALSE,
  prop_terms = 0.5,
  update_prop = TRUE,
  #results = NULL,
  removals = NULL,
  skip = FALSE,
  id = rand_id("predictor_best")
) {
  add_step(
    recipe,
    step_predictor_best_new(
      terms = enquos(...),
      score = rlang::enexpr(score), # Or score = score?
      role = role,
      trained = trained,
      prop_terms = prop_terms,
      update_prop = update_prop,
      #results = results,
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
    #results,
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
      #results = results,
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
  check_number_decimal(
    x$prop_terms,
    min = .Machine$double.eps,
    max = 1,
    arg = "prop_terms"
  )

  if (x$update_prop) {
    x$prop_terms <- update_prop(length(col_names), x$prop_terms)
  }

  wts <- get_case_weights(info, training)
  were_weights_used <- are_weights_used(wts, unsupervised = TRUE)
  if (isFALSE(were_weights_used)) {
    wts <- NULL
  }

  outcome_name <- pull_outcome_column_name(info)

  if (length(col_names) > 1) {
    filter <- calculate_predictor_best(
      score = x$score,
      prop_terms = x$prop_terms,
      outcome = outcome_name,
      data = training[, c(outcome_name, col_names)]
    )
  } else {
    filter <- character(0)
  }

  step_predictor_best_new(
    terms = x$terms,
    score = x$score,
    role = x$role,
    trained = TRUE,
    #results = score_objs,
    prop_terms = x$prop_terms,
    update_prop = x$update_prop,
    removals = filter,
    skip = x$skip,
    id = x$id,
    case_weights = were_weights_used
  )
}

calculate_predictor_best <- function(
  score,
  prop_terms,
  outcome = character(0),
  data
) {
  score_function <- paste0("score_", score)

  fm <- stats::as.formula(paste(outcome, "~ ."))

  score_res <- compute_score(
    score_function,
    args = list(),
    form = fm,
    data = data #,
    #weights = wts
  )

  # ------------------------------------------------------------------------------
  # Fill in missings

  # The current filtro::fill_safe_value() only applies to class_score, not df nor tibble.

  score_df <- # save for tidy method
    score_res |>
    filtro::fill_safe_value(return_results = TRUE)

  # ------------------------------------------------------------------------------
  # filter predictors
  final_res <- score_df |>
    dplyr::slice_max(score, prop = prop_terms, with_ties = TRUE) |>
    dplyr::pull("predictor")

  if (length(final_res) == 0) {
    # final_res <- fallback_pred()
  }
  final_res
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
  title <- "Feature selection on "
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
tidy.step_predictor_best <- function(x, ...) {
  if (is_trained(x)) {
    res <- tibble::tibble(terms = unname(x$removals))
  } else {
    term_names <- sel2char(x$terms)
    res <- tibble::tibble(terms = term_names)
  }
  res$id <- x$id
  res
}

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

#' @export
required_pkgs.step_predictor_desirability <- function(x, ...) {
  c("important", "filtro")
}
