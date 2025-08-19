#' Feature Selection
#'
#' `step_select_1()` creates a *specification* of a recipe step that will
#' perform feature selection by ...
#'
#' @inheritParams recipes::step_center
#' @param threshold ...
#' @param removals A character string that contains the names of columns that
#'   should be removed. These values are not determined until [recipes::prep()]
#'   is called.
#' @return An updated version of `recipe` with the new step added to the
#'  sequence of any existing operations.
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
#' step <- "step_select_1"
#' result <- knitr::knit_child("man/rmd/tunable-args.Rmd")
#' cat(result)
#' ```
#'
#' The underlying operation does not allow for case weights.
#'
#' @examples
#' library(recipes)
#'
#' rec <- recipe(mpg ~ ., data = mtcars) |>
#'   step_select_1(all_predictors())
#'
#' prepped <- prep(rec)
#'
#' bake(prepped, mtcars)
#'
#' tidy(prepped, 1)
step_select_1 <- function(
  recipe,
  ...,
  role = NA,
  trained = FALSE,
  threshold = 0.9,
  removals = NULL,
  skip = FALSE,
  id = rand_id("select_1")
) {
  add_step(
    recipe,
    step_select_1_new(
      terms = enquos(...),
      role = role,
      trained = trained,
      threshold = threshold,
      removals = removals,
      skip = skip,
      id = id,
      case_weights = NULL
    )
  )
}

step_select_1_new <-
  function(
    terms,
    role,
    trained,
    threshold,
    removals,
    skip,
    id,
    case_weights
  ) {
    step(
      subclass = "select_1",
      terms = terms,
      role = role,
      trained = trained,
      threshold = threshold,
      removals = removals,
      skip = skip,
      id = id,
      case_weights = case_weights
    )
  }

#' @export
prep.step_select_1 <- function(x, training, info = NULL, ...) {
  col_names <- recipes_eval_select(x$terms, training, info)
  check_type(training[, col_names], types = c("double", "integer"))
  check_number_decimal(x$threshold, min = 0, max = 1, arg = "threshold")

  wts <- get_case_weights(info, training)
  were_weights_used <- are_weights_used(wts, unsupervised = TRUE)
  if (isFALSE(were_weights_used)) {
    wts <- NULL
  }

  if (length(col_names) > 1) {
    filter <- character(0)
  } else {
    filter <- character(0)
  }

  step_select_1_new(
    terms = x$terms,
    role = x$role,
    trained = TRUE,
    threshold = x$threshold,
    removals = filter,
    skip = x$skip,
    id = x$id,
    case_weights = were_weights_used
  )
}

#' @export
bake.step_select_1 <- function(object, new_data, ...) {
  new_data <- recipes_remove_cols(new_data, object)
  new_data
}

#' @export
print.step_select_1 <- function(x, width = max(20, options()$width - 36), ...) {
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
tidy.step_select_1 <- function(x, ...) {
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
tunable.step_select_1 <- function(x, ...) {
  tibble::tibble(
    name = "threshold",
    call_info = list(
      list(pkg = "dials", fun = "threshold")
    ),
    source = "recipe",
    component = "step_select_1",
    component_id = x$id
  )
}
