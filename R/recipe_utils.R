# For an expression or quosure, list the user-specificied score functions
extract_score_names <- function(x, call = rlang::caller_env()) {
  if (rlang::is_quosure(x)) {
    if (rlang::quo_is_missing(x)) {
      cli::cli_abort("{.arg score} cannot be empty.", call = call)
    }
    x <- rlang::quo_get_expr(x)
  }
  res <- all.vars(x)
  check_score_names(res, call)
}

check_score_names <- function(x, call) {
  x <- unique(x)
  res <- grep("^score_", x, value = TRUE)
  if (length(res) == 0) {
    cli::cli_abort("No score objects were found in {.arg score}.", call = call)
  }
  res
}

# TODO this should go into recipes
pull_outcome_column_name <- function(x) {
  outcome_name <- x$variable[x$role == "outcome"]
  num_outcomes <- length(outcome_name)
  if (num_outcomes != 1) {
    cli::cli_abort("One column should have a role of {.val outcome}.")
  }
  outcome_name
}

check_weights <- function(object, weights) {
  if (object@case_weights & !is.null(weights)) {
    res <- weights
  } else {
    res <- NULL
  }
  res
}

compute_score <- function(score, args, form, data, weights = NULL) {
  score_obj <- find_score_object(score)

  # Process case weights
  weights <- check_weights(score_obj, weights)
  if (!is.null(weights)) {
    args$case_weights <- weights
  }

  cl <- rlang::call2(
    "fit",
    .ns = "generics",
    object = quote(score_obj),
    formula = quote(form),
    data = quote(data)
  )
  cl <- rlang::call_modify(cl, !!!args)

  res <- try(rlang::eval_tidy(cl), silent = TRUE)

  # if error return all NA
  res
}

# Temporary solution
find_score_object <- function(x) {
  utils::getFromNamespace(x, "filtro")
}

update_prop <- function(num_cols, prop) {
  min_prop <- 1 / num_cols
  if (prop < min_prop) {
    prop <- min_prop + 2 * .Machine$double.eps
  }
  prop
}

tidy_filtro_rec <- function(x, ...) {
  if (is_trained(x)) {
    if (is.null(x$results)) {
      res <- tibble::tibble(terms = character(), id = character())
    } else {
      res <-
        x$results |>
        dplyr::select(-outcome, terms = predictor) |>
        dplyr::relocate(dplyr::any_of("removed"), .after = c(terms))
    }
  } else {
    term_names <- sel2char(x$terms)
    res <- tibble::tibble(terms = term_names)
  }
  res$id <- x$id
  res
}

all_scores_missing <- function(x) {
  scores <- dplyr::select(x, -outcome, -predictor)
  all_missing <- purrr::map_lgl(scores, ~ all(is.na(.x)))
  all(all_missing)
}
