#' Compute permutation-based predictor importance
#'
#' [importance_perm()] computes model-agnostic variable importance scores by
#' permuting individual predictors (one at a time) and measuring how worse
#' model performance becomes.
#'
#' @param wflow A fitted [workflows::workflow()].
#' @param data A data frame of the data passed to [workflows::fit.workflow()],
#' including the outcome and case weights (if any).
#' @param metrics A [yardstick::metric_set()] or `NULL`.
#' @param eval_time For censored regression models, a vector of time points at
#' which the survival probability is estimated. This is only needed if a dynamic
#' metric is used, such as the Brier score or the area under the ROC curve.
#' @param type A character string for which _level_ of predictors to compute.
#' A value of `"original"` (default) will return values in the same
#' representation of `data`. Using `"derived"` will compute them for any derived
#' features/predictors, such as dummy indicator columns, etc.
#' @param size How many data points to predict for each permutation iteration.
#' @param times How many iterations to repeat the calculations.
#' @param event_level A single string. Either `"first"` or `"second"` to specify
#'   which level of `truth` to consider as the "event". This argument is only
#'   applicable when `estimator = "binary"`.
#' @details
#' The function can compute importance at two different levels.
#'
#' - The "original" predictors are the unaltered columns in the source data set.
#'   For example, for a categorical predictor used with linear regression, the
#'   original predictor is the factor column.
#' - "Derived" predictors are the final versions given to the model. For the
#'   categorical predictor example, the derived versions are the binary
#'   indicator variables produced from the factor version.
#'
#' This can make a difference when pre-processing/feature engineering is used.
#' This can help us understand _how_ a predictor can be important
#'
#' Importance scores are computed for each predictor (at the specified level)
#' and each performance metric. If no metric is specified, defaults are used:
#'
#' - Classification: [yardstick::brier_class()], [yardstick::roc_auc()], and
#'   [yardstick::accuracy()].
#' - Regression:  [yardstick::rmse()] and [yardstick::rsq()].
#' - Censored regression: [yardstick::brier_survival()]
#'
#' For censored data, importance is computed for each evaluation time (when a
#' dynamic metric is specified).
#'
#' By default, no parallelism is used to process models in \pkg{tune}; you have
#' to opt-in.
#'
#' ## Using future to parallel process
#'
#' You should install the package and choose your flavor of parallelism using
#' the [plan][future::plan] function. This allows you to specify the number of
#' worker processes and the specific technology to use.
#'
#' For example, you can use:
#'
#' ```r
#'    library(future)
#'    plan(multisession, workers = 4)
#' ```
#' and work will be conducted simultaneously (unless there is an exception; see
#' the section below).
#'
#' See [future::plan()] for possible options other than `multisession`.
#'
#' ## Using mirai  to parallel process
#'
#' To configure parallel processing with \pkg{mirai}, use the
#' [mirai::daemons()] function. The first argument, `n`, determines the number
#' of parallel workers. Using `daemons(0)` reverts to sequential processing.
#'
#' The arguments `url` and `remote` are used to set up and launch parallel
#' processes over the network for distributed computing. See [mirai::daemons()]
#' documentation for more details.
#'
#' @return A tibble with extra classes `"importance_perm"` and either
#' "`original_importance_perm"` or "`derived_importance_perm"`. The columns are:
#' -  `.metric` the name of the performance metric:
#' -  `predictor`: the predictor
#' -  `n`: the number of usable results (should be the same as `times`)
#' -  `mean`: the average of the differences in performance. For each metric,
#'    larger values indicate worse performance (i.e., higher importance).
#' -  `std_err`: the standard error of the differences.
#' -  `importance`: the mean divided by the standard error.
#'  -  For censored regression models, an additional `.eval_time` column may also
#' be included (depending on the metric requested).
#' @examples
#' if (rlang::is_installed(c("modeldata", "recipes", "workflows", "parsnip"))) {
#'   library(modeldata)
#'   library(recipes)
#'   library(workflows)
#'   library(dplyr)
#'   library(parsnip)
#'
#'   set.seed(12)
#'   dat_tr <-
#'     sim_logistic(250, ~ .1 + 2 * A - 3 * B + 1 * A *B, corr = .7) |>
#'     dplyr::bind_cols(sim_noise(250, num_vars = 10))
#'
#'   rec <-
#'     recipe(class ~ ., data = dat_tr) |>
#'     step_interact(~ A:B) |>
#'     step_normalize(all_numeric_predictors()) |>
#'     step_pca(contains("noise"), num_comp = 5)
#'
#'   lr_wflow <- workflow(rec, logistic_reg())
#'   lr_fit <- fit(lr_wflow, dat_tr)
#'
#'   set.seed(39)
#'   orig_res <- importance_perm(lr_fit, data = dat_tr, type = "original",
#'                               size = 100, times = 3)
#'   orig_res
#'
#'   set.seed(39)
#'   deriv_res <- importance_perm(lr_fit, data = dat_tr, type = "derived",
#'                                size = 100, times = 3)
#'   deriv_res
#' }
#' @export
importance_perm <- function(
  wflow,
  data,
  metrics = NULL,
  type = "original",
  size = 500,
  times = 10,
  eval_time = NULL,
  event_level = "first"
) {
  if (!workflows::is_trained_workflow(wflow)) {
    cli::cli_abort("The workflow in {.arg wflow} should be trained.")
  }
  type <- rlang::arg_match(type, c("original", "derived"))
  metrics <- tune::check_metrics_arg(metrics, wflow)
  pkgs <- required_pkgs(wflow)
  pkgs <- c("important", "tune", "yardstick", pkgs)
  pkgs <- unique(pkgs)
  rlang::check_installed(pkgs)

  par_choice <- choose_framework(wflow, fake_ctrl)

  # ------------------------------------------------------------------------------
  # Pull appropriate source data
  # TODO extract and use case weights

  if (type == "original") {
    extracted_data <- extract_data_original(wflow, data)
  } else {
    extracted_data <- extract_data_derived(wflow, data)
  }
  extracted_data_nms <- colnames(extracted_data)
  outcome_nm <- tune::outcome_names(wflow)
  extracted_data_nms <- extracted_data_nms[extracted_data_nms != outcome_nm]
  n <- nrow(extracted_data)
  size <- min(floor(n * 0.8), size)

  # ------------------------------------------------------------------------------
  # Prepare for permutations. A large `perm_combos` data frame is created to
  # optimize how well parallel processing speeds-up computations

  info <- tune::metrics_info(metrics)
  id_vals <- get_parallel_seeds(times) |>
    purrr::map(~ list(seed = list(.x), id = sample.int(1e6, 1)))

  perm_combos <- tidyr::crossing(id = id_vals, column = extracted_data_nms)
  perm_combos <-
    vctrs::vec_chop(
      perm_combos,
      indicies = as.list(vctrs::vec_seq_along(perm_combos))
    )

  perm_bl <- dplyr::tibble(id = id_vals)
  perm_bl <-
    vctrs::vec_chop(perm_bl, indicies = as.list(vctrs::vec_seq_along(perm_bl)))

  # ----------------------------------------------------------------------------
  # Determine whether to load packages or not

  if (par_choice == "future") {
    rlang::local_options(doFuture.rng.onMisuse = "ignore")
  } else if (par_choice == "sequential") {
    # Don't fully load anything if running sequentially
    pkgs <- character(0)
  }
  # ----------------------------------------------------------------------------
  # Generate all permutations

  permute <- TRUE
  perms_cl <- parallel_cl(par_choice, perm_combos)

  res_perms <-
    rlang::eval_tidy(perms_cl) |>
    purrr::list_rbind()

  # ----------------------------------------------------------------------------
  # Get un-permuted performance statistics (per id value)

  permute <- FALSE
  bl_cl <- parallel_cl(par_choice, perm_bl)

  res_bl <-
    rlang::eval_tidy(bl_cl) |>
    purrr::list_rbind() |>
    dplyr::rename(baseline = .estimate) |>
    dplyr::select(-predictor)

  # ----------------------------------------------------------------------------
  # Combine and summarize results

  has_eval_time <- any(names(res_perms) == ".eval_time")

  join_groups <- c(".metric", ".estimator")
  if (has_eval_time) {
    join_groups <- c(join_groups, ".eval_time")
  }

  res <-
    dplyr::full_join(res_perms, res_bl, by = c(join_groups, "id")) |>
    dplyr::full_join(info, by = ".metric") |>
    dplyr::mutate(
      # TODO add (log) ratio?
      importance = dplyr::if_else(
        direction == "minimize",
        .estimate - baseline,
        baseline - .estimate
      )
    )

  summarize_groups <- c(".metric", "predictor")
  if (has_eval_time) {
    summarize_groups <- c(summarize_groups, ".eval_time")
  }

  res <-
    res |>
    dplyr::summarize(
      permuted = mean(.estimate, na.rm = TRUE),
      n = sum(!is.na(importance)),
      mean = mean(importance, na.rm = TRUE),
      sd = sd(importance, na.rm = TRUE),
      std_err = sd / sqrt(n),
      importance = dplyr::if_else(sd == 0, 0, mean / std_err),
      .by = c(dplyr::all_of(summarize_groups))
    ) |>
    dplyr::select(-sd, -permuted) |>
    dplyr::arrange(dplyr::desc(importance))

  class(res) <- c(
    "importance_perm",
    paste0(type, "_importance_perm"),
    class(res)
  )
  res
}

metric_wrapper <- function(
  vals,
  is_perm,
  type,
  wflow_fitted,
  pkgs = character(0),
  dat,
  metrics,
  size,
  outcome,
  eval_time,
  event_level
) {
  if (is_perm) {
    col <- vals$column[[1]]
  } else {
    col <- NULL
  }

  if (length(pkgs) > 0) {
    sshh_load <- purrr::quietly(library)
    load_res <- purrr::map(pkgs, sshh_load, character.only = TRUE)
  }

  res <-
    metric_iter(
      column = col,
      seed = vals$id[[1]],
      type = type,
      wflow_fitted = wflow_fitted,
      dat = dat,
      metrics = metrics,
      size = size,
      outcome = outcome,
      eval_time = eval_time,
      event_level = event_level
    )
  res
}

metric_iter <- function(
  column = NULL,
  seed,
  type,
  wflow_fitted,
  dat,
  metrics,
  size,
  outcome,
  eval_time,
  event_level
) {
  orig_seed <- .Random.seed
  # Set seed within the worker process
  assign(".Random.seed", seed$seed[[1]], envir = .GlobalEnv)
  withr::defer(assign(".Random.seed", orig_seed, envir = .GlobalEnv))

  # ----------------------------------------------------------------------------

  info <- tune::metrics_info(metrics)
  n <- nrow(dat)

  if (!is.null(column)) {
    if (!any(names(dat) == column)) {
      cli::cli_abort(
        "Column {column} was not in the data set. Existing columns
  								  are: {names(dat)}."
      )
    }
    dat[[column]] <- sample(dat[[column]])
  }
  if (!is.null(size)) {
    ind <- sample.int(n, size)
    dat <- dat[ind, ]
  }

  # ------------------------------------------------------------------------------
  # Predictions. Use a wrapper because a simple `augment()` works for original
  # predictors but not for derived.
  preds <- predictions(wflow_fitted, dat, type, eval_time = eval_time)

  # ------------------------------------------------------------------------------
  # Compute metrics

  res <-
    tune::.estimate_metrics(
      preds,
      metric = metrics,
      param_names = NULL,
      outcome_name = outcome,
      event_level = event_level,
      metrics_info = info
    )

  if (is.null(column)) {
    column <- ".baseline"
  }
  res$predictor <- column
  res$id <- seed$id
  res
}

# TODO silently bad results when an in-line transformation is used with
# add_model(x formula = log(y) ~ x) _or_ fails due to not finding the outcome
# column when add_formula(log(y) ~ .) is used

predictions <- function(wflow, new_data, type, eval_time) {
  if (type == "original") {
    preds <- augment(wflow, new_data = new_data, eval_time = eval_time)
  } else {
    preds <-
      wflow |>
      extract_fit_parsnip() |>
      augment(new_data = new_data, eval_time = eval_time)
    use_post <- has_postprocessor(wflow)
    if (use_post) {
      post_proc <- extract_postprocessor(wflow)
      preds <- predict(post_proc, preds)
    }
  }
  preds
}
