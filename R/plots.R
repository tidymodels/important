#' Visualize importance scores
#' @param object A tibble of results from [importance_perm()].
#' @param metric A character vector or `NULL` for which metric to plot. By
#' default, all metrics will be shown via facets. Possible options are
#' the entries in `.metric` column of the object.
#' @param eval_time For censored regression models, a vector of time points at
#' which the survival probability is estimated.
#' @param top An integer for how many terms to show. To define importance when
#' there are multiple metrics, the rankings of predictors are computed across
#' metrics and the average rank is used. In the case of tied rankings, all the
#' ties are included.
#' @param type A character value. The default is `"importance"` which shows the
#' overall signal-to-noise ration (i.e., mean divided by standard error).
#' Alternatively, `"difference"` shows the mean difference value with standard
#' error bounds.
#' @param std_errs The number of standard errors to plot (when `type = "difference"`).
#' @param ... Not used.
#' @return A `ggplot2` object.
#' @examplesIf !is_cran_check()
#' if (!rlang::is_installed(c("modeldata", "recipes", "workflows"))) {
#'   library(modeldata)
#'   library(recipes)
#'   library(workflows)
#'   library(dplyr)
#'
#' 	data(ad_data, package = "modeldata")
#'
#' 	ad_rec <-
#' 		recipe(Class ~ ., data = ad_data) |>
#' 		step_pca(all_numeric_predictors(), -male, -age, num_comp = 5) |>
#' 		step_dummy(all_factor_predictors()) |>
#' 		step_zv(all_predictors())
#'
#' 	ad_wflow <- workflow(ad_rec, logistic_reg())
#' 	ad_fit <- fit(ad_wflow, data = ad_data)
#'
#' 	###
#'
#' 	set.seed(392)
#' 	imp_orig <- importance_perm(ad_fit, data = ad_data, type = "original")
#'
#' 	autoplot(imp_derv, top = 10)
#'
#' 	###
#'
#' 	set.seed(392)
#' 	imp_derv <- importance_perm(ad_fit, data = ad_data, type = "derived")
#'
#' 	autoplot(imp_derv)
#' 	autoplot(imp_derv, metric = "brier_class", type = "difference")
#' }
#' @export
autoplot.importance_perm <- function(
  object,
  top = Inf,
  metric = NULL,
  eval_time = NULL,
  type = "importance",
  std_errs = stats::qnorm(0.95),
  ...
) {
  type <- rlang::arg_match(type, values = c("importance", "difference"))

  if (!is.null(metric)) {
    object <- object[object$.metric %in% metric, ]
    if (nrow(object) == 0) {
      cli::cli_abort("No data left when filtering over {.val {metric}}.")
    }
  }
  overall_rank <-
    object |>
    dplyr::mutate(ranking = rank(-importance)) |>
    dplyr::summarize(
      ranking = mean(ranking),
      .by = c(predictor)
    ) |>
    dplyr::arrange(ranking)

  num_pred <- vctrs::vec_unique_count(object$predictor)
  top <- min(top, num_pred)
  overall_rank <-
    overall_rank |>
    dplyr::slice_min(ranking, n = top) |>
    dplyr::select(predictor)

  object <- dplyr::inner_join(object, overall_rank, by = "predictor")
  object$predictor <- factor(
    object$predictor,
    levels = rev(overall_rank$predictor)
  )

  p <-
    ggplot2::ggplot(object, ggplot2::aes(y = predictor)) +
    ggplot2::geom_vline(xintercept = 0, col = "red", lty = 2) +
    ggplot2::labs(y = NULL, x = "Permutation Importance Score")
  if (length(unique(object$.metric)) > 1) {
    p <- p + ggplot2::facet_wrap(~.metric)
  }
  if (type == "importance") {
    p <- p + ggplot2::geom_point(ggplot2::aes(x = importance))
  } else if (type == "difference") {
    num_rows <- vctrs::vec_unique_count(object$predictor)
    p <- p +
      ggplot2::geom_point(ggplot2::aes(x = mean)) +
      ggplot2::geom_errorbar(
        ggplot2::aes(
          xmin = mean - std_errs * std_err,
          xmax = mean + std_errs * std_err
        ),
        width = num_rows / 50,
        alpha = 1 / 2
      )
  }
  p
}
