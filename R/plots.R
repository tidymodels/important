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
#' @examples
#' # Pre-computed results. See code at
#' system.file("make_imp_example.R", package = "important")
#'
#' # Load the results
#' load(system.file("imp_examples.RData", package = "important"))
#'
#' # A classification model with two classes and highly correlated predictors.
#' # To preprocess them, PCA feature extraction is used.
#' #
#' # Let’s first view the importance in terms of the original predictor set
#' # using 50 permutations:
#'
#' imp_orig
#'
#' autoplot(imp_orig, top = 10)
#'
#' # Now assess the importance in terms of the PCA components
#'
#' imp_derv
#'
#' autoplot(imp_derv)
#' autoplot(imp_derv, metric = "brier_class", type = "difference")
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
