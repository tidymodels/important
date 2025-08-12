parallel_cl <- function(framework = "sequential", arg) {
  cl <- match.call()
  if (as.character(cl$arg) == "perm_combos") {
    perm_arg <- TRUE
  } else {
    perm_arg <- FALSE
  }

  base_fn <- fns[[framework]]

  iter_cl <- rlang::call2(
    fns[[framework]][[1]],
    .ns = fns[[framework]][[2]],
    !!!rlang::syms(cl$arg),
    quote(metric_wrapper),

    is_perm = perm_arg,
    type = quote(type),
    wflow_fitted = quote(wflow),
    dat = quote(extracted_data),
    metrics = quote(metrics),
    size = quote(size),
    outcome = quote(outcome_nm),
    eval_time = quote(eval_time),
    event_level = quote(event_level)
  )
  if (framework == "future") {
    iter_cl <- rlang::call_modify(
      iter_cl,
      future.label = "permutations-%d",
      future.seed = NULL
    )
  }

  iter_cl
}
parallel_cl("mirai", perm_combos)
parallel_cl("future", perm_bl)
