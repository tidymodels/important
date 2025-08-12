parallel_cl <- function(framework = "sequential", arg) {
  cl <- match.call()

  base_fn <- fns[[framework]]

  iter_cl <- rlang::call2(
    fns[[framework]][[1]],
    .ns = fns[[framework]][[2]],
    !!!rlang::syms(cl$arg),
    quote(metric_wrapper)
  )

  base_args <-
  	list(
  		is_perm = quote(permute),
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
  	rlang::check_installed("future")
    iter_cl <- rlang::call_modify(
      iter_cl,
      !!!base_args,
      future.label = "permutations-%d",
      future.seed = NULL
    )
  } else if (framework == "mirai") {
  	rlang::check_installed("mirai")
  	iter_cl <- rlang::call_modify(iter_cl, .args = base_args)
  } else {
  	iter_cl <- rlang::call_modify(iter_cl, !!!base_args)
  }

  iter_cl
}
parallel_cl("mirai", perm_combos)
parallel_cl("future", perm_bl)
