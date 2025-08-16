# Code to make the calls sequentially or in parallel
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
      pkgs = quote(pkgs),
      metrics = quote(metrics),
      size = quote(size),
      outcome = quote(outcome_nm),
      eval_time = quote(eval_time),
      event_level = quote(event_level)
    )

  if (framework == "future") {
    rlang::check_installed(c("future", "future.apply"))
    iter_cl <- rlang::call_modify(
      iter_cl,
      !!!base_args,
      future.label = "permutations-%d",
      future.seed = TRUE
    )
  } else if (framework == "mirai") {
    rlang::check_installed("mirai")
    iter_cl <- rlang::call_modify(iter_cl, .args = base_args)
  } else {
    iter_cl <- rlang::call_modify(iter_cl, !!!base_args)
  }

  iter_cl
}

# ------------------------------------------------------------------------------

# Utilities from the tune package: requires rlang, cli, mirai, future, and
# parallel packages. These are tested in tune so we nocov them

# fake helper

fake_ctrl <- list(pkgs = character(0), allow_par = TRUE)

# nocov start

# mirai_map() acts a little different form map(), lapply(), etc. It requires
# that the elements in .args be the args (not symbols). It also requires an
# extra step to collect the results and coerce them into a list.
eval_mirai <- function(.x, .f, ..., .args) {
  .args <- lapply(.args, get, envir = parent.frame())
  res <- mirai::mirai_map(.x, .f, ..., .args = .args)
  mirai::collect_mirai(res)
}

fns <- list(
  sequential = list(fn = "lapply", ns = NULL),
  future = list(fn = "future_lapply", ns = "future.apply"),
  mirai = list(fn = "eval_mirai", ns = NULL)
)

has_non_par_pkgs <- function(object, control, verbose = FALSE) {
  pkgs <- character(0)

  if (!is.null(object)) {
    pkgs <- required_pkgs(object)
  }
  if (!is.null(control)) {
    pkgs <- c(pkgs, control$pkgs)
  }
  pkgs <- unique(pkgs)
  if (length(pkgs) == 0) {
    return(FALSE)
  }
  naughty_list <- c("keras", "rJava")
  has_pkg <- pkgs %in% naughty_list
  if (any(has_pkg)) {
    pkgs <- pkgs[has_pkg]
    if (verbose) {
      cli::cli_inform(
        "These packages cannot be used with explicit parallel processing: {.pkg {pkgs}}."
      )
    }
  }
  any(has_pkg)
}

future_installed <- function() {
  rlang::is_installed("future")
}
mirai_installed <- function() {
  rlang::is_installed("mirai")
}

get_future_workers <- function(verbose) {
  has_future <- future_installed()

  if (has_future) {
    future_workers <- future::nbrOfWorkers()
    if (verbose) {
      if (future_workers == 0) {
        cli::cli_inform(
          "{.pkg future} is not active."
        )
      } else {
        cli::cli_inform(
          "{.pkg future} is active with {future_workers} worker{?s}."
        )
      }
    }
  } else {
    if (verbose) {
      cli::cli_inform("{.pkg future} is not installed.")
    }

    future_workers <- 0L
  }
  future_workers
}

get_mirai_workers <- function(verbose) {
  if (!mirai_installed()) {
    if (verbose) {
      cli::cli_inform("{.pkg mirai} is not installed.")
    }
    return(0L)
  }

  # note connections will be 0 if `!daemons_set()`
  mirai_workers <- mirai::status()$connections

  if (verbose) {
    if (mirai_workers == 0) {
      cli::cli_inform(
        "{.pkg mirai} is not active."
      )
    } else {
      cli::cli_inform(
        "{.pkg mirai} is active with {mirai_workers} worker{?s}."
      )
    }
  }

  mirai_workers
}

choose_framework <- function(
  object = NULL,
  control = NULL,
  verbose = FALSE,
  default = "mirai"
) {
  if (!is.null(control)) {
    if (!control$allow_par) {
      return("sequential")
    }
  }

  if (has_non_par_pkgs(object, control, verbose)) {
    return("sequential")
  }

  has_future <- future_installed()
  has_mirai <- mirai_installed()

  if (!has_future & !has_mirai) {
    if (verbose) {
      cli::cli_inform("Neither {.pkg mirai} or {.pkg future} are installed.")
    }
    return("sequential")
  }

  mirai_workers <- get_mirai_workers(verbose)
  future_workers <- get_future_workers(verbose)

  neither <- future_workers < 2 & mirai_workers < 2
  both <- future_workers >= 2 & mirai_workers >= 2

  if (neither) {
    if (verbose) {
      cli::cli_inform("Too few workers for parallel processing.")
    }
    return("sequential")
  }

  if (both) {
    if (verbose) {
      cli::cli_inform(
        "Multiple workers exist for both {.pkg mirai} and {.pkg future};
        falling back to the default of {.pkg {default}}."
      )
    }
    return(default)
  }

  if (future_workers >= 2) {
    res <- "future"
  } else {
    res <- "mirai"
  }

  if (verbose) {
    cli::cli_inform("{.pkg {res}} will be used for parallel processing}.")
  }

  res
}

get_parallel_seeds <- function(workers) {
  # Get current rng info and save
  orig_state <- .Random.seed
  orig_kind <- RNGkind()[1]
  # Reset the stream to get new rng's
  on.exit({
    RNGkind(orig_kind)
    assign(".Random.seed", orig_state, globalenv())
  })

  # Set to type used for multiple streams
  RNGkind("L'Ecuyer-CMRG")

  # Capture the seed to make more seeds.
  .seed <- .Random.seed

  res <- vector(mode = "list", length = workers)
  for (i in seq_along(res)) {
    res[[i]] <- parallel::nextRNGSubStream(.seed)
    .seed <- parallel::nextRNGStream(.seed)
  }

  res
}

# nocov end
