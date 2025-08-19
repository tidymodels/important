# Check input for step_predictor_desirability()
check_desirability_arg <- function(x) {
	# check for `c()`
	# check not empty
}

# For an expression or quosure, list the user-specificied score functions
extract_score_names <- function(x, call = rlang::caller_env()) {
	if (rlang::is_quosure(x)) {
		if (rlang::quo_is_missing(x)) {
			cli::cli_abort("{.arg score} cannot be empty.", call = call)
		}
		x <- rlang::quo_get_expr(x)
	}
	res <- unique(all.vars(x))
	res <- grep("^score_", res, value = TRUE)
	if (length(res) == 0) {
		cli::cli_abort("No score objects were found in {.arg score}.", call = call)
	}
	res
}

