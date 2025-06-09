forge_predictors <- function(new_data, workflow) {
  mold <- hardhat::extract_mold(workflow)
  forged <- hardhat::forge(new_data, blueprint = mold$blueprint)
  forged$predictors
}

# TODO case weights
# TODO use original data if not available in workflow
extract_data_original <- function(wflow, data, ...) {
  if (!tibble::is_tibble(data)) {
    data <- tibble::as_tibble(data)
  }
  # TODO should we get price or log(price) when log(price) ~ blah is used?

  ptypes <- wflow$pre$mold$blueprint$ptypes
  extras <- wflow$pre$mold$blueprint$extra_role_ptypes
  for (i in seq_along(extras)) {
    ptypes[[2 + i]] <- extras[[i]]
  }
  ptypes <- purrr::list_cbind(unname(ptypes))
  col_names <- colnames(ptypes)
  data <- data[, col_names]
  hardhat::scream(data, ptypes)
}

extract_data_derived <- function(
  wflow,
  data,
  type = c("predictors", "outcomes"),
  bind = TRUE
) {
  type <- rlang::arg_match(type, c("predictors", "outcomes"), multiple = TRUE)
  res <- list()
  if (any(type == "predictors")) {
    res$predictors <- forge_predictors(data, wflow)
  }
  if (any(type == "outcomes")) {
    bp <- wflow |> hardhat::extract_mold() |> purrr::pluck("blueprint")
    res$outcomes <- hardhat::forge(data, bp, outcomes = TRUE)$outcomes
  }
  if (bind) {
    # TODO reorder so outcome is first
    res <- purrr::list_cbind(unname(res))
  }
  if (!tibble::is_tibble(res)) {
    res <- tibble::as_tibble(res)
  }
  res
}
