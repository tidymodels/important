# model_pipe? or a method just to make a workflow

# add_step() would need to be a generic

# As steps are added we can set their ids (perhaps numbered) for easy
# removal or muting

pipeline <- function(x, data, mode = NULL) {
  # get data types, roles etc.

  # set default mode

  # set case weights (if any)

  res <- list(
    plist = data[0, ],
    roles = character(),
    mode = character(),
    pre = list(),
    model = list(),
    post = list()
  )
  class(res) <- "model_pipeline"
  res
}

set_formula_model <- function(x) {
}

set_formula <- function(x) {
}


pipeline(mpg ~ ., data = mtcars) |>
  step_normalize(all_predictors()) |>
  set_model(linear_reg) |>
  set_engine() |>
  adjust_numeric_range()
