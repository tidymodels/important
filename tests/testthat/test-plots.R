test_that("autoplot - classification", {
  skip_if_not_installed("modeldata")
  skip_if_not_installed("recipes")

  suppressPackageStartupMessages(library(recipes))

  data(ad_data, package = "modeldata")

  ad_rec <-
    recipe(Class ~ ., data = ad_data) |>
    step_pca(all_numeric_predictors(), -male, -age, num_comp = 20) |>
    step_dummy(all_factor_predictors())

  cls_r_fit <-
    workflow(ad_rec, logistic_reg()) |>
    fit(data = ad_data)

  set.seed(1)
  res_orig <-
    importance_perm(
      wflow = cls_r_fit,
      dat = ad_data,
      metrics = cls_mtr,
      type = "original",
      size = 20,
      times = 3
    )

  p1 <- autoplot(res_orig)
  expect_s3_class(p1, "ggplot")
  expect_equal(
    p1$labels,
    list(
      y = NULL,
      x = "Permutation Importance Score",
      xintercept = "xintercept"
    )
  )
  expect_equal(names(p1$mapping), "y")
  expect_equal(
    rlang::quo_get_expr(p1$mapping$y) |> as.character(),
    "predictor"
  )
  expect_equal(length(p1$layers), 2)
  expect_equal(names(p1$layers[[1]]$mapping), "xintercept")
  expect_equal(
    rlang::quo_get_expr(p1$layers[[1]]$mapping$xintercept) |> as.character(),
    "xintercept"
  )
  expect_equal(names(p1$layers[[2]]$mapping), "x")
  expect_equal(
    rlang::quo_get_expr(p1$layers[[2]]$mapping$x) |> as.character(),
    "importance"
  )

  p2 <- autoplot(res_orig, top = 3, type = "difference")
  expect_s3_class(p2, "ggplot")
  expect_equal(
    length(levels(p2$data$predictor)),
    3L
  )
  expect_equal(
    p2$labels,
    list(
      y = NULL,
      x = "Permutation Importance Score",
      xintercept = "xintercept",
      xmin = "mean - std_errs * std_err",
      xmax = "mean + std_errs * std_err"
    )
  )

  expect_equal(length(p2$layers), 3)
  expect_equal(names(p2$mapping), "y")
  expect_equal(
    rlang::quo_get_expr(p2$mapping$y) |> as.character(),
    "predictor"
  )
  expect_equal(names(p2$layers[[1]]$mapping), "xintercept")
  expect_equal(
    rlang::quo_get_expr(p2$layers[[1]]$mapping$xintercept) |> as.character(),
    "xintercept"
  )
  expect_equal(names(p2$layers[[2]]$mapping), "x")
  expect_equal(
    rlang::quo_get_expr(p2$layers[[2]]$mapping$x) |> as.character(),
    "mean"
  )
  expect_equal(names(p2$layers[[3]]$mapping), c("xmin", "xmax"))
  expect_equal(
    rlang::quo_get_expr(p2$layers[[3]]$mapping$xmin) |> as.character(),
    c("-", "mean", "std_errs * std_err")
  )
  expect_equal(
    rlang::quo_get_expr(p2$layers[[3]]$mapping$xmax) |> as.character(),
    c("+", "mean", "std_errs * std_err")
  )

  ###

  set.seed(1)
  res_derv <-
    importance_perm(
      wflow = cls_r_fit,
      dat = ad_data,
      metrics = cls_mtr,
      type = "derived",
      size = 20,
      times = 3
    )

  p3 <- autoplot(res_derv, metric = "brier_class")
  expect_s3_class(p3, "ggplot")
  expect_equal(
    p3$labels,
    list(
      y = NULL,
      x = "Permutation Importance Score",
      xintercept = "xintercept"
    )
  )
  expect_equal(length(p3$layers), 2)
  expect_equal(names(p3$mapping), "y")
  expect_equal(
    rlang::quo_get_expr(p3$mapping$y) |> as.character(),
    "predictor"
  )
  expect_equal(names(p3$layers[[1]]$mapping), "xintercept")
  expect_equal(
    rlang::quo_get_expr(p3$layers[[1]]$mapping$xintercept) |> as.character(),
    "xintercept"
  )
  expect_equal(names(p3$layers[[2]]$mapping), "x")
  expect_equal(
    rlang::quo_get_expr(p3$layers[[2]]$mapping$x) |> as.character(),
    "importance"
  )
})
