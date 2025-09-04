test_that("step works", {
  set.seed(1)
  rec <- recipe(mpg ~ ., data = mtcars) |>
    step_predictor_desirability(
      all_predictors(),
      score = goals,
      prop_terms = 0.2
    )

  prepped <- prep(rec)

  res_bake <- bake(prepped, mtcars)
  res_tidy <- tidy(prepped, 1)

  score_res <-
    list(
      filtro::score_cor_pearson |> filtro::fit(mpg ~ ., data = mtcars),
      filtro::score_cor_spearman |> filtro::fit(mpg ~ ., data = mtcars)
    ) |>
    filtro::fill_safe_values(transform = TRUE) |>
    dplyr::mutate(
      d_pearson = desirability2::d_max(cor_pearson, use_data = TRUE),
      d_spearman = desirability2::d_box(cor_spearman, low = 0.7, high = 1.0),
      d_overall = desirability2::d_overall(dplyr::across(dplyr::starts_with(
        "d_"
      )))
    )

  retained <- score_res |>
    dplyr::slice_max(d_overall, prop = 0.2, with_ties = TRUE)

  expect_identical(
    sort(setdiff(names(mtcars), names(res_bake))),
    sort(setdiff(names(mtcars)[-1], retained$predictor))
  )

  expect_identical(
    sort(res_tidy$terms[res_tidy$removed]),
    sort(setdiff(names(mtcars)[-1], retained$predictor))
  )
  expect_named(
    res_tidy,
    c(
      "terms",
      "removed",
      "cor_pearson",
      "cor_spearman",
      ".d_max_cor_pearson",
      ".d_box_cor_spearman",
      ".d_overall",
      "id"
    )
  )
})

test_that("EVERYTHING MUST GO", {
  bad_goals <-
    desirability2::desirability(
      constrain(cor_spearman, low = 2, high = 3)
    )

  set.seed(1)
  rec <- recipe(mpg ~ ., data = mtcars) |>
    step_predictor_desirability(
      all_predictors(),
      score = bad_goals,
      prop_terms = 0.2
    )

  prepped <- prep(rec)

  res_bake <- bake(prepped, mtcars)
  res_tidy <- tidy(prepped, 1)

  expect_identical(
    names(res_bake),
    "mpg"
  )

  expect_identical(
    sort(res_tidy$terms[res_tidy$removed]),
    sort(names(mtcars)[-1])
  )
  expect_named(
    res_tidy,
    c(
      "terms",
      "removed",
      "cor_spearman",
      ".d_box_cor_spearman",
      ".d_overall",
      "id"
    )
  )
})

test_that("wrong score type", {
  wrong_goals <-
    desirability2::desirability(
      maximize(xtab_pval_fisher)
    )

  set.seed(1)
  rec <- recipe(mpg ~ ., data = mtcars) |>
    step_predictor_desirability(
      all_predictors(),
      score = wrong_goals,
      prop_terms = 0.2
    )

  expect_snapshot_warning(prepped <- prep(rec))

  res_bake <- bake(prepped, mtcars)
  res_tidy <- tidy(prepped, 1)

  expect_identical(
    sort(names(res_bake)),
    sort(names(mtcars))
  )

  expect_identical(
    sort(res_tidy$terms[res_tidy$removed]),
    character(0)
  )
  expect_named(
    res_tidy,
    c("terms", "removed", "xtab_pval_fisher", "id")
  )
})

test_that("keep everything", {
  easy_goals <-
    desirability2::desirability(
      constrain(cor_spearman, low = -2, high = 3)
    )

  set.seed(1)
  rec <- recipe(mpg ~ ., data = mtcars) |>
    step_predictor_desirability(
      all_predictors(),
      score = easy_goals,
      prop_terms = 0.2
    )

  prepped <- prep(rec)

  res_bake <- bake(prepped, mtcars)
  res_tidy <- tidy(prepped, 1)

  expect_identical(
    sort(names(res_bake)),
    sort(names(mtcars))
  )

  expect_identical(
    sort(res_tidy$terms[res_tidy$removed]),
    character(0)
  )
  expect_named(
    res_tidy,
    c(
      "terms",
      "removed",
      "cor_spearman",
      ".d_box_cor_spearman",
      ".d_overall",
      "id"
    )
  )
})

test_that("case weights work", {
  unweighted <- recipe(mpg ~ ., data = mtcars) |>
    step_predictor_desirability(
      all_predictors(),
      score = goals,
      prop_terms = 0.2
    ) |>
    prep()

  weighted <- recipe(mpg ~ ., data = mtcars_wts) |>
    step_predictor_desirability(
      all_predictors(),
      score = goals,
      prop_terms = 0.2
    ) |>
    prep()

  expect_snapshot(print(weighted))

  unweighted_res <- tidy(unweighted, number = 1) |>
    dplyr::select(terms, unweighted = cor_spearman)
  weighted_res <- tidy(weighted, number = 1) |>
    dplyr::select(terms, weighted = cor_spearman)
  both_res <- dplyr::full_join(unweighted_res, weighted_res, by = "terms")
  expect_false(isTRUE(all.equal(both_res$weighted, both_res$unweighted)))
})

# Infrastructure ---------------------------------------------------------------

test_that("bake method errors when needed non-standard role columns are missing", {
  # Here for completeness
  # step_predictor_desirability() removes variables and thus does not care if they are not there.
  expect_true(TRUE)
})

test_that("empty printing", {
  skip_if(getRversion() <= "4.3.0")
  rec <- recipe(mpg ~ ., mtcars)
  expect_snapshot(step_predictor_desirability(rec), error = TRUE)
})

test_that("printing", {
  set.seed(1)
  rec <- recipe(mpg ~ ., data = mtcars) |>
    step_predictor_desirability(
      all_predictors(),
      score = goals
    )

  expect_snapshot(print(rec))
  expect_snapshot(prep(rec))
})

test_that("tunable is setup to work with extract_parameter_set_dials", {
  skip_if_not_installed("dials", minimum_version = "1.4.1.9000")

  rec <- recipe(~., data = mtcars) |>
    step_predictor_desirability(
      all_predictors(),
      score = goals,
      prop_terms = hardhat::tune()
    )

  params <- extract_parameter_set_dials(rec)

  expect_s3_class(params, "parameters")
  expect_identical(nrow(params), 1L)
})

test_that("bad args", {
  expect_snapshot(
    recipe(mpg ~ ., mtcars) |>
      step_predictor_desirability(
        all_predictors(),
        score = goals,
        prop_terms = 2
      ) |>
      prep(),
    error = TRUE
  )
})

test_that("0 and 1 rows data work in bake method", {
  data <- mtcars
  rec <- recipe(mpg ~ ., data) |>
    step_predictor_desirability(all_numeric_predictors(), score = goals, ) |>
    prep()

  expect_identical(
    nrow(bake(rec, slice(data, 1))),
    1L
  )
  expect_identical(
    nrow(bake(rec, slice(data, 0))),
    0L
  )
})
