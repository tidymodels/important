test_that("step works", {
  rec <- recipe(mpg ~ ., data = mtcars) |>
    step_predictor_best(
      all_predictors(),
      score = "cor_pearson",
      prop_terms = 1 / 2
    )

  prepped <- prep(rec)
  res_bake <- bake(prepped, mtcars)
  res_tidy <- tidy(prepped, 1)

  cor_pearson_res <- filtro::score_cor_pearson |>
    filtro::fit(mpg ~ ., data = mtcars)

  cor_pearson_res <- cor_pearson_res |>
    filtro::fill_safe_value(transform = TRUE)
  exp <- cor_pearson_res@results |>
    dplyr::slice_max(score, prop = 1 / 2, with_ties = TRUE) |>
    dplyr::pull(predictor)

  expect_identical(
    sort(setdiff(names(mtcars), names(res_bake))),
    sort(setdiff(names(mtcars)[-1], exp))
  )

  expect_identical(
    sort(res_tidy$terms[res_tidy$removed]),
    sort(setdiff(names(mtcars)[-1], exp))
  )
  expect_named(
    res_tidy,
    c("terms", "removed", "score", "id")
  )
})

test_that("EVERYTHING MUST GO", {
  rec <- recipe(mpg ~ ., data = mtcars) |>
    step_predictor_best(
      all_predictors(),
      score = "cor_pearson",
      prop_terms = 0.0,
      update_prop = FALSE
    )

  prepped <- prep(rec)
  res_bake <- bake(prepped, mtcars)
  res_tidy <- tidy(prepped, 1)

  expect_identical(
    sort(setdiff(names(mtcars), names(res_bake))),
    sort(names(mtcars)[-1])
  )
  expect_true(
    all(res_tidy$removed)
  )
  expect_named(
    res_tidy,
    c("terms", "removed", "score", "id")
  )
})

test_that("keep everything", {
  set.seed(1)
  rec <- recipe(mpg ~ ., data = mtcars) |>
    step_predictor_best(
      all_predictors(),
      score = "cor_pearson",
      prop_terms = 1
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
    c("terms", "removed", "score", "id")
  )
})

test_that("case weights work", {
  unweighted <- recipe(mpg ~ ., data = mtcars) |>
    step_predictor_best(
      all_predictors(),
      score = "cor_pearson",
      prop_terms = 1 / 2
    ) |>
    prep()

  weighted <- recipe(mpg ~ ., data = mtcars_wts) |>
    step_predictor_best(
      all_predictors(),
      score = "cor_pearson",
      prop_terms = 1 / 2
    ) |>
    prep()

  expect_snapshot(print(weighted))

  unweighted_res <- tidy(unweighted, number = 1) |>
    dplyr::select(terms, unweighted = score)
  weighted_res <- tidy(weighted, number = 1) |>
    dplyr::select(terms, weighted = score)
  both_res <- dplyr::full_join(unweighted_res, weighted_res, by = "terms")
  expect_false(isTRUE(all.equal(both_res$weighted, both_res$unweighted)))
})

test_that("missing score arg", {
  skip_if(getRversion() <= "4.3.0")
  expect_snapshot(
    error = TRUE,
    recipe(mpg ~ ., data = mtcars) |>
      step_predictor_best(
        all_predictors(),
        prop_terms = 1 / 2
      )
  )
})

# Infrastructure ---------------------------------------------------------------
test_that("bake method errors when needed non-standard role columns are missing", {
  # Here for completeness
  # step_predictor_best() removes variables and thus does not care if they are not there.
  expect_true(TRUE)
})

test_that("empty printing", {
  rec <- recipe(mpg ~ ., mtcars)
  rec <- step_predictor_best(rec, score = "cor_pearson")

  expect_snapshot(rec)

  rec <- prep(rec, mtcars)

  expect_snapshot(rec)
})

test_that("empty selection prep/bake is a no-op", {
  rec1 <- recipe(mpg ~ ., mtcars)
  rec2 <- step_predictor_best(rec1, score = "cor_pearson")

  rec1 <- prep(rec1, mtcars)
  rec2 <- prep(rec2, mtcars)

  baked1 <- bake(rec1, mtcars)
  baked2 <- bake(rec2, mtcars)

  expect_identical(baked1, baked2)
})

test_that("empty selection tidy method works", {
  rec <- recipe(mpg ~ ., mtcars)
  rec <- step_predictor_best(rec, score = "cor_pearson")

  expect <- tibble(terms = character(), id = character())

  expect_identical(tidy(rec, number = 1), expect)

  rec <- prep(rec, mtcars)

  expect_identical(tidy(rec, number = 1), expect)
})

test_that("printing", {
  set.seed(1)
  rec <- recipe(mpg ~ ., data = mtcars) |>
    step_predictor_best(all_predictors(), score = "cor_pearson")

  expect_snapshot(print(rec))
  expect_snapshot(prep(rec))
})

test_that("tunable is setup to work with extract_parameter_set_dials", {
  skip_if_not_installed("dials", minimum_version = "1.4.1.9000")
  rec <- recipe(~., data = mtcars) |>
    step_predictor_best(
      all_predictors(),
      score = "cor_pearson",
      prop_terms = hardhat::tune()
    )

  params <- extract_parameter_set_dials(rec)

  expect_s3_class(params, "parameters")
  expect_identical(nrow(params), 1L)
})

test_that("bad args", {
  expect_snapshot(
    recipe(mpg ~ ., mtcars) |>
      step_predictor_best(
        all_predictors(),
        prop_terms = 2,
        score = "cor_pearson"
      ) |>
      prep(),
    error = TRUE
  )
})

test_that("0 and 1 rows data work in bake method", {
  data <- mtcars
  rec <- recipe(mpg ~ ., data) |>
    step_predictor_best(all_numeric_predictors(), score = "cor_pearson") |>
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
