test_that("step works", {
  rec <- recipe(mpg ~ ., data = mtcars) |>
    step_predictor_retain(
      all_predictors(),
      score = abs(cor_pearson) >= 0.75 & abs(cor_spearman) >= 0.6
    )

  prepped <- prep(rec)
  res_bake <- bake(prepped, mtcars)
  res_tidy <- tidy(prepped, 1)

  cor_pearson_res <- filtro::score_cor_pearson |>
    filtro::fit(mpg ~ ., data = mtcars)
  cor_spearman_res <- filtro::score_cor_spearman |>
    filtro::fit(mpg ~ ., data = mtcars)

  exp <- filtro::fill_safe_values(list(
    cor_pearson_res,
    cor_spearman_res
  )) |>
    dplyr::filter(abs(cor_pearson) < 0.75 | abs(cor_spearman) < 0.6) |>
    dplyr::pull(predictor)

  expect_identical(
    sort(setdiff(names(mtcars), names(res_bake))),
    sort(exp)
  )
  expect_identical(
    sort(res_tidy$terms[res_tidy$removed]),
    sort(exp)
  )
  expect_named(
    res_tidy,
    c("terms", "removed", "cor_pearson", "cor_spearman", "id")
  )
})

test_that("EVERYTHING MUST GO", {
  rec <- recipe(mpg ~ ., data = mtcars) |>
    step_predictor_retain(
      all_predictors(),
      score = abs(cor_pearson) >= Inf & abs(cor_spearman) >= Inf
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
    c("terms", "removed", "cor_pearson", "cor_spearman", "id")
  )
})

test_that("keep everything", {
  set.seed(1)
  rec <- recipe(mpg ~ ., data = mtcars) |>
    step_predictor_retain(
      all_predictors(),
      score = abs(cor_pearson) >= -1 & abs(cor_spearman) >= -1
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
    c("terms", "removed", "cor_pearson", "cor_spearman", "id")
  )
})

test_that("allows for one score", {
  rec <- recipe(mpg ~ ., data = mtcars) |>
    step_predictor_retain(
      all_predictors(),
      score = abs(cor_pearson) >= 0.7
    )

  prepped <- prep(rec)
  res_bake <- bake(prepped, mtcars)
  res_tidy <- tidy(prepped, 1)

  cor_pearson_res <- filtro::score_cor_pearson |>
    filtro::fit(mpg ~ ., data = mtcars)

  exp <- cor_pearson_res@results |>
    dplyr::filter(abs(score) < 0.7) |>
    dplyr::pull(predictor)

  expect_identical(
    sort(setdiff(names(mtcars), names(res_bake))),
    sort(exp)
  )
  expect_named(
    res_tidy,
    c("terms", "removed", "cor_pearson", "id")
  )
})

test_that("case weights work", {
  unweighted <- recipe(mpg ~ ., data = mtcars) |>
    step_predictor_retain(
      all_predictors(),
      score = abs(cor_pearson) >= 0.75 & abs(cor_spearman) >= 0.6
    ) |>
    prep()

  weighted <- recipe(mpg ~ ., data = mtcars_wts) |>
    step_predictor_retain(
      all_predictors(),
      score = abs(cor_pearson) >= 0.75 & abs(cor_spearman) >= 0.6
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
  # step_predictor_retain() removes variables and thus does not care if they are not there.
  expect_true(TRUE)
})

test_that("empty printing", {
  rec <- recipe(mpg ~ ., mtcars)
  rec <- step_predictor_retain(rec)

  expect_snapshot(rec)

  rec <- prep(rec, mtcars)

  expect_snapshot(rec)
})

test_that("empty selection prep/bake is a no-op", {
  rec1 <- recipe(mpg ~ ., mtcars)
  rec2 <- step_predictor_retain(rec1)

  rec1 <- prep(rec1, mtcars)
  rec2 <- prep(rec2, mtcars)

  baked1 <- bake(rec1, mtcars)
  baked2 <- bake(rec2, mtcars)

  expect_identical(baked1, baked2)
})

test_that("empty selection tidy method works", {
  rec <- recipe(mpg ~ ., mtcars)
  rec <- step_predictor_retain(rec)

  expect <- tibble(terms = character(), id = character())

  expect_identical(tidy(rec, number = 1), expect)

  rec <- prep(rec, mtcars)

  expect_identical(
    tidy(rec, number = 1),
    tibble::tibble(
      terms = character(0),
      removed = logical(0),
      id = character(0)
    )
  )
})

test_that("printing", {
  set.seed(1)
  rec <- recipe(mpg ~ ., data = mtcars) |>
    step_predictor_retain(all_predictors(), score = abs(cor_pearson) >= 0.75)

  expect_snapshot(print(rec))
  expect_snapshot(prep(rec))
})

test_that("bad args", {
  expect_snapshot(
    recipe(mpg ~ ., mtcars) |>
      step_predictor_retain(all_predictors(), threshold = 2) |>
      prep(),
    error = TRUE
  )
})

test_that("0 and 1 rows data work in bake method", {
  data <- mtcars
  rec <- recipe(mpg ~ ., data) |>
    step_predictor_retain(
      all_numeric_predictors(),
      score = abs(cor_pearson) >= 0.75
    ) |>
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
