test_that("prediction api - original predictors, regression", {
  predictors <- important:::forge_predictors(head(CO2_ex), reg_1d_fit)
  predictions <- important:::predictions(
    reg_1d_fit,
    predictors,
    type = "original",
    eval_time = NULL
  )
  exp_ptype <-
    tibble::tibble(
      .pred = numeric(0),
      `(Intercept)` = numeric(0),
      conc = numeric(0)
    )
  expect_equal(predictions[0, ], exp_ptype)
  expect_equal(nrow(predictions), nrow(predictors))
})

test_that("prediction api - derived predictors, regression", {
  skip_if_not_installed("recipes")

  derived_predictors <-
    reg_r_fit |>
    extract_recipe() |>
    recipes::bake(head(CO2_ex), recipes::all_predictors())

  predictions <- important:::predictions(
    reg_r_fit,
    derived_predictors,
    type = "derived",
    eval_time = NULL
  )
  exp_ptype <-
    tibble::tibble(
      .pred = numeric(0),
      conc = numeric(0),
      Type_Mississippi = numeric(0)
    )
  expect_equal(predictions[0, ], exp_ptype)
  expect_equal(nrow(predictions), nrow(derived_predictors))
})


test_that("prediction api - original predictors, classification", {
  skip_if_not_installed("modeldata")
  skip_if_not_installed("recipes")

  exp_ptype <-
    tibble::tibble(
      .pred_class = structure(
        integer(0),
        levels = c("Impaired", "Control"),
        class = "factor"
      ),
      .pred_Impaired = numeric(0),
      .pred_Control = numeric(0),
      tau = numeric(0),
      p_tau = numeric(0),
      VEGF = numeric(0),
      MMP10 = numeric(0),
      Genotype = structure(
        integer(0),
        levels = levels(ad_data_small$Genotype),
        class = "factor"
      ),
      male = numeric(0)
    )

  predictors <- ad_data_small |> select(-Class)
  predictions <- important:::predictions(
    cls_r_fit,
    predictors,
    type = "original",
    eval_time = NULL
  )

  expect_equal(predictions[0, ], exp_ptype)
  expect_equal(nrow(predictions), nrow(predictors))
})


test_that("prediction api - derived predictors, classification", {
  skip_if_not_installed("modeldata")
  skip_if_not_installed("recipes")

  exp_ptype <-
    tibble::tibble(
      .pred_class = structure(
        integer(0),
        levels = c("Impaired", "Control"),
        class = "factor"
      ),
      .pred_Impaired = numeric(0),
      .pred_Control = numeric(0),
      male = numeric(0),
      PC1 = numeric(0),
      PC2 = numeric(0),
      Genotype_E2E3 = numeric(0),
      Genotype_E2E4 = numeric(0),
      Genotype_E3E3 = numeric(0),
      Genotype_E3E4 = numeric(0),
      Genotype_E4E4 = numeric(0)
    )

  derived_predictors <-
    cls_r_fit |>
    extract_recipe() |>
    recipes::bake(head(ad_data_small), recipes::all_predictors())

  predictions <- important:::predictions(
    cls_r_fit,
    derived_predictors,
    type = "derived",
    eval_time = NULL
  )

  expect_equal(predictions[0, ], exp_ptype)
  expect_equal(nrow(predictions), nrow(derived_predictors))
})


test_that("prediction api - original predictors, censored regression", {
  skip_if_not_installed("censored")

  exp_ptype <-
    tibble::tibble(
      .pred = list(),
      .pred_time = numeric(0),
      year = numeric(0),
      runtime = numeric(0)
    )
  exp_pred_ptype <-
    tibble::tibble(
      .eval_time = numeric(0),
      .pred_survival = numeric(0)
    )

  predictors <- important:::forge_predictors(
    head(time_to_million_small),
    srv_fit
  )
  predictions <- important:::predictions(
    srv_fit,
    predictors,
    type = "original",
    eval_time = srv_times
  )

  expect_equal(predictions[0, ], exp_ptype)
  expect_equal(nrow(predictions), nrow(predictors))

  expect_equal(predictions$.pred[[1]][0, ], exp_pred_ptype)
  expect_equal(nrow(predictions$.pred[[1]]), length(srv_times))
})

test_that("prediction api - derived predictors, censored regression", {
  skip_if_not_installed("censored")

  exp_ptype <-
    tibble::tibble(
      .pred = list(),
      .pred_time = numeric(0),
      year = numeric(0),
      runtime = numeric(0)
    )
  exp_pred_ptype <-
    tibble::tibble(
      .eval_time = numeric(0),
      .pred_survival = numeric(0)
    )

  derived_predictors <-
    srv_fit |>
    extract_preprocessor() |>
    model.frame(data = head(time_to_million_small)) |>
    dplyr::select(-event_time)

  predictions <- important:::predictions(
    srv_fit,
    derived_predictors,
    type = "derived",
    eval_time = srv_times
  )

  expect_equal(predictions[0, ], exp_ptype)
  expect_equal(nrow(predictions), nrow(derived_predictors))

  expect_equal(predictions$.pred[[1]][0, ], exp_pred_ptype)
  expect_equal(nrow(predictions$.pred[[1]]), length(srv_times))
})

# ------------------------------------------------------------------------------

test_that("compute metrics - original predictors, regression", {
  mtr_ptype <-
    tibble::tibble(
      .metric = character(0),
      .estimator = character(0),
      .estimate = numeric(0),
      predictor = character(0),
      id = numeric(0)
    )

  set.seed(1)
  reg_bl <-
    important:::metric_iter(
      column = NULL,
      seed = ex_seed,
      type = "original",
      wflow_fitted = reg_v_fit,
      dat = CO2_ex,
      metrics = reg_mtr,
      size = 20,
      outcome = "uptake",
      eval_time = NULL,
      event_level = "first"
    )

  expect_equal(reg_bl[0, ], mtr_ptype)
  expect_equal(nrow(reg_bl), 2L)
  expect_equal(reg_bl$.metric, c("rsq", "mae"))
  expect_equal(reg_bl$predictor, rep(".baseline", 2))

  set.seed(1)
  conc_bl <-
    important:::metric_iter(
      column = "conc",
      seed = ex_seed,
      type = "original",
      wflow_fitted = reg_v_fit,
      dat = CO2_ex,
      metrics = reg_mtr,
      size = 20,
      outcome = "uptake",
      eval_time = NULL,
      event_level = "first"
    )

  expect_equal(conc_bl[0, ], mtr_ptype)
  expect_equal(nrow(conc_bl), 2L)
  expect_equal(conc_bl$.metric, c("rsq", "mae"))
  expect_equal(conc_bl$predictor, rep("conc", 2))

  expect_true(
    reg_bl$.estimate[reg_bl$.metric == "rsq"] !=
      conc_bl$.estimate[conc_bl$.metric == "rsq"]
  )

  expect_true(
    reg_bl$.estimate[reg_bl$.metric == "mae"] <
      conc_bl$.estimate[conc_bl$.metric == "mae"]
  )
})


test_that("compute metrics - derived predictors, regression", {
  skip_if_not_installed("recipes")

  mtr_ptype <-
    tibble::tibble(
      .metric = character(0),
      .estimator = character(0),
      .estimate = numeric(0),
      predictor = character(0),
      id = numeric(0)
    )

  derived_predictors <-
    reg_r_fit |>
    extract_recipe() |>
    recipes::bake(CO2_ex)

  set.seed(1)
  reg_bl <-
    important:::metric_iter(
      column = NULL,
      seed = ex_seed,
      type = "derived",
      wflow_fitted = reg_r_fit,
      dat = derived_predictors,
      metrics = reg_mtr,
      size = 20,
      outcome = "uptake",
      eval_time = NULL,
      event_level = "first"
    )

  expect_equal(reg_bl[0, ], mtr_ptype)
  expect_equal(nrow(reg_bl), 2L)
  expect_equal(reg_bl$.metric, c("rsq", "mae"))
  expect_equal(reg_bl$predictor, rep(".baseline", 2))

  set.seed(1)
  type_bl <-
    important:::metric_iter(
      column = "Type_Mississippi",
      seed = ex_seed,
      type = "derived",
      wflow_fitted = reg_r_fit,
      dat = derived_predictors,
      metrics = reg_mtr,
      size = 20,
      outcome = "uptake",
      eval_time = NULL,
      event_level = "first"
    )

  expect_equal(type_bl[0, ], mtr_ptype)
  expect_equal(nrow(type_bl), 2L)
  expect_equal(type_bl$.metric, c("rsq", "mae"))
  expect_equal(type_bl$predictor, rep("Type_Mississippi", 2))

  expect_true(
    reg_bl$.estimate[reg_bl$.metric == "rsq"] !=
      type_bl$.estimate[type_bl$.metric == "rsq"]
  )

  expect_true(
    reg_bl$.estimate[reg_bl$.metric == "mae"] <
      type_bl$.estimate[type_bl$.metric == "mae"]
  )
})


test_that("compute metrics - original predictors, classification", {
  skip_if_not_installed("modeldata")

  mtr_ptype <-
    tibble::tibble(
      .metric = character(0),
      .estimator = character(0),
      .estimate = numeric(0),
      predictor = character(0),
      id = numeric(0)
    )

  set.seed(1)
  cls_bl <-
    important:::metric_iter(
      column = NULL,
      seed = ex_seed,
      type = "original",
      wflow_fitted = cls_v_fit,
      dat = ad_data_small,
      metrics = cls_mtr,
      size = 20,
      outcome = "Class",
      eval_time = NULL,
      event_level = "first"
    )

  expect_equal(cls_bl[0, ], mtr_ptype)
  expect_equal(nrow(cls_bl), 3L)
  expect_equal(cls_bl$.metric, c("kap", "mcc", "brier_class"))
  expect_equal(cls_bl$predictor, rep(".baseline", 3))

  set.seed(1)
  cls_tau <-
    important:::metric_iter(
      column = "tau",
      seed = ex_seed,
      type = "original",
      wflow_fitted = cls_v_fit,
      dat = ad_data_small,
      metrics = cls_mtr,
      size = 20,
      outcome = "Class",
      eval_time = NULL,
      event_level = "first"
    )

  expect_equal(cls_tau[0, ], mtr_ptype)
  expect_equal(nrow(cls_tau), 3L)
  expect_equal(cls_tau$.metric, c("kap", "mcc", "brier_class"))
  expect_equal(cls_tau$predictor, rep("tau", 3))

  expect_true(
    cls_bl$.estimate[cls_bl$.metric == "kap"] !=
      cls_tau$.estimate[cls_tau$.metric == "kap"]
  )

  expect_true(
    cls_bl$.estimate[cls_bl$.metric == "mcc"] !=
      cls_tau$.estimate[cls_tau$.metric == "mcc"]
  )

  expect_true(
    cls_bl$.estimate[cls_bl$.metric == "brier_class"] <
      cls_tau$.estimate[cls_tau$.metric == "brier_class"]
  )
})


test_that("compute metrics - derived predictors, classification", {
  skip_if_not_installed("modeldata")
  skip_if_not_installed("recipes")

  mtr_ptype <-
    tibble::tibble(
      .metric = character(0),
      .estimator = character(0),
      .estimate = numeric(0),
      predictor = character(0),
      id = numeric(0)
    )

  derived_predictors <-
    cls_r_fit |>
    extract_recipe() |>
    recipes::bake(ad_data_small)

  set.seed(1)
  cls_bl <-
    important:::metric_iter(
      column = NULL,
      seed = ex_seed,
      type = "derived",
      wflow_fitted = cls_r_fit,
      dat = derived_predictors,
      metrics = cls_mtr,
      size = 20,
      outcome = "Class",
      eval_time = NULL,
      event_level = "first"
    )

  expect_equal(cls_bl[0, ], mtr_ptype)
  expect_equal(nrow(cls_bl), 3L)
  expect_equal(cls_bl$.metric, c("kap", "mcc", "brier_class"))
  expect_equal(cls_bl$predictor, rep(".baseline", 3))

  set.seed(1)
  cls_pc1 <-
    important:::metric_iter(
      column = "PC1",
      seed = ex_seed,
      type = "derived",
      wflow_fitted = cls_r_fit,
      dat = derived_predictors,
      metrics = cls_mtr,
      size = 20,
      outcome = "Class",
      eval_time = NULL,
      event_level = "first"
    )

  expect_equal(cls_pc1[0, ], mtr_ptype)
  expect_equal(nrow(cls_pc1), 3L)
  expect_equal(cls_pc1$.metric, c("kap", "mcc", "brier_class"))
  expect_equal(cls_pc1$predictor, rep("PC1", 3))

  expect_true(
    cls_bl$.estimate[cls_bl$.metric == "kap"] !=
      cls_pc1$.estimate[cls_pc1$.metric == "kap"]
  )

  expect_true(
    cls_bl$.estimate[cls_bl$.metric == "mcc"] !=
      cls_pc1$.estimate[cls_pc1$.metric == "mcc"]
  )

  expect_true(
    cls_bl$.estimate[cls_bl$.metric == "brier_class"] <
      cls_pc1$.estimate[cls_pc1$.metric == "brier_class"]
  )
})


test_that("compute metrics - original predictors, censored regression", {
  skip_if_not_installed("censored")

  mtr_ptype <-
    tibble::tibble(
      .metric = character(0),
      .estimator = character(0),
      .eval_time = numeric(0),
      .estimate = numeric(0),
      predictor = character(0),
      id = numeric(0)
    )
  mtr_nms <- c(
    "roc_auc_survival",
    "roc_auc_survival",
    "roc_auc_survival",
    "roc_auc_survival",
    "concordance_survival"
  )

  set.seed(1)
  srv_bl <-
    important:::metric_iter(
      column = NULL,
      seed = ex_seed,
      type = "original",
      wflow_fitted = srv_fit,
      dat = time_to_million_small,
      metrics = srv_mtr,
      size = 20,
      outcome = "event_time",
      eval_time = srv_times,
      event_level = "first"
    )

  expect_equal(srv_bl[0, ], mtr_ptype)
  expect_equal(nrow(srv_bl), 5L)
  expect_equal(srv_bl$.metric, mtr_nms)
  expect_equal(srv_bl$predictor, rep(".baseline", 5))

  set.seed(1)
  srv_year <-
    important:::metric_iter(
      column = "year",
      seed = ex_seed,
      type = "original",
      wflow_fitted = srv_fit,
      dat = time_to_million_small,
      metrics = srv_mtr,
      size = 20,
      outcome = "event_time",
      eval_time = srv_times,
      event_level = "first"
    )

  expect_equal(srv_year[0, ], mtr_ptype)
  expect_equal(nrow(srv_year), 5L)
  expect_equal(srv_year$.metric, mtr_nms)
  expect_equal(srv_year$predictor, rep("year", 5))

  expect_true(
    srv_bl$.estimate[
      srv_bl$.metric == "roc_auc_survival" & srv_bl$.eval_time == 0.25
    ] !=
      srv_year$.estimate[
        srv_year$.metric == "roc_auc_survival" & srv_bl$.eval_time == 0.25
      ]
  )

  expect_true(
    srv_bl$.estimate[
      srv_bl$.metric == "roc_auc_survival" & srv_bl$.eval_time == 0.5
    ] !=
      srv_year$.estimate[
        srv_year$.metric == "roc_auc_survival" & srv_bl$.eval_time == 0.5
      ]
  )

  expect_true(
    srv_bl$.estimate[
      srv_bl$.metric == "roc_auc_survival" & srv_bl$.eval_time == 0.75
    ] !=
      srv_year$.estimate[
        srv_year$.metric == "roc_auc_survival" & srv_bl$.eval_time == 0.75
      ]
  )

  expect_true(
    srv_bl$.estimate[srv_bl$.metric == "concordance_survival"] !=
      srv_year$.estimate[srv_year$.metric == "concordance_survival"]
  )
})


test_that("compute metrics - derived predictors, censored regression", {
  skip_if_not_installed("censored")

  mtr_ptype <-
    tibble::tibble(
      .metric = character(0),
      .estimator = character(0),
      .eval_time = numeric(0),
      .estimate = numeric(0),
      predictor = character(0),
      id = numeric(0)
    )
  mtr_nms <- c(
    "roc_auc_survival",
    "roc_auc_survival",
    "roc_auc_survival",
    "roc_auc_survival",
    "concordance_survival"
  )

  derived_predictors <-
    srv_fit |>
    extract_preprocessor() |>
    model.frame(data = time_to_million_small)

  set.seed(1)
  srv_bl <-
    important:::metric_iter(
      column = NULL,
      seed = ex_seed,
      type = "derived",
      wflow_fitted = srv_fit,
      dat = derived_predictors,
      metrics = srv_mtr,
      size = 20,
      outcome = "event_time",
      eval_time = srv_times,
      event_level = "first"
    )

  expect_equal(srv_bl[0, ], mtr_ptype)
  expect_equal(nrow(srv_bl), 5L)
  expect_equal(srv_bl$.metric, mtr_nms)
  expect_equal(srv_bl$predictor, rep(".baseline", 5))

  set.seed(1)
  srv_runtime <-
    important:::metric_iter(
      column = "runtime",
      seed = ex_seed,
      type = "derived",
      wflow_fitted = srv_fit,
      dat = derived_predictors,
      metrics = srv_mtr,
      size = 20,
      outcome = "event_time",
      eval_time = srv_times,
      event_level = "first"
    )

  expect_equal(srv_runtime[0, ], mtr_ptype)
  expect_equal(nrow(srv_runtime), 5L)
  expect_equal(srv_runtime$.metric, mtr_nms)
  expect_equal(srv_runtime$predictor, rep("runtime", 5))

  expect_true(
    srv_bl$.estimate[
      srv_bl$.metric == "roc_auc_survival" & srv_bl$.eval_time == 0.25
    ] !=
      srv_runtime$.estimate[
        srv_runtime$.metric == "roc_auc_survival" & srv_bl$.eval_time == 0.25
      ]
  )

  expect_true(
    srv_bl$.estimate[
      srv_bl$.metric == "roc_auc_survival" & srv_bl$.eval_time == 0.5
    ] !=
      srv_runtime$.estimate[
        srv_runtime$.metric == "roc_auc_survival" & srv_bl$.eval_time == 0.5
      ]
  )

  expect_true(
    srv_bl$.estimate[
      srv_bl$.metric == "roc_auc_survival" & srv_bl$.eval_time == 0.75
    ] !=
      srv_runtime$.estimate[
        srv_runtime$.metric == "roc_auc_survival" & srv_bl$.eval_time == 0.75
      ]
  )

  expect_true(
    srv_bl$.estimate[srv_bl$.metric == "concordance_survival"] !=
      srv_runtime$.estimate[srv_runtime$.metric == "concordance_survival"]
  )

  ###

  expect_snapshot(
    {
      important:::metric_iter(
        column = "moash",
        seed = ex_seed,
        type = "derived",
        wflow_fitted = srv_fit,
        dat = derived_predictors,
        metrics = srv_mtr,
        size = 20,
        outcome = "event_time",
        eval_time = srv_times,
        event_level = "first"
      )
    },
    error = TRUE
  )
})

test_that("importance_perm() function - regression", {
  res_ptype <-
    tibble::tibble(
      .metric = character(0),
      predictor = character(0),
      n = integer(0),
      mean = numeric(0),
      std_err = numeric(0),
      importance = numeric(0)
    )

  orig_ptype <- res_ptype
  class(orig_ptype) <- c(
    "importance_perm",
    "original_importance_perm",
    class(res_ptype)
  )

  derv_ptype <- res_ptype
  class(derv_ptype) <- c(
    "importance_perm",
    "derived_importance_perm",
    class(res_ptype)
  )

  set.seed(1)
  res_orig <-
    importance_perm(
      wflow = reg_v_fit,
      dat = CO2_ex,
      metrics = reg_mtr,
      type = "original",
      size = 20,
      times = 3
    )

  expect_equal(res_orig[0, ], orig_ptype)
  expect_equal(nrow(res_orig), 4L)
  expect_equal(unique(sort(res_orig$.metric)), c("mae", "rsq"))
  expect_equal(unique(sort(res_orig$predictor)), c("Type", "conc"))
  expect_equal(res_orig$n, rep(3, 4))

  ###

  set.seed(1)
  res_derv <-
    importance_perm(
      wflow = reg_f_fit,
      dat = CO2_ex,
      metrics = reg_mtr,
      type = "derived",
      size = 20,
      times = 3
    )

  expect_equal(res_derv[0, ], derv_ptype)
  expect_equal(nrow(res_derv), 6L)
  expect_equal(unique(sort(res_derv$.metric)), c("mae", "rsq"))
  expect_equal(
    unique(sort(res_derv$predictor)),
    c("(Intercept)", "TypeMississippi", "conc")
  )
  expect_equal(res_derv$n, rep(3, 6))
})


test_that("importance_perm() function - classification", {
  skip_if_not_installed("modeldata")
  skip_if_not_installed("recipes")

  res_ptype <-
    tibble::tibble(
      .metric = character(0),
      predictor = character(0),
      n = integer(0),
      mean = numeric(0),
      std_err = numeric(0),
      importance = numeric(0)
    )

  orig_ptype <- res_ptype
  class(orig_ptype) <- c(
    "importance_perm",
    "original_importance_perm",
    class(res_ptype)
  )

  derv_ptype <- res_ptype
  class(derv_ptype) <- c(
    "importance_perm",
    "derived_importance_perm",
    class(res_ptype)
  )

  set.seed(1)
  res_orig <-
    importance_perm(
      wflow = cls_f_fit,
      dat = ad_data_small,
      metrics = cls_mtr,
      type = "original",
      size = 20,
      times = 3
    )

  expect_equal(res_orig[0, ], orig_ptype)
  expect_equal(nrow(res_orig), 18L)
  expect_equal(
    unique(sort(res_orig$.metric)),
    c("brier_class", "kap", "mcc")
  )
  expect_equal(
    unique(sort(res_orig$predictor)),
    c("Genotype", "MMP10", "VEGF", "male", "p_tau", "tau")
  )
  expect_equal(res_orig$n, rep(3, 18))

  ###

  set.seed(1)
  res_derv <-
    importance_perm(
      wflow = cls_r_fit,
      dat = ad_data_small,
      metrics = cls_mtr,
      type = "derived",
      size = 20,
      times = 3
    )

  expect_equal(res_derv[0, ], derv_ptype)
  expect_equal(nrow(res_derv), 24L)
  expect_equal(
    unique(sort(res_derv$.metric)),
    c("brier_class", "kap", "mcc")
  )
  expect_equal(
    unique(sort(res_derv$predictor)),
    c(
      "Genotype_E2E3",
      "Genotype_E2E4",
      "Genotype_E3E3",
      "Genotype_E3E4",
      "Genotype_E4E4",
      "PC1",
      "PC2",
      "male"
    )
  )
  expect_equal(res_derv$n, rep(3, 24))
})


test_that("importance_perm() function - censored regression", {
  skip_if_not_installed("censored")

  res_ptype <-
    tibble::tibble(
      .metric = character(0),
      predictor = character(0),
      .eval_time = numeric(0),
      n = integer(0),
      mean = numeric(0),
      std_err = numeric(0),
      importance = numeric(0)
    )

  orig_ptype <- res_ptype
  class(orig_ptype) <- c(
    "importance_perm",
    "original_importance_perm",
    class(res_ptype)
  )

  derv_ptype <- res_ptype
  class(derv_ptype) <- c(
    "importance_perm",
    "derived_importance_perm",
    class(res_ptype)
  )

  set.seed(1)
  res_orig <-
    importance_perm(
      wflow = srv_fit,
      dat = time_to_million_small,
      metrics = srv_mtr,
      type = "original",
      size = 20,
      times = 3,
      eval_time = srv_times
    )

  expect_equal(res_orig[0, ], orig_ptype)
  expect_equal(nrow(res_orig), 10L)
  expect_equal(
    unique(sort(res_orig$.metric)),
    c("concordance_survival", "roc_auc_survival")
  )
  expect_equal(
    unique(sort(res_orig$predictor)),
    c("runtime", "year")
  )
  expect_equal(res_orig$n, rep(3, 10L))

  ###

  set.seed(1)
  res_derv <-
    importance_perm(
      wflow = srv_fit,
      dat = time_to_million_small,
      metrics = srv_mtr,
      type = "derived",
      size = 20,
      times = 3,
      eval_time = srv_times
    )

  expect_equal(res_derv[0, ], derv_ptype)
  expect_equal(nrow(res_derv), 10L)
  expect_equal(
    unique(sort(res_derv$.metric)),
    c("concordance_survival", "roc_auc_survival")
  )
  expect_equal(
    unique(sort(res_derv$predictor)),
    c("runtime", "year")
  )
  expect_equal(res_derv$n, rep(3, 10L))
})
