suppressPackageStartupMessages(library(workflows))
suppressPackageStartupMessages(library(tune))
suppressPackageStartupMessages(library(dplyr))
suppressPackageStartupMessages(library(purrr))
suppressPackageStartupMessages(library(parsnip)) # imported by tune
suppressPackageStartupMessages(library(yardstick)) # imported by tune
suppressPackageStartupMessages(library(filtro))
suppressPackageStartupMessages(library(desirability2))
suppressPackageStartupMessages(library(S7))

# ------------------------------------------------------------------------------
# regression examples

CO2_ex <- CO2 |> dplyr::select(-Plant, -Treatment)

co2_rec <- recipes::recipe(uptake ~ ., data = CO2_ex) |>
  recipes::step_dummy(recipes::all_factor_predictors())

reg_f_wflow <- workflows::workflow(uptake ~ ., parsnip::linear_reg())
reg_r_wflow <- workflows::workflow(co2_rec, parsnip::linear_reg())
reg_v_wflow <-
  workflows::workflow() |>
  workflows::add_model(parsnip::linear_reg()) |>
  workflows::add_variables(outcomes = uptake, predictors = c(everything()))
reg_1d_wflow <- workflows::workflow(uptake ~ conc, parsnip::linear_reg())

reg_f_fit <- workflows:::fit.workflow(reg_f_wflow, CO2_ex)
reg_r_fit <- workflows:::fit.workflow(reg_r_wflow, CO2_ex)
reg_v_fit <- workflows:::fit.workflow(reg_v_wflow, CO2_ex)
reg_1d_fit <- workflows:::fit.workflow(reg_1d_wflow, CO2_ex)

reg_mtr <- yardstick::metric_set(yardstick::rsq, yardstick::mae)

# ------------------------------------------------------------------------------
# classification examples

cls_f_wflow <- workflows::workflow(Class ~ ., parsnip::logistic_reg())
cls_v_wflow <-
  workflows::workflow() |>
  workflows::add_model(parsnip::logistic_reg()) |>
  workflows::add_variables(outcomes = Class, predictors = c(everything()))
cls_1d_wflow <- workflows::workflow(Class ~ tau, parsnip::logistic_reg())

if (rlang::is_installed("modeldata")) {
  suppressPackageStartupMessages(library(modeldata))

  ad_data_small <-
    modeldata::ad_data |>
    dplyr::select(Class, tau, p_tau, VEGF, MMP10, Genotype, male)

  ad_rec <-
    recipes::recipe(Class ~ ., data = ad_data_small) |>
    recipes::step_pca(tau, p_tau, VEGF, MMP10, num_comp = 2) |>
    recipes::step_dummy(recipes::all_factor_predictors())

  cls_r_wflow <- workflows::workflow(ad_rec, parsnip::logistic_reg())

  cls_f_fit <- workflows:::fit.workflow(cls_f_wflow, ad_data_small)
  cls_r_fit <- workflows:::fit.workflow(cls_r_wflow, ad_data_small)
  cls_v_fit <- workflows:::fit.workflow(cls_v_wflow, ad_data_small)
  cls_1d_fit <- workflows:::fit.workflow(cls_1d_wflow, ad_data_small)
}

cls_mtr <- yardstick::metric_set(
  yardstick::brier_class,
  yardstick::kap,
  yardstick::mcc
)

# ------------------------------------------------------------------------------
# survival examples

if (rlang::is_installed("censored")) {
  suppressPackageStartupMessages(library(censored))

  time_to_million_small <-
    censored::time_to_million |>
    dplyr::select(time, event, year, runtime) |>
    dplyr::mutate(event_time = survival::Surv(time, event), .keep = "unused") |>
    dplyr::slice(1:150)

  srv_wflow <- workflows::workflow(event_time ~ ., parsnip::survival_reg())
  srv_fit <- workflows:::fit.workflow(srv_wflow, time_to_million_small)
  srv_times <- (1:4) / 4
}

srv_mtr <- yardstick::metric_set(
  yardstick::concordance_survival,
  yardstick::roc_auc_survival
)


# ------------------------------------------------------------------------------

# fmt: skip
ex_seed <-
	list(
		seed = list(
			c(10407L, -216878323L, -959064183L, 1254362331L,
				-777164474L, -83959242L, -852110966L)),
		id = 548676L
	)

# ------------------------------------------------------------------------------
# for recipes

POTATO <- function(x) {
  rlang::enquo(x)
}

goals <-
  desirability2::desirability(
    maximize(cor_pearson),
    constrain(cor_spearman, low = 0.7, high = 1)
  )

mtcars_wts <- mtcars
.wts <- seq(0, 1, length.out = 32)
mtcars_wts$case_weights <- hardhat::importance_weights(.wts)
