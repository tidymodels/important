suppressPackageStartupMessages(library(workflows))
suppressPackageStartupMessages(library(tune))
suppressPackageStartupMessages(library(dplyr))
suppressPackageStartupMessages(library(purrr))
suppressPackageStartupMessages(library(recipes)) # imported by tune
suppressPackageStartupMessages(library(parsnip)) # imported by tune
suppressPackageStartupMessages(library(yardstick)) # imported by tune

# ------------------------------------------------------------------------------
# regression examples

CO2_ex <- CO2 %>% dplyr::select(-Plant, -Treatment)

co2_rec <- recipe(uptake ~ ., data = CO2_ex) %>%
  step_dummy(all_factor_predictors())

reg_f_wflow <- workflow(uptake ~ ., linear_reg())
reg_r_wflow <- workflow(co2_rec, linear_reg())
reg_v_wflow <-
  workflow() %>%
  add_model(linear_reg()) %>%
  add_variables(outcomes = uptake, predictors = c(everything()))
reg_1d_wflow <- workflow(uptake ~ conc, linear_reg())

reg_f_fit <- fit(reg_f_wflow, CO2_ex)
reg_r_fit <- fit(reg_r_wflow, CO2_ex)
reg_v_fit <- fit(reg_v_wflow, CO2_ex)
reg_1d_fit <- fit(reg_1d_wflow, CO2_ex)

reg_mtr <- metric_set(rsq, mae)

# ------------------------------------------------------------------------------
# classification examples

cls_f_wflow <- workflow(Class ~ ., logistic_reg())
cls_v_wflow <-
  workflow() %>%
  add_model(logistic_reg()) %>%
  add_variables(outcomes = Class, predictors = c(everything()))
cls_1d_wflow <- workflow(Class ~ tau, logistic_reg())

if (rlang::is_installed("modeldata")) {
  suppressPackageStartupMessages(library(modeldata))
  data(ad_data, package = "modeldata")

  ad_data_small <-
    ad_data %>%
    dplyr::select(Class, tau, p_tau, VEGF, MMP10, Genotype, male)

  ad_rec <-
    recipe(Class ~ ., data = ad_data_small) %>%
    step_pca(tau, p_tau, VEGF, MMP10, num_comp = 2) %>%
    step_dummy(all_factor_predictors())

  cls_r_wflow <- workflow(ad_rec, logistic_reg())

  cls_f_fit <- fit(cls_f_wflow, ad_data_small)
  cls_r_fit <- fit(cls_r_wflow, ad_data_small)
  cls_v_fit <- fit(cls_v_wflow, ad_data_small)
  cls_1d_fit <- fit(cls_1d_wflow, ad_data_small)
}

cls_mtr <- metric_set(brier_class, kap, mcc)

# ------------------------------------------------------------------------------
# survival examples

if (rlang::is_installed("censored")) {
  suppressPackageStartupMessages(library(censored))
  data(time_to_million, package = "censored")

  time_to_million_small <-
    time_to_million %>%
    dplyr::select(time, event, year, runtime) %>%
    dplyr::mutate(event_time = Surv(time, event), .keep = "unused") %>%
    dplyr::slice(1:150)

  srv_wflow <- workflow(event_time ~ ., survival_reg())
  srv_fit <- fit(srv_wflow, time_to_million_small)
  srv_times <- (1:4) / 4
}

srv_mtr <- metric_set(concordance_survival, roc_auc_survival)
