library(tidymodels)
library(important)

data(ad_data, package = "modeldata")

ad_rec <-
	recipe(Class ~ ., data = ad_data) |>
	step_pca(all_numeric_predictors(), -male, -age, num_comp = 5) |>
	step_dummy(all_factor_predictors()) |>
	step_zv(all_predictors())

ad_wflow <- workflow(ad_rec, logistic_reg())
ad_fit <- fit(ad_wflow, data = ad_data)

###

set.seed(392)
imp_orig <- importance_perm(ad_fit, data = ad_data, type = "original", times = 50)

###

set.seed(392)
imp_derv <- importance_perm(ad_fit, data = ad_data, type = "derived", times = 50)

###

save(imp_orig, imp_derv, file = "inst/imp_examples.RData", compress = TRUE)

# after installed, reference via
# system.file("imp_examples.RData", package = "important")
