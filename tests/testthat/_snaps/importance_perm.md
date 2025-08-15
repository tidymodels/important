# compute metrics - derived predictors, censored regression

    Code
      important:::metric_iter(column = "moash", seed = ex_seed, type = "derived",
        wflow_fitted = srv_fit, dat = derived_predictors, metrics = srv_mtr, size = 20,
        outcome = "event_time", eval_time = srv_times, event_level = "first")
    Condition
      Error in `important:::metric_iter()`:
      ! Column moash was not in the data set. Existing columns are: event_time, year, and runtime.

