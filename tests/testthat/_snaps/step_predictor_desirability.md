# wrong score type

    All score computations failed; skipping feature selection.

# case weights work

    Code
      print(weighted)
    Message
      
      -- Recipe ----------------------------------------------------------------------
      
      -- Inputs 
      Number of variables by role
      outcome:       1
      predictor:    10
      case_weights:  1
      
      -- Training information 
      Training data contained 32 data points and no incomplete rows.
      
      -- Operations 
      * Feature selection via desirability functions (`cor_pearson` and
        `cor_spearman`) on: disp, hp, drat, qsec, vs, am, gear, carb | Trained,
        weighted

# empty printing

    Code
      step_predictor_desirability(rec)
    Condition
      Error in `step_predictor_desirability()`:
      ! argument "score" is missing, with no default

# printing

    Code
      print(rec)
    Message
      
      -- Recipe ----------------------------------------------------------------------
      
      -- Inputs 
      Number of variables by role
      outcome:    1
      predictor: 10
      
      -- Operations 
      * Feature selection via desirability functions (`cor_pearson` and
        `cor_spearman`) on: all_predictors()

---

    Code
      prep(rec)
    Message
      
      -- Recipe ----------------------------------------------------------------------
      
      -- Inputs 
      Number of variables by role
      outcome:    1
      predictor: 10
      
      -- Training information 
      Training data contained 32 data points and no incomplete rows.
      
      -- Operations 
      * Feature selection via desirability functions (`cor_pearson` and
        `cor_spearman`) on: drat, qsec, am, gear, carb | Trained

# bad args

    Code
      prep(step_predictor_desirability(recipe(mpg ~ ., mtcars), all_predictors(),
      score = goals, prop_terms = 2))
    Condition
      Error in `step_predictor_desirability()`:
      Caused by error in `prep()`:
      ! `prop_terms` must be a number between 2.22044604925031e-16 and 1, not the number 2.

