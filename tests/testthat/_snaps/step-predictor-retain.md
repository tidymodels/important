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
      * Feature selection using `cor_pearson` and `cor_spearman` removing: drat,
        qsec, vs, am, gear, carb | Trained, weighted

# empty printing

    Code
      rec
    Message
      
      -- Recipe ----------------------------------------------------------------------
      
      -- Inputs 
      Number of variables by role
      outcome:    1
      predictor: 10
      
      -- Operations 
      * Feature selection using for: <none>

---

    Code
      rec
    Message
      
      -- Recipe ----------------------------------------------------------------------
      
      -- Inputs 
      Number of variables by role
      outcome:    1
      predictor: 10
      
      -- Training information 
      Training data contained 32 data points and no incomplete rows.
      
      -- Operations 
      * Feature selection using removing: <none> | Trained

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
      * Feature selection using `cor_pearson` for: all_predictors()

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
      * Feature selection using `cor_pearson` removing: drat qsec, ... | Trained

# bad args

    Code
      prep(step_predictor_retain(recipe(mpg ~ ., mtcars), all_predictors(),
      threshold = 2))
    Condition
      Error in `step_predictor_retain()`:
      Caused by error in `prep()`:
      ! The following argument was specified but does not exist: `threshold`.

