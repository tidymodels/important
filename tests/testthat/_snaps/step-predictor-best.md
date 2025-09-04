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
      * Feature selection via `cor_pearson` on: drat qsec, ... | Trained, weighted

# missing score arg

    Code
      step_predictor_best(recipe(mpg ~ ., data = mtcars), all_predictors(),
      prop_terms = 1 / 2)
    Condition
      Error in `step_predictor_best()`:
      ! argument "score" is missing, with no default

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
      * Feature selection via `cor_pearson` on: <none>

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
      * Feature selection via `cor_pearson` on: <none> | Trained

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
      * Feature selection via `cor_pearson` on: all_predictors()

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
      * Feature selection via `cor_pearson` on: qsec, vs, am, gear, carb | Trained

# bad args

    Code
      prep(step_predictor_best(recipe(mpg ~ ., mtcars), all_predictors(), prop_terms = 2,
      score = "cor_pearson"))
    Condition
      Error in `step_predictor_best()`:
      Caused by error in `prep()`:
      ! `prop_terms` must be a number between 2.22044604925031e-16 and 1, not the number 2.

