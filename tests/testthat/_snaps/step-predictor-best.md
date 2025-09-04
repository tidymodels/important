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
      * Feature selection on: <none>

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
      * Feature selection on: <none> | Trained

# printing

    Code
      print(rec)
    Message
      
      -- Recipe ----------------------------------------------------------------------
      
      -- Inputs 
      Number of variables by role
      predictor: 11
      
      -- Operations 
      * Feature selection on: all_predictors()

---

    Code
      prep(rec)
    Condition
      Error in `step_predictor_best()`:
      Caused by error in `pull_outcome_column_name()`:
      ! One column should have a role of "outcome".

# bad args

    Code
      prep(step_predictor_best(recipe(mpg ~ ., mtcars), all_predictors(), prop_terms = 2))
    Condition
      Error in `step_predictor_best()`:
      Caused by error in `prep()`:
      ! `prop_terms` must be a number between 2.22044604925031e-16 and 1, not the number 2.

