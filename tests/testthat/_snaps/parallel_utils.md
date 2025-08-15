# no parallelism

    Code
      important:::choose_framework(verbose = TRUE)
    Message
      mirai is not active.
      future is active with 1 worker.
      Too few workers for parallel processing.
    Output
      [1] "sequential"

# enable future parallelism

    Code
      important:::choose_framework(verbose = TRUE)
    Message
      mirai is not active.
      future is active with 1 worker.
      Too few workers for parallel processing.
    Output
      [1] "sequential"

---

    Code
      important:::choose_framework(verbose = TRUE)
    Message
      mirai is not active.
      future is active with 2 workers.
      future will be used for parallel processing}.
    Output
      [1] "future"

---

    Code
      important:::choose_framework(verbose = TRUE)
    Message
      mirai is not active.
      future is active with 1 worker.
      Too few workers for parallel processing.
    Output
      [1] "sequential"

---

    Code
      important:::choose_framework(verbose = TRUE)
    Message
      mirai is not active.
      future is active with 1 worker.
      Too few workers for parallel processing.
    Output
      [1] "sequential"

# enable mirai parallelism

    Code
      important:::choose_framework(verbose = TRUE)
    Message
      mirai is not active.
      future is active with 1 worker.
      Too few workers for parallel processing.
    Output
      [1] "sequential"

---

    Code
      important:::choose_framework(verbose = TRUE)
    Message
      mirai is active with 2 workers.
      future is active with 1 worker.
      mirai will be used for parallel processing}.
    Output
      [1] "mirai"

---

    Code
      important:::choose_framework(verbose = TRUE)
    Message
      mirai is active with 1 worker.
      future is active with 1 worker.
      Too few workers for parallel processing.
    Output
      [1] "sequential"

---

    Code
      important:::choose_framework(verbose = TRUE)
    Message
      mirai is not active.
      future is active with 1 worker.
      Too few workers for parallel processing.
    Output
      [1] "sequential"

# break parallelism tie

    Code
      important:::choose_framework(verbose = TRUE)
    Message
      mirai is active with 2 workers.
      future is active with 2 workers.
      Multiple workers exist for both mirai and future; falling back to the default of mirai.
    Output
      [1] "mirai"

