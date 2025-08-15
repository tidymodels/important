test_that("no parallelism", {
	skip_if_not_installed("mirai")

	# ---------------------------------------------------------------------------
	# default

	expect_equal(important:::choose_framework(), "sequential")
	expect_snapshot(important:::choose_framework(verbose = TRUE))

})

test_that("enable future parallelism", {
	skip_if_not_installed("future")
	skip_if_not_installed("mirai")

	library(future)

	# ----------------------------------------------------------------------------
	# sequential; not enough workers

	expect_equal(important:::choose_framework(), "sequential")
	expect_snapshot(important:::choose_framework(verbose = TRUE))

	# ----------------------------------------------------------------------------
	# parallel

	plan(multisession(workers = 2))
	expect_equal(important:::choose_framework(), "future")
	expect_snapshot(important:::choose_framework(verbose = TRUE))

	# ----------------------------------------------------------------------------
	# sequential due to restrictions

	plan(multisession(workers = 1))

	expect_equal(important:::choose_framework(), "sequential")
	expect_snapshot(important:::choose_framework(verbose = TRUE))

	plan(sequential)
	detach("package:future", character.only = TRUE)

	expect_equal(important:::choose_framework(), "sequential")
	expect_snapshot(important:::choose_framework(verbose = TRUE))
})

test_that("enable mirai parallelism", {
	skip_if_not_installed("mirai", minimum_version = "2.4.0")

	library(mirai)

	# ----------------------------------------------------------------------------
	# sequential; not enough workers

	expect_equal(important:::choose_framework(), "sequential")
	expect_snapshot(important:::choose_framework(verbose = TRUE))

	# ----------------------------------------------------------------------------
	# parallel

	daemons(2)

	expect_equal(important:::choose_framework(), "mirai")
	expect_snapshot(important:::choose_framework(verbose = TRUE))

	# ----------------------------------------------------------------------------
	# sequential due to restrictions

	daemons(0)
	daemons(1)

	expect_equal(important:::choose_framework(), "sequential")
	expect_snapshot(important:::choose_framework(verbose = TRUE))

	# teardown
	daemons(0)
	## Give daemons a chance to shutdown
	Sys.sleep(1)

	expect_equal(important:::choose_framework(), "sequential")
	expect_snapshot(important:::choose_framework(verbose = TRUE))
})

test_that("break parallelism tie", {
	skip_if_not_installed("mirai", minimum_version = "2.4.0")
	skip_if_not_installed("future")
	skip_on_cran()

	library(mirai)
	daemons(2)

	library(future)
	plan(multisession(workers = 2))

	expect_equal(important:::choose_framework(), "mirai")
	expect_snapshot(important:::choose_framework(verbose = TRUE))

	# teardown
	daemons(0)
	## Give daemons a chance to shutdown
	Sys.sleep(1)

	plan(sequential)
})


test_that("same results using mirai", {
	skip_if_not_installed("mirai")
	skip_if_not_installed("modeldata")
	skip_on_cran()

	set.seed(1)
	dat <- modeldata::sim_regression(500)

	lr_fit <-
		workflows::workflow(outcome ~ ., parsnip::linear_reg()) |>
		workflows:::fit.workflow(data = dat)

	set.seed(1)
	seq_res <- importance_perm(lr_fit, dat, times = 3)

	tmp <- mirai::daemons(2)

	set.seed(1)
	mirai_res <- importance_perm(lr_fit, dat, times = 3)

	expect_equal(seq_res, mirai_res)

	tmp <- mirai::daemons(0)
})

test_that("same results using future", {
	skip_if_not_installed("future")
	skip_if_not_installed("future.apply")
	skip_if_not_installed("modeldata")
	skip_on_cran()

	set.seed(1)
	dat <- modeldata::sim_regression(500)

	lr_fit <-
		workflows::workflow(outcome ~ ., parsnip::linear_reg()) |>
		workflows:::fit.workflow(data = dat)

	set.seed(1)
	seq_res <- importance_perm(lr_fit, dat, times = 3)

	future::plan(future::multisession(workers = 2))

	set.seed(1)
	future_res <- importance_perm(lr_fit, dat, times = 3)

	expect_equal(seq_res, future_res)

	future::plan("sequential")
})
