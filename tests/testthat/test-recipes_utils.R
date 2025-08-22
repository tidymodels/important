test_that("Find score objects in an expression or quosure", {

	obj_1 <- POTATO(c(minimize(score_a), constrain(score_b)))
	res_1 <- important:::extract_score_names(obj_1)
	expect_equal(res_1, c("score_a", "score_b"))
	expect_equal(
		obj_1 |> rlang::quo_get_expr() |> important:::extract_score_names(),
		c("score_a", "score_b")
	)

	obj_2 <- POTATO(c(minimize(score_a), constrain(score_a)))
	res_2 <- important:::extract_score_names(obj_2)
	expect_equal(res_2, c("score_a"))
	expect_equal(
		obj_2 |> rlang::quo_get_expr() |> important:::extract_score_names(),
		c("score_a")
	)

	obj_3 <- POTATO(score_a < 2 & score_b > 3)
	res_3 <- important:::extract_score_names(obj_3)
	expect_equal(res_3, c("score_a", "score_b"))
	expect_equal(
		obj_3 |> rlang::quo_get_expr() |> important:::extract_score_names(),
		c("score_a", "score_b")
	)

	obj_4 <- POTATO(a < 2 & b > 3)
	expect_snapshot(important:::extract_score_names(obj_4), error = TRUE)

	obj_5 <- POTATO()
	expect_snapshot(important:::extract_score_names(obj_5), error = TRUE)

	obj_6 <- POTATO(I(score_a < 2) & predictor == "another potato")
	res_6 <- important:::extract_score_names(obj_6)
	expect_equal(res_6, c("score_a"))
	expect_equal(
		obj_6 |> rlang::quo_get_expr() |> important:::extract_score_names(),
		c("score_a")
	)

	obj_7 <- POTATO(score_a / c < 2)
	res_7 <- important:::extract_score_names(obj_7)
	expect_equal(res_7, c("score_a"))
	expect_equal(
		obj_7 |> rlang::quo_get_expr() |> important:::extract_score_names(),
		c("score_a")
	)

})
