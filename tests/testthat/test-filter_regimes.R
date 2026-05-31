test_that("filter_regimes works", {

  result_default <- filter_regimes()

  result_single <- filter_regimes("Parliamentary democracy")

  result_vector <- filter_regimes(c("Parliamentary democracy", "Military dictatorship"))

  expect_s3_class(result_default, "data.frame")
  expect_s3_class(result_single,  "data.frame")
  expect_s3_class(result_vector,  "data.frame")

  expect_equal(
    sort(unique(as.character(result_default$regime_category))),
    sort(c("Parliamentary democracy", "Civilian dictatorship", "Presidential democracy",
           "Military dictatorship", "Mixed democratic", "Royal dictatorship"))
  )

  expect_true(all(result_single$regime_category == "Parliamentary democracy"))

  expect_true(all(result_vector$regime_category %in% c("Parliamentary democracy", "Military dictatorship")))

  expect_error(
    filter_regimes("Invalid Regime"),
    "Selected types must be in:"
  )
})
