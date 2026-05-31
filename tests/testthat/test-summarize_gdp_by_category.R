test_that("summarize GDP by category works", {

  result <- summarize_gdp_by_category(regime_category)

  expect_s3_class(result, "gt_tbl")

  expect_equal(
    names(result$`_data`),
    c("regime_category", "n", "mean_GDP", "med_GDP", "sd_GDP", "se_GDP", "min_gdp", "max_gdp")
  )

  expect_true(all(diff(result$`_data`$mean_GDP) <= 0))

  expect_false(anyNA(result$`_data`$mean_GDP))
  expect_false(anyNA(result$`_data`$n))
})
