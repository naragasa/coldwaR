test_that("summarize population by category works", {

  result <- summarize_pop_by_category("regime_category")

  expect_s3_class(result, "gt_tbl")

  expect_equal(
    names(result$`_data`),
    c("regime_category", "n", "mean_pop", "med_pop", "sd_pop", "se_pop", "min_pop", "max_pop")
  )

  expect_true(all(diff(result$`_data`$mean_pop) <= 0))

  expect_false(anyNA(result$`_data`$mean_pop))
  expect_false(anyNA(result$`_data`$n))
  })
