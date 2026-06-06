test_that("Plot population by category works", {

  pop_plot_regime <- plot_pop_by_category("regime_category")
  pop_plot_gov    <- plot_pop_by_category("gov_type")

  expect_s3_class(pop_plot_regime, "ggplot")
  expect_s3_class(pop_plot_gov, "ggplot")
})
