test_that("Plot GDP by category works", {

  gdp_plot_regime <- plot_gdp_by_category("regime_category")
  gdp_plot_gov    <- plot_gdp_by_category("gov_type")

  expect_s3_class(gdp_plot_regime, "ggplot")
  expect_s3_class(gdp_plot_gov, "ggplot")
})
