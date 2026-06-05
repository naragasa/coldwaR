test_that("log_welch_anova() returns a gt table for all valid input combinations", {

  expect_s3_class(log_welch_anova("GDP",        "gov_type"),        "gt_tbl")
  expect_s3_class(log_welch_anova("GDP",        "regime_category"), "gt_tbl")
  expect_s3_class(log_welch_anova("Population", "gov_type"),        "gt_tbl")
  expect_s3_class(log_welch_anova("Population", "regime_category"), "gt_tbl")

})

test_that("log_welch_anova() errors correctly on invalid inputs", {

  # Missing arguments
  expect_error(log_welch_anova(),                          "quant_var` argument is missing")
  expect_error(log_welch_anova("GDP"),                     "`cat_var` argument is missing")

  # Wrong type
  expect_error(log_welch_anova(123,    "gov_type"),        "`quant_var` must be of type character")
  expect_error(log_welch_anova("GDP",  123),               "`cat_var` must be of type character")

  # Invalid string values
  expect_error(log_welch_anova("gdp",  "gov_type"),        "Selected `quant_var` must be one of")
  expect_error(log_welch_anova("GDP",  "government_type"), "Selected `cat_var` must be one of")

})
