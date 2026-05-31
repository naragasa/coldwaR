test_that("filter_gov_types works", {

  result_default <- filter_gov_types()

  result_single <- filter_gov_types("Pure Democracy")

  result_vector <- filter_gov_types(c("Pure Democracy", "Communist"))

  expect_s3_class(result_default, "data.frame")
  expect_s3_class(result_single,  "data.frame")
  expect_s3_class(result_vector,  "data.frame")

  expect_equal(
    sort(unique(as.character(result_default$gov_type))),
    sort(c("Pure Democracy", "Pure Monarchy", "Democratic Monarchy", "Communist", "Other"))
  )

  expect_true(all(result_single$gov_type == "Pure Democracy"))

  expect_true(all(result_vector$gov_type %in% c("Pure Democracy", "Communist")))

  expect_error(
    filter_gov_types("Invalid Type"),
    "Selected types must be in:"
  )
})
