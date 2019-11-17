## Script to check the autogeneration elements of the package
test_that("Autogeneration works", {
  test_dict <- metaVariableAppend(iris, list())
  expect_equal(test_dict$Species$data_category, "Category")
  expect_equal(test_dict$Species$class , "factor")
  expect_equal(test_dict$Sepal.Length$class, "numeric")
  expect_equal(test_dict$Petal.Length$name, "Petal.Length")
  expect_equal(test_dict$Petal.Length$data_category, "PII/Value")
})

