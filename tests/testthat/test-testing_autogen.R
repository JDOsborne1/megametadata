## Script to check the autogeneration elements of the package
test_that("Autogeneration works", {
  test_dict <- metaVariableAppend(iris, list())
  expect_equal(test_dict$Species$data_category, "Category")
  expect_equal(test_dict$Species$class , "factor")
  expect_equal(test_dict$Sepal.Length$class, "numeric")
  expect_equal(test_dict$Petal.Length$name, "Petal.Length")
  expect_equal(test_dict$Petal.Length$data_category, "PII/Value")
})

test_that("Auto Expansion by specification works", {
        new_dict <- metaUpdateDictWithSpec(tester, spec, iris, "AccountLevel")
  expect_equal(new_dict$AccountLevel$Species$levels, c("setosa", "versicolor", "virginica"))
  expect_equal(new_dict$AccountLevel$Species$ordered , FALSE)
  expect_equal(new_dict$AccountLevel$Sepal.Width$minimum , 2)
  expect_equal(new_dict$AccountLevel$Sepal.Width$uniqueness , uniqueness(iris$Sepal.Width), tolerance = 0.0002)
  expect_equal(new_dict$AccountLevel$Petal.Length$range , utilRange(iris$Petal.Length))

})

