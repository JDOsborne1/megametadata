test_that("category extraction works", {
  expect_equal(metaCategoryExtract("Sepal.Length", megametadata::tester, "AccountLevel"), "PII/Value")
})
test_that("name extraction works", {
  expect_equal(metaNameExtract("Sepal.Length", megametadata::tester, "AccountLevel"), "Sepal.Length")
})
test_that("class extraction works", {
  expect_equal(metaClassExtract(3, megametadata::tester, "AccountLevel"), "list")
  expect_equal(metaClassExtract(1, megametadata::tester, "DatasetLevel"), "character")
})
test_that("category extraction error handling works", {
  expect_error(metaCategoryExtract(1, megametadata::tester, "DatasetLevel"), "Not a field element")
})
test_that("name extraction error handling works", {
  expect_error(metaNameExtract(1, megametadata::tester, "DatasetLevel"), "Not a field element")
})
