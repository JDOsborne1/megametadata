test_that("nominal split works", {
        split <- metaSplitNominal(megametadata::tester, "AccountLevel")
  expect_equal(split$nominal, "Species")
  expect_equal(split$continuous, c("Sepal.Length", "Sepal.Width",  "Petal.Length", "Petal.Width" ))
})
