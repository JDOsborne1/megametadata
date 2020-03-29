test_that("Nominal Split works: MMD example", {
        split_data <- metaSplitNominal(tester, "AccountLevel")
        expect_equal(split_data$nominal, "Species")
        expect_equal(split_data$continuous, c("Sepal.Length", "Sepal.Width",  "Petal.Length", "Petal.Width" ))
})
test_that("Nominal Split works: USR example", {
        # split_data <- metaSplitNominal(UltraSpoofR::irisUltra$meta)
        # expect_equal(split_data$nominal, "Species")
        # expect_equal(split_data$continuous, c("Sepal.Length", "Sepal.Width",  "Petal.Length", "Petal.Width" ))
})
test_that("Nominal Split error handling works", {
        expect_error(metaSplitNominal(list()), "Extract Level Required for list dicts")

        expect_error(metaSplitNominal(factor(c(10,2,1))), "No method to support that structure of metadata")
})
