test_that("S3 Dictionary Generic Works", {
        test_dict <- metaDictionary.default(the_data = iris)
        test_dict2 <- metaDictionary(iris)
        expect_equal(test_dict, test_dict2)

        expect_equal(test_dict$Sepal.Width$class, "numeric")

        expect_equal(metaDictionary(list()), "no method defined yet for lists")
})
