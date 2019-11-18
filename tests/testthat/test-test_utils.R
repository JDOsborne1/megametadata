test_that("normalisation works", {
        expect_equal(normalise(c(0,1, 2, 4, 1)), c(0.00, 0.25, 0.50, 1.00, 0.25))
        expect_equal(uniqueness(c(2,1,2,2)), 0.25)
        expect_equal(uniqueness(c(1,1,1)), 0)
})

test_that("char Length Works", {
        expect_equal(constCharLength(c("aaa", "aa", "aaaa")), FALSE)
        expect_equal(constCharLength(c("aaa", "aaa", "aaa")), TRUE)
})

test_that("form checkers work", {
        expect_equal(postForm("BA1 8AZ"), TRUE)
        expect_equal(dateForm("2019-01-01"), TRUE)
})
