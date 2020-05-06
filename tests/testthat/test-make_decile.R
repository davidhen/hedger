context("make_decile")

testthat::test_that("errors", {
  testthat::expect_error(
    make_decile(c(-1, 1, 2, 3, 4, 5, 6, 7),
                "There are values outside the possible number of Scottish datazones")
  )

  testthat::expect_error(
    make_decile(c(1, 2, 3, 4, 5, 6, 7, 6977),
                "There are values outside the possible number of Scottish datazones")
  )

  testthat::expect_error(
    make_decile(c("1", "2", "3", "4"),
                "The rank variable must be of type double, numerical or integer")
  )

  testthat::expect_error(
    make_decile(c(TRUE, TRUE, FALSE, TRUE, FALSE, FALSE),
                "The rank variable must be of type double, numerical or integer")
  )
})

