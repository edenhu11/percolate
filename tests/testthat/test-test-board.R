context("test board fn")

test_that("board() works", {
  mat1 <- matrix(0, 5, 5)
  mat2 <- board(mat1)
  expect_equivalent(mat1, unclass(mat2) & attr(mat2, "n") == 5 & attr(mat2, "p") == 1)
})

test_that("board() errors for non-square matrix", {
  expect_error(board(matrix(0, 1, 2)))
})

test_that("board() errors for containing numbers other 0, 1, 2", {
  mat1 <- matrix(0, 3, 3)
  mat1[4] <- 3
  expect_error(board(mat1))
})

test_that("board() errors for non-matrix", {
  expect_error(board(c(0, 0)))
})
