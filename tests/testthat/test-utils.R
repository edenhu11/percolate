context("test utils")
#(1)
test_that("generate_board_mat() works", {
  res <- generate_board_mat()
  expect_true(nrow(res) == 5 && all(res == 0 | res == 1))
})
#(2)
test_that("generate_board_mat() works", {
  res <- generate_board_mat(n = 3)
  expect_true(nrow(res) == 3 && all(res == 0 | res == 1))
})
#(3)
test_that("generate_board_mat() works", {
  res <- generate_board_mat(p = 0)
  expect_true(nrow(res) == 5 && all(res == 1))
})
#(4)
test_that("generate_board_mat() works", {
  res <- generate_board_mat(p = 1)
  expect_true(nrow(res) == 5 && all(res == 0))
})
#(5)
test_that("generate_board_mat() errors for invalid n", {
  expect_error(generate_board_mat(n = c(1,2)))
  expect_error(generate_board_mat(n = "asdf"))
  expect_error(generate_board_mat(n = 5.4))
  expect_error(generate_board_mat(n = -5))
})

test_that("is_valid() errors for non-square matrix", {
  expect_error(is_valid(matrix(0, 1, 2)))
})

test_that("is_valid() errors for containing numbers other 0, 1, 2", {
  mat1 <- matrix(0, 3, 3)
  mat1[4] <- 3
  expect_error(is_valid(mat1))
})

test_that("is_valid() errors for containing strings", {
  mat1 <- matrix("a", 3, 3)
  expect_error(is_valid(mat1))
})

test_that("read_boards() works", {
  load(url("https://raw.githubusercontent.com/benjaminleroy/36-350-summer-data/master/Week5/percolate_test.Rdata"))
  lst_boards <- read_boards("https://raw.githubusercontent.com/benjaminleroy/36-350-summer-data/master/Week5/percolation_write_test.txt")
  expect_identical(board_list, lst_boards)
})

test_that("read_boards() works", {
  lst_boards <- read_boards("https://raw.githubusercontent.com/benjaminleroy/36-350-summer-data/master/Week5/percolation_write_test1.txt")
  expect_equal(list(NA), lst_boards)
})

test_that("read_boards() works", {
  lst_boards <- read_boards("https://raw.githubusercontent.com/benjaminleroy/36-350-summer-data/master/Week5/percolation_write_test2.txt")
  expect_equal(list(NA), lst_boards)
})

test_that("read_boards() works", {
  expect_warning(lst_boards <- read_boards("https://raw.githubusercontent.com/benjaminleroy/36-350-summer-data/master/Week5/percolation_write_test3.txt"))
  expect_equal(list(NA), lst_boards)
})

test_that("read_boards() works", {
  lst_boards <- read_boards("https://raw.githubusercontent.com/benjaminleroy/36-350-summer-data/master/Week5/percolation_write_test4.txt")
  expect_equal(list(NA), lst_boards)
})

test_that("read_boards() works", {
  lst_boards <- read_boards("https://raw.githubusercontent.com/benjaminleroy/36-350-summer-data/master/Week5/percolation_write_test5.txt")
  expect_equal(list(NA), lst_boards)
})

test_that("read_boards() works", {
  lst_boards <- read_boards("https://raw.githubusercontent.com/benjaminleroy/36-350-summer-data/master/Week5/percolation_write_test6.txt")
  expect_equal(list(NA), lst_boards)
})
