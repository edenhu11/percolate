context("test percolate")

#(1)
test_that("percolate() works", {
  my_board <- board(matrix(1, 10, 10))
  lst <- percolate(my_board)
  expect_true(lst$result_board == board(matrix(2, 10, 10)) && lst$result == TRUE)
})

#(2)
test_that("percolate() works", {
  my_board <- board(matrix(0, 10, 10))
  lst <- percolate(my_board)
  expect_true(lst$result_board == board(matrix(0, 10, 10)) && lst$result == FALSE)
})

#(3)
test_that("percolate() works", {
  a <- rep(c(0), length.out = 10)
  b <- rep(c(1), length.out = 90)
  a <- c(a, b)
  my_board <- board(matrix(a, 10, 10, byrow = TRUE))
  lst <- percolate(my_board)
  expect_true(lst$result_board == my_board && lst$result == FALSE)
})

#(4)
test_that("percolate() works", {
  a <- rep(c(1), length.out = 90)
  b <- rep(c(0), length.out = 10)
  a <- c(a, b)
  my_board <- board(matrix(a, 10, 10, byrow = TRUE))
  lst <- percolate(my_board)
  c <- rep(c(2), length.out = 90)
  c <- c(c, b)
  other_board <- board(matrix(c, 10, 10, byrow = TRUE))
  expect_true(lst$result_board == other_board && lst$result == FALSE)
})

test_that("percolate.board() works with all the test cases",{
  load(url("https://raw.githubusercontent.com/benjaminleroy/36-350-summer-data/master/Week5/percolate_test.Rdata"))

  your_result_list <- lapply(board_list, percolate)

  bool_vec <- sapply(1:length(result_list), function(x){
    your_board <- your_result_list[[x]]$result_board
    result_board <- result_list[[x]]$result_board

    identical(your_board, result_board) *
      (your_result_list[[x]]$result == result_list[[x]]$result)
  })

  expect_true(all(bool_vec))
})
