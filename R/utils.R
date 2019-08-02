#' generates a matrix of 0 and 1
#'
#' @param n integer, the dimension of the square matrix
#' @param p a number between 0, 1, it's the probability to have 0's
#'
#' @return board with n rows and n columns and floor(p*n^2) 0's, the rest are 1's
#' @export
#'
#' @examples generate_board_mat(n = 5, p = 0.25)
generate_board_mat <- function(n = 5, p = 0.25) {
  assert_that(length(n) == 1 && is.numeric(n)
              && n > 0 && n %% 1 == 0,
              msg = "n is not a positive integer")
  assert_that(p >= 0 && p <= 1,
              msg = "p is not between 0 and 1")
  board <- matrix(1, n, n)
  pos <- sample(1:n^2, floor(p*n^2), replace=FALSE)
  board[pos] <- 0
  board
}

#' test whether a matrix a valid
#'
#' @param mat matrix
#'
#' @return boolean values. True if matrix is square and only contains 0, 1, 2
#' @export
#'
#' @examples is_valid(matrix(0, 1, 2))
is_valid <- function(mat) {
  assert_that(is.matrix(mat) && nrow(mat) == ncol(mat) && all(mat == 1 | mat == 0 | mat == 2),
              msg = "not valid matrix")
  return (TRUE)
}

#' transforms a string of * and . to 0 and 1
#'
#' @param s string, containing * and .
#'
#' @return vector of 0 and 1
#' @export
#'
#' @examples transform_one("***.")
transform_one <- function(s) {
  res_vec <- vector()
  for (i in 1:nchar(s)) {
    if (substr(s, i, i) == "*") {
      res_vec <- c(res_vec, 0)
    }
    else if (substr(s, i, i) == ".") {
      res_vec <- c(res_vec, 1)
    }
  }
  res_vec
}

#' transform a vector of strings containing * and .
#'
#' @param v vector, of strings of * and .
#' @param d integer, dimension of the matrix
#'
#' @return board obj, of the matrix containing only 0 and 1
#' @export
#'
#' @examples transform_vec <- function(c("."), 1)
transform_vec <- function(v, d) {
  res_vec <- vector()
  for (i in 1:length(v)) {
    vec <- transform_one(v[i])
    res_vec <- c(res_vec, vec)
  }
  mat <- matrix(res_vec, d, d, byrow = TRUE)
  board(mat)
}

#' check if a string that only contains * and . and space
#'
#' @param v string, containing only * and . and space
#'
#' @return boolean TRUE it only contains * and . and space
#' @export
#'
#' @examples check_contain("***.")
check_contain <- function(v) {
  for (i in 1:nchar(v)) {
    if (substr(v, i, i) != "*" && substr(v, i, i) != "." && substr(v, i, i) != " ") {
      return (FALSE)
    }
  }
  return (TRUE)
}

#' read in a file of matrices
#'
#' @param file link
#'
#' @return list of transformed board objs
#' @export
#'
#' @examples read_boards("https://raw.githubusercontent.com/benjaminleroy/36-350-summer-data/master/Week5/percolation_write_example.txt")
read_boards <- function(file) {
  txt <- readLines(file)
  txt <- txt[txt != ""]
  txt <- trimws(txt)

  res_lst <- vector(mode = "list")
  bool <- FALSE
  for (i in 1:length(txt)) {
    if (txt[i] == "----" && i != length(txt)) {
      if (!is.na(as.numeric(txt[i+1])) && as.numeric(txt[i+1]) > 0) {
        dim <- as.numeric(txt[i+1])
        for (j in ((i+2):(i+1+dim))) {
          if ((i+2+dim > length(txt)) | (txt[i+2+dim] != "----") |
              (nchar(txt[j]) != (2*dim-1)) |
              (check_contain(txt[j]) == FALSE)) {
            bool <- TRUE
          }
        }
        if (bool == FALSE) {
          ve <- txt[(i+2):(i+1+dim)]
          #print(ve)
          lst <- transform_vec(ve, dim)
          #print(lst)
          res_lst <- c(res_lst, list(lst))
        }
        else {
          res_lst <- c(res_lst, list(NA))
        }
      }
      else {
        res_lst <- c(res_lst, list(NA))
      }
    }
  }
  assert_that((sum(txt == "----") - 1) == length(res_lst),
              msg = "file is not properly formatted")
  res_lst
}
