percolate <- function(x) {
  UseMethod("percolate", x)
}

#' check the whole board if there is more possible water flows
#'
#' @param x board obj
#'
#' @return boolean TRUE if there is more possible water flows
#' @export
#'
#' @examples
check_possible <- function(x) {
  n <- attr(x, "n")
  dirs <- list(list(0, -1), list(0, 1), list(1, 0), list(-1, 0))
  for (i in 1:n) {
    for (j in 1:n) {
      if (x[i, j] == 1) {
        for (d in 1:4) {
          d1 <- dirs[[d]][[1]]
          d2 <- dirs[[d]][[2]]
          if ((1<=i+d1 && i+d1<=n && 1<=j+d2 && j+d2<=n) && (x[i+d1, j+d2] == 2)) {
            return (TRUE)
          }
        }
      }
    }
  }
  return (FALSE)
}


#' returns the board with water filled
#'
#' @param x board obj
#'
#' @return board with all possible open squares filled
#' @export
#'
#' @examples
percolate.board <- function(x) {
  is_valid(x)
  n <- attr(x, "n")
  assert_that(all(x == 0 | x == 1), msg = "not valid matrix")
  #fill first row
  for (j in 1:n) {
    if (x[1, j] == 1) {
      x[1, j] = 2
    }
  }
  #fill rest
  dirs <- list(list(0, -1), list(0, 1), list(1, 0), list(-1, 0))
  while (check_possible(x)) {
    for (i in 1:n) {
      for (j in 1:n) {
        if (x[i, j] == 2) {
          for (d in 1:4) {
            d1 <- dirs[[d]][[1]]
            d2 <- dirs[[d]][[2]]
            if ((1<=i+d1 && i+d1<=n && 1<=j+d2 && j+d2<=n) && (x[i+d1, j+d2] == 1)) {
              x[i+d1, j+d2] = 2
            }
          }
        }
      }
    }
  }
  return (list(result_board = x, result = any(x[n,] == 2)))
}
