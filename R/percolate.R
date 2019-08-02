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
#' mat_example_list <- list(matrix(c(1,1,1,1,0,
#'                                  0,0,0,1,0,
#'                                  1,1,1,1,0,
#'                                  0,1,0,0,0,
#'                                  0,1,1,1,1), 5, 5),
#'                        matrix(c(1,1,1,1,0,
#'                                 0,0,0,1,0,
#'                                 0,1,1,1,0,
#'                                 0,1,0,0,0,
#'                                 0,1,1,1,1), 5, 5),
#'                        matrix(c(1,1,1,1,0,
#'                                 0,0,0,1,0,
#'                                 0,1,1,0,0,
#'                                 0,1,0,0,0,
#'                                 0,1,1,1,1), 5, 5))
#'
#'board_example_list <- lapply(mat_example_list, board)

#'percolate_bool <- function(v) {
#'  lst <- percolate(v)
#'  return (lst$result)
#'}
#'lapply(board_example_list, percolate_bool)
#'
#'percolate_board <- function(v) {
#'  lst <- percolate(v)
#'  return (lst$result_board)
#'}
#'board_example_list_after <- lapply(board_example_list, percolate_board)
#'
#'after_plot <- lapply(board_example_list_after, plot)
#'before_plot <- lapply(board_example_list, plot)
#'gg_list <- vector(mode = "list")
#'gg_list <- c(gg_list, before_plot)
#'gg_list <- c(gg_list, after_plot)
#'grid.arrange(grobs = gg_list, nrow = 2)

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
