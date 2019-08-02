#' Creating board object
#'
#' @param mat matrix, default to be null, if not null, mat will be used for board
#' @param n integer, dimension of the board
#' @param p number between 0 and 1, probability of having 0's
#'
#' @return board object
#' @export
#'
#' @examples board(n = 5, p = .25)
board <- function(mat = NULL, n = 5, p = .25) {
  if (!is.null(mat)) {
    is_valid(mat)
    object <- mat
    attr(object, "n") <- nrow(mat)
    attr(object, "p") <- sum(mat == 0)/(nrow(mat)^2)
  }
  else {
    mat1 <- generate_board_mat(n, p)
    object <- mat1
    attr(object, "n") <- n
    attr(object, "p") <- p
  }
  class(object) <- c("board", "matrix")
  object
}
