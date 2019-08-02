#' plot the board
#'
#' @param x board object
#'
#' @return ggplot of the board with squares 0 = black, 1 = white, 2 = blue
#' @export
#' @import ggplot2
#' @import tidyr
#' @examples plot.board(board(matrix(c(0,1,1,1,0,0,1,1,0,1,0,0,1,0,0,0,0,0,2,2,2,2,2,2,0), 5, 5))
plot.board <- function(x) {
  is_valid(x)
  n <- attr(x, "n")
  df <- tidyr::gather(data.frame(row = 1:n, x),
                      key = "column", value = "value", -row)
  df$column <- as.numeric(substr(df$column, 2, nchar(df$column)))
  df$value <- factor(df$value, levels=c(0, 1, 2))
  ggplot(df, aes(x = column, y = -row)) +
    geom_tile(aes(fill = value)) +
    scale_fill_manual(values = c('0'="black", '1'="white", '2'="lightblue3")) +
    theme(legend.position = "none") +
    theme_void() +
    coord_equal() +
    labs(title = paste("Size:", n))
}
