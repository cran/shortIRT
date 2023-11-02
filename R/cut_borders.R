#' Cut borders
#'
#' Extract the limits of the intervals obtained from sub setting a vector
#'
#' @param x numeric/integer vector
#'
#' @returns
#'
#' A data frame with two columns. The first column contains the lower bounds of each interval. The second column contains the upper bound of each interval
#' @export
#'
#' @examples
#' x <- seq(-3, 3, length = 5)
#' groups <- cut(x, 5, include.lowest = TRUE)
#' boundaries <- cut_borders(groups)
cut_borders <- function(x){
  pattern <- "(\\(|\\[)(-*[0-9]+\\.*[0-9]*),(-*[0-9]+\\.*[0-9]*)(\\)|\\])"

  start <- as.numeric(gsub(pattern,"\\2", x))
  end <- as.numeric(gsub(pattern,"\\3", x))

  data.frame(start, end)
}
