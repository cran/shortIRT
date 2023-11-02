#' Difference between \eqn{\theta}s
#'
#' Compute the difference between a starting value of \eqn{\theta} and the \eqn{\theta} estimated with the STF
#'
#' @param results The object obtained from the stf-generating functions
#' @param true_theta vector, optional vector of length equal to the number of rows in the original data frame with the true \eqn{\theta} of the respondents. Default is NULL, such that the \eqn{\theta} estimated with the full-length test will be considered as the starting \eqn{\theta}
#'
#' @returns A data frame with number of rows equal to the number of respondents and 3 columns, one with the starting/true \eqn{\theta}, one with the \eqn{\theta} estimated with the STF, and the difference between the starting/true and the estimated one.
#' @export
#'
#' @examples
#' # set a seed to replicate the results
#' set.seed(999)
#' # Simulate person and item parameters
#' true_theta <- rnorm(1000)
#' b <- runif(30, -3, 3)
#' a <- runif(30, 0.6, 2)
#' parameters <- data.frame(b, a)
#' # simulate data
#' data <- sirt::sim.raschtype(true_theta, b = b, fixed.a = a)
#' stf <- uip(data, starting_theta = true_theta, item_par = parameters, num_item = 5)
#' # without starting theta
#' my_diff <- diff_theta(stf)
#' head(my_diff)
diff_theta <- function(results, true_theta = NULL) {
  difference <- results$theta
  if (is.null(true_theta)) {
    lab <- "starting_theta"
  } else {
    if (length(true_theta) != nrow(difference)) {
      stop("True theta must have the same length as the estimated theta")
    }
    difference$starting_theta <- true_theta
    lab <- "true_theta"
  }
  difference$difference <- difference$starting_theta - difference$stf_theta
  difference$abs_difference <- abs(difference$difference)
  names(difference)[colnames(difference) == "starting_theta"] <- lab
  return(difference)
}