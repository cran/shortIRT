#' Plot the difference between \eqn{\theta}s
#'
#' Plot the difference or the absolute difference between the starting \eqn{\theta} and the \eqn{\theta} estimated with the STF as a function of different levels of the latent trait
#'
#' @param difference data.frame, data frame obtained with the function [diff_theta()]
#' @param type character, type of difference, either as is ("diff") or absolute ("absolute_diff"). Default is "diff".
#' @param levels integer, number of levels of the starting \eqn{\theta} Default is 4
#'
#' @importFrom dplyr %>%
#'
#' @return A ggplot object
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
#' # compute the difference between starting theta and that estimated with the stf
#' my_diff <- diff_theta(stf)
#' # plot the difference with default number of levels
#' plot_difference(my_diff, type = "diff")
#' # plot the absolute difference with 10 levels
#' plot_difference(my_diff, type = "absolute_diff", levels = 10)
plot_difference <- function(difference,
                            type = c("diff", "absolute_diff"),
                            levels = 4) {
  difference$levels <- ggplot2::cut_number(difference[,
                                                         grepl("starting|true",
                                                               colnames(difference))],
                                              levels)
  mean_diff <- difference %>%
    dplyr::group_by(levels) %>%
    dplyr::summarise(mean = mean(.data$difference),
                     mean_abs = mean(.data$abs_difference))
  if (length(type) > 1) {
    type <- "diff"
  } else {
    type <- type
  }
  if (type == "diff") {
    graph <- ggplot2::ggplot(mean_diff,
                    aes( x = .data$levels, y = .data$mean, group = 1)) +
      geom_line(linewidth = 1.2)
    min_y <- -abs(min(difference[,
                                 grepl("starting|true",
                                       colnames(difference))]))-1
    ylab <- "Difference"
    a <- 0.00
  } else if (type == "absolute_diff") {
    graph <- ggplot2::ggplot(mean_diff,
                             ggplot2::aes( x = .data$levels,
                                           y = .data$mean_abs, group = 1)) + ggplot2::geom_line(linewidth = 1.2)
    min_y <- 0
    ylab <- "Absolute difference"
    a <- 1.00
  }
  graph <- graph + ggplot2::ylab(type) + ggplot2::ylim(min_y,
                                     abs(min(difference[,
                                                        grepl("starting|true",
                                                              colnames(difference))]))+1) +
    ggplot2::ylab(ylab) +
    ggplot2::geom_hline(yintercept = a, linetype = 2,
                        color = "grey", linewidth = 1.2)

  return(graph)
}
