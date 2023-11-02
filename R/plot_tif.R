#' Plot Test Information Functions
#'
#' Plot the test information functions of the short test form (default), of the full length test or of both versions
#'
#' @param results The object obtained from the stf-generating functions
#' @param tif character, define the TIF to plot, either "stf" (TIF of the STF), "full", (TIF of the full-length test) or "both" (TIF of both STF and full-length test). Default is "stf"
#'
#' @return A ggplot object
#' @export
#' @import ggplot2
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
#' # plot the test information function of the full-length test
#' plot_tif(stf, tif = "full")
#' # plot the test information of the full-length test and of the short test form
#' plot_tif(stf, tif = "both")
plot_tif <- function(results, tif = c("stf", "full", "both")) {
  stf <- data.frame(theta = results$info_stf$theta,
                     info =  results$info_stf$test_info_curve,
                     tif = "short test form")
  full <-  data.frame(theta = results$info_full$theta,
                     info =  results$info_full$test_info_curve,
                     tif = "full-length test")
  both <- rbind(stf, full)
  if (length(tif) > 2) {
    tif <- "stf"
  } else {
    tif <- tif
  }
  if (tif == "stf") {
    data <- stf
  } else if (tif == "full") {
    data <- full
  } else if (tif == "both") {
    data <- both
  }

  graph <- ggplot2::ggplot(data,
                           ggplot2::aes(x = .data$theta,
                                        y = .data$info, group = 1)) +
    ggplot2::geom_line(linewidth = 1.2) + facet_wrap(~.data$tif)
  return(graph)
}
