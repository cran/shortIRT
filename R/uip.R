#' Unequal interval procedure
#'
#' Create a Short Test Form (STF) using the \eqn{\theta}-target procedure based on the unequal segmentation of the latent trait (Unequal Interval Procedure, EIP)
#'
#' @inheritParams bench
#'
#'
#' @returns
#' A list of length 5:
#'
#' - item_stf: data.frame, contains the items included in the STF. The number of rows is equal to the number of items included in the STF. The \eqn{\theta}-targets and the item information functions of the optimal item for each \eqn{\theta}-target are reported as well
#'
#' - summary: data.frame, contains  the list of items included in the STF and the test information on both the full-length test and the STF
#'
#' - info_stf: list, contains the item information functions of the STF
#'
#' - info_full: list, contains the item information functions of the full-length test
#'
#' - theta: data.frame, contains the starting \eqn{\theta} and the \eqn{\theta} estimated with the STF
#'
#'
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
#' stf_uip = uip(data, starting_theta = true_theta, item_par = parameters, num_item = 10)
#' # check the obtained short test form
#' stf_uip$item_stf
#' # check the comparison between the short test form and the full-length test
#' stf_uip$summary
uip <- function(data,
                item_par = NULL,
                seed = 999,
                starting_theta = NULL,
                num_item = NULL) {
  if (is.null(num_item)) {
    stop("You must specify the number of items for the STFs!")
  }
  if(is.null(item_par)) {
    start_model <- TAM::tam.mml.2pl(data, verbose = FALSE, irtmodel = "2PL")
    b_true <- matrix(cbind(1:length(start_model$item$xsi.item),
                           start_model$item$xsi.item),
                     ncol = 2)
    a_true <- array(c(rep(0, length(start_model$item$B.Cat1.Dim1)), start_model$item$B.Cat1.Dim1),
                    c(length(start_model$item$B.Cat1.Dim1),2,1),
                    dimnames = list(paste0("I", 1:length(start_model$item$B.Cat1.Dim1)),
                                    c("Cat0", "Cat1"),
                                    "Dim01"))
  } else {
    b_true <- matrix(cbind(1:nrow(item_par),
                           item_par[,1]),
                     ncol = 2)
    a_true <- array(c(rep(0, nrow(item_par)), item_par[,2]),
                    c(nrow(item_par),2,1),
                    dimnames = list(paste0("I", 1:nrow(item_par)),
                                    c("Cat0", "Cat1"),
                                    "Dim01"))
    start_model <- TAM::tam.mml(resp=data, xsi.fixed = b_true, B = a_true, verbose = FALSE)
  }
  item_names <- change_names(data)$item_names
  data <- change_names(data)$dat

  lab_item <- 1:ncol(data)
  if (!is.null(starting_theta)) {
    if (length(starting_theta) != nrow(data)) {
      stop("True theta must be equal to the number of subjects in the data frame")
    }
  } else {
    starting_theta <- start_model$person$EAP
  }
  info_start <- mean(TAM::IRT.informationCurves(start_model,
                                                theta = starting_theta)$test_info_curve)
  info_full <- TAM::IRT.informationCurves(start_model,
                                          theta = starting_theta)
      num_clusters <- num_item
    theta_mat <- matrix(starting_theta, ncol = 1)
    cluster <- stats::kmeans(theta_mat,
                      centers = num_clusters)
    cluster <- cluster$centers[,1]

  cluster_data   <- NULL
  info_data_cluster <- NULL

  value_cluster <- cluster
    for(i in 1:length(lab_item)) {
      for(j in 1:length(value_cluster)) {

        temp_cluster_data   <- data.frame(theta_target = TAM::IRT.informationCurves(start_model,
                                                                               theta = value_cluster[j],
                                                                               iIndex = lab_item[i])$theta,

                                          item_info = colSums(TAM::IRT.informationCurves(start_model,
                                                                                    theta = value_cluster[j],
                                                                                    iIndex = lab_item[i])$info_curves_item),
                                          item = lab_item[i],
                                          num_item = paste("STF-",
                                                           num_item, sep = ""))

        info_data_cluster <- rbind(info_data_cluster, temp_cluster_data  )
      }
    }


    temp_data_cluster <- NULL
    temp_maxcluster <- NULL
    temp <- NULL
    max_temp_cluster <- NULL

    temp_maxcluster <- stats::aggregate(item_info ~ item + theta_target,
                                 data = info_data_cluster, max)
    temp_maxcluster$stf_length <- unique(info_data_cluster$num_item)
    for (i in 1:length(unique(temp_maxcluster$theta_target))) {
      temp <- temp_maxcluster[which(temp_maxcluster$item_info == max(temp_maxcluster$item_info)), ]
      temp_maxcluster <- temp_maxcluster[which(temp_maxcluster$item != temp$item &
                                                 temp_maxcluster$theta_target != temp$theta_target), ]
      max_temp_cluster <-rbind(max_temp_cluster, temp)
    }

    item_names <- item_names[max_temp_cluster$item, ]
    selected_uip <- max_temp_cluster
    selected_uip$item <- item_names$old_names
    selected_uip <- selected_uip[order(selected_uip$theta_target), ]


  # given the number(s) of items in num_item, filter out the selected ones from the
  # full-length test, estimate the model on the resulting short form(s), and
  # compute the IIF and TIF

    out_cluster <- data[, c(max_temp_cluster[max_temp_cluster$stf_length %in% unique(max_temp_cluster$stf_length),
                                                  "item"])]
    model_out_cluster <- TAM::tam.mml(out_cluster,
                                      xsi.fixed = cbind(1:ncol(out_cluster),
                                                        b_true[as.integer(gsub("item", '',
                                                                               colnames(out_cluster))), 2]),
                                      B= array(c(rep(0, ncol(out_cluster)),
                                                 a_true[,2,][as.integer(gsub("item", "",
                                                                             colnames(out_cluster)))]),
                                               c(ncol(out_cluster),2,1),
                                               dimnames = list(colnames(out_cluster),
                                                               c("Cat0", "Cat1"),
                                                               "Dim01")),
                                 verbose = FALSE)
    info_out_cluster <- TAM::IRT.informationCurves(model_out_cluster,
                                                   theta = starting_theta)

  # summary

    info_summary_cluster <- data.frame(info_test = mean(info_out_cluster$test_info_curve),
                       stf_length = unique(max_temp_cluster$stf_length),
                       item = paste(item_names[item_names$new_names %in% colnames(out_cluster),
                                               "old_names"], collapse = ", "))



  info_summary_cluster <-  rbind(info_summary_cluster,
                                 data.frame(info_test = info_start,
                                            stf_length = "all",
                                            item = "all"))
  info_summary_cluster$selection <- "UIP"
  theta <- data.frame(starting_theta = starting_theta,
                      stf_theta = model_out_cluster$person$EAP)
  uip_results = list(item_stf = selected_uip,
                     summary = info_summary_cluster,
                     info_stf = info_out_cluster,
                     info_full = info_full,
                     theta = theta)
  return(uip_results)
}
