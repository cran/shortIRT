#' Equal Interval Procedure
#'
#' Create a Short Test Form (STF) using the \eqn{\theta}-target procedure based on the equal segmentation of the latent trait (Equal Interval Procedure, EIP)
#'
#' @inheritParams bench
#' @param theta_targets vector, define the specific \eqn{\theta} targets for the user defined procedure. Might also be the same \eqn{\theta} target repeated for as many times as the number of items to be included in the short test form
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
#' stf <- eip(data, starting_theta = true_theta, item_par = parameters, num_item = 5)
#' # check the obtained short test form
#' stf$item_stf
#' # check the comparison between the short test form and the full-length test
#' stf$summary
#'
#' # Short test form with cut off values
#' stf_cutoff <- eip(data, starting_theta = true_theta,
#' item_par = parameters, theta_targets = rep(2, 5))
#' stf_cutoff$item_stf
eip <- function(data,
                item_par = NULL,
                seed = 999,
                starting_theta = NULL,
                num_item = NULL,
                theta_targets = NULL) {
  if (is.null(num_item) & is.null(theta_targets)) {
    stop("You must specify a number of items for the STFs or a vector of theta targets!")
  }

  if (!is.null(theta_targets) & length(theta_targets) == 1) {
    stop("Theta targets must be a vector of length > 2")
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
    start_model <- TAM::tam.mml(resp=data, xsi.fixed = b_true,
                               B = a_true, verbose = FALSE)
  }

  item_names <- change_names(data)$item_names
  data <- change_names(data)$data

  lab_item <- 1:ncol(data)
  num_item <- num_item

  if (!is.null(starting_theta)) {
    if (length(starting_theta) != nrow(data)) {
      stop("starting_theta theta must be equal to the number of subjects in the data frame")
    }
  } else {
    starting_theta <- start_model$person$EAP
  }



  info_start <- mean(TAM::IRT.informationCurves(start_model,
                                                theta = starting_theta)$test_info_curve)
  info_full <- TAM::IRT.informationCurves(start_model,
                                          theta = starting_theta)
  if (!is.null(theta_targets)) {
    cut_value <- theta_targets
  } else {
      intervals <- seq(min(starting_theta), max(starting_theta),
                       length =num_item)
      groups <- cut(intervals, num_item, include.lowest = TRUE)
      cut_value<- cut_borders(groups)
    cut_value$mean_theta <- rowMeans(cut_value)
    cut_value <- cut_value$mean_theta
  }


  # Compute IIF for each theta target

  info_test <- NULL
  temp <- list()
  value <- list()
  temp_data <- NULL
  info_data <- NULL


    for(i in 1:length(lab_item)) {
      for(j in 1:length(cut_value)) {

        temp_data <- data.frame(theta_target = TAM::IRT.informationCurves(start_model,
                                                                     theta = cut_value[j],
                                                                     iIndex = lab_item[i])$theta,
                                item_info = mean(colSums(TAM::IRT.informationCurves(start_model,
                                                                               theta = cut_value[j],
                                                                               iIndex = lab_item[i])$info_curves_item)),
                                item = lab_item[i],
                                num_item = paste("STF-", length(cut_value), sep = ""))

        info_data <- rbind(info_data, temp_data)
      }
    }

  # select the item with highest IIF for each theta target
  if (length(unique(cut_value)) > 1) {
    temp_maxinfo <- stats::aggregate(item_info ~ item + theta_target,
                              data = info_data, max)
    temp_maxinfo$stf_length <- unique(info_data$num_item)

    temp <- NULL
    max_temp <- NULL

    for (i in 1:length(unique(temp_maxinfo$theta_target))) {
      temp <- temp_maxinfo[which(temp_maxinfo$item_info == max(temp_maxinfo$item_info)), ]
      temp_maxinfo <- temp_maxinfo[which(temp_maxinfo$item != temp$item &
                                           temp_maxinfo$theta_target != temp$theta_target), ]
      max_temp <-rbind(max_temp, temp)

    }

    } else {
    info_data <- info_data[order(info_data$item_info, decreasing = TRUE),]
    max_temp <- dplyr::distinct(info_data)
    max_temp <- max_temp[1:length(theta_targets), ]
    max_temp$stf_length <- unique(info_data$num_item)
    }
  item_names <- item_names[max_temp$item, ]
  selected_eip <- max_temp
  selected_eip$item <- item_names$old_names
  selected_eip <- selected_eip[order(selected_eip$theta_target), ]

  # given the number(s) of items in num_item, filter out the selected ones from the
  # full-length test, estimate the model on the resulting short form(s), and
  # compute the IIF and TIF

    out_eip <- data[, c(max_temp[max_temp$stf_length %in%unique(max_temp$stf_length),
                                      "item"])]

    model_out_eip <- TAM::tam.mml(out_eip,
                                  xsi.fixed = cbind(1:ncol(out_eip),
                                                    b_true[as.integer(gsub("item", '',
                                                                              colnames(out_eip))), 2]),
                                  B= array(c(rep(0, ncol(out_eip)),
                                             a_true[,2,][as.integer(gsub("item", "",
                                                                             colnames(out_eip)))]),
                                           c(ncol(out_eip),2,1),
                                           dimnames = list(colnames(out_eip),
                                                           c("Cat0", "Cat1"),
                                                           "Dim01")),
                             verbose = FALSE)
    info_out_eip <- TAM::IRT.informationCurves(model_out_eip,
                                               theta = starting_theta)
    #names(info_out_eip)[[i]] <- unique(max_temp$stf_length)[


  # Summary
  info_summary_eip <- NULL

  info_summary_eip <- data.frame(info_test = mean(info_out_eip$test_info_curve),
                                 stf_length = unique(max_temp$stf_length),
                       item = paste(item_names[item_names$new_names %in% colnames(out_eip),
                                               "old_names"], collapse = ", "))

  info_summary_eip <-  rbind(info_summary_eip,
                             data.frame(info_test = (info_start),
                                        stf_length = "all",
                                        item = "all"))
  theta <- data.frame(starting_theta = starting_theta,
                      stf_theta = model_out_eip$person$EAP)
  info_summary_eip$selection <- "EIP"
  eip_results = list(item_stf = selected_eip,
                     summary = info_summary_eip,
                     info_stf = info_out_eip,
                     info_full = info_full,
                     theta = theta)
  return(eip_results)
}
