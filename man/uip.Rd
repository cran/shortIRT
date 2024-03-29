% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/uip.R
\name{uip}
\alias{uip}
\title{Unequal interval procedure}
\usage{
uip(data, item_par = NULL, seed = 999, starting_theta = NULL, num_item = NULL)
}
\arguments{
\item{data}{data.frame, subject \eqn{\times} item matrix containing the accuracy responses}

\item{item_par}{matrix, two-column matrix containing the item parameters. The first column must contain the difficulty parameters \eqn{b_i}, the second column must contain the discrimination parameters \eqn{a_i}.}

\item{seed}{integer, define the random seed. Default is 999}

\item{starting_theta}{vector, define the starting \eqn{\theta} of the subjects.  If empty, the \eqn{\theta} values will be estimated from the data}

\item{num_item}{integer, the number of items to include in the short test form}
}
\value{
A list of length 5:

- item_stf: data.frame, contains the items included in the STF. The number of rows is equal to the number of items included in the STF. The \eqn{\theta}-targets and the item information functions of the optimal item for each \eqn{\theta}-target are reported as well

- summary: data.frame, contains  the list of items included in the STF and the test information on both the full-length test and the STF

- info_stf: list, contains the item information functions of the STF

- info_full: list, contains the item information functions of the full-length test

- theta: data.frame, contains the starting \eqn{\theta} and the \eqn{\theta} estimated with the STF
}
\description{
Create a Short Test Form (STF) using the \eqn{\theta}-target procedure based on the unequal segmentation of the latent trait (Unequal Interval Procedure, EIP)
}
\examples{
# set a seed to replicate the results
set.seed(999)
# Simulate person and item parameters
true_theta <- rnorm(1000)
b <- runif(30, -3, 3)
a <- runif(30, 0.6, 2)
parameters <- data.frame(b, a)
# simulate data
data <- sirt::sim.raschtype(true_theta, b = b, fixed.a = a)
stf_uip = uip(data, starting_theta = true_theta, item_par = parameters, num_item = 10)
# check the obtained short test form
stf_uip$item_stf
# check the comparison between the short test form and the full-length test
stf_uip$summary
}
