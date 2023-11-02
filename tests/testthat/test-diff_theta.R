test_that("difference computes the difference between theta and estimated theta", {
  # Simulate person and item parameters
  starting_theta = rnorm(1000)
  b <- runif(100, -3, 3)
  a <- runif(100, 0.6, 2)
  parameters <- data.frame(b, a)
  # simulate data
  data <- sirt::sim.raschtype(starting_theta, b = b, fixed.a = a)
  stf <- uip(data, starting_theta = starting_theta, item_par = parameters, num_item = 5)
  my_diff <- diff_theta(stf)
  expect_equal(ncol(my_diff), 4)
  expect_equal(my_diff$difference, my_diff$starting_theta - my_diff$stf_theta)
})
