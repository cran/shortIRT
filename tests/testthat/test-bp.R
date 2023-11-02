test_that("Error when num_item is NULL", {
  true_theta = rnorm(100)
  b <- runif(100, -3, 3)
  a <- runif(100, 0.6, 2)
  parameters <- data.frame(b, a)
  data <- sirt::sim.raschtype(true_theta, b = b, fixed.a = a)
  expect_error(bp(data, starting_theta = true_theta, item_par = parameters),
               "You must specify the number of items for the STFs!")
})
