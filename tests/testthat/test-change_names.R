test_that("change_names actual changes names", {
  data <- data.frame(matrix(1:20, nrow = 4, ncol = 5))
  new_data <- change_names(data)
  expect_true(all(colnames(data) == new_data$item_names$old_names))
})
