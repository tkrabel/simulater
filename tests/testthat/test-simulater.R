context("test-simulater.R")

test_that("Wrong input is handled", {

  # Error
  expect_error(
    simulater(2, 2, 2, 3, fun_weights = 3),
    "must have same length"
  )

  # Warning
  expect_warning(
    simulater(n_obs = 2, n_vars = 3, max_order = 4, n_components = 4,
              fun_prob = 0.5),
    "'fun_prob' > 0 but no functions were passed"
  )
})
