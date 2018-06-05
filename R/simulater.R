#' Function to create data from arbitrary non-linear DGP
#'
#' @param n_obs number of observations that shall be created
#' @param n_vars number of inputs of the DGP
#' @param n_noise number of noise variables
#' @param n_components number of components in the DGP formula
#' @param max_order the maximum interaction order in the DGP
#' @param stn signal-to-noise ratio
#' @param funs list containing functions that will at random be applied to some inputs in the formula
#' @param fun_weights vector specifying the relative frequency with which a function shall be chosen among the specified functions (doesn't have to add up to one)
#' @param fun_prob the probability that functional transformation will be applied to an input in the formula
#'
#' @return returns a DGP object that contains
#' - a string describing the DGP formula
#' - a data.frame containing the data generated from the DGP with n_obs observations
#'
#' @export
#'
#' @importFrom magrittr %>%, set_colnames
#' @importFrom purrr map, map2
#' @examples
simulater <- function(n_obs,
                      n_vars,
                      n_noise,
                      n_components,
                      max_order,
                      stn,
                      funs,
                      fun_weights,
                      fun_prob) {

  # args
  # Libs
  library(magrittr)
  library(dplyr)
  library(purrr)
  funs <- list(pow2 = function(x) return(x^2),
               pow3 = function(x) return(x^3),
               sin = sin,
               exp = exp)
  fun_weights <- c(5, 5, 1, 2)
  n_obs <- 4e5
  n_vars <- 3
  n_noise <- 10
  n_components <- 5
  max_order <- 4
  stn <- 1
  fun_prob <- 0.5
  #

  # Features
  x_cols <- sprintf("x%s", 1:n_vars)
  X <- matrix(rnorm(n_obs * n_vars, 0, 1), nrow = n_obs, ncol = n_vars) %>%
    set_colnames(x_cols)
  feature_names <- names(funs) %>%
    map(., function(fun_name) { sprintf("funs$%s(%s)", fun_name, x_cols) }) %>%
    do.call(c, .)
  feature_names <- c(x_cols, feature_names)
  n_features    <- length(feature_names)

  # Compute the weights of identity functions to have 50% change of being drawn
  iweight <- (1 - fun_prob) / fun_prob * sum(fun_weights)
  pweight <- c(iweight, fun_weights) %>%
    rep(., each = n_vars)

  # Extract the number of draws per formula component
  orders <- c(max_order, sample(seq_len(max_order), n_components - 1, TRUE))
  coefs  <- runif(n_components, -1, 1) %>% round(., 2)
  draws  <- map2(
    .x = orders,
    .y = seq_len(length(orders)),
    function(order, i) {
      draw <- if (i <= n_vars) {
        c(i, sample(seq_len(n_features)[-i], order - 1, prob = pweight[-i]))
      } else {
        sample(seq_len(n_features), order, prob = pweight)
      }
    }
  )

  # Construct formula
  form_x <- draws %>%
    map(., function(idx) feature_names[idx]) %>%
    map(., function(feature) paste(feature, collapse = "*")) %>%
    map2(., coefs, function(x, coef) { sprintf("%s*%s", coef, x) }) %>%
    paste(., collapse = " + ")

  # Compute true y
  command <- gsub(x = form_x, pat = "x(\\d+)", repl = "X[,\\1]")
  true_y <- eval(parse(text = command))

  # Change Formulo better reflect R formulas
  form_x <- form_x %>%
    gsub(x = ., pat = "funs\\$", repl = "I(") %>%         # Wrap functions in I()
    gsub(x = ., pat = ")", repl = "))") %>%
    gsub("(?<=^|\\+ )[^\\*]+\\*", "", ., perl = TRUE) %>% # Remove coefficients
    gsub(x = ., pat = "\\*", repl = ":")

  # Noise: stn
  sd_norm <- stn * mean(abs(true_y - median(true_y)))
  target <- true_y + rnorm(n_obs, 0, sd_norm)

  # Add final noise component
  dgp <- sprintf("y ~ %s ~ N(0, %s)", form_x, round(sd_norm, 2))

  # Create noise
  mat_noise <- matrix(rnorm(n_obs*n_noise), ncol = n_noise) %>%
    set_colnames(sprintf("noise%s", seq_len(n_noise)))

  # TODO(tkrabel): Add correlated features

  # Return final data frame
  df_out <- data.frame(target, X, mat_noise)

  df_out
}

