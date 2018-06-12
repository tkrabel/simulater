#' Function to create a random DGP
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
#' @importFrom magrittr %>%, set_colnames
#' @importFrom purrr map, map2
#'
#' @examples
simulater <- function(n_obs,
                      n_vars,
                      n_components,
                      max_order,
                      n_noise = 0,
                      stn = 0.5,
                      funs = NULL,
                      fun_weights = NULL,
                      fun_prob = 0.5) {

  # Warnings
  if (is.null(funs) && fun_prob > 0) {
    warning("'fun_prob' > 0 but no functions were passed as an argument")
    fun_prob <- 0
  }
  if (n_components < n_vars)
    warning("You have chosen 'n_components' < 'n_vars', which means that it
            may be that not all variables are used in the formula")

  # Errors
  if (length(funs) != length(fun_weights) & !is.null(fun_weights))
    stop("'funs' and 'fun_weights' must have same length")
  stopifnot(n_obs > 0, n_vars > 0, n_noise >= 0, n_components > 0, max_order > 0,
            stn >= 0, fun_prob >= 0, fun_prob <= 1)

  # Features
  x_cols <- sprintf("x%s", 1:n_vars)
  X <- matrix(rnorm(n_obs * n_vars, 0, 1), nrow = n_obs, ncol = n_vars) %>%
    set_colnames(x_cols)
  feature_names <- names(funs) %>%
    map(., function(fun_name) { sprintf("funs$%s(%s)", fun_name, x_cols) }) %>%
    do.call(c, .)
  feature_names <- c(x_cols, feature_names)
  n_features    <- length(feature_names)

  # Compute the weights of identity functions to have  (1 - funprob) % change of
  # being drawn
  fun_weights <- if (!is.null(funs) && is.null(fun_weights)) rep(1, length(funs))
  iweight <- if (!fun_prob) 1 else (1 - fun_prob) / fun_prob * sum(fun_weights)
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
        c(i, sample(seq_len(n_features)[-i], order - 1, TRUE, prob = pweight[-i]))
      } else {
        sample(seq_len(n_features), order, TRUE, prob = pweight)
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

  # Change Formula to better reflect R formulas
  form_x <- form_x %>%
    gsub(x = ., pat = "funs\\$", repl = "I(") %>%         # Wrap functions in I()
    gsub(x = ., pat = ")", repl = "))") %>%
    gsub("(?<=^|\\+ )[^\\*]+\\*", "", ., perl = TRUE) %>% # Remove coefficients
    gsub(x = ., pat = "\\*", repl = ":") %>%
    simplify_formula()

  # Noise: stn
  sd_norm <- stn * mean(abs(true_y - median(true_y)))
  target <- true_y + rnorm(n_obs, 0, sd_norm)

  # Add final noise component
  dgp <- sprintf("y ~ -1 + %s ~ N(0, %s)", form_x, round(sd_norm, 2))

  # Create noise
  mat_noise <- NULL
  if (n_noise > 0) {
    # Create noise
    mat_noise <- matrix(rnorm(n_obs * n_noise), ncol = n_noise) %>%
      set_colnames(sprintf("noise%s", seq_len(n_noise)))
  }

  # Return final data frame
  df_out <- as.data.frame(cbind(target, X, mat_noise))

  list(data = df_out,
       formula = dgp)
}
