#' Simplifies a formula
#'
#' @param formula A character string describing the RHS of a formula
#'
#' @return A simplified version of it, i.e. makes \code{x1:x1:x2} to
#'   \code{I(x1)^2:x2}
#' @export
#'
#' @examples
#' simplify_formula("x1:x1:x2 + x2:x3:x3:x3")
simplify_formula <- function(formula) {
  strsplit(formula, split = " \\+ ") %>% .[[1]] %>%
    vapply(., simplify_term, "character") %>%
    paste(., collapse = " + ")
}

#' Simplify a term of a formula
#'
#' @param term a character string containing one component of a formula
#'
#' @description This function is a helper for \code{simplify_formula} and won't
#'   be called elsewhere
#'
#' @return Returns a character string resembling the simplified version of the
#'   component
#' @export
#'
#' @examples
#' simplify_term("x1:x1")
simplify_term <- function(term) {

  # Simplify x1:x1:x2 -> I(x1)^2:I(x2)^1
  table  <- strsplit(term, ":") %>% .[[1]] %>% table()
  vars   <- attr(table, "dimnames") %>% unlist()
  nums   <- table %>% as.numeric()
  simple <- sprintf("I(%s)^%s", vars, nums)

  # Remove reduntant notation: I(x1)^1 -> x1
  reduce     <- grep(x = simple, pat = "\\)\\^1$", val = TRUE)
  not_reduce <- setdiff(simple, reduce)
  reduce     <- gsub(x = reduce, pat = "(\\)\\^1$)|(^I\\()", repl = "")

  simple <- c(reduce, not_reduce) %>%
    paste(., collapse = ":")
  return(simple)
}


