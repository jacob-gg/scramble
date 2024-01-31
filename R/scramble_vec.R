#' scramble_vec
#'
#' Scrambles the data in an atomic vector while retaining the type and
#' basic properties (approximate min/max for numeric data; approximate
#' length for character data; approximate \code{NA} proportion; etc.) of the
#' input. Principled scrambling approaches are defined for logical, integer
#' (including factor class data), double, and character data.
#'
#' The scrambling approach taken for each type of input is outlined below:
#'
#' \itemize{
#'   \item Logical: Resample \code{TRUE}/\code{FALSE}/\code{NA} while retaining
#'   the approximate original proportions of each value.
#'   \item Integer: Pull new values at random from the integer sequence running
#'   from the minimum value to the maximum value of \code{v} (ignoring \code{Inf}
#'   and \code{-Inf}) while retaining the approximate proportion of \code{NA}
#'   values as is in the input. If a factor is given, a factor is returned.
#'   \item Double: Pull new values at random from a uniform distribution running
#'   from the minimum value to the maximum value of \code{v} (ignoring \code{Inf}
#'   and \code{-Inf}) while retaining the approximate proportion of \code{NA}
#'   values as is in the input.
#'   \item Character: For each unique string in \code{v}, generate a corresponding
#'   scrambled string. The scrambled strings are randomly constructed from the
#'   alphanumeric characters and have lengths equal to the median length of the
#'   strings in \code{v}. Then randomly sample the scrambled strings with
#'   probabilities equal to the proportional representation of each scrambled
#'   string's corresponding original string while retaining the approximate
#'   proportion of \code{NA} values as is in the input.
#'   \item If \code{v} is not logical, integer, double, or character, draw
#'   scrambled values from N(0, 1) (and print an alert if in interactive mode).
#' }
#'
#' @param v An atomic vector.
#' @param keep_names \code{TRUE} or \code{FALSE}, indicating whether to retain
#' the element names of the input vector (if any are present) in the scrambled
#' vector.
#' @return An atomic vector the length of \code{v} with type logical, integer,
#' double, or character (depending on the type of \code{v}).
#' @examples
#' set.seed(248)
#' scramble_vec(mtcars$mpg)
#' scramble_vec(precip)
#'
#' @export
scramble_vec <- function(v, keep_names = TRUE) {
  # This function responds to the *base type* of the input vector,
  # not the (inherited) class(es). The (current) exception is factors,
  # which have base type "integer" and receive special handling (to ensure
  # factor-in/factor-out). The diagram in section 3.4 is the reference:
  # https://adv-r.hadley.nz/vectors-chap.html?q=factor#dates
  # May add special handling for: dates (currently treated as double)

  stopifnot('argument to scramble_vec() must be an atomic vector' = is.atomic(v),
            'argument to scramble_vec() must have length >= 1' = length(v) >= 1,
            'keep_names must have length = 1 and be TRUE or FALSE' = length(keep_names) == 1 && keep_names %in% c(TRUE, FALSE))

  scrambled_vec <- switch(typeof(v),
                          logical = scramble_lgcl(v),
                          integer = scramble_int(v),
                          double = scramble_dbl(v),
                          character = scramble_char(v),
                          scramble_unsure(v))

  if (keep_names) {
    names(scrambled_vec) <- names(v)
  }

  scrambled_vec
}
