% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/scramble_vec.R
\name{scramble_vec}
\alias{scramble_vec}
\title{scramble_vec}
\usage{
scramble_vec(v, keep_names = TRUE)
}
\arguments{
\item{v}{An atomic vector.}

\item{keep_names}{\code{TRUE} or \code{FALSE}, indicating whether to retain
the element names of the input vector (if any are present) in the scrambled
vector.}
}
\value{
An atomic vector the length of \code{v} with type logical, integer,
double, or character (depending on the type of \code{v}).
}
\description{
Scrambles the data in an atomic vector while retaining the type and
basic properties (approximate min/max for numeric data; approximate
length for character data; approximate \code{NA} proportion; etc.) of the
input. Principled scrambling approaches are defined for logical, integer
(including factor class data), double, and character data.
}
\details{
The scrambling approach taken for each type of input is outlined below:

\itemize{
  \item Logical: Resample \code{TRUE}/\code{FALSE}/\code{NA} while retaining
  the approximate original proportions of each value.
  \item Integer: Pull new values at random from the integer sequence running
  from the minimum value to the maximum value of \code{v} (ignoring \code{Inf}
  and \code{-Inf}) while retaining the approximate proportion of \code{NA}
  values as is in the input. If a factor is given, a factor is returned.
  \item Double: Pull new values at random from a uniform distribution running
  from the minimum value to the maximum value of \code{v} (ignoring \code{Inf}
  and \code{-Inf}) while retaining the approximate proportion of \code{NA}
  values as is in the input.
  \item Character: For each unique string in \code{v}, generate a corresponding
  scrambled string. The scrambled strings are randomly constructed from the
  alphanumeric characters and have lengths equal to the median length of the
  strings in \code{v}. Then randomly sample the scrambled strings with
  probabilities equal to the proportional representation of each scrambled
  string's corresponding original string while retaining the approximate
  proportion of \code{NA} values as is in the input.
  \item If \code{v} is not logical, integer, double, or character, draw
  scrambled values from N(0, 1) (and print an alert if in interactive mode).
}
}
\examples{
set.seed(248)
scramble_vec(mtcars$mpg)
scramble_vec(precip)

}
