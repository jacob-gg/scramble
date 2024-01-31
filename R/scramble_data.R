#' scramble_data
#'
#' Scrambles the data in a data frame or matrix while retaining the type and
#' basic properties (approximate min/max for numeric data, approximate
#' length for character data, approximate \code{NA} proportion, etc.) of the
#' input. Principled scrambling approaches are defined for logical, integer,
#' double, and character data. See \code{?scramble_vec} for info on the specific
#' approach taken for each data type.
#'
#' @param dat A data frame or matrix.
#' @param keep_col_names \code{TRUE} or \code{FALSE}, indicating whether to retain
#' the input data's column names in the scrambled data. (If \code{FALSE}, the
#' scrambled data's column names are \code{V1}, \code{V2}, etc.)
#' @param keep_row_names \code{TRUE} or \code{FALSE}, indicating whether to retain
#' the input data's row names in the scrambled data. (If \code{FALSE}, the
#' scrambled data's row names are \code{1}, \code{2}, etc.)
#' @return A data frame or matrix, depending on \code{dat}.
#' @examples
#' set.seed(248)
#' scramble_data(mtcars)
#' scramble_data(USPersonalExpenditure, FALSE, FALSE)
#'
#' @export
scramble_data <- function(dat, keep_col_names = TRUE, keep_row_names = TRUE) {
  if ((inherits(dat, 'data.frame') | inherits(dat, 'matrix')) == FALSE) {
    stop('input data must be a data frame or matrix', call. = FALSE)
  }
  stopifnot('keep_col_names must have length = 1 and be TRUE or FALSE' = length(keep_col_names) == 1 && keep_col_names %in% c(TRUE, FALSE),
            'keep_row_names must have length = 1 and be TRUE or FALSE' = length(keep_row_names) == 1 && keep_row_names %in% c(TRUE, FALSE))

  if (inherits(dat, 'data.frame')) {
    scrambled_data <- data.frame(lapply(dat, function(x) scramble_vec(x, keep_names = FALSE)),
                                 row.names = if (keep_row_names) { rownames(dat) } else { NULL })
    if (!keep_col_names) {
      colnames(scrambled_data) <- paste0('V', 1:ncol(scrambled_data))
    }
    scrambled_data
  } else {
    matrix(scramble_vec(dat, keep_names = FALSE),
           nrow = dim(dat)[1], ncol = dim(dat)[2],
           dimnames = list(if (keep_row_names) { rownames(dat) } else { NULL },
                           if (keep_col_names) { colnames(dat) } else { NULL }))
  }
}
