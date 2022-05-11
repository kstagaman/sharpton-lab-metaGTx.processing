#' Set Redo Variables to TRUE or FALSE
#'
#' Set the state (TRUE or FALSE) of one or more redo variables. This can also be acheived with assign_redo(), but these functions require only one argument.
#' @param redo.vars A character vector of the variable names you want to give TRUE or FALSE to
#' @export
#' @examples
#' chunk.names <- c("chunk1", "chunk2")
#'
#' assign.redo(chunk.names)
#'
#' set.redo.true("chunk1")


set.redo.true <- function(redo.vars) {
  redo.vars <- as.character(redo.vars)
  for (var in redo.vars) {
    assign(var, TRUE, envir = redo)
  }
}

#' @export
set.redo.false <- function(redo.vars) {
  redo.vars <- as.character(redo.vars)
  for (var in redo.vars) {
    assign(var, FALSE, envir = redo)
  }
}
