#' @name generate.SGE.command
#' @title Generate SGE command
#' @description This function generates a character string of a command for SGE_Batch or SGE_Array and its arguments.
#' @param c character; a string containing the command you want to submit (SGE_Batch) OR a path and name to a file containg commands (SGE_Array).
#' @param ... other commands to pass SGE_Batch/SGE_Array. Names must match short or long version found in that tool's help page.
#' @seealso \code{\link{generate.full.commands}}
#' @export


generate.SGE.command <- function(c, ...) {
  require(magrittr)
  vargs <- rlang::list2(...)
  vargs <- vargs[!sapply(vargs, is.null)]
  if (file.exists(c)) {
    cmd.base <- paste("SGE_Array -c", c)
  } else {
    cmd.base <- paste("SGE_Batch -c", c)
  }

  cmd.args <- sapply(seq_along(vargs), function(arg) {
    arg.name <- names(vargs)[[arg]]
    arg.val <- vargs[[arg]]
    dashes <- ifelse(nchar(arg.name) > 1, 2, 1)
    sep <- ifelse(arg.name == "qsub_options", "=", " ")
    paste0(paste(rep("-", dashes), collapse = ""), arg.name, sep, arg.val) %>% return()
  }) %>% paste(collapse = " ")
  paste0(
    "Please run the following command in the appropriate directory\n",
    "on a submission machine:\n\n",
    paste(cmd.base, cmd.args),
    "\n"
  ) %>% cat(sep = "\n")
}
