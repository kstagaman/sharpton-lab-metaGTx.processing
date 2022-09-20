#' @name generate.tool.command
#' @title Generate tool command
#' @description This function generates a character string of a command given a tool and its arguments.
#' @param tool character; name of the appropriate tool, e.g. "kneaddata" or "humann".
#' @param tool.path character; path to script for sourcing tool, if required by system, this will also append the command `source <PATH> ; ` to the full commands. NULL means this is not prepended. Default is NULL.
#' @param ... other commands to pass to appropriate tool. Names must match short or long version found in that tool's help page. If the flag takes no argument in the tool, pass "flag" to the argument. E.g. for bash command `ls -l` you could run `generate.tool.command(tool = "ls", l = "flag")`
#' @seealso \code{\link{system}}, \code{\link{list2}}
#' @export

generate.tool.command <- function(tool, tool.path = NULL, ...) {
  require(magrittr)
  require(stringr)
  vargs <- rlang::list2(...)
  vargs <- vargs[!sapply(vargs, is.null)]
  if (is.null(run.env$bin.path)) {
    cmd.base <- tool
  } else {
    cmd.base <- file.path(run.env$bin.path, tool)
  }
  if (!is.null(tool.path)) {
    cmd.base <- paste("source", tool.path, cmd.base)
  }
  cmd.args <- sapply(seq_along(vargs), function(arg) {
    arg.name <- names(vargs)[[arg]]
    arg.val <- ifelse(vargs[[arg]] == "flag", "", vargs[[arg]])
    dashes <- ifelse(nchar(arg.name) > 1, 2, 1)
    paste0(
      paste(rep("-", dashes), collapse = ""),
      str_replace_all(arg.name, "\\.", "-"),
      " ",
      arg.val
    ) %>% return()
  }) %>% paste(collapse = " ")
  paste(cmd.base, cmd.args) %>% return()
}
