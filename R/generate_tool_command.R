#' @name generate.tool.command
#' @title Generate tool command
#' @description This function generates a character string of a command given a tool and its arguments.
#' @param tool character; name of the appropriate tool, e.g. "kneaddata" or "humann".
#' @param ... other commands to pass to appropriate tool. Names must match short or long version found in that tool's help page.
#' @seealso \code{\link{system}}, \code{\link{list2}}
#' @export

generate.tool.command <- function(tool, ...) {
  require(magrittr)
  require(stringr)
  vargs <- rlang::list2(...)
  vargs <- vargs[!sapply(vargs, is.null)]
  cmd.base <- tool
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
  paste(cmd.base, cmd.args, ";") %>% return()
}
