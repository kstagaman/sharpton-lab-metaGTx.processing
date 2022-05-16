#' @name execute.commands
#' @title Execute commands
#' @description This function takes a character vector of shell commands and uses `system` to execute them
#' @param commands character; vector of shell commands.
#' @seealso \code{\link{system}}
#' @export

execute.commands <- function(commands) {
  for (command in commands) {
    system(command)
  }
}
