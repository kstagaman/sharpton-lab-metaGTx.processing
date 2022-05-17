#' @name execute.commands
#' @title Execute commands
#' @description This function takes a character vector of shell commands and uses `system` to execute them
#' @param commands character; vector of shell commands.
#' @param n.cores integer; number of cores to utilize. 1 = serial processing; more than 1 = parallel processin. NOTE: setting n.cores > 1 in an already parallel computational context can cause issues. Default is 1.
#' @seealso \code{\link{system}}
#' @export

execute.commands <- function(commands, n.cores = 1) {
  if (n.cores == 1 ) {
  for (command in commands) {
    system(command)
  }
  } else {
    require(doParallel)
    require(foreach)
    require(magrittr)
    cl <- parallel::makeCluster(n.cores, type = "FORK") %>% try(silent = T)
    if ("try-error" %in% class(cl)) {
      doParallel::stopImplicitCluster()
      rlang::abort(paste(cat(cl, sep = "\n")))
    }
    doParallel::registerDoParallel(cl, n.cores)
    commands <- c("echo hello", "echo world")
    par.loop <- foreach::foreach(command = commands) %dopar% {
      system(command)
    } %>% try(silent = T)
    parallel::stopCluster(cl)
    if ("try-error" %in% class(par.loop)) {
      cat(par.loop, sep = "\n")
    }
  }
}
