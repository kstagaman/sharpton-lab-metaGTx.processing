#' @name extract.metaphlan2.files
#' @title Extract metaphlan2 results
#' @description This function extracts the humann metaphlan2 bug list file from a tgz file
#' @param match.pattern character; pattern to match desired files and directories. Default is "tgz$"
#' @param location character; path to directory with files/directories you want to manipulate.
#' @param move.to character; path to which to move extracted file.
#' @param n.cores integer; if 1, will run serially, otherwise will extract in parallel utilzing up to the number of cores specified. Default is 1.
#' @export

extract.metaphlan2.files <- function(match.pattern = "tgz$", location, move.to, n.cores = 1) {
  require(magrittr)
  require(stringr)

  if (n.cores == 1) {
    for (tgz in list.files(path = location, full.names = T, pattern = match.pattern)) {
      metaphlan.file <- untar(tarfile = tgz, list = T) %>%
        str_subset("metaphlan_bugs_list.tsv$")
      untar(tarfile = tgz, files = metaphlan.file)

      metaphlan.untarred <- file.path(getwd(), metaphlan.file)
      file.copy(from = metaphlan.untarred, to = move.to)

      gzip.cmd <- paste("gzip -v", file.path(move.to, basename(metaphlan.file)))
      system(gzip.cmd)

      file.remove(metaphlan.untarred)

      to.remove <- ifelse(
        str_detect(dirname(metaphlan.untarred), "/$"),
        dirname(metaphlan.untarred),
        paste0(dirname(metaphlan.untarred), "/")
      ) %>%
        str_replace_all("//", "/")

      base.dir <- ifelse(
        str_detect(run.env$base.dir, "/$"),
        run.env$base.dir,
        paste0(run.env$base.dir, "/")
      ) %>%
        str_replace_all("//", "/")

      while (to.remove != base.dir) {
        cat(paste("Removing", to.remove), sep = "\n")
        remove.success <- file.remove(to.remove)
        if (!remove.success) {
          rlang::abort(paste("File", to.remove, "could not be removed"))
        }
        to.remove <- str_remove(to.remove, "[\\w-]+/$")
      }
    }
  } else {
    require(doParallel)
    require(foreach)

    cl <- parallel::makeCluster(n.cores, type = "FORK")
    doParallel::registerDoParallel(cl, n.cores)

    par.loop <- foreach::foreach(
      tgz = list.files(path = location, full.names = T, pattern = match.pattern)
    ) %dopar% {
      metaphlan.file <- untar(tarfile = tgz, list = T) %>%
        str_subset("metaphlan_bugs_list.tsv$")
      untar(tarfile = tgz, files = metaphlan.file)

      metaphlan.untarred <- file.path(getwd(), metaphlan.file)
      file.copy(from = metaphlan.untarred, to = move.to)

      gzip.cmd <- paste("gzip -v", file.path(move.to, basename(metaphlan.file)))
      system(gzip.cmd)

      file.remove(metaphlan.untarred)

      to.remove <- ifelse(
        str_detect(dirname(metaphlan.untarred), "/$"),
        dirname(metaphlan.untarred),
        paste0(dirname(metaphlan.untarred), "/")
      ) %>%
        str_replace_all("//", "/")

      base.dir <- ifelse(
        str_detect(run.env$base.dir, "/$"),
        run.env$base.dir,
        paste0(run.env$base.dir, "/")
      ) %>%
        str_replace_all("//", "/")

      while (to.remove != base.dir) {
        cat(paste("Removing", to.remove), sep = "\n")
        file.remove(to.remove)
        to.remove <- str_remove(to.remove, "[\\w-]+/$")
      }
    } %>% try(silent = T)

    parallel::stopCluster(cl)
    if ("try-error" %in% class(par.loop)) {
      cat(par.loop, sep = "\n")
    }
  }
}



