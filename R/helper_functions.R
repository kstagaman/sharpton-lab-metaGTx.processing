#' Helper functions
#'
#' Functions for tarring, zipping, and moving files.
#' @param match.pattern character; pattern to match desired files and directories. Default NULL
#' @param location character; path to directory with files/directories you want to manipulate.
#' @param move.from character; path to directory you want to move files/directories from.
#' @param move.to character; path to directory you want to move files/directories to.
#' @seealso \code{\link{system}}, \code{\link{generate.full.commands}}
#' @export

tgz.directories <- function(location, match.pattern = NULL) {
  require(magrittr)
  require(stringr)
  if (is.null(match.pattern)) {
    target.dir <- location
  } else {
    target.dir <- list.dirs(path = location, full.names = TRUE) %>%
      str_subset(match.pattern)
  }
  if (length(target.dir) > 1) {
    rlang::abort(
      paste0("Pattern '", match.pattern, "' matches more than one directory")
    )
  } else if (length(target.dir) == 0) {
    rlang::inform("No directories detected, nothing done.")
  } else {
    tar(tarfile = paste0(target.dir, ".tgz"), files = target.dir, compression = "gzip")
  }
}

#' @export

remove.directories <- function(location, match.pattern = NULL) {
  require(magrittr)
  require(stringr)
  if (is.null(match.pattern)) {
    target.dirs <- location
  } else {
    target.dirs <- list.dirs(path = location, full.names = TRUE) %>%
      str_subset(match.pattern)
  }
  if (length(target.dir) == 0) {
    rlang::inform("No directories detected, nothing done.")
  } else {
    for (target.dir in target.dirs) {
      file.remove(list.files(path = target.dir, full.names = T, recursive = T))
      file.remove(target.dir)
    }
  }
}

#' @export

gzip.files <- function(location, match.pattern = NULL) {
  files <- list.files(path = location, pattern = match.pattern, full.names = T)
  for (file in files) {
    cmd <- paste("gzip -v", file)
    system(cmd)
  }
}

#' @export

move.files <- function(move.from, move.to, match.pattern = NULL) {
  require(magrittr)
  if (is.null(match.pattern)) {
    copy.res <- file.copy(from = move.from, to = move.to) %>% try()
    if (!{"try-error" %in% class(copy.res)}) { file.remove(move.from) }
  } else {
    files <- list.files(path = move.from, pattern = match.pattern, full.names = T)
    for (file in files) {
      copy.res <- file.copy(from = file, to = move.to) %>% try()
      if (!{"try-error" %in% class(copy.res)}) { file.remove(file) }
    }
  }
}
