#' Extract metaphlan2 results
#'
#' This function extracts the humann metaphlan2 bug list file from a tgz file
#' @param match.pattern character; pattern to match desired files and directories. Default is "tgz$"
#' @param location character; path to irectory with files/directories you want to manipulate.
#' @param move.to character; path to which to move extracted file.
#' @seealso \code{\link{system}}, \code{\link{generate.full.commands}}
#' @export

extract.metaphlan2.files <- function(match.pattern = "tgz$", location, move.to) {
  require(magrittr)
  require(stringr)
  for (tgz in list.files(path = location, full.names = T, pattern = match.pattern)) {
    metaphlan.file <- untar(tarfile = tgz, list = T) %>%
      str_subset("metaphlan_bugs_list.tsv$")
    untar(tarfile = tgz, files = metaphlan.file)

    metaphlan.untarred <- file.path(getwd(), metaphlan.file)
    file.copy(from = metaphlan.untarred, to = move.to)
    gzip.cmd <- paste("gzip -v", file.path(move.to, basename(metaphlan.file)))
    system(gzip.cmd)
    file.remove(metaphlan.untarred)
    to.remove <- str_split(dirname(metaphlan.untarred), "/") %>%
      sapply(`[`, 1) %>%
      list.dirs(recursive = TRUE)
    for (i in rev(seq_along(to.remove))) {
      file.remove(to.remove[i])
    }
  }
}

