#' @name symlink.files
#' @title Create symlinks to files
#' @description This function creates symlinks in the current directory to files in the provided directory.
#' @param file.dir character; path to directory containing files you want to link to.
#' @param pattern character; matching pattern for files you want to link.
#' @param replacements list; a named list of patterns and replacements for file names, e.g. =list(to.replace = c("A", "B"), replace.by = c("a", "b")). Default is NULL.
#' @seealso \code{\link{system}}, \code{\link{str_split}}, \code{\link{list.files}}
#' @export

symlink.files <- function(
    file.dir,
    pattern,
    replacements = NULL
) {
  require(magrittr)
  require(stringr)
  for (file in list.files(path = file.dir, pattern = pattern, full.names = T)) {
    link.name = basename(file)

    if (!is.null(replacements)) {
      to.replace <- replacements$to.replace
      replace.by <- replacements$replace.by
      if (length(to.replace) != length(replace.by) & length(replace.by) != 1) {
        rlang::abort("The number of patterns to be replaced should be equal to the number of replacements, or the number of replacements should equal 1.")
      } else {
        for (i in 1:length(to.replace)) {
          j <- ifelse(length(replace.by) == 1, 1, i)
          link.name <- str_replace_all(link.name, to.replace[i], replace.by[j])
        }
      }
    }
    cmd <- paste("ln -sv", file, link.name)
    system(cmd)
  }
}
