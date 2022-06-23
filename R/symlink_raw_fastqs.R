#' @name symlink.raw.fastqs
#' @title Create symlinks to fastq(.gz) files
#' @description This function creates symlinks in the current directory to fastq OR fastq.gz files in the provided directory.
#' @param fastq.dirs character; path to raw FASTQs directory **or** a *named* vector of paths to raw FASTQs directories. If a vector, function will preppend name of path to sample names to keep them distinct.
#' @param delim character; delimiter for finding sample names in fastq file names.
#' @param sample.field integer; field (after cutting) in which to find sample names.
#' @param pattern character; matching pattern for files you want to link. Default is 'lane.*fastq\\.gz'.
#' @param split character; character(s) to split the sample name from the read ID in link name, e.g. sample01-R1.fastq.gz OR sample01--R1.fastq.gz. Default is '--'".
#' @param replacements list; a named list of patterns and replacements for file names, e.g. =list(to.replace = c("A", "B"), replace.by = c("a", "b")). Default is NULL.
#' @param save.sample.names logical, whether to grab sample names from file names (TRUE) let user supply sample names directly (FALSE). Default is TRUE.
#' @seealso \code{\link{system}}, \code{\link{str_split}}, \code{\link{list.files}}, \code{\link{create.processing.env}}
#' @export

symlink.raw.fastqs <- function(
    fastq.dirs,
    delim,
    sample.field,
    pattern = "lane.*fastq\\.gz",
    split = "--",
    replacements = NULL,
    save.sample.names = TRUE
) {
  require(magrittr)
  require(stringr)
  if (!is.environment(run.env)) {
    rlang::abort(
      "Object `run.env' does not exist, please run the function create_processing_env() first"
    )
  }
  if (length(fastq.dirs) > 1 & is.null(names(fastq.dirs))) {
    rlang::abort("More than one fastq directory is supplied, but the vector is unnamed. Please supply names.")
  }
  for (i in seq_along(fastq.dirs)) {
    fastqs <- list.files(path = fastq.dirs[i], pattern = pattern, full.names = T)
    if (length(fastqs) == 0) {
      rlang::abort("No files in the supplied directory match the supplied pattern.")
    }
    for (fastq in fastqs) {
      base.name <- basename(fastq) %>%
        stringr::str_split(pattern = delim) %>%
        sapply(`[`, sample.field)
      name <- ifelse(length(fastq.dirs) > 1, paste0(names(fastq.dirs)[i], "_", base.name), base.name)
      run.env$samples <- c(run.env$samples, name)

      if (!is.null(replacements)) {
        to.replace <- replacements$to.replace
        replace.by <- replacements$replace.by
        if (length(to.replace) != length(replace.by) & length(replace.by) != 1) {
          rlang::abort("The number of patterns to be replaced should be equal to the number of replacements, or the number of replacements should equal 1.")
        } else {
          for (i in 1:length(to.replace)) {
            j <- ifelse(length(replace.by) == 1, 1, i)
            name <- str_replace_all(name, to.replace[i], replace.by[j])
          }
        }
      }
      read <- str_extract(fastq, "R[12]")
      link.name <- paste0(name, split, read, ".fastq.gz")
      cmd <- paste("ln -sv", fastq, link.name)
      system(cmd)
    }
  }
  # print(samples)
}
