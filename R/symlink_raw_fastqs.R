#' Create symlinks to fastq(.gz) files
#'
#' This function creates symlinks in the current directory to fastq OR fastq.gz files in the provided directory.
#' @param fastq.dir character; path to directory containing raw FASTQ files.
#' @param delim character; delimiter for finding sample names in fastq file names.
#' @param sample.field integer; field (after cutting) in which to find sample names.
#' @param pattern character; matching pattern for files you want to link. Default is 'lane.*fastq\\.gz'.
#' @param split character; character(s) to split the sample name from the read ID in link name, e.g. sample01-R1.fastq.gz OR sample01--R1.fastq.gz. Default is '--'".
#' @param replacements list; a named list of patterns and replacements for file names, e.g. =list(to.replace = c("A", "B"), replace.by = c("a", "b")). Default is NULL.
#' @seealso \code{\link{system}}, \code{\link{str_split}}, \code{\link{list.files}}, \code{\link{create.processing.env}}
#' @export

symlink.raw.fastqs <- function(
    fastq.dir,
    delim,
    sample.field,
    pattern = "lane.*fastq\\.gz",
    split = "--",
    replacements = NULL
) {
  require(magrittr)
  require(stringr)
  if (!is.environment(run.env)) {
    rlang::abort(
      "Object `run.env' does not exist, please run the function create_processing_env() first"
    )
  } else {
    samples <- NULL
  }
  for (fastq in list.files(path = fastq.dir, pattern = pattern, full.names = T)) {
    name = basename(fastq) %>%
      stringr::str_split(pattern = delim) %>%
      sapply(`[`, sample.field)
    samples <- c(samples, name)

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
  # print(samples)
  assign(x = "samples", value = sort(unique(samples)), envir = run.env)
}
