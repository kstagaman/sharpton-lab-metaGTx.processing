#' @name get.pct.unalign
#' @title Get percent unaligned
#' @description This function extracts the humann log file from a tgz file and gets the percent unaligned sequences for both nucleotides and protein translations.
#' @param match.pattern character; pattern to match desired files and directories. Default is "tgz$"
#' @param location character; path to directory with files/directories you want to manipulate.
#' @param out.file character; path and name of output file.
#' @export

get.pct.unalign <- function(match.pattern = "tgz$", location, out.file) {
  output.df <- data.frame()
  for (tgz in list.files(path = location, full.names = T, pattern = match.pattern)) {
    log.file <- untar(tarfile = tgz, list = T) %>%
      str_subset("log$")
    untar(tarfile = tgz, files = log.file)
    lines <- readLines(log.file)
    nucl.unalign <- str_subset(lines, "Unaligned reads after nucleotide alignment") %>%
      str_extract("\\d+[\\d\\.]* \\%") %>%
      str_remove(" \\%")
    prot.unalign <- str_subset(lines, "Unaligned reads after translated alignment") %>%
      str_extract("\\d+[\\d\\.]* \\%") %>%
      str_remove(" \\%")
    output.df <- rbind(
      output.df,
      data.frame(
        Log.file = basename(log.file),
        Nucl.pct.unalign = nucl.unalign,
        Prot.pct.unalign = prot.unalign
      )
    )
    file.remove(log.file)
    to.remove <- str_split(dirname(log.file), "/") %>%
      sapply(`[`, 1) %>%
      list.dirs(recursive = TRUE)
    for (i in rev(seq_along(to.remove))) {
      file.remove(to.remove[i])
    }
  }
  write.csv(output.df, file = out.file, row.names = F)
}

