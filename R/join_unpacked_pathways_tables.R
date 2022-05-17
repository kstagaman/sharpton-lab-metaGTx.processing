#' @name join.unpacked.pathways.tables
#' @title Join single-sample unpacked pathways tables
#' @description This function takes multiple humann unpacked pathwaysfiles and combines them into a single output file.
#' @param match.pattern character; pattern to match desired files and directories. Default is "metaphlan_bugs_list"
#' @param input.dir character; path to input directory.
#' @param output.file character; path and name of output file.
#' @export

join.unpacked.pathways.tables <- function(match.pattern = "unpacked_pathways_relab", input.dir, output.file) {
  require(data.table)
  require(stringr)
  require(magrittr)
  files <- list.files(path = input.dir, pattern = match.pattern, full.names = T)
  res.dt <- NULL
  for (file in files) {
    file.name <- str_remove(basename(file), paste0(match.pattern, "\\.tsv$"))
    dt <- read.table(file = file, sep = "\t", header = T, comment.char = "", fill = T) %>% as.data.table()
    names(dt) <- str_remove_all(names(dt), "^X\\.+")
    if (is.null(res.dt)) {
      res.dt <- dt
    } else {
      res.dt <- merge(res.dt, dt, by = names(dt)[str_detect(names(dt), "[Pp]athway")], all = T)
    }
  }
  write.table(x = res.dt, file = output.file, sep = "\t", row.names = F, na = "0", quote = F)
  cat(paste("Gene table created:", output.file), sep = "\n")
}
