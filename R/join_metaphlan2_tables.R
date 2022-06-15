#' @name join.metaphlan2.tables
#' @title Join single-sample metaphlan2 tables
#' @description This function takes multiple humann metaphlan2 bug list files and combines them into a single output file.
#' @param match.pattern character; pattern to match desired files and directories. Default is "metaphlan_bugs_list"
#' @param input.dir character; path to input directory.
#' @param output.file character; path and name of output file.
#' @export

join.metaphlan2.tables <- function(match.pattern = "metaphlan_bugs_list", input.dir, output.file) {
  require(data.table)
  require(stringr)
  require(magrittr)
  files <- list.files(path = input.dir, pattern = match.pattern, full.names = T)
  res.dt <- NULL
  for (file in files) {
    file.name <- str_remove(basename(file), paste0(match.pattern, "\\.tsv$"))
    dt <- read.table(file = file, header = T, sep = "\t", skip = 3, comment.char = "") %>% as.data.table()
    names(dt) <- str_remove_all(names(dt), "^X\\.+")
    names(dt)[!str_detect(names(dt), "clade|id")] <- paste0(file.name, names(dt)[!str_detect(names(dt), "clade|id")])
    ncbi.id.col <- names(dt)[str_detect(names(dt), "id")]
    dt[[ncbi.id.col]] <- as.character(dt[[ncbi.id.col]])
    if (is.null(res.dt)) {
      res.dt <- dt
    } else {
      res.dt <- merge(res.dt, dt, by = names(dt)[str_detect(names(dt), "clade|id")], all = T)
    }
  }
  write.table(x = res.dt, file = output.file, sep = "\t", row.names = F, na = "0", quote = F)
  cat(paste("Taxonomy table created:", output.file), sep = "\n")
}
