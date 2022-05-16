#' @name generate.template
#' @title Generate Pipeline Template
#' @description Generate an R script template for running the Sharpton Lab dada2 pipeline
#' @param filename A string to name the resulting template. If the .R extension is not included in the filename, it will be appended. Default is "metaGTx_processing.R"
#' @export
#' @examples
#' generate.template()
#' file.show("metaGTx_processing.R")

generate.template <- function(filename = "metaGTx_processing.R") {
  if (!grepl("\\.R$", filename)) { filename <- paste0(filename, ".R")}
  writeLines(template.code, con = filename)
}
