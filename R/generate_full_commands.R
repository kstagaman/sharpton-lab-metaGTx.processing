#' @name generate.full.commands
#' @title Generate full commands
#' @description This function generates a single-line tool command with the supplied parameters for each file in the supplied directory as well as appending other commands, such as gzipping and moving results files.
#' @param input.dir character; path to directory containing input files. Default is NULL.
#' @param paired logical; are their paired R1 and R2 files that should be analyzed concurrently? Default is TRUE.
#' @param tmp.dir character; path to temporary directory for direct output. After files are written here, they will be moved to `output.dir`. Default is NULL.
#' @param output.dir character; path to storage directory for output. If no `tmp.dir` output will be written here, otherwise it will be written to `tmp.dir` first and then moved here. Default is '.'.
#' @param prepend character; any command you want run before the rest of the full command. Default is NULL.
#' @param src.tool.path character; path to script for sourcing tool, if required by system, this will also append the command `source <PATH> ; ` to the full commands. NULL means this is not prepended. Default is NULL.
#' @param zip.output logical; whether to gzip final output (occurs before moving files if `tmp.dir` is set). Default is TRUE.
#' @param write.to character; file name to write commands to, if NULL, only returns commands as character vector. Default is NULL
#' @param ... other commands to pass to appropriate tool. Names must match short or long version found in that tool's help page.
#' @seealso \code{\link{system}}, \code{\link{list2}}, \code{\link{generate.tool.command}}
#' @export

generate.full.commands <- function(
    input.dir = NULL,
    paired = TRUE,
    tmp.dir = NULL,
    output.dir = ".",
    prepend = NULL,
    src.tool.path = NULL,
    zip.output = TRUE,
    write.to = NULL,
    ...
) {
  require(magrittr)
  require(stringr)
  direct.out <- ifelse(is.null(tmp.dir), output.dir, tmp.dir)
  if (is.null(run.env$bin.path)) {
    r.cmd <- "Rscript"
  } else {
    r.cmd <- file.path(run.env$bin.path, "Rscript")
  }
  commands <- sapply(run.env$samples, function(sample) {
    files <- list.files(path = input.dir, pattern = sample, full.names = T)
    cmds <- prepend
    if (paired & length(files) != 2) {
      rlang::abort(
        paste("Argument `paired' set to TRUE, but only 1 file detected for sample", sample)
      )
    } else if (paired) {
      cmds <- generate.tool.command(
        input = files[1],
        input = files[2],
        output = direct.out,
        tool.path = src.tool.path,
        ...
      ) %>% c(cmds, .)
    } else {
      cmds <- generate.tool.command(
        input = files[1],
        output = direct.out,
        tool.path = src.tool.path,
        ...
      ) %>% c(cmds, .)
    }
    if (length(cmds) == 2) {
      cmds <- paste(cmds, collapse = " ; ")
    }
    if (zip.output) {
      cmds <- c(
        cmds,
        paste0(
          r.cmd," -e \"metaGTx.processing::tgz.directories(location='", direct.out,
          "', match.pattern='", sample, "')\""
        ),
        paste0(
          r.cmd, " -e \"metaGTx.processing::remove.directories(location='", direct.out,
          "', match.pattern='", sample, "')\""
        ),
        paste0(
          r.cmd, " -e \"metaGTx.processing::gzip.files(location='", direct.out,
          "', match.pattern='", sample, "')\""
        )
      )
    }
    if (!is.null(tmp.dir)) {
      cmds <- c(
        cmds,
        paste0(
          r.cmd, " -e \"metaGTx.processing::move.files(move.from='", tmp.dir,
          "', move.to='", output.dir,
          "', match.pattern='", sample, "')\""
        )
      )
    }
    paste(cmds, collapse = " && ") %>%
      return()
  })
  if (!is.null(write.to)) {
    writeLines(text = commands, con = write.to)
    rlang::inform(paste("Commands written to", write.to))
  }
  return(commands)
}
