#' @name create.processing.env
#' @title Create processing environment
#' @description This function creates a new environment called `run.env` to store all user-set variables with the option to save as an RDA file for easy reading-in to a future session.
#' @param base.dir character; path to working directory. It is helpful to make this explicit (rather than using '.') for generating SGE_Array commands.
#' @param raw.seq.dirs character; path to raw FASTQs directory **or** a *named* vector of paths to raw FASTQs directories. If a vector, `symlink.raw.fastqs` will preppend name of path to sample names to keep them distinct.
#' @param interactive logical; TRUE = run command interactively, no swarm computing possible, FALSE = generate SGE_Batch or SGE_Array commands to submit manually. Default is FALSE
#' @param job.queue character; the queue to that jobs will be sent to, for generating SGE commands. Default is NULL.
#' @param jobs.dir character; path to write command files to for submission to the scheduler from the appropriate machine, e.g., "/home/micro/stagamak/Jobs". Default is ".".
#' @param max.cores integer; maximum cores to take advantage of (when utilizing swarm processing this number will be divided by max.concurrent jobs to determine the per-job number of cores). Default is 1.
#' @param max.concurrent.jobs integer; maximum concurrent jobs to run. If `interactive` == TRUE, this will be forced to 1. Default is 1.
#' @param link.dir character; path to directory wherein you want to make symlinks to raw FASTQs. If set to NULL, will use the `base.dir`. Default is NULL.
#' @param store.dir character; path to directory where all output will be stored. If set to NULL, will use the `base.dir`. Default is NULL.
#' @param temp.dir character; path to a directory where output will first be written before moving to the permanent output directory, if NULL, output is written directly to `store.dir`. Default is NULL.
#' @param max.memory character; maximum memory to set for jobs submitted to SGE_Batch or SGE_Array. Must take the form of e.g. "10G" or "50G" Setting to NULL means this parameter is not specified in the command. Default is NULL.
#' @param qsub.options character; further qsub arguments you want included in the SGE_Batch or SGE_Array commands, e.g., get email at end of job with "'-m ae -M yourname@example.com'". This argument must be in double and single quotes like example to be parsed correctly. Setting to NULL means this parameter i snot specified in the command. Default is NULL.
#' @param save.env.dir character; if not NULL, will create and save this environment in the provided directory (if directory does not already exist, it will be created in `base.dir`) with the date and time of creation in the file name. Default is NULL.
#' @param ... additional variables you want to set here
#' @seealso \code{\link{assign}}, \code{\link{new.env}}, \code{\link{call_match}}, \code{\link{symlink.raw.fastqs}}
#' @export

run.env <- new.env()

#' @export

create.processing.env <- function(
    base.dir,
    raw.seq.dirs,
    interactive = FALSE,
    job.queue = NULL,
    jobs.dir = ".",
    max.cores = 1,
    max.concurrent.jobs = 1,
    link.dir = NULL,
    store.dir = NULL,
    temp.dir = NULL,
    max.memory = NULL,
    qsub.options = NULL,
    save.env.dir = NULL,
    ...
) {
  require(magrittr)
  require(stringr)
  vars <- as.list(rlang::call_match(defaults = TRUE))[-1]
  # print(run.env)
  for (i in seq_along(vars)) {
    other.arg.match <- paste(vars[[i]]) %>%
      sapply(function(x) { str_detect(x, paste(names(vars), collapse = "|")) }) %>%
      any()
    if (other.arg.match) {
      var.value <- eval(vars[[i]])
    } else {
      var.value <- vars[[i]]
    }
    assign(x = names(vars)[i], value = var.value, envir = run.env)
  }
  if (!is.null(save.env.dir)) {
    if (!dir.exists(save.env.dir)) {
      dir.create(file.path(base.dir, save.env.dir))
    }
    date.time <- Sys.time() %>%
      str_replace_all(" ", "_") %>%
      str_replace_all(":", "-")
    save(
      run.env,
      file = file.path(
        save.env.dir,
        paste0("metaGTx_processing_environment_", date.time, ".rda")
      )
    )
  }
}
