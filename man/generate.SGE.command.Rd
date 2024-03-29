% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/generage_SGE_command.R
\name{generate.SGE.command}
\alias{generate.SGE.command}
\title{Generate SGE command}
\usage{
generate.SGE.command(c, ...)
}
\arguments{
\item{c}{character; a string containing the command you want to submit (SGE_Batch) OR a path and name to a file containg commands (SGE_Array).}

\item{...}{other commands to pass SGE_Batch/SGE_Array. Names must match short or long version found in that tool's help page.}
}
\description{
This function generates a character string of a command for SGE_Batch or SGE_Array and its arguments.
}
\seealso{
\code{\link{generate.full.commands}}
}
