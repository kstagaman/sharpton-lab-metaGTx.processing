% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/symlink_files.R
\name{symlink.files}
\alias{symlink.files}
\title{Create symlinks to files}
\usage{
symlink.files(file.dir, pattern, replacements = NULL)
}
\arguments{
\item{file.dir}{character; path to directory containing files you want to link to.}

\item{pattern}{character; matching pattern for files you want to link.}

\item{replacements}{list; a named list of patterns and replacements for file names, e.g. =list(to.replace = c("A", "B"), replace.by = c("a", "b")). Default is NULL.}
}
\description{
This function creates symlinks in the current directory to files in the provided directory.
}
\seealso{
\code{\link{system}}, \code{\link{str_split}}, \code{\link{list.files}}
}
