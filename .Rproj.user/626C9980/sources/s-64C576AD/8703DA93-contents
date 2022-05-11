#' Install Chocophlan Database
#'
#' This function installs the chocophlan database for using with Humann3.
#'@param location character; path to install location.
#'@seealso \code{\link{system}}, \code{\link{list2}}
#'@export

install.chocophlan <- function(location) {
  cmd <- paste("humann_databases --update-config yes --download chocophlan full", location)
  system(cmd)
}
