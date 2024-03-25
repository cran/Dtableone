#' Install and Load Required Packages
#'
#' This function checks if a package is installed. If not, it stops and suggests
#' the user to install the package manually. Once installed, the package is loaded
#' into the R session.
#'
#' @param pkg A character string naming the package to be loaded.
#'
#' @return No return value. This function is called for its side effect of
#' loading a package into the R session. It does not attempt to install the package
#' automatically, relying instead on the user's action based on the provided
#' message.
#'
#' @examples
#' \dontrun{
#'   # To use this function, make sure the required package is already installed.
#'   # For example, to load the ggplot2 package, first ensure it's installed:
#'   # install.packages("ggplot2")
#'   loadPackage("ggplot2")
#' }
#'
#' @export
loadPackage <- function(pkg){
  if (!requireNamespace(pkg, quietly = TRUE)) {
    stop(paste("Package", pkg, "is not installed. Please install it using install.packages(\"", pkg, "\")", sep=""))
  }
  library(pkg, character.only = TRUE)
}
