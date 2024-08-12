#' Update and Reinstall Package
#'
#' This function automates the process of updating and reinstalling the package during development.
#'
#' @param pkg The name of the package
#' @param restart Whether to restart the R session after reinstalling (default: FALSE)
#'
#' @export
update_package <- function(pkg = "contdid", restart = FALSE) {
  if (pkg %in% (.packages())) {
    detach(paste0("package:", pkg), unload = TRUE, character.only = TRUE)
  }

  remove.packages(pkg)

  Rcpp::compileAttributes()
  devtools::clean_dll()
  devtools::build()
  devtools::install()

  if (restart) {
    .rs.restartR()
  } else {
    library(pkg, character.only = TRUE)
  }

  cat(paste0("Package '", pkg, "' has been updated and reinstalled.\n"))
}
