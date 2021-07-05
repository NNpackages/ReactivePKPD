#' The NNPKPD-package enviroment
#'
#' An environment for various NNPKPD package objects for session caching and
#' package parameters. Used internally by the NNPKPD package.
#'
#' @details
#' This environment is used by the shiny application \code{\link{runPKPDreview}}.
#'
#' Using an environment for this is useful as we do not
#' accidentially create copies of the environment.
#'
#' @aliases nnr_environment nnr_env
#' @keywords internal
pkpd_environment <- new.env()


#' Working with the PKPD environment
#'
#' @param x The variable name to either get, set or remove from the PKPD
#'   environement. For Getting and setting variables x is a variable name, given as
#'   a character string. No coercion is done, and the first element of a
#'   character vector of length greater than one will be used, with a warning.
#'   For remove it can be a vector of characters of the names that should be removed.
#' @param value A value to be assigned to x
#' @param remove logical indicating if the objects given in x should be removed.
#' @param clear_environment Shoud the environment be cleared completely
#' @param verbose Should the function be verbose.
#' @param ... Aditional arguments passed on to lower level functions.
#'
#' @return Gets, sets, or removes the elements supplied in x from the environment
#' @export
#'
#' @examples
#'
#' # Assign the value 2 to object a
#' PKPDEnv("a", 2, verbose = TRUE)
#'
#' # Get the value assigned to object a
#' PKPDEnv("a", verbose = TRUE)
#'
#' # Remove object "a" from the environment
#' PKPDEnv("a", remove = TRUE, verbose = TRUE)
#'
#' # When the objects are not found
#' PKPDEnv("a", verbose = TRUE)
#' PKPDEnv("a", remove = TRUE, verbose = TRUE)
#'
#' # assign and clear the environement
#' PKPDEnv("a", 2)
#' PKPDEnv("b", 3)
#'
#' # What does the environment contain
#' PKPDEnv()
#'
#' # Clear the environment
#' PKPDEnv(clear_environment = TRUE, verbose = TRUE)
#' PKPDEnv()
PKPDEnv <- function(x, value, remove = FALSE,
                    clear_environment = FALSE,
                    verbose = FALSE, ...) {

  if (!missing(x) & missing(value) & !remove & !clear_environment) {
    if (verbose) cat(paste("Getting", x, "\n"))
    if (x %in% ls(envir = pkpd_environment)) {
      return(get(x = x, envir = pkpd_environment, inherits = FALSE, ...))
    } else {
      if (verbose) message("variable ", x, " not found")
      return(NULL)
    }
  }

  if (!missing(value)) {
    if (verbose) cat(paste("Setting", x))
    return(assign(x, value, envir = pkpd_environment, ...))
  }

  if (remove) {
    if (any(x %in% ls(envir = pkpd_environment))) {
      if (verbose) cat(paste("Removed objects:\n", paste(x[x %in% ls(envir = pkpd_environment)], collapse = ",\n")))
      rm(list = x[x %in% ls(envir = pkpd_environment)], envir = pkpd_environment, ...)
    } else {
      if (verbose) message("No objects removed")
    }
  }

  if (clear_environment) {
    if (length(ls(envir = pkpd_environment))) {
      if (verbose) cat("NNPKPD environment cleared for:\n", ls(envir = pkpd_environment))
      rm(list = ls(envir = pkpd_environment), envir = pkpd_environment, ...)
    } else {
      if (verbose) message("No variables found in environment")
    }
  }

  if (missing(x) & !clear_environment) {
    ls(envir = pkpd_environment)
  }
}



