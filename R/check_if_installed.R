#' Checking if needed package is installed

#' @param package A string naming the package, whose installation needs to be
#'   checked in any of the libraries
#' @param reason A phrase describing why the package is needed. The default is a
#'   generic description.
#' @param stop Logical that decides whether the function should stop if the
#'   needed package is not installed.
#' @param ... Currently ignored
#'
#' @noRd

# TODO: delete this function after insight version "> 14.0" is on CRAN
check_if_installed <- function(package,
                               reason = "for this function to work",
                               stop = TRUE,
                               ...) {
  # does it need to be displayed?
  is_installed <- requireNamespace(package, quietly = TRUE)
  if (!is_installed) {
    # prepare the message
    message <- paste0(
      "Package '", package, "' is required ", reason, ".\n",
      "Please install it by running install.packages('", package, "')."
    )

    if (stop) stop(message, call. = FALSE) else warning(message, call. = FALSE)
  }
  invisible(is_installed)
}
