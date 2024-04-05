#' Convert annmatrix Objects to and from Other Types
#'
#' Methods for turning R objects to class annmatrix and vice versa.
#'
#' \code{as.annmatrix} will attempt to convert an object to annmatrix.
#'
#' \code{as.matrix} will turn an \code{annmatrix} object into a regular matrix.
#'
#' \code{is.annmatrix} checks if the object is an instance of \code{annmatrix}.
#'
#' @param x an R object.
#' @param ... additional arguments to be passed to or from methods.
#'
#' @return \code{is.annmatrix} returns TRUE if object is of class 'annmatrix' and FALSE otherwise.
#'         \code{as.annmatrix} methods return an object of class 'annmatrix'.
#'         \code{as.matrix} returns a regular matrix.
#'
#' @examples
#' # construct annmatrix object
#' x <- matrix(rnorm(20*10), 20, 10)
#' X <- as.annmatrix(x)
#'
#' X$group  <- rep(c("case", "control"), each = 5)
#' X$gender <- sample(c("M", "F"), 10, replace = TRUE)
#' X@chr    <- sample(c("chr1", "chr2"), 20, replace = TRUE)
#' X@pos    <- runif(20, 0, 1000000)
#'
#' is.matrix(x)
#' is.matrix(X)
#'
#' is.annmatrix(x)
#' is.annmatrix(X)
#'
#' as.matrix(X)
#'
#' @author Karolis Koncevičius
#' @name convert
#' @export
as.annmatrix <- function(x) {
  UseMethod("as.annmatrix")
}

#' @rdname convert
#' @export
as.annmatrix.default <- function(x) {
  if(is.null(x)) {
    annmatrix()
  } else {
    stop("no applicable method for converting an object of class '", class(x)[1], "' to annmatrix")
  }
}

#' @rdname convert
#' @export
as.annmatrix.matrix <- function(x) {
  annmatrix(x)
}

#' @rdname convert
#' @export
as.annmatrix.annmatrix <- function(x) {
  x
}

#' @rdname convert
#' @export
as.matrix.annmatrix <- function(x, ...) {
  attr(x, ".annmatrix.rann") <- NULL
  attr(x, ".annmatrix.cann") <- NULL
  unclass(x)
}

#' @rdname convert
#' @export
is.annmatrix <- function(x) {
  inherits(x, "annmatrix")
}

