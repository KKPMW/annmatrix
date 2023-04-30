#' annmatrix Objects and Basic Functionality
#'
#' Annotated matrix is a regular matrix with additional functionality for
#' attaching persistent information about row and column entries. Annotations
#' associated with rows and columns are preserved after subsetting,
#' transposition, and various other matrix-specific operations. Intended use
#' case is for storing and manipulating genomic datasets that typically
#' consist of a matrix of measurements (like gene expression values) as well as
#' annotations about rows (i.e. genomic locations) and annotations about
#' columns (i.e. meta-data about collected samples). But \code{annmatrix}
#' objects are also expected be useful in various other contexts.
#'
#' \code{as.annmatrix()} converts a matrix to an \code{annmatrix} object. The
#' function expects \code{x} to be a \code{matrix} and \code{rowanns} and
#' \code{colanns} to be of class \code{data.frame}. If the passed objects are
#' of a different class they will be converted via the use of \code{as.matrix}
#' and \code{as.data.frame}.
#'
#' \code{is.annmatrix} checks if the object is an instance of \code{annmatrix}.
#'
#' \code{as.matrix} will turn an \code{annmatrix} object into a regular matrix.
#'
#' \code{X[i,j]} returns a selected subset of annmatrix object. Row and column
#' annotations are preserved and subsetted where needed. In the special case
#' when only one column or row is selected in order to be consistent with the
#' \code{matrix} behavior the dimensions of matrix are dropped and a vector is
#' returned. Just like in the case of matrices the additional argument
#' \code{drop=FALSE} can be provided in order to return a proper matrix
#' instead.
#'
#' \code{rowanns} and \code{colanns} returns the selected field from column and
#' row annotations respectively. When the selected field is not specified the
#' whole annotation \code{data.frame} is returned.
#'
#' \code{@} and \code{$} are convenience shortcuts for selecting annotations.
#' \code{X@value} selects an existing column from row annotations while
#' \code{X$value} selects a column from column annotations. An empty selection
#' of \code{X@''} and \code{X$''} will return the whole annotation data.frame
#' for rows and columns respectively.
#'
#' \code{rowanns<-} and \code{colanns<-} functions can be used to replace the
#' column and row annotations respectively. When the selected field is not
#' specified the whole annotation \code{data.frame} is replaced.
#'
#' \code{@<-} and \code{$<-} are convenience shortcuts for the above (see
#' Examples). A replacement of an empty value - \code{X@'' <- df} and
#' \code{X$'' <- df} will replace the whole annotation data.frame.
#'
#' @param x,object an R object.
#' @param rann annotation \code{data.frame} for rows of the \code{annmatrix} object.
#' @param cann annotation \code{data.frame} for columns of the \code{annmatrix} object.
#' @param i subset for rows.
#' @param j subset for columns.
#' @param drop if TRUE (default) subsetting a single row or column will returned a vector.
#' @param names a character vector of existing row/column annotation names.
#' @param name a name of an existing row/column annotation.
#' @param value a value that will be assigned to row/column annotation field.
#' @param ... further arguments passed to or from methods.
#'
#' @examples
#' # construct annmatrix object
#' x <- matrix(rnorm(20*10), 20, 10)
#'
#' coldata <- data.frame(group  = rep(c("case", "control"), each = 5),
#'                       gender = sample(c("M", "F"), 10, replace = TRUE))
#'
#' rowdata <- data.frame(chr = sample(c("chr1", "chr2"), 20, replace = TRUE),
#'                       pos = runif(20, 0, 1000000))
#'
#' X <- as.annmatrix(x, rowdata, coldata)
#'
#' is.matrix(x)
#' is.matrix(X)
#'
#' is.annmatrix(x)
#' is.annmatrix(X)
#'
#' # manipulating annotations without using shortcuts
#' rowanns(X)
#' colanns(X)
#'
#' rowanns(X, "chr")
#' rowanns(X, "gene") <- letters[1:20]
#' rowanns(X, c("chr", "gene"))
#' rowanns(X, "gene") <- NULL
#' rowanns(X)
#'
#' colanns(X, "group")
#' colanns(X, "age") <- 1:10*10
#' colanns(X, "age")
#' colanns(X, "age") <- NULL
#' colanns(X, "age")
#'
#' # more convenient
#' X@''
#' X@chr
#' X@gene <- letters[1:20]
#' X@gene
#' X@gene <- NULL
#' X@gene
#'
#' X$''
#' X$group
#' X$age <- 1:10*10
#' X$age
#' X$age <- NULL
#' X$age
#' X$'' <- data.frame(id = 1:10, name = LETTERS[1:10])
#' X$name
#'
#' # annotations are preserved after subsetting
#' Y <- X[X@chr == "chr1", X$name %in% c("A", "B", "C")]
#' Y@chr
#' Y$''
#'
#' Y[, 1]
#' Y[, 1, drop = FALSE]
#'
#' @author Karolis Koncevičius
#' @name annmatrix
#' @export
as.annmatrix <- function(x, rann, cann) {

  if (missing(x)) {
    x <- matrix(nrow = 0, ncol = 0)
  } else {
    x <- as.matrix(x)
  }

  if (missing(rann) || is.null(rann)) {
    rann <- data.frame(row.names = seq_len(nrow(x)))
  } else if (is.vector(rann)) {
    rann <- as.data.frame(rann)
    names(rann) <- deparse(substitute(rann))
  } else {
    rann <- as.data.frame(rann)
  }

  if (missing(cann) || is.null(cann)) {
    cann <- data.frame(row.names = seq_len(ncol(x)))
  } else if (is.vector(cann)) {
    cann <- as.data.frame(cann)
    names(cann) <- deparse(substitute(cann))
  } else {
    cann <- as.data.frame(cann)
  }

  if (nrow(x) != nrow(rann)) {
    stop("Number of 'rann' rows must match the number of rows in 'x'")
  }
  if (ncol(x) != nrow(cann)) {
    stop("Number of 'cann' rows must match the number of columns in 'x'")
  }

  structure(x, class = c("annmatrix", oldClass(x)), .annmatrix.rann = rann, .annmatrix.cann = cann)
}

#' @rdname annmatrix
#' @export
is.annmatrix <- function(x) {
  inherits(x, "annmatrix")
}

#' @rdname annmatrix
#' @export
as.matrix.annmatrix <- function(x, ...) {
  attr(x, ".annmatrix.rann") <- NULL
  attr(x, ".annmatrix.cann") <- NULL
  unclass(x)
}

#' @rdname annmatrix
#' @export
`[.annmatrix` <- function(x, i, j, ..., drop = TRUE) {
  mat <- NextMethod("[")

  if (is.matrix(mat)) {

    if (missing(i)) {
      attr(mat, ".annmatrix.rann") <- attr(x, ".annmatrix.rann")
    } else {
      if (is.character(i)) i <- match(i, rownames(x))
      attr(mat, ".annmatrix.rann") <- attr(x, ".annmatrix.rann")[i,,drop = FALSE]
    }

    if (missing(j)) {
      attr(mat, ".annmatrix.cann") <- attr(x, ".annmatrix.cann")
    } else {
      if (is.character(j)) j <- match(j, colnames(x))
      attr(mat, ".annmatrix.cann") <- attr(x, ".annmatrix.cann")[j,,drop = FALSE]
    }

    class(mat) <- append("annmatrix", oldClass(mat))
  }

  mat
}

#' @rdname annmatrix
#' @export
rowanns <- function(x, names) {
  if (missing(names)) {
    attr(x, ".annmatrix.rann")
  } else if (length(names) == 1) {
    attr(x, ".annmatrix.rann")[[names]]
  } else {
    attr(x, ".annmatrix.rann")[,names]
  }
}

#' @rdname annmatrix
#' @export
`rowanns<-` <- function(x, names, value) {
  rann <- attr(x, ".annmatrix.rann")

  if (missing(names)) {
    if (is.null(value)) {
      rann <- data.frame(row.names = 1:nrow(x))
    } else if (!is.data.frame(value)) {
      stop("row annotations should be in a data.frame")
    } else if (nrow(value) != nrow(x)) {
      stop("new row annotation data should have the same number of rows as there are rows in the matrix")
    } else {
      rann <- value
    }
  } else {
    rann[,names] <- value
  }

  attr(x, ".annmatrix.rann") <- rann
  x
}

#' @rdname annmatrix
#' @export
colanns <- function(x, names) {
  if (missing(names)) {
    attr(x, ".annmatrix.cann")
  } else if (length(names) == 1) {
    attr(x, ".annmatrix.cann")[[names]]
  } else {
    attr(x, ".annmatrix.cann")[,names]
  }
}


#' @rdname annmatrix
#' @export
`colanns<-` <- function(x, names, value) {
  cann <- attr(x, ".annmatrix.cann")

  if (missing(names)) {
    if (is.null(value)) {
      cann <- data.frame(row.names = 1:ncol(x))
    } else if (!is.data.frame(value)) {
      stop("column annotations should be in a data.frame")
    } else if (nrow(value) != ncol(x)) {
      stop("new column annotation data should have the same number of rows as there are columns in the matrix")
    } else {
      cann <- value
    }
  } else {
    cann[,names] <- value
  }

  attr(x, ".annmatrix.cann") <- cann
  x
}
#' @rdname annmatrix
#' @export
`@.annmatrix` <- function(object, name) {
  if (nchar(name) == 0) {
    rowanns(object)
  } else {
    rowanns(object, name)
  }
}

#' @rdname annmatrix
#' @export
`@<-.annmatrix` <- function(object, name, value) {
  if (nchar(name) == 0) {
    rowanns(object) <- value
  } else {
    rowanns(object, name) <- value
  }
  object
}

#' @rdname annmatrix
#' @export
`$.annmatrix` <- function(x, name) {
  if (nchar(name) == 0) {
    colanns(x)
  } else {
    colanns(x, name)
  }
}

#' @rdname annmatrix
#' @export
`$<-.annmatrix` <- function(x, name, value) {
  if (nchar(name) == 0) {
    colanns(x) <- value
  } else {
    colanns(x, name) <- value
  }
  x
}

