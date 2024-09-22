#' Bind several annmatrix Objects Together
#'
#' Implementation of \code{rbind} and \code{cbind} methods for annmatrix objects.
#'
#' All the inputs are bound together in exact same way as if all the annmatrix objects were regular matrices.
#' Then, the row and column annotations of the supplied annmatrix objects are merged together.
#' Missing annotations are filled in using 'NA' values.
#'
#' For demonstration purposes only \code{rbind} will be described with \code{cbind} behaving accordingly.
#'
#' 1) Matrix.
#' The obtained matrix will be exactly the same as calling \code{rbind} with all annmatrix objects replaced by regular matrices.
#'
#' 2) Column annotations.
#' When \code{rbind} is called the matrices are all assumed to have the same set of columns.
#' Hence, the column annotations are assumed to be shared between all provided annmatrix objects.
#' Thus, in order to retain all possible column annotations, they are merged together.
#' This way any column annotation field present in at least one of the provided annmatrix objects will be present in the final result.
#' In case of conflicts, when the same annotation field is present in multiple annmatrix objects but contains different values, the first occuring instance is selected and an appropriate warning is displayed.
#' Non-annmatrix objects are assumed to share the column annotations present in supplied annmatrix objects.
#'
#' 3) Row annotations.
#' When \code{rbind} is called the matrices are assumed to have a separate unique set of rows.
#' Hence no conflicts between annotation values are possible for row annotations.
#' In order to retain all possible row annotations, row annotations are merged together.
#' Thus, the resulting row annotation data frame will have as many fields as there were unique row annotation fields among all the provided annmatrix objects.
#' Unlike with column annotations, if a particular annmatrix only had a subset of the final collection of annotation fields, then the missing fields are added and the annotation is filled with NA values.
#' All the rows associated with non-annmatrix objects will have missing (NA) values for all the annotation fields.
#'
#' @param ... (generalized) vector or matrix objects
#'
#' @return a single annmatrix object resulting from binding all the inputs together
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
#' X <- annmatrix(x, rowdata, coldata)
#'
#' X1 <- X[1:10,]
#' X2 <- X[11:20,]
#' all.equal(X, rbind(X1, X2))
#'
#' X1 <- X[,1:5]
#' X2 <- X[,6:10]
#' all.equal(X, cbind(X1, X2))
#'
#' X11 <- X[1:10, 1:5]
#' X21 <- X[11:20, 1:5]
#' X12 <- X[1:10, 6:10]
#' X22 <- X[11:20, 6:10]
#' all.equal(X, cbind(rbind(X11, X21), rbind(X12, X22)))
#'
#' @author Karolis Koncevičius
#' @name bind
#' @export
rbind.annmatrix <- function(...) {
  args  <- list(...)

  # collect all row and column annotations from annmatrix objects
  # then turn all annmatrix objects to regular matrices
  ranns <- vector(length(args), mode = "list")
  canns <- vector(length(args), mode = "list")
  for (i in 1:length(args)) {
    if (is.annmatrix(args[[i]])) {
      ranns[[i]] <- rowanns(args[[i]])
      canns[[i]] <- colanns(args[[i]])
      attr(args[[i]], ".annmatrix.rann") <- NULL
      attr(args[[i]], ".annmatrix.cann") <- NULL
      class(args[[i]]) <- setdiff(class(args[[i]]), "annmatrix")
    }
  }

  # call regular rbind on the arguments without any annmatrix objects
  res <- do.call(rbind, args)

  # we can only safely continue if the resulting object is a regular matrix
  if (all(class(res) == c("matrix", "array"))) {

    # 1. deal with column annotations
    # column annotations will just gain more columns collected from all annmatrix objects
    # so gather all of them as separate entries in a list
    canns <- unlist(canns, recursive = FALSE)

    # when two annmatrices have same column annotations we can safely remove duplicates
    canns <- canns[!duplicated(canns)]

    # but need check for entries with same names but different values
    # first construct a data.frame using only unique fields
    uniques <- !duplicated(names(canns))
    dups    <- canns[!uniques]
    canns   <- data.frame(canns[uniques])

    # in case conflicting entries are detected
    if (length(dups) > 0) {

      # first show a warning
      warning("conflicting annmatrix column annotations - using the ones that occurred first but re-writing missing values")

      # then check which entries in the original have NA values
      # we will only be re-writing those
      missnames <- names(which(colSums(is.na(canns)) > 0))
      dups <- dups[names(dups) %in% missnames]

      # if there is still something to do
      if (length(dups) > 0) {

        # go through each duplicated entry and rewrite missing values
        for (i in seq_along(dups)) {
          ann <- names(dups)[i]
          na  <- is.na(canns[[ann]])
          canns[[ann]][na] <- dups[[i]][na]
        }
      }

    }

    # 2. deal with row annotations
    # for rows we don't care about duplicates
    # this is because in rbind each row has only one entry of specified annotation
    # first get all the column names from non-empty annotations
    rnames <- unique(unlist(sapply(ranns, names)))

    # then go through the row annotations and adjust them
    for (i in 1:length(ranns)) {

      # if annotation is non-empty then expand it by filling NAs in new columns
      if (!is.null(ranns[[i]])) {
        ranns[[i]][,setdiff(rnames, names(ranns[[i]]))] <- NA

      # if annotation is empty then we need to create a data.frame filled with NAs
      # there will be 3 possibilities:
      # 1) argument was empty (length zero) - create data.frame with 0 rows
      # 2) argument has no dimensions, meaning  it was a vector - create data.frame with 1 rows
      # 3) argument had 2 dimensions - create data.frame with as many rows as there were in argument
      # 4) argument dimension size was above 2 - create data.frame with 1 rows
      } else {
        nr   <- 1
        if (length(args[[i]]) == 0) {
          nr <- 0
        } else if (length(dim(args[[i]])) == 2) {
          nr <- dim(args[[i]])[1]
        }
        ranns[[i]] <- data.frame(matrix(NA, nrow = nr, ncol = length(rnames), dimnames = list(NULL, rnames)))
      }

    }

    # finally bind all the row annotations into a single data.frame
    ranns <- do.call(rbind, ranns)

    res   <- annmatrix(res, rann = ranns, cann = canns)

  }

  res
}

#' @rdname bind
#' @export
cbind.annmatrix <- function(...) {
  args  <- list(...)

  # follows the same logic as rbind, look there for comments
  ranns <- vector(length(args), mode = "list")
  canns <- vector(length(args), mode = "list")
  for (i in 1:length(args)) {
    if (is.annmatrix(args[[i]])) {
      ranns[[i]] <- rowanns(args[[i]])
      canns[[i]] <- colanns(args[[i]])
      attr(args[[i]], ".annmatrix.rann") <- NULL
      attr(args[[i]], ".annmatrix.cann") <- NULL
      class(args[[i]]) <- setdiff(class(args[[i]]), "annmatrix")
    }
  }

  res <- do.call(cbind, args)

  if (all(class(res) == c("matrix", "array"))) {

    ranns <- unlist(ranns, recursive = FALSE)
    ranns <- ranns[!duplicated(ranns)]

    dups <- duplicated(names(ranns))
    if (any(dups)) {
      warning("conflicting annmatrix row annotations - using the ones that occurred first")
      ranns <- ranns[!dups]
    }

    ranns <- data.frame(ranns)

    cnames <- unique(unlist(sapply(canns, names)))

    for (i in 1:length(canns)) {
      if (!is.null(canns[[i]])) {
        canns[[i]][,setdiff(cnames, names(canns[[i]]))] <- NA
      } else {
        nc   <- 1
        if (length(args[[i]]) == 0) {
          nc <- 0
        } else if (length(dim(args[[i]])) == 2) {
          nc <- dim(args[[i]])[2]
        }
        canns[[i]] <- data.frame(matrix(NA, nrow = nc, ncol = length(cnames), dimnames = list(NULL, cnames)))
      }
    }

    canns <- do.call(rbind, canns)
    res   <- annmatrix(res, rann = ranns, cann = canns)

  }

  res
}
