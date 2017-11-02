#' @name existsAndNotNULL
#' @title Tests whether an object exists and is non-null
#' @param var Object name as character string
#' @param envir Environment in which to check
#' @param inherits Passed to \code{inherits} argument of \code{\link{exists}}.
#' @return TRUE if the object both exists and is not NULL. FALSE otherwise.
#' @export
#' @author Joshua Keller
#' @seealso \code{\link{exists}}
existsAndNotNULL <- function(var, envir=parent.frame(), inherits=TRUE){
    varcall <- parse(text=var)
    exists(var, envir=envir, inherits=inherits) && !is.null(eval(varcall, envir=envir))
}

#' @name setDefault
#' @title Sets variable to default value
#' @param var Name of object to set to default value. 
#' @param default Named list of default values.  The name of one (and only one) element must match the provided string in \code{var}.
#' @param envir Environment in which to set the value.
#' @export
#' @author Joshua Keller
#' @seealso assign
setDefault <- function(var, defaults, envir=parent.frame(), verbose=TRUE){
    if (length(var)>1) stop("'var' must have length 1.")
    if (!is.character(var)) stop("'var' must be a character string.")
    assignval <- defaults[[var]]
    if (verbose) cat("Assigning ", var, " to default of ", assignval, "\n")
    assign(var, assignval, envir=envir)
}

#' @name check_and_set_Default
#' @rdname  setDefault
#' @details \code{check_and_set_Default} First checks to see if a variable has been set to a value. If not, it calls \code{setDefaul} to set to default value.
#' @export
check_and_set_Default <- function(var, defaults=NULL, envir=parent.frame(), inherits=FALSE, verbose=TRUE){
    donothing <- existsAndNotNULL(var, envir=envir, inherits=inherits)
    if (!donothing){
        setDefault(var, defaults, envir=envir, verbose=verbose)
    }
    invisible(get(var, envir=envir));
}



