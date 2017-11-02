#' @name existsAndNotNULL
#' @title Tests whether an object exists and is non-null
#' @param var Object name as character string
#' @param envir Environment in which to check
#' @param scrit Should only the specified environment be checked. Passed to \code{inherits} argument of \code{\link{exists}}.
#' @return TRUE if the object both exists and is not NULL. FALSE otherwise.
#' @export
#' @author Joshua Keller
#' @seealso \code{\link{exists}}
existsAndNotNULL <- function(var, envir=parent.frame(), strict=FALSE){
    varcall <- parse(text=var)
    exists(var, envir=envir, inherits=!strict) && !is.null(eval(varcall))
}

#' @name setDefault
#' @title Sets variable to default value
#' @param var Name of object to set to default value. 
#' @param default Named list of default values.  The name of one (and only one) element must match the provided string in \code{var}.
#' @param pos Environment in which assignment to \code{var} is done. Passed to \code{\link{assign}}.
#' @export
#' @author Joshua Keller
#' @seealso assign
setDefault <- function(var, defaults, pos=parent.frame()){
    if (length(var)>1) stop("'var' must have length 1.")
    if (!is.chracter(var)) stop("'var' must be a character string.")
    assignval <- defaults[[var]]
    cat("Assigning ", var, " to default of ", assignval, "\n")
    assign(var, assignval, pos=pos)
}

#' @name check_and_set_Default
#' @rdname  setDefault
#' @details \code{check_and_set_Default} First checks to see if a variable has been set to a value. If not, it calls \code{setDefaul} to set to default value.
#' @export
check_and_set_Default <- function(var, defaults=NULL, pos=parent.frame()){
    donothing <- existsAndNotNULL(var)
    if (!donothing){
        setDefault(var, defaults, pos=pos)
    }
    invisible(get(var));
}



