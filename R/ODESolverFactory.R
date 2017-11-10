
#' ODESolverFactory
#'
#' ODESolverFactory helps to create a solver given only the name as string
#' @param ... additional parameters
#'
#' @rdname ODESolverFactory-class
#' @family ODESolver helpers
#' @export
.ODESolverFactory <- setClass("ODESolverFactory", slots = c(
    # A factory class that creates an ODESolver using a name
    solverName = "character"))


#' ODESolverFactory generic
#'
#' @rdname ODESolverFactory-class
#' @export
#' @example ./inst/examples/SHOApp.R
setGeneric("ODESolverFactory", function(...) standardGeneric("ODESolverFactory"))


#' @rdname ODESolverFactory-class
setGeneric("createODESolver", function(object, ...)
    standardGeneric("createODESolver"))

#' createODESolver
#'
#' This is a factory method that creates an ODESolver using a name.
#'
#' @param object an solver object
#' @param ode an ODE object
#' @param solverName the desired solver as a string
#'
#' @rdname ODESolverFactory-class
#' @family ODESolver helpers
#' @export
setMethod("createODESolver", "ODESolverFactory", function(object, ode, solverName, ...) {
    object@solverName <- trimws(tolower(solverName))
    if (object@solverName == "rk4")
        return(RK4(ode))
    else if (object@solverName == "dormandprince45")
        return(DormandPrince45(ode))
    else if (object@solverName == "rk45")
        return(DormandPrince45(ode))
    else if (object@solverName == "euler")
        return(Euler(ode))
    else if (object@solverName == "eulerrichardson")
        return(EulerRichardson(ode))
    else if (object@solverName == "verlet")
        return(Verlet(ode))
    else
        return(NULL)
})



#' ODESolverFactory constructor
#'
#' @rdname ODESolverFactory-class
#' @importFrom methods new
#' @export
#' @example ./inst/examples/SHOApp.R
setMethod("ODESolverFactory", signature("ANY"), function(...) {
    # constructor for .ODESolverFactory
    factory <- .ODESolverFactory()
    return(factory)
})
