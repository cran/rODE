# ODESolverFactory.R

###' include ODE.R Euler.R
###'

#' ODESolverFactory helps to create a solver given only the name as string
#'
#' @rdname ODESolverFactory-class
#' @export
.ODESolverFactory <- setClass("ODESolverFactory", slots = c(
    # A factory class that creates an ODESolver using a name
    solverName = "character"))

#' @rdname createODESolver-method
setGeneric("createODESolver", function(object, ...)
    standardGeneric("createODESolver"))

#' This is a factory method that creates an ODESolver using a name.
#' @rdname createODESolver-method
#' @param object an solver object
#' @param ode an ODE object
#' @param solverName the desired solver as a string
#' @param ... an additional parameter
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
    else if (object@solverName == "verlet")
        return(Verlet(ode))
    else
        return(NULL)
})


#' ODESolverFactory constructor
#'
#' @rdname ODESolverFactory-class
#' @param ... additional parameters
#' @importFrom methods new
#' @export
#' @example ./inst/examples/SHOApp.R
setMethod("ODESolverFactory", signature("ANY"), function(...) {
    # constructor for .ODESolverFactory
    factory <- .ODESolverFactory()
    return(factory)
})
