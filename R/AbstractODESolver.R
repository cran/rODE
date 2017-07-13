# AbstractODESolver.R

#' AbstractODESolver class
#'
#' Defines the basic methods for all the ODE solvers.
#'
#' Inherits from: ODESolver class
#'
#' @param ode an ODE object
#' @param ... additional parameters
#'
#' @rdname AbstractODESolver-class
#'
#' @include ODE.R ODESolver.R ode_generics.R
.AbstractODESolver <- setClass("AbstractODESolver", slots = c(
                    stepSize = "numeric",
                    numEqn   = "numeric",
                    ode      = "ODE"
                ), prototype = prototype(
                    stepSize = 0.1,
                    numEqn = 0
                ), contains = c("ODESolver")
                )


setMethod("initialize", "AbstractODESolver", function(.Object, .ode, ...) {
    .Object <- init(.Object, 0.1)                   # default value for stepSize
    return(.Object)
})

#' @rdname step-method
setMethod("step", "AbstractODESolver", function(object, ...) {
    object
})


#' @rdname setStepSize-method
setMethod("setStepSize", "AbstractODESolver", function(object, stepSize, ...) {
    object@stepSize = stepSize
    object
})

#' @rdname init-method
setMethod("init", "AbstractODESolver", function(object, stepSize, ...) {
    object@stepSize <- stepSize
    state <- getState(object@ode)
    if (is.null(state)) {
        object@numEqn <-  0
    } else {
        object@numEqn = length(state)
    }
    object
})

#' @rdname getStepSize-method
setMethod("getStepSize", "AbstractODESolver", function(object, ...) {
    return(object@stepSize)
})



# constructor methods

#' AbstractODESolver constructor `missing``
#'
#' @param ode an ODE object
#' @param ...  additional parameters
#'
#' @importFrom methods new
#' @export
setMethod("AbstractODESolver", signature(ode = "missing"), function(ode, ...) {
    # use this method when no ODE object is passed
    if (missing(ode)) {
        ode <- new("ODE")
        warning("No ODE supplied. Using an empty one!")
    }
    odesolver <- .AbstractODESolver(ode = ode)
    odesolver@ode <- ode
    odesolver
})


#' AbstractODESolver constructor `ODE`
#'
#' Uses this constructor when ODE object is passed
#'
#' @param ode an ODE object
#' @param ...  additional parameters
#'
#' @importFrom methods new
setMethod("AbstractODESolver", signature(ode = "ODE"), function(ode, ...) {
    odesolver <- .AbstractODESolver(ode = ode)
    odesolver@ode <- ode
    odesolver
})
