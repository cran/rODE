
#' AbstractODESolver class
#'
#' Defines the basic methods for all the ODE solvers.
#'
#' Inherits from: ODESolver class
#'
#' @param ode an ODE object
#' @param object a class object
#' @param stepSize the size of the step
#' @param value the step size value
#' @param ... additional parameters
#'
#' @rdname AbstractODESolver-class
#'
#' @include ODE.R ODESolver.R ode_generics.R
.AbstractODESolver <- setClass("AbstractODESolver",
                        slots = c(
                            stepSize = "numeric",
                            numEqn   = "numeric",
                            ode      = "ODE"),
                        prototype = prototype(
                            stepSize = 0.1,
                            numEqn = 0),
                    contains = c("ODESolver")
                    )

#' AbstractODESolver generic
#'
#' @rdname AbstractODESolver-class
#' @export
setGeneric("AbstractODESolver", function(ode, ...)  standardGeneric("AbstractODESolver"))


setMethod("initialize", "AbstractODESolver", function(.Object, .ode, ...) {
    .Object <- init(.Object, 0.1)                   # default value for stepSize
    return(.Object)
})


#' @rdname AbstractODESolver-class
setMethod("step", "AbstractODESolver", function(object, ...) {
    object
})


#' @rdname AbstractODESolver-class
setMethod("getODE", "AbstractODESolver", function(object, ...) {
    object@ode
})


#' @rdname AbstractODESolver-class
setMethod("setStepSize", "AbstractODESolver", function(object, stepSize, ...) {
    object@stepSize = stepSize
    object
})


#' @rdname AbstractODESolver-class
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


#' @rdname AbstractODESolver-class
setReplaceMethod("init", "AbstractODESolver", function(object, ..., value) {
    stepSize <- value
    object@stepSize <- stepSize
    state <- getState(object@ode)
    if (is.null(state)) {
        object@numEqn <-  0
    } else {
        object@numEqn = length(state)
    }
    object
})


#' @rdname AbstractODESolver-class
setMethod("getStepSize", "AbstractODESolver", function(object, ...) {
    return(object@stepSize)
})



#' AbstractODESolver
#'
#' AbstractODESolver constructor missing
#'
#' @rdname AbstractODESolver-class
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


#' AbstractODESolver
#'
#' AbstractODESolver constructor ODE. Uses this constructor when ODE object is passed
#'
#' @rdname AbstractODESolver-class
#'
#' @importFrom methods new
#' @export
#' @examples
#' # This is how we start defining a new ODE solver: Euler
#' .Euler <- setClass("Euler",              # Euler solver very simple; no slots
#'      contains = c("AbstractODESolver"))
#'
#'
#'
#' # Here we define the ODE solver Verlet
#' .Verlet <- setClass("Verlet", slots = c(
#'     rate1 = "numeric",                          # Verlet calculates two rates
#'     rate2 = "numeric",
#'     rateCounter = "numeric"),
#' contains = c("AbstractODESolver"))
#'
#'
#'
#' # This is the definition of the ODE solver Runge-Kutta 4
#' .RK4 <- setClass("RK4", slots = c(       # On the other hand RK4 uses 4 rates
#'    rate1 = "numeric",
#'    rate2 = "numeric",
#'    rate3 = "numeric",
#'    rate4 = "numeric",
#'    estimated_state = "numeric"),         # and estimates another state
#' contains = c("AbstractODESolver"))
#'
setMethod("AbstractODESolver", signature(ode = "ODE"), function(ode, ...) {
    odesolver <- .AbstractODESolver(ode = ode)
    odesolver@ode <- ode
    odesolver
})
