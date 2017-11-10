
#' ODE class
#'
#' Defines an ODE object for any solver
#'
#' @param object a class object
#' @param state current state
#' @param ... additional parameters
#'
#' @rdname ODE-class
#' @include ode_generics.R
#' @example ./inst/examples/PendulumApp.R
#' @example ./inst/examples/PendulumEulerApp.R
setClass("ODE", slots = c(
    state = "numeric",              # variables
    rate  = "numeric"               # derivatives
))

#' ODE constructor
#'
#' @rdname ODE-class
#' @export
ODE <- function() {
    ode <- new("ODE")
    ode
}


#' @rdname ODE-class
#' @aliases getState,getState-method
setMethod("getState", "ODE", function(object, ...) {
    # Gets the state variables.
    return(object@state)
})

#' @rdname ODE-class
#' @aliases getRate,getRate-method
#' @example ./inst/examples/KeplerApp.R
#' @example ./inst/examples/FallingParticleODE.R
setMethod("getRate", "ODE", function(object, state, ...) {
    # Gets the rate of change using the argument's state variables.
    return(object@rate)
})


# setReplaceMethod("setSolver", "ODE", function(object, value) {
#     object@odeSolver <- value
#     object
# })


