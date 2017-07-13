# ODE.R

#
# ODE base class
#
# Inherited by: ODE solver objects

#' Defines an ODE object for any solver
#'
#'
#' @include ode_generics.R
#' @example ./inst/examples/PendulumApp.R
#' @example ./inst/examples/PendulumEulerApp.R
setClass("ODE", slots = c(
    state = "numeric",              # variables
    rate  = "numeric"               # derivatives
))

#' @rdname getState-method
setMethod("getState", "ODE", function(object, ...) {
    # Gets the state variables.
    return(object@state)
})

#' @rdname getRate-method
#' @example ./inst/examples/KeplerApp.R
#' @example ./inst/examples/FallingParticleODE.R
setMethod("getRate", "ODE", function(object, state, ...) {
    # Gets the rate of change using the argument's state variables.
    return(object@rate)
})
