# Euler. R

# /**
# * Euler implements an Euler method ODE solver.
# *
# * The Euler method is unstable for many systems.  It is included as an
# * example of how to use the ODE and ODESolver interface.
# *
# * @author              Wolfgang Christian
# * @version 1.0
# * converted to R by    Alfonso R> Reyes
# */


#' Euler class
#'
#' @param ode an ODE object
#' @param ... additional parameters
#'
#' @rdname Euler-class
#'
.Euler <- setClass("Euler",
            contains = c("AbstractODESolver")
        )

setMethod("initialize", "Euler", function(.Object, ode, ...) {
    tryCatch({
        if (missing(ode)) stop("ode param not supplied")
    }, error = "No ode param")

    # initialized the Euler ODE solver
    .Object@ode <- ode                          # set the ode to ODESolver slot
    .Object@ode@rate <- vector("numeric")       # create vector for the rate
    return(.Object)
})


#' @rdname init-method
#' @importFrom methods callNextMethod
setMethod("init", "Euler", function(object, stepSize, ...) {
    object <- callNextMethod(object, stepSize)           # call superclass init
    object@ode@rate <- vector("numeric", object@numEqn)  # make the rate vector
    invisible(object)                                               #   right dimensions
})


#' @rdname step-method
setMethod("step", signature(object = "Euler"), function(object, ...) {
    # step through the differential equation
    state <- getState(object@ode)                         # get the state
    rate  <- getRate(object@ode, state)  # get the rate

    for (i in 1:object@numEqn) {
        state[i] <- state[i] + object@stepSize * rate[i]  # calc the new state
    }
    object@ode@state <- state              # return state and rate for new iter
    object@ode@rate  <- rate
    invisible(object)                      # use this object to ressign in R

})


#' @rdname setStepSize-method
setMethod("setStepSize", "Euler", function(object, stepSize, ...) {
    # set the time step
    object@stepSize <-  stepSize
    invisible(object)
})

#' @rdname setStepSize-method
setMethod("getStepSize", "Euler", function(object, ...) {
    return(object@stepSize)
})


#' Euler constructor when `ODE` passed
#'
#' @rdname Euler-class
#'
#' @importFrom methods new
#' @export
#' @example ./inst/examples/FallingParticleODEApp.R
#' @example ./inst/examples/KeplerEuler.R
#' @example ./inst/examples/PlanetApp.R
#' @example ./inst/examples/RigidBodyNXFApp.R
setMethod("Euler", signature(ode = "ODE"), function(ode, ...) {
    .euler <- .Euler(ode = ode)
    .euler <- init(.euler, .euler@stepSize)
    return(.euler)
})

#' Euler constructor `missing` is passed
#'
#' @rdname Euler-class
#'
#' @importFrom methods new
setMethod("Euler", signature(ode = "missing"), function(ode, ...) {
    # use this constructor when no ODE object is passed
    if (missing(ode)) {
        ode <- new("ODE")
        warning("No ODE supplied. Using an empty one!")
    }
    .euler <- .Euler(ode = ode)
    .euler <- init(.euler, .euler@stepSize)
    return(.euler)
})
