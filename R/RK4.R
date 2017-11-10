
#' RK4 class
#'
#' @param object internal passing object
#' @param stepSize the size of the step
#' @param value value for the step
#' @rdname RK4-class
#' @include AbstractODESolver.R
#' @export
.RK4 <- setClass("RK4", slots = c(
          rate1 = "numeric",
          rate2 = "numeric",
          rate3 = "numeric",
          rate4 = "numeric",
          estimated_state = "numeric"
        ),
            contains = c("AbstractODESolver")
        )

#' RK4 generic
#'
#' @rdname RK4-class
#' @export
#' @example ./inst/examples/Projectile.R
#' @example ./inst/examples/PendulumApp.R
setGeneric("RK4", function(ode, ...) standardGeneric("RK4"))


setMethod("initialize", "RK4", function(.Object, ode, ...) {
    # initialize the class
    .Object@ode <- ode
    return(.Object)
})


#' @rdname RK4-class
#' @rdname init,init-methods
#' @importFrom methods callNextMethod
setMethod("init", "RK4", function(object, stepSize, ...) {
    # inititalize the solver
    object <- callNextMethod(object, stepSize)        # call superclass init

    # set the rate vectors to the number of equations
    object@rate1 <- vector("numeric", object@numEqn)  # make the rate vector
    object@rate2 <- vector("numeric", object@numEqn)  # make the rate vector
    object@rate3 <- vector("numeric", object@numEqn)  # make the rate vector
    object@rate4 <- vector("numeric", object@numEqn)  # make the rate vector

    object@estimated_state <- vector("numeric", object@numEqn)

    object
})

#' @rdname RK4-class
#' @importFrom methods callNextMethod
setReplaceMethod("init", "RK4", function(object, ..., value) {
    # initialize the solver
    object <- callNextMethod(object, value)        # call superclass init
    # set the rate vectors to the number of equations
    object@rate1 <- vector("numeric", object@numEqn)  # make the rate vector
    object@rate2 <- vector("numeric", object@numEqn)  # make the rate vector
    object@rate3 <- vector("numeric", object@numEqn)  # make the rate vector
    object@rate4 <- vector("numeric", object@numEqn)  # make the rate vector
    object@estimated_state <- vector("numeric", object@numEqn)
    object
})

#' @rdname RK4-class
setMethod("step", "RK4", function(object, ...) {
    # step through the differential equation solver
    state <- getState(object@ode)                         # get the state vector

    if (is.null(state)) {
        return(object@stepSize)
    }
    if (length(state) != object@numEqn) {
        object <- init(object, object@stepSize)
    }

    # get the rate at the initial state
    object@rate1 <- getRate(object@ode, state)
    for (i in 1:object@numEqn) {
        object@estimated_state[i] <- state[i] + object@stepSize * object@rate1[i] / 2
    }

    # get the rate at the estimated state above
    object@rate2 <- getRate(object@ode, object@estimated_state)

    for (i in 1:object@numEqn) {
        object@estimated_state[i] <- state[i] + object@stepSize * object@rate2[i] / 2
    }

    # get the rate at the estimated state above
    object@rate3 <- getRate(object@ode, object@estimated_state)
    for (i in 1:object@numEqn) {
        object@estimated_state[i] <- state[i] + object@stepSize * object@rate3[i]
    }

    # get the rate at the estimated state above
    object@rate4 <- getRate(object@ode, object@estimated_state)
    for (i in 1:object@numEqn) {
        # update the state before leaving
        object@ode@state[i] <- state[i] + object@stepSize *
            (object@rate1[i] + 2 * object@rate2[i] + 2 * object@rate3[i] +
            object@rate4[i]) / 6.0
    }
    object                                 # use this object to reassign in R
})



#' RK4 class constructor
#'
#' @rdname RK4-class
#'
#' @param ode an ODE object
#' @param ... additional parameters
#'
#' @importFrom methods new
#' @export
#' @example ./inst/examples/ReactionApp.R
setMethod("RK4", signature(ode = "ODE"), function(ode, ...) {
    # constructor for RK4 ODE solver
    .rk4 <- .RK4(ode = ode)
    .rk4 <- init(.rk4, .rk4@stepSize)
    return(.rk4)
})
