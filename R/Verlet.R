
#' Verlet ODE solver class
#'
#' @param ode an ODE object
#' @param object a class object
#' @param stepSize size of the step
#' @param ... additional parameters
#'
#' @example ./inst/examples/KeplerEnergyApp.R
#' @rdname Verlet-class
.Verlet <- setClass("Verlet", slots = c(
              rate1 = "numeric",
              rate2 = "numeric",
              rateCounter = "numeric"
            ),
                contains = c("AbstractODESolver")
            )

#' Verlet generic
#'
#' @rdname Verlet-class
#' @export
#' @example ./inst/examples/LogisticApp.R
setGeneric("Verlet", function(ode, ...)  standardGeneric("Verlet"))


setMethod("initialize", "Verlet", function(.Object, ode, ...) {
    # initialize the class
    .Object@rateCounter <- -1
    .Object@ode <- ode
    return(.Object)
})

#' @rdname Verlet-class
#' @aliases init-methods
#' @importFrom methods callNextMethod
setMethod("init", "Verlet", function(object, stepSize, ...) {
    # inititalize the solver
    object <- callNextMethod(object, stepSize)        # call superclass init

    # set the rate vectors to the number of equations
    object@rate1 <- vector("numeric", object@numEqn)  # make the rate vector
    object@rate2 <- vector("numeric", object@numEqn)  # make the rate vector
    object@rateCounter <- -1
    object
})


#' @rdname Verlet-class
#' @aliases getRateCounter,getRateCounter-method
setMethod("getRateCounter", "Verlet", function(object, ...) {
    return(object@rateCounter)
})


#' @rdname Verlet-class
setMethod("step", "Verlet", function(object, ...) {
    # state[]: x1, d x1/dt, x2, d x2/dt .... xN, d xN/dt, t
    state <- getState(object@ode)                         # get the state vector

    if (length(state) != object@numEqn) {
        object <- init(object, object@stepSize)
    }

    object@rateCounter <- 0     #  getRate has not been called
    object@rate1 <- getRate(object@ode, state)
    # cat("rate1="); print(object@rate1)

    dt2 <- object@stepSize * object@stepSize  # the step size squared
    # increment the positions using the velocity and acceleration
    for (i in seq(1, object@numEqn-1, 2)) {
        # cat(sprintf("i1=%3d \n", i-1))
        state[i] <- state[i] +
            object@stepSize * object@rate1[i] +
            dt2 * object@rate1[i+1] / 2
        # cat(sprintf("i1=%3d, state1=%12f, rate1=%11f\n", i-1, state[i], object@rate1[i]))
    }
    object@rateCounter <- 1  # getRate has been called once
    object@rate2 <- getRate(object@ode, state)
    # cat("rate2="); print(object@rate2)

    object@rateCounter <- 2  # getRate has been called twice

    for (i in seq(2, object@numEqn, 2)) {
        # increment the velocities with the average rate
        state[i] <- state[i] + object@stepSize * (object@rate1[i] +
            object@rate2[i]) / 2.0
        # cat(sprintf("i2=%3d, state1=%12f, rate2=%11f \n", i-1, state[i], object@rate2[i]))
    }
    if (object@numEqn%%2 == 1) { # last equation if  we have an odd number of equations
        ##  in Java this is written like this:
        ##  state[numEqn-1] = state[numEqn-1] + stepSize*rate1[numEqn-1];
        state[object@numEqn] <- state[object@numEqn] +
            object@stepSize * object@rate1[object@numEqn]

        # cat(sprintf("@numEqn2=%3d, state[@numEqn-1]=%12f rate1=%12f\n\n",
        #             object@numEqn%%2, state[object@numEqn],
        #             object@rate1[object@numEqn]))
    }
    object@ode@state <- state
    object                                  # use this object to reassign in R
})





#' Verlet class constructor ODE
#'
#' @rdname Verlet-class
#'
#' @export
#' @example ./inst/examples/KeplerEnergyApp.R
#' @example ./inst/examples/LogisticApp.R
setMethod("Verlet", signature(ode = "ODE"), function(ode, ...) {
    .verlet <- .Verlet(ode = ode)
    .verlet <- init(.verlet, .verlet@stepSize)
    return(.verlet)
})
