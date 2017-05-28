
# * An Euler-Richardson (midpoint) method ODE solver.
# *
# * The Euler-Richardson method uses the state at the beginning of the interval
# * to estimate the state at the midpoint.
# *
#     * x(midpoint) = x(n) + v(n)*dt/2
#     * v(midpoint) = v(n) + a(n)*dt/2
#     * t(midpoint) = t(n) + dt/2
#     *
#     * The midpoint state is then used to calculate the final state.
#     * @author             Wolfgang Christian
#     * converted to R by   Alfonso R. Reyes


#' EulerRichardson class
#'
#' @param ode an ODE object
#' @param ... additional parameters
#'
#' @rdname EulerRichardson-class
.EulerRichardson <- setClass("EulerRichardson", slots = c(
                      midstate = "numeric"          # this is the midpoint slot
                    ),
                        contains = c("AbstractODESolver")
                    )

setMethod("initialize", "EulerRichardson", function(.Object, ode, ...) {
                # initialize the class
                .Object@ode <- ode
                .Object@ode@rate <- vector("numeric")        # create vector with no length
                return(.Object)
            })


#' @rdname init-method
#' @importFrom methods callNextMethod
setMethod("init", "EulerRichardson", function(object, stepSize, ...) {
    # inititalize the solver
    object <- callNextMethod(object, stepSize)           # call superclass init
    object@ode@rate <- vector("numeric", object@numEqn)  # make the rate vector
    object@midstate <- vector("numeric", object@numEqn)  #    same size as state
    object
})


#' @rdname step-method
setMethod("step", "EulerRichardson", function(object, ...) {
    # step through the diffrential equation
    state <- getState(object@ode)                         # get the state vector
    rate  <- getRate(object@ode, state)

    dt2 <- object@stepSize / 2                            # divide stepSize

    for (i in 1:object@numEqn) {
        # estimate the state at the midpoint
        object@midstate[i] <- state[i] + rate[i] * dt2
    }

    rate <- getRate(object@ode, object@midstate) # rate based on midpoint

    for (i in 1:object@numEqn) {
        state[i] <- state[i] + object@stepSize * rate[i] # calc new state
    }

    object@ode@state <- state       # return state and rate for new iter
    object@ode@rate  <- rate
    object                          # use this object to reassign in R
})



#' EulerRichardson constructor ODE
#'
#' @param ode an ODE object
#' @param ...  additional parameters
#'
#' @importFrom methods new
#' @export
setMethod("EulerRichardson", signature(ode = "ODE"), function(ode, ...) {
    # constructor for Euler-Richardson ODE solver
    eulerRichardson <- .EulerRichardson(ode = ode)
    eulerRichardson <- init(eulerRichardson, eulerRichardson@stepSize)                         # diff 5
    return(eulerRichardson)
})

