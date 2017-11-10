# ODETest.R
# Called as base class for examples:
#                         ComparisonRK45App.R
#                         ComparisonRK45ODEApp.R

#' ODETest as an example of ODE class inheritance
#'
#' ODETest is a base class for examples ComparisonRK45App.R and
#' ComparisonRK45ODEApp.R. ODETest also uses an environment variable to store
#' the rate counts.
#'
#' @rdname ODE-class-example
#' @include ODE.R
setClass("ODETest", slots = c(
    n     = "numeric",           # counts the number of getRate evaluations
    stack = "environment"        # environnment object to accumulate rate counts
    ),
    contains = c("ODE")
    )

setMethod("initialize", "ODETest", function(.Object, ...) {
    .Object@stack$rateCounts <-  0              # counter for rate calculations
    .Object@state <- c(5.0, 0.0)
    return(.Object)
})

#' @rdname getExactSolution-method
setMethod("getExactSolution", "ODETest", function(object, t, ...) {
    return(5.0 * exp(-t))
})

#' @rdname getState-method
setMethod("getState", "ODETest", function(object, ...) {
    object@state
})

#' @rdname getRate-method
setMethod("getRate", "ODETest", function(object, state, ...) {
    object@rate[1] <- - state[1]
    object@rate[2] <-  1            # rate of change of time, dt/dt
    # accumulate how many times the rate has been called to calculate
    object@stack$rateCounts <- object@stack$rateCounts + 1
    object@state <- state
    object@rate
})


#' @rdname getRateCounts-method
setMethod("getRateCounts", "ODETest", function(object, ...) {
    # use environment stack to accumulate rate counts
    object@stack$rateCounts
})

# constructor
ODETest <- function() {
    odetest <- new("ODETest")
    odetest
}
