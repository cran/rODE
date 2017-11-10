# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~  base class: KeplerVerlet.R

setClass("KeplerDormandPrince45", slots = c(
    GM = "numeric",
    odeSolver = "DormandPrince45",
    counter = "numeric"
    ),
    contains = c("ODE")
)

setMethod("initialize", "KeplerDormandPrince45", function(.Object, ...) {
    .Object@GM <- 4 * pi * pi         # gravitation constant times combined mass
    .Object@state <- vector("numeric", 5)  # x, vx, y, vy, t
    .Object@odeSolver <- DormandPrince45(.Object)
    .Object@counter <- 0
    return(.Object)
})

setMethod("doStep", "KeplerDormandPrince45", function(object, ...) {
    object@odeSolver <- step(object@odeSolver)
    object@state <- object@odeSolver@ode@state
    object
})

setMethod("getTime", "KeplerDormandPrince45", function(object, ...) {
    return(object@state[5])
})

setMethod("getEnergy", "KeplerDormandPrince45", function(object, ...) {
    ke <- 0.5 * (object@state[2] * object@state[2] +
                     object@state[4] * object@state[4])
    pe <- -object@GM / sqrt(object@state[1] * object@state[1] +
                                object@state[3] * object@state[3])
    return(pe+ke)
})

setMethod("init", "KeplerDormandPrince45", function(object, initState, ...) {
    object@state <- initState
    # call init in AbstractODESolver
    object@odeSolver <- init(object@odeSolver, getStepSize(object@odeSolver))
    object@counter <- 0
    object
})

setReplaceMethod("init", "KeplerDormandPrince45", function(object, ..., value) {
    object@state <- value
    # call init in AbstractODESolver
    object@odeSolver <- init(object@odeSolver, getStepSize(object@odeSolver))
    object@counter <- 0
    object
})

setMethod("getRate", "KeplerDormandPrince45", function(object, state, ...) {
    # Computes the rate using the given state.
    r2 <- state[1] * state[1] + state[3] * state[3]  # distance squared
    r3 <- r2 * sqrt(r2)   # distance cubed
    object@rate[1] <- state[2]
    object@rate[2] <- (- object@GM * state[1]) / r3
    object@rate[3] <- state[4]
    object@rate[4] <- (- object@GM * state[3]) / r3
    object@rate[5] <- 1   # time derivative

    object@counter <- object@counter + 1
    object@rate
})

setMethod("getState", "KeplerDormandPrince45", function(object, ...) {
    # Gets the state variables.
    return(object@state)
})

setReplaceMethod("setSolver", "KeplerDormandPrince45", function(object, value) {
    object@odeSolver <- value
    object
})

# constructor
KeplerDormandPrince45 <- function() {
    kepler <- new("KeplerDormandPrince45")
    return(kepler)
}
