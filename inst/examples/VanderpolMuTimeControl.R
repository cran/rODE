# class: VanderpolMuTimeControl.R
# This is a modification of the original Vanderpol.R script
# In this version, we will add tha ability of setting mu and time lapse.
# This example is also shown in the Matlab help guide
library(rODE)

setClass("VanderPol", slots = c(
    mu = "numeric"
),
contains = c("ODE")
)

setMethod("initialize", "VanderPol", function(.Object, ...) {
    .Object@mu <- 1.0                 # gravitation constant times combined mass
    .Object@state <- vector("numeric", 3)  # y1, y2, t
    return(.Object)
})


setMethod("getState", "VanderPol", function(object, ...) {
    # Gets the state variables.
    return(object@state)
})


setMethod("getRate", "VanderPol", function(object, state, ...) {
    # Computes the rate using the given state.
    object@rate[1] <- state[2]
    object@rate[2] <-  object@mu * (1 - state[1]^2) * state[2] - state[1]
    object@rate[3] <- 1
    object@rate
})

# constructor
VanderPol <- function(y1, y2, mu = 1.0) {
    VanderPol <- new("VanderPol")
    VanderPol@state[1] = y1
    VanderPol@state[2] = y2
    VanderPol@state[3] = 0
    VanderPol@mu       = mu
    return(VanderPol)
}
