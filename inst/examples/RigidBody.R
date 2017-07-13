# RigidBody.R
# example of a nonstiff system is the system of equations describing
# the motion of a rigid body without external forces.

setClass("RigidBodyNXF", slots = c(
    g = "numeric"
),
prototype = prototype(
    g = 9.8
),
contains = c("ODE")
)


setMethod("initialize", "RigidBodyNXF", function(.Object, ...) {
    .Object@state <- vector("numeric", 5)
    return(.Object)
})


setMethod("getState", "RigidBodyNXF", function(object, ...) {
    # Gets the state variables.
    return(object@state)
})


setMethod("getRate", "RigidBodyNXF", function(object, state, ...) {
    # Gets the rate of change using the argument's state variables.
    object@rate[1] <- state[2] * state[3]
    object@rate[2] <- - state[1] * state[3]
    object@rate[3] <- -0.51 * state[1] * state[2]
    object@rate[4] <- 1

    object@rate
})


# constructor
RigidBodyNXF <- function(y1, y2, y3) {
    .RigidBodyNXF <- new("RigidBodyNXF")
    .RigidBodyNXF@state[1] <- y1
    .RigidBodyNXF@state[2] <- y2
    .RigidBodyNXF@state[3] <- y3
    .RigidBodyNXF@state[4] <- 0
    .RigidBodyNXF
}
