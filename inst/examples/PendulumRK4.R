# ###################
# PendulumRK4.R
#
# The original Pendulum uses Euler-Richardson solver



setClass("PendulumRK4", slots = c(
    omega0Squared = "numeric",
    state = "numeric",
    odeSolver = "RK4"
    ),
    prototype = prototype(
        omega0Squared = 3,
        state = c(0, 0, 0)
    ),
    contains = c("ODE")
    )

setMethod("initialize", "PendulumRK4", function(.Object) {
    .Object@odeSolver <- RK4(.Object)
    return(.Object)
})

setMethod("setStepSize", signature("PendulumRK4"), function(object, dt, ...) {
    # use explicit parameter declaration
    # setStepSize generic may use two different step parameters: stepSize and dt
    object@odeSolver <- setStepSize(object@odeSolver, dt)
    object
})



setMethod("step", "PendulumRK4", function(object) {
    object@odeSolver <- step(object@odeSolver)
    object@rate  <- object@odeSolver@ode@rate
    object@state <- object@odeSolver@ode@state
    object
})

setMethod("setState", "PendulumRK4", function(object, theta, thetaDot) {
    object@state[1] <- theta     # angle
    object@state[2] <- thetaDot  # derivative of angle
    #                              state[3] is time
    object@odeSolver@ode@state <- object@state
    object
})

setMethod("getState", "PendulumRK4", function(object) {
    object@state
})


setMethod("getRate", "PendulumRK4", function(object, state, ...) {
    object@rate[1] <- state[2]     # rate of change of angle                                      # diff 11
    object@rate[2] <- -object@omega0Squared * sin(state[1])  # rate of change of dtheta
    object@rate[3] <- 1            # rate of change of time, dt/dt

    object@state <- object@odeSolver@ode@state <- state
    object@rate
})


# constructor
PendulumRK4 <- function()  new("PendulumRK4")
