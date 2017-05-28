# ++++++++++++++++++++++++++++++++++++++++++++++++++   application:  SpringRK4.R
# Simulation of a spring considering no friction
library(rODE)


setClass("SpringRK4", slots = c(
    # we should improve this by letting the user entered these values
    K         = "numeric",
    mu        = "numeric",
    mass      = "numeric",
    state     = "numeric",
    odeSolver = "RK4"
    ),
    prototype = prototype(
        K = 1,
        state = c(0, 0, 0)
    ),
    contains = c("ODE")
    )

setMethod("initialize", "SpringRK4", function(.Object) {
    # we should improve this by letting the user entered these values
    .Object@K    <- 1.0
    .Object@mu   <- 1.5
    .Object@mass <- 20
    .Object@odeSolver <- RK4(.Object)
    return(.Object)
})

setMethod("setStepSize", signature("SpringRK4"), function(object, dt, ...) {
    # use explicit parameter declaration
    # setStepSize generic may use two different step parameters: stepSize and dt
    object@odeSolver <- setStepSize(object@odeSolver, dt)
    object
})


setMethod("step", "SpringRK4", function(object) {
    object@odeSolver <- step(object@odeSolver)
    object@rate  <- object@odeSolver@ode@rate
    object@state <- object@odeSolver@ode@state
    object
})

setMethod("setState", "SpringRK4", function(object, theta, thetaDot) {
    object@state[1] <- theta     # angle
    object@state[2] <- thetaDot  # derivative of the angle
    #                              state[3] is time
    object@odeSolver@ode@state <- object@state      # set state on solver
    object
})

setMethod("getState", "SpringRK4", function(object) {
    object@state
})

setMethod("getRate", "SpringRK4", function(object, state, ...) {
    # enter the derivatives here
    object@rate[1] <- state[2]     # rate of change of angle
    object@rate[2] <- -object@mu /  object@mass * state[2] - object@K * state[1]
    object@rate[3] <- 1            # rate of change of time, dt/dt

    object@rate
})

# constructor
SpringRK4 <- function()  new("SpringRK4")


# run application
SpringRK4App <- function(verbose = FALSE) {
    theta    <- 0
    thetaDot <- -0.2
    tmax     <- 22; dt <- 0.1
    ode <- new("ODE")
    spring <- SpringRK4()
    spring@state[3] <- 0      # set time to zero, t = 0
    spring <- setState(spring, theta, thetaDot)
    spring <- setStepSize(spring, dt = dt) # using stepSize in RK4
    spring@odeSolver <- setStepSize(spring@odeSolver, dt) # set new step size
    rowvec <- vector("list")
    i <- 1
    while (spring@state[3] <= tmax)    {
        rowvec[[i]] <- list(t  = spring@state[3],      # angle
                            y1 = spring@state[1],      # derivative of the angle
                            y2 = spring@state[2])      # time
        i <- i + 1
        spring <- step(spring)
    }
    DT <- data.table::rbindlist(rowvec)
    return(DT)
}

# show solution
solution <- SpringRK4App(TRUE)
plot(solution)
