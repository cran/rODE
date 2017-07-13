#
# PendulumRK4App.R
#

library(ggplot2)

source(paste(system.file("examples", package = "rODE"),
             "PendulumRK4.R",
             sep ="/"))

PendulumRK4App <- function(verbose = FALSE) {

    ode <- new("ODE")
    pendulum <- PendulumRK4()

    dt <- 0.1
    theta <- 0.2
    thetaDot <- 0

    pendulum@state[3] <- 0      # set time to zero, t = 0

    pendulum <- setState(pendulum, theta, thetaDot)
    pendulum <- setStepSize(pendulum, dt = dt) # using stepSize in RK4

    pendulum@odeSolver <- setStepSize(pendulum@odeSolver, dt) # set new step size

    rowvec <- vector("list")
    i <- 1
    while (pendulum@state[3] <= 20)    {
        rowvec[[i]] <- list(state1 = pendulum@state[1], # angle
                            state2 = pendulum@state[2],      # derivative of the angle
                            state3 = pendulum@state[3])       # time
        i <- i + 1
        pendulum <- step(pendulum)
    }
    return(data.table::rbindlist(rowvec))

}


solution <- PendulumRK4App()
plot(solution)
