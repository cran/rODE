# ++++++++++++++++++++++++++++++++++++++++++++++++++      example: PendulumApp.R
# Simulation of a pendulum using the EulerRichardson ODE solver

suppressPackageStartupMessages(library(ggplot2))

importFromExamples("Pendulum.R")      # source the class

PendulumApp <- function(verbose = FALSE) {
    # initial values
    theta <- 0.2
    thetaDot <- 0
    dt <- 0.1
    ode <- new("ODE")
    pendulum <- Pendulum()
    pendulum@state[3] <- 0      # set time to zero, t = 0
    pendulum <- setState(pendulum, theta, thetaDot)
    pendulum <- setStepSize(pendulum, dt = dt) # using stepSize in RK4
    pendulum@odeSolver <- setStepSize(pendulum@odeSolver, dt) # set new step size
    rowvec <- vector("list")
    i <- 1
    while (pendulum@state[3] <= 40)    {
        rowvec[[i]] <- list(t  = pendulum@state[3],    # time
                            theta = pendulum@state[1], # angle
                            thetadot = pendulum@state[2]) # derivative of angle
        pendulum <- step(pendulum)
        i <- i + 1
    }
    DT <- data.table::rbindlist(rowvec)
    return(DT)
}
# show solution
solution <- PendulumApp()
plot(solution)
