# +++++++++++++++++++++++++++++++++++++++++++++++++  example: PendulumEulerApp.R
# Pendulum simulation with the Euler ODE solver
# Notice how Euler is not applicable in this case as it diverges very quickly
# even when it is using a very small `delta t``?ODE

importFromExamples("PendulumEuler.R")      # source the class

PendulumEulerApp <- function(verbose = FALSE) {
    # initial values
    theta <- 0.2
    thetaDot <- 0
    dt <- 0.01
    pendulum <- PendulumEuler()
    pendulum@state[3] <- 0      # set time to zero, t = 0
    pendulum <- setState(pendulum, theta, thetaDot)
    stepSize <- dt
    pendulum <- setStepSize(pendulum, stepSize)
    pendulum@odeSolver <- setStepSize(pendulum@odeSolver, dt) # set new step size
    rowvec <- vector("list")
    i <- 1
    while (getState(pendulum)[3] <= 50)    {
        rowvec[[i]] <- list(t        = getState(pendulum)[3],
                            theta    = getState(pendulum)[1],
                            thetaDot = getState(pendulum)[2])
        pendulum <- step(pendulum)
        i <- i + 1
    }
    DT <- data.table::rbindlist(rowvec)
    return(DT)
}

solution <- PendulumEulerApp()
plot(solution)
