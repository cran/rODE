# ++++++++++++++++++++++++++++++++++++++++++++++++++   example: KeplerEulerApp.R
# Caclculation of the energy for a particle
# Demostration of the use of the EUler ODE solver
#

importFromExamples("KeplerEuler.R")      # source the class

KeplerEulerApp <- function(verbose = FALSE) {
    # initial values
    x  <- 1
    vx <- 0
    y  <- 0
    vy <- 2 * pi
    dt <- 0.01
    tol <- 1e-3

    particle <- Kepler()                           # create a Kepler ODE object

    # Two ways of initializing the ODE object
      # particle <- init(particle, c(x, vx, y, vy, 0)) # set initial values
    init(particle) <-  c(x, vx, y, vy, 0)       # set particle initial values

    odeSolver <- Euler(particle)                   # select the solver

    # Two ways of initializing the solver
      # odeSolver <- init(odeSolver, dt)               # start the solver
    init(odeSolver) <-  dt

    particle@odeSolver <- odeSolver               # copy solver to ODE object
    initialEnergy <- getEnergy(particle)         # calculate the initial energy
    rowVector <- vector("list")
    i <- 1
    while (i < 100) {
        rowVector[[i]] <- list(t  = getTime(particle),
                               x  = particle@state[1],
                               vx = particle@state[2],
                               y  = particle@state[3],
                               vy = particle@state[4],
                               E  = getEnergy(particle))
        particle <- doStep(particle)
        energy   <- getEnergy(particle)
        i <- i + 1
    }
    DT <- data.table::rbindlist(rowVector)
    return(DT)

}

solution <- KeplerEulerApp()
plot(solution)
