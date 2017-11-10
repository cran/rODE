# ++++++++++++++++++++++++++++++++++++++++++++++++++  example: KeplerEnergyApp.R
# Demostration of the use of the Verlet ODE solver
#

importFromExamples("KeplerEnergy.R") # source the class Kepler

KeplerEnergyApp <- function(verbose = FALSE) {
    # initial values
    x  <- 1
    vx <- 0
    y  <- 0
    vy <- 2 * pi
    dt <- 0.01
    tol <- 1e-3
    particle <- KeplerEnergy()

    # Two ways of initializing the ODE object
      # particle <- init(particle, c(x, vx, y, vy, 0))
    init(particle) <- c(x, vx, y, vy, 0)

    odeSolver <- Verlet(particle)

    # Two ways of initializing the solver
      # odeSolver <- init(odeSolver, dt)
    init(odeSolver) <-  dt

    particle@odeSolver <- odeSolver
    initialEnergy <- getEnergy(particle)
    rowVector <- vector("list")
    i <- 1
    while (getTime(particle) <= 1.20) {
        rowVector[[i]] <- list(t  = getState(particle)[5],
                               x  = getState(particle)[1],
                               vx = getState(particle)[2],
                               y  = getState(particle)[3],
                               vy = getState(particle)[4],
                               E  = getEnergy(particle))
        particle <- doStep(particle)
        energy   <- getEnergy(particle)
        i <- i + 1
    }
    DT <- data.table::rbindlist(rowVector)
    return(DT)
}


solution <- KeplerEnergyApp()
plot(solution)

