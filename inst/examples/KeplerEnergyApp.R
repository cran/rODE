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
    particle <- init(particle, c(x, vx, y, vy, 0))
    odeSolver <- Verlet(particle)
    odeSolver <- init(odeSolver, dt)
    particle@odeSolver <- odeSolver
    initialEnergy <- getEnergy(particle)
    rowVector <- vector("list")
    i <- 1
    while (getTime(particle) <= 1.20) {
        rowVector[[i]] <- list(t  = particle@state[5],
                               x  = particle@state[1],
                               vx = particle@state[2],
                               y  = particle@state[3],
                               vy = particle@state[4],
                               E  = getEnergy(particle))
        particle <- doStep(particle)
        energy <- getEnergy(particle)
        i <- i + 1
    }
    DT <- data.table::rbindlist(rowVector)
    return(DT)
}


solution <- KeplerEnergyApp()
plot(solution)

