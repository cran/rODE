# ++++++++++++++++++++++++++++++++++++++++++ example: KeplerDormandPrince45App.R
# Demostration of the use of ODE solver RK45 for a particle subjected to
# a inverse-law force. The difference with the example KeplerApp is we are
# seeing the effect in thex and y axis on the particle.
# The original routine used the Verlet ODE solver

importFromExamples("KeplerDormandPrince45.R")

KeplerDormandPrince45App <- function(verbose = FALSE) {
    # values for the examples
    x  <- 1
    vx <- 0
    y  <- 0
    vy <- 2 * pi
    dt <- 0.01          # step size
    tol <- 1e-3         # tolerance
    particle <- Kepler()                            # use class Kepler
    particle <- init(particle, c(x, vx, y, vy, 0))  # enter state vector
    odeSolver <- DormandPrince45(particle)      # select the ODE solver
    odeSolver <- init(odeSolver, dt)            # start the solver
    odeSolver <- setTolerance(odeSolver, tol)   # this works for adaptive solvers
    particle@odeSolver <- odeSolver             # copy the solver to ODE object
    initialEnergy <- getEnergy(particle)        # calculate the energy
    rowVector <- vector("list")
    i <- 1
    while (getTime(particle) < 1.5) {
        rowVector[[i]] <- list(t  = particle@state[5],
                               x  = particle@state[1],
                               vx = particle@state[2],
                               y  = particle@state[3],
                               vx = particle@state[4],
                               energy = getEnergy(particle) )
        particle <- doStep(particle)            # advance one step
        energy   <- getEnergy(particle)         # calculate energy
        i <- i + 1
    }
    DT <- data.table::rbindlist(rowVector)
    return(DT)
}


solution <- KeplerDormandPrince45App()
plot(solution)

