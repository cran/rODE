# ++++++++++++++++++++++++++++++++++++++++++ example: KeplerDormandPrince45App.R
# Demostration of the use of ODE solver RK45 for a particle subjected to
# a inverse-law force. The difference with the example KeplerApp is we are
# seeing the effect in thex and y axis on the particle.
# The original routine used the Verlet ODE solver

importFromExamples("KeplerDormandPrince45.R")

set_solver <- function(ode_object, solver) {
    slot(ode_object, "odeSolver") <- solver
    ode_object
}

KeplerDormandPrince45App <- function(verbose = FALSE) {
    # values for the examples
    x  <- 1
    vx <- 0
    y  <- 0
    vy <- 2 * pi
    dt <- 0.01          # step size
    tol <- 1e-3         # tolerance
    particle  <- KeplerDormandPrince45()                      # use class Kepler

    # Two ways of initializing the ODE object
      # particle  <- init(particle, c(x, vx, y, vy, 0))  # enter state vector
    init(particle) <- c(x, vx, y, vy, 0)

    odeSolver <- DormandPrince45(particle)      # select the ODE solver

    # Two ways of initializing the solver
      # odeSolver <- init(odeSolver, dt)            # start the solver
    init(odeSolver) <-  dt

    # Two ways of setting the tolerance
      # odeSolver <- setTolerance(odeSolver, tol) # this works for adaptive solvers
    setTolerance(odeSolver) <- tol
    setSolver(particle) <-  odeSolver

    initialEnergy <- getEnergy(particle)        # calculate the energy
    rowVector <- vector("list")
    i <- 1
    while (getTime(particle) < 1.5) {
    rowVector[[i]] <- list(t  = getState(particle)[5],
                           x  = getState(particle)[1],
                           vx = getState(particle)[2],
                           y  = getState(particle)[3],
                           vx = getState(particle)[4],
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

