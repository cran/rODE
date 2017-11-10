# +++++++++++++++++++++++++++++++++++++++++++++++++++   application:  Logistic.R
# Simulates the logistic equation
importFromExamples("Logistic.R")

# Run the application
LogisticApp <- function(verbose = FALSE) {
    x  <- 0.1
    vx <- 0
    r  <- 2        # Malthusian parameter (rate of maximum population growth)
    K  <- 10.0     # carrying capacity of the environment
    dt   <- 0.01; tol  <- 1e-3; tmax <- 10
    population <- Logistic()

    # Two ways of initializing the object
      # population <- init(population, c(x, vx, 0), r, K)
    init(population) <-  list(initState = c(x, vx, 0),
                              r = r,
                              K = K)

    odeSolver <- Verlet(population)

    # Two ways of initializing the solver
      # odeSolver <- init(odeSolver, dt)
    init(odeSolver) <-  dt

    population@odeSolver <- odeSolver
    # setSolver(population) <-  odeSolver

    rowVector <- vector("list")
    i <- 1
    while (getTime(population) <= tmax) {
        rowVector[[i]] <- list(t = getTime(population),
                               s1 = getState(population)[1],
                               s2 = getState(population)[2])
        population <- doStep(population)
        i <- i + 1
    }
    DT <- data.table::rbindlist(rowVector)
    return(DT)
}
# show solution
solution <- LogisticApp()
plot(solution)
