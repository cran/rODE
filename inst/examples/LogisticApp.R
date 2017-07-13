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
    population <- init(population, c(x, vx, 0), r, K)
    odeSolver <- Verlet(population)
    odeSolver <- init(odeSolver, dt)
    population@odeSolver <- odeSolver
    rowVector <- vector("list")
    i <- 1
    while (getTime(population) <= tmax) {
        rowVector[[i]] <- list(t = getTime(population),
                               s1 = population@state[1],
                               s2 = population@state[2])
        population <- doStep(population)
        i <- i + 1
    }
    DT <- data.table::rbindlist(rowVector)
    return(DT)
}
# show solution
solution <- LogisticApp()
plot(solution)
