importFromExamples("SHO.R")

# SHOApp.R
SHOApp <- function(...) {
    x <- 1.0; v <- 0; k <- 1.0; dt <- 0.01; tolerance <- 1e-3
    sho    <- SHO(x, v, k)
    solver_factory <- ODESolverFactory()
    solver <- createODESolver(solver_factory, sho, "DormandPrince45")
    # solver <- DormandPrince45(sho)                    # this can also be used
    solver <- setTolerance(solver, tolerance)
    solver <- init(solver, dt)
    i <- 1; rowVector <- vector("list")
    while (sho@state[3] <= 500) {
        rowVector[[i]] <- list(x = sho@state[1],
                               v = sho@state[2],
                               t = sho@state[3])
        solver <- step(solver)
        sho    <- solver@ode
        i <- i + 1
    }
    return(data.table::rbindlist(rowVector))
}

solution <- SHOApp()
plot(solution)


