
importFromExamples("AdaptiveStep.R")

# running function
AdaptiveStepApp <- function(verbose = FALSE) {
    ode        <- new("Impulse")
    ode_solver <- RK45(ode)

    # Two ways to initialize the solver
      # ode_solver <- init(ode_solver, 0.1)
    init(ode_solver) <- 0.1

    # two ways to set tolerance
      # ode_solver <- setTolerance(ode_solver, 1.0e-4)
    setTolerance(ode_solver) <- 1.0e-4

    i <- 1; rowVector <- vector("list")
    while (getState(ode)[1] < 12) {
        rowVector[[i]] <- list(s1 = getState(ode)[1],
                               s2 = getState(ode)[2],
                               t  = getState(ode)[3])
        ode_solver <- step(ode_solver)
        ode <- getODE(ode_solver)
        i <- i + 1
    }
    return(data.table::rbindlist(rowVector))
}

# run application
solution <- AdaptiveStepApp()
plot(solution)
