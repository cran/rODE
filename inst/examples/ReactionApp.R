# +++++++++++++++++++++++++++++++++++++++++++++++++++ application: ReactionApp.R
# ReactionApp solves an autocatalytic oscillating chemical
# reaction (Brusselator model) using
# a fourth-order Runge-Kutta algorithm.

importFromExamples("Reaction.R")      # source the class

ReactionApp <- function(verbose = FALSE) {
    X <- 1; Y <- 5;
    dt <- 0.1

    reaction <- Reaction(c(X, Y, 0))
    solver <- RK4(reaction)
    rowvec <- vector("list")
    i <- 1
    while (solver@ode@state[3] < 100) {             # stop at t = 100
        rowvec[[i]] <- list(t = solver@ode@state[3],
                            X = solver@ode@state[1],
                            Y = solver@ode@state[2])
        solver <- step(solver)
        i <-  i + 1
    }
    DT <- data.table::rbindlist(rowvec)
    return(DT)
}


solution <- ReactionApp()
plot(solution)


