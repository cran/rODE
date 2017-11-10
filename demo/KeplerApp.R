#  +++++++++++++++++++++++++++++++++++++++++++++++++++++++++ example KeplerApp.R
#  KeplerApp solves an inverse-square law model (Kepler model) using an adaptive
#  stepsize algorithm.
#  Application showing two planet orbiting
#  File in examples: KeplerApp.R

importFromExamples("Kepler.R") # source the class Kepler

KeplerApp <- function(verbose = FALSE) {

    # set the orbit into a predefined state.
    r <- c(2, 0)                                   # orbit radius
    v <- c(0, 0.25)                                # velocity
    dt <- 0.1
    planet <- Kepler(r, v)                         # make up an ODE object
    solver <- RK45(planet)
    rowVector <- vector("list")
    i <- 1
    while (getState(planet)[5] <= 10) {
        rowVector[[i]] <- list(t  = planet@state[5],
                               planet1.r = getState(planet)[1],
                               p1anet1.v = getState(planet)[2],
                               planet2.r = getState(planet)[3],
                               p1anet2.v = getState(planet)[4])
        solver <- step(solver)
        planet <- getODE(solver)
        i <-  i + 1
    }
    DT <- data.table::rbindlist(rowVector)

    return(DT)
}


solution <- KeplerApp()
plot(solution)


