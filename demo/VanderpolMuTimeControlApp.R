# +++++++++++++++++++++++++++++++++++++     example: VanderpolMuTimeControlApp.R
# This is a modification of the original Vanderpol.R script
# In this version, we will add tha ability of setting mu and time lapse.
# This example is also shown in the Matlab help guide

importFromExamples("VanderpolMuTimeControl.R")

# run the application
VanderpolMuTimeControlApp <- function(verbose = FALSE) {
    # set the orbit into a predefined state.
    y1 <- 2; y2 <- 0; mu <- 10; tmax <- mu * 3; dt <- 0.01
    rigid_body <- VanderPol(y1, y2, mu)
    solver <- RK45(rigid_body)
    rowVector <- vector("list")
    i <- 1
    while (getState(rigid_body)[3] <= tmax) {
        rowVector[[i]] <- list(t  = getState(rigid_body)[3],
                               y1 = getState(rigid_body)[1],
                               y2 = getState(rigid_body)[2]
                               )
        solver     <- step(solver)
        rigid_body <- getODE(solver)
        i <-  i + 1
    }
    DT <- data.table::rbindlist(rowVector)
    return(DT)

}
# show solution
solution <- VanderpolMuTimeControlApp()
plot(solution)

