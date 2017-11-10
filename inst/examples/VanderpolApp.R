# ++++++++++++++++++++++++++++++++++++++++++++++++   application: VanderPolApp.R
# Solution of the Van der Pol equation
#
importFromExamples("VanderPol.R")

# run the application
VanderpolApp <- function(verbose = FALSE) {
    # set the orbit into a predefined state.
    y1 <- 2; y2 <- 0; dt <- 0.1;
    rigid_body <- VanderPol(y1, y2)
    solver <- RK45(rigid_body)
    rowVector <- vector("list")
    i <- 1
    while (getState(rigid_body)[3] <= 20) {
        rowVector[[i]] <- list(t  = getState(rigid_body)[3],
                               y1 = getState(rigid_body)[1],
                               y2 = getState(rigid_body)[2])
        solver     <- step(solver)
        rigid_body <- getODE(solver)
        i <-  i + 1
    }
    DT <- data.table::rbindlist(rowVector)
    return(DT)

}
# show solution
solution <- VanderpolApp()
plot(solution)

