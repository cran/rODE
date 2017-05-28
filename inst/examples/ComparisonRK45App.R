# ++++++++++++++++++++++++++++++++++++++++++++++++  example: ComparisonRK45App.R
# Compares the solution by the RK45 ODE solver versus the analytical solution
# Example file: ComparisonRK45App.R
# ODE Solver:   Runge-Kutta 45
# Class:        RK45

importFromExamples("ODETest.R")

 ComparisonRK45App <- function(verbose = FALSE) {
     ode <- new("ODETest")                     # create an `ODETest` object
     ode_solver <- RK45(ode)                   # select the ODE solver
     ode_solver <- setStepSize(ode_solver, 1)      # set the step
     ode_solver <- setTolerance(ode_solver, 1e-8)  # set the tolerance
     time <-  0
     rowVector <- vector("list")
     i <- 1
     while (time < 50) {
         rowVector[[i]] <- list(t  = ode_solver@ode@state[2],
                                s1 = getState(ode_solver@ode)[1],
                                s2 = getState(ode_solver@ode)[2],
                                xs = getExactSolution(ode_solver@ode, time),
                                rc = getRateCounts(ode),
                                time = time)
         ode_solver <- step(ode_solver)       # advance one step
         stepSize <-  ode_solver@stepSize     # update the step size
         time <- time + stepSize
         state <- getState(ode_solver@ode)    # get the `state` vector
         i <- i + 1
     }
     DT <- data.table::rbindlist(rowVector)    # a data table with the results
     return(DT)
 }
# show solution
solution <- ComparisonRK45App()                          # run the example
plot(solution)
