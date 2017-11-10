## ----message=FALSE, results="hold"---------------------------------------
library(rODE)

# ODETest.R

setClass("ODETest", slots = c(
    stack = "environment"           # environment object inside the class
    ),
    contains = c("ODE")
    )

setMethod("initialize", "ODETest", function(.Object, ...) {
    .Object@stack$n <-  0               # "n" belongs to the class environment
    .Object@state   <- c(1.0, 0.0)
    return(.Object)
})

setMethod("getExactSolution", "ODETest", function(object, t, ...) {
    # analytical solution
    return(exp(-t))
})

setMethod("getState", "ODETest", function(object, ...) {
    object@state
})

setMethod("getRate", "ODETest", function(object, state, ...) {
    object@rate[1] <- -state[1]
    object@rate[2] <-  1                        # rate of change of time, dt/dt
    object@stack$n <-  object@stack$n + 1       # add 1 to the rate count
    object@rate
})

# constructor
ODETest <- function() {
    odetest <- new("ODETest")
    odetest
}

## ------------------------------------------------------------------------
ComparisonEulerApp <- function(stepSize) {
    ode <- new("ODETest")
    ode_solver <- Euler(ode)
    ode_solver <- setStepSize(ode_solver, stepSize)
    time <-  0
    rowVector <- vector("list")
    i <-  1
    while (time < 5) {
        state <- getState(ode_solver@ode)
        time <- state[2]
        error <- getExactSolution(ode_solver@ode, time) - state[1]
        rowVector[[i]] <- list(step_size = stepSize, 
                               t = time, 
                               y_t = state[1], 
                               error = error, 
                               rel_err = error / getExactSolution(ode_solver@ode, time),
                               steps = ode_solver@ode@stack$n
                               )
        ode_solver <- step(ode_solver)
        i <- i + 1
    }
    data.table::rbindlist(rowVector)
}
dt <- ComparisonEulerApp(stepSize = 0.2)
dt[round(t,1) %in% c(1.0, 2.0, 3.0, 4.0, 5.0)]

## ------------------------------------------------------------------------
# get a summary table for different step sizes
get_table <- function(stepSize) {
    dt <- ComparisonEulerApp(stepSize)
    dt[round(t,2) %in% c(1.0, 2.0, 3.0, 4.0, 5.0)]
}

step_sizes <- c(0.2, 0.1, 0.05)
dt_li <- lapply(step_sizes, get_table)
data.table::rbindlist(dt_li)

## ------------------------------------------------------------------------
ComparisonRK4App <- function(stepSize) {
    ode <- new("ODETest")
    ode_solver <- RK4(ode)
    ode_solver <- setStepSize(ode_solver, stepSize)
    time <-  0
    rowVector <- vector("list")
    i <-  1
    while (time < 5) {
        state <- getState(ode_solver@ode)
        time <- state[2]
        error <- getExactSolution(ode_solver@ode, time) - state[1]
        rowVector[[i]] <- list(step_size = stepSize, 
                               t = time, 
                               y_t = state[1], 
                               error = error, 
                               rel_err = error / getExactSolution(ode_solver@ode, time),
                               steps = ode_solver@ode@stack$n
                               )
        ode_solver <- step(ode_solver)
        i <- i + 1
    }
    data.table::rbindlist(rowVector)
}

# get a summary table for different step sizes
get_table <- function(stepSize) {
    dt <- ComparisonRK4App(stepSize)
    dt[round(t,2) %in% c(1.0, 2.0, 3.0, 4.0, 5.0)]
}

step_sizes <- c(0.2, 0.1, 0.05)
dt_li <- lapply(step_sizes, get_table)
data.table::rbindlist(dt_li)

