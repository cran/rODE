# +++++++++++++++++++++++++++++++++++++++++++++++++++   application:  Logistic.R
# Simulates the logistic equation

setClass("Logistic", slots = c(
    K = "numeric",
    r = "numeric",
    odeSolver = "Verlet",
    counter = "numeric"
),
contains = c("ODE")
)

setMethod("initialize", "Logistic", function(.Object, ...) {
    .Object@K <- 10
    .Object@r <- 1.0
    .Object@state <- vector("numeric", 3)  # x, vx
    .Object@odeSolver <- Verlet(.Object)
    .Object@counter <- 0
    return(.Object)
})

setMethod("doStep", "Logistic", function(object, ...) {
    # cat("state@doStep=", object@state, "\n")
    object@odeSolver <- step(object@odeSolver)
    object@state <- object@odeSolver@ode@state
    object
})

setMethod("getTime", "Logistic", function(object, ...) {
    return(object@state[3])
})


setMethod("init", "Logistic", function(object, initState, r, K, ...) {
    object@r <- r
    object@K <- K
    object@state <- initState
    object@odeSolver <- init(object@odeSolver, getStepSize(object@odeSolver))
    object@counter <- 0
    object
})


setMethod("getRate", "Logistic", function(object, state, ...) {
    # Computes the rate using the given state.
    object@rate[1] <- state[2]
    object@rate[2] <- object@r * state[1] * (1 - state[1] / object@K)
    object@rate[3] <- 1   # time derivative
    object@counter <- object@counter + 1
    object@rate

})


setMethod("getState", "Logistic", function(object, ...) {
    # Gets the state variables.
    return(object@state)
})

# constructor
Logistic <- function() {
    logistic <- new("Logistic")
    return(logistic)
}

# Run the application
LogisticVerletApp <- function(verbose = FALSE) {
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
solution <- LogisticVerletApp()
plot(solution)
