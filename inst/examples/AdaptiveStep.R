# AdaptiveStep.R

setClass("Impulse", contains = c("ODE"),
         slots = c(
             epsilon = "numeric"
         ))

setMethod("initialize", signature = c("Impulse"), function(.Object) {
    .Object@epsilon <- 0.01
    .Object@state <- c(-3.0, 1.0, 0.0)    # x, v, t
    return(.Object)
})

setMethod("getState", signature = c("Impulse"), function(object, ...) {
    return(object@state)
})

setMethod("getRate", signature = c("Impulse"), function(object, state, ...) {
    object@rate[1] <- state[2]
    object@rate[2] <- object@epsilon / ( object@epsilon * object@epsilon +
                                             state[1] * state[1] )
    object@rate[3] <- 1                             # dt/dt
    return(object@rate)
})
