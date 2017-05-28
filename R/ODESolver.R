# ODESolver.R
#

#' ODESolver virtual class
#'
#' A virtual class inherited by AbstractODESolver
#'
#' @param object a class object
#' @param stepSize size of the step
#'
#' @include ode_generics.R
setClass("ODESolver")


#' Set initial values and get ready to start the solver
#'
#' @rdname init-method
setMethod("init", "ODESolver", function(object, stepSize, ...) {
    object
})


#' @rdname step-method
setMethod("step", "ODESolver", function(object, ...) {
    object
})

#' Set the size of the step
#'
#' @rdname setStepSize-method
setMethod("setStepSize", "ODESolver", function(object, stepSize, ...) {
    # set the current value of the step
    object
})

#' @rdname getStepSize-method
setMethod("getStepSize", "ODESolver", function(object, ...) {
    # get the current value of the step
    object@stepSize
})
