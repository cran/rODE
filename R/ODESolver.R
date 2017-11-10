
#' ODESolver virtual class
#'
#' A virtual class inherited by AbstractODESolver
#'
#' @param object a class object
#' @param stepSize size of the step
#' @param ... additional parameters
#'
#' @rdname ODESolver-class
#' @family ODESolver helpers
#' @include ode_generics.R
.ODESolver <- setClass("ODESolver")

#' ODESolver constructor
#' @rdname ODESolver-class
#' @export
ODESolver <- function(object, stepSize, ...) {
    .ODESolver
}

#' Set initial values and get ready to start the solver
#' @rdname ODESolver-class
setMethod("init", "ODESolver", function(object, stepSize, ...) {
    NULL
})

#' @rdname ODESolver-class
setMethod("step", "ODESolver", function(object, ...) {
    object
})


#' @rdname ODESolver-class
setMethod("getODE", "ODESolver", function(object, ...) {
    object@ode
})


# #' #' @rdname setSolver-method
# #' setMethod("setSolver", "ODESolver", function(object, ode, ...) {
# #'     object@ode <- ode
# #'     object@ode
# #' })



#' Set the size of the step
#'
#' @rdname ODESolver-class
setMethod("setStepSize", "ODESolver", function(object, stepSize, ...) {
    # set the current value of the step
    object@stepSize = stepSize
    object
})

#' @rdname ODESolver-class
setMethod("getStepSize", "ODESolver", function(object, ...) {
    # get the current value of the step
    object@stepSize
})


