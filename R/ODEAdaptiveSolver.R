# ODEAdaptiveSolver.R
#
# Base class to be inherited by adaptive solvers such as RK45
#

#' @include ODESolver.R
setClass("ODEAdaptiveSolver", slots = c(
    NO_ERROR                  = "numeric",
    DID_NOT_CONVERGE          = "numeric",
    BISECTION_EVENT_NOT_FOUND = "numeric"
    ),
    prototype = prototype(
        NO_ERROR                  = 0,
        DID_NOT_CONVERGE          = 1,
        BISECTION_EVENT_NOT_FOUND = 2
    ),
    contains = c("ODESolver")
)

#' @rdname setTolerance-method
setMethod("setTolerance", "ODEAdaptiveSolver", function(object, tol) {
})


#' @rdname getTolerance-method
setMethod("getTolerance", "ODEAdaptiveSolver", function(object) {
})

#' @rdname getErrorCode-method
setMethod("getErrorCode", "ODEAdaptiveSolver", function(object) {
})
