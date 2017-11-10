#' ODEAdaptiveSolver class
#'
#' Base class to be inherited by adaptive solvers such as RK45
#'
#' @param object a class object
#' @param tol tolerance
#' @param value the value for the tolerance
#' @param ... additional parameters
#'
#' @rdname ODEAdaptiveSolver-class
#' @include ODESolver.R
#' @export
.ODEAdaptiveSolver <- setClass("ODEAdaptiveSolver",
        slots = c(
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


#' ODEAdaptiveSolver generic
#'
#' @rdname ODEAdaptiveSolver-class
setGeneric("ODEAdaptiveSolver", function(...) standardGeneric("ODEAdaptiveSolver"))


#' @rdname ODEAdaptiveSolver-class
setMethod("setTolerance", "ODEAdaptiveSolver", function(object, tol) {
    NULL
})

#' @rdname ODEAdaptiveSolver-class
setReplaceMethod("setTolerance", "ODEAdaptiveSolver", function(object, ..., value) {
    NULL
})

#' @rdname ODEAdaptiveSolver-class
setMethod("getTolerance", "ODEAdaptiveSolver", function(object) {
})

#' @rdname ODEAdaptiveSolver-class
setMethod("getErrorCode", "ODEAdaptiveSolver", function(object) {
})





#' ODEAdaptiveSolver constructor
#'
#' @rdname ODEAdaptiveSolver-class
#' @export
setMethod("ODEAdaptiveSolver", signature("ANY"), function(...) {
 # constructor for ODEAdaptiveSolver
 solver <- .ODEAdaptiveSolver()
 return(solver)
})
