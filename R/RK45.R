#' RK45 ODE solver class
#'
#' @param ode and ODE object
#'
#' @rdname RK45-class
#' @example ./inst/examples/ComparisonRK45App.R
#' @example ./inst/examples/KeplerApp.R
setClass("RK45",
    contains = c("DormandPrince45")
)

setMethod("initialize", "RK45", function(.Object, ode, ...) {
    # initialized the ODE solver
    .Object@ode <- ode                          # set the ode to ODESolver slot
    return(.Object)
})



#' RK45 class constructor
#'
#' @rdname RK45-class
#'
#' @export
RK45 <- function(ode) {
    rk45 <- DormandPrince45(ode)             # equivalent to Java "super(ode)"
    return(rk45)
}


