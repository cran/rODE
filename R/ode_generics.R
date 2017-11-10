
#' getRateCounts
#'
#' Get the number of times that the rate has been calculated
#'
#' @param object a class object
#' @param ... additional parameters
#' @rdname getRateCounts-method
#' @export
setGeneric("getRateCounts", function(object, ...)
    standardGeneric("getRateCounts"))



#' getRate
#'
#' Get a new rate given a state
#'
#' @rdname getRate-method
#' @param object a class object
#' @param state current state
#' @param ... additional parameters
#' @export
#' @example ./inst/examples/Kepler.R
setGeneric("getRate", function(object, state, ...) standardGeneric("getRate"))


#' getState
#'
#' Get current state of the system
#'
#' @param object a class object
#' @param ... additional parameters
#' @rdname getState-method
#' @export
#' @example ./inst/examples/VanderpolApp.R
#' @example ./inst/examples/SpringRK4App.R
setGeneric("getState", function(object, ...) standardGeneric("getState"))


#' step
#'
#' Advances a step within the ODE solver
#'
#' @param object a class object
#' @param ... additional parameters
#' @rdname step-method
#' @export
#' @example ./inst/examples/ReactionApp.R
setGeneric("step", function(object, ...) standardGeneric("step"))


#' getODE
#'
#' Get the ODE status from the solver
#'
#' @param object a class object
#' @param ... additional parameters
#' @rdname getODE-method
#' @export
setGeneric("getODE", function(object, ...) standardGeneric("getODE"))


#' setSolver
#'
#' Set a solver over an ODE object
#'
#' @param object a class object
#' @param value value to be set
#' @rdname setSolver-method
#' @export
setGeneric("setSolver<-", function(object, value) {standardGeneric("setSolver<-")})


#' getStepSize
#'
#' Get the current value of the step size
#'
#' @param object a class object
#' @param ... additional parameters
#' @rdname getStepSize-method
#' @export
#' @example ./inst/examples/ComparisonRK45ODEApp.R
setGeneric("getStepSize", function(object, ...) standardGeneric("getStepSize"))



#' doStep
#'
#' Perform a step
#'
#' @param object a class object
#' @param ... additional parameters
#' @rdname doStep-method
#' @export
#' @example ./inst/examples/PlanetApp.R
#' @example ./inst/examples/LogisticApp.R
setGeneric("doStep", function(object, ...) standardGeneric("doStep"))


#' init
#'
#' Set initial values before starting the ODE solver
#'
#' Sets the tolerance like this: solver <- init(solver, dt)
#' Not all super classes require an init method.
#'
#' @param object a class object
#' @param ... additional parameters
#' @param value a value to set
#' @rdname init-method
#' @export
#' @examples
#' # init method in Kepler.R
#' setMethod("init", "Kepler", function(object, initState, ...) {
#'     object@state <- initState
#'     object@odeSolver <- init(object@odeSolver, getStepSize(object@odeSolver))
#'     object@counter <- 0
#'     object
#' })
#'
#' # init method in LogisticApp.R
#' setMethod("init", "Logistic", function(object, initState, r, K, ...) {
#'     object@r <- r
#'     object@K <- K
#'     object@state <- initState
#'     object@odeSolver <- init(object@odeSolver, getStepSize(object@odeSolver))
#'     object@counter <- 0
#'     object
#' })
#'
#' # init method in Planet.R
#' setMethod("init", "Planet", function(object, initState, ...) {
#'     object@state <- object@odeSolver@ode@state <- initState
#'     # initialize providing the step size
#'     object@odeSolver <- init(object@odeSolver, getStepSize(object@odeSolver))
#'     object@rate <- object@odeSolver@ode@rate
#'     object@state <- object@odeSolver@ode@state
#'     object
#' })
setGeneric("init", function(object, ...) standardGeneric("init"))

#' init<-
#'
#' Set initial values before starting the ODE solver
#'
#' Sets the tolerance like this: init(solver) <- dt
#'
#' @rdname init-method
#' @export
setGeneric("init<-", function(object, ..., value) standardGeneric("init<-"))


#' setStepSize
#'
#' setStepSize uses either of two step parameters: stepSize and dt
#' stepSize works for most of the applications
#' dt is used in Pendulum
#'
#' @param object a class object
#' @param ... additional parameters
#'
#' @rdname setStepSize-method
#' @export
#' @example ./inst/examples/SpringRK4App.R
#' @example ./inst/examples/ComparisonRK45App.R
setGeneric("setStepSize", function(object, ...) standardGeneric("setStepSize"))


#' setState
#'
#' New setState that should work with different methods
#'  "theta", "thetaDot":  used in PendulumApp
#'  "x", "vx", "y", "vy": used in ProjectileApp
#'
#' @param object a class object
#' @param ... additional parameters
#'
#' @rdname setState-method
#' @export
#' @example ./inst/examples/ProjectileApp.R
#' @example ./inst/examples/PendulumApp.R
setGeneric("setState", function(object, ...) standardGeneric("setState"))


#' setTolerance
#'
#' Set the tolerance for the solver
#'
#' Sets the tolerance like this: odeSolver <- setTolerance(odeSolver, tol)
#'
#' @param object a class object
#' @param tol tolerance
#'
#' @rdname setTolerance-method
#' @export
#' @example ./inst/examples/ComparisonRK45App.R
#' @example ./inst/examples/KeplerDormandPrince45App.R
#' @example ./inst/examples/AdaptiveStepApp.R
setGeneric("setTolerance", function(object, tol) standardGeneric("setTolerance"))


#' setTolerance<-
#'
#' Set the tolerance for the solver
#'
#' Sets the tolerance like this: setTolerance(odeSolver) <- tol
#'
#' @param ... additional parameters
#' @param value a value to set
#'
#' @rdname setTolerance-method
#' @export
setGeneric("setTolerance<-", function(object, ..., value) standardGeneric("setTolerance<-"))


#' getTolerance
#'
#' Get the tolerance for the solver
#'
#' @param object a class object
#' @param ... additional parameters
#'
#' @rdname getTolerance-method
#' @export
setGeneric("getTolerance", function(object, ...) standardGeneric("getTolerance"))



#' getErrorCode
#'
#' Get an error code
#'
#' @param object a class object
#' @param ... additional parameters
#' @param tol tolerance
#' @rdname getErrorCode-method
#' @export
#' @examples
#' setMethod("getErrorCode", "DormandPrince45", function(object) {
#' return(object@error_code)
#' })
setGeneric("getErrorCode", function(object, tol, ...)
    standardGeneric("getErrorCode"))



#' enableRuntimeExceptions
#'
#' Enable Runtime Exceptions
#'
#' @param object a class object
#' @param enable a boolean to enable exceptions
#' @param ... additional parameters
#'
#' @rdname enableRuntimeExceptions-method
#' @export
#' @examples
#' setMethod("enableRuntimeExceptions", "DormandPrince45", function(object, enable) {
#'     object@enableExceptions <- enable
#' })
setGeneric("enableRuntimeExceptions", function(object, enable, ...)
    standardGeneric("enableRuntimeExceptions"))



#' getRateCounter
#'
#' Get the rate counter
#'
#' How many times the rate has changed with a step
#'
#' @param object a class object
#' @param ... additional parameters
#'
#' @rdname getRateCounter-method
#' @export
#' @example ./inst/examples/ComparisonRK45App.R
setGeneric("getRateCounter", function(object, ...)
    standardGeneric("getRateCounter"))


#' getTime
#'
#' Get the elapsed time
#'
#' @param object a class object
#' @param ... additional parameters
#'
#' @rdname getTime-method
#' @export
#' @example ./inst/examples/LogisticApp.R
#' @example ./inst/examples/KeplerEnergy.R
setGeneric("getTime", function(object, ...) standardGeneric("getTime"))


#' getEnergy
#'
#' Get the calculated energy level
#'
#' @param object a class object
#' @param ... additional parameters
#'
#' @rdname getEnergy-method
#' @export
#' @example ./inst/examples/KeplerEnergy.R
setGeneric("getEnergy", function(object, ...) standardGeneric("getEnergy"))


#' getExactSolution
#'
#' Compare analytical and calculated solutions
#'
#' @param object a class object
#' @param ... additional parameters
#' @param t time ath what we are performing the evaluation
#'
#' @rdname getExactSolution-method
#' @export
#' @example ./inst/examples/ComparisonRK45App.R
#' @example ./inst/examples/ODETest.R
setGeneric("getExactSolution", function(object, t, ...)
    standardGeneric("getExactSolution"))

