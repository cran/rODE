#' Get the number of times that the rate has been calculated
#'
#' @param object a class object
#' @param ... additional parameters
#' @rdname getRateCounts-method
#' @export
setGeneric("getRateCounts", function(object, ...)
    standardGeneric("getRateCounts"))


# Generic functions for constructors +++++++++++++++++++++++++++++++++++++++++
#' @rdname AbstractODESolver-class
#' @export
#' @examples
#' # This is how we start defining a new ODE solver: Euler
#' .Euler <- setClass("Euler",              # Euler solver very simple; no slots
#'      contains = c("AbstractODESolver"))
#'
#'
#'
#' # Here we define the ODE solver Verlet
#' .Verlet <- setClass("Verlet", slots = c(
#'     rate1 = "numeric",                          # Verlet calculates two rates
#'     rate2 = "numeric",
#'     rateCounter = "numeric"),
#' contains = c("AbstractODESolver"))
#'
#'
#'
#' # This is the definition of the ODE solver Runge-Kutta 4
#' .RK4 <- setClass("RK4", slots = c(       # On the other hand RK4 uses 4 rates
#'    rate1 = "numeric",
#'    rate2 = "numeric",
#'    rate3 = "numeric",
#'    rate4 = "numeric",
#'    estimated_state = "numeric"),         # and estimates another state
#' contains = c("AbstractODESolver"))
#'
setGeneric("AbstractODESolver", function(ode, ...)
    standardGeneric("AbstractODESolver"))


#' @rdname EulerRichardson-class
#' @export
#' @example ./inst/examples/PendulumApp.R
setGeneric("EulerRichardson", function(ode, ...)
    standardGeneric("EulerRichardson"))


#' @rdname RK4-class
#' @export
#' @example ./inst/examples/Projectile.R
#' @example ./inst/examples/PendulumApp.R
setGeneric("RK4", function(ode, ...)
    standardGeneric("RK4"))


#' @rdname Verlet-class
#' @export
#' @example ./inst/examples/Logistic.R
setGeneric("Verlet", function(ode, ...)
    standardGeneric("Verlet"))

#' @rdname Euler-class
#' @export
#' @example ./inst/examples/RigidBodyNXFApp.R
setGeneric("Euler", function(ode, ...)
    standardGeneric("Euler"))

# +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

#' Get a new rate given a state
#'
#' @rdname getRate-method
#' @param object a class object
#' @param state current state
#' @param ... additional parameters
#' @export
#' @example ./inst/examples/Kepler.R
setGeneric("getRate", function(object, state, ...) standardGeneric("getRate"))


#' Get current state of the system
#'
#' @param object a class object
#' @param ... additional parameters
#' @rdname getState-method
#' @export
#' @example ./inst/examples/VanderPol.R
#' @example ./inst/examples/SpringRK4.R
setGeneric("getState", function(object, ...) standardGeneric("getState"))


#' Advances a step in the ODE solver
#'
#' @param object a class object
#' @param ... additional parameters
#' @rdname step-method
#' @export
#' @example ./inst/examples/ReactionApp.R
setGeneric("step", function(object, ...) standardGeneric("step"))



#' Get the step size
#'
#' @param object a class object
#' @param ... additional parameters
#' @rdname getStepSize-method
#' @export
#' @example ./inst/examples/ComparisonRK45ODEApp.R
setGeneric("getStepSize", function(object, ...) standardGeneric("getStepSize"))



#' Perform a step
#'
#' @param object a class object
#' @param ... additional parameters
#' @rdname doStep-method
#' @export
#' @example ./inst/examples/PlanetApp.R
#' @example ./inst/examples/Logistic.R
setGeneric("doStep", function(object, ...) standardGeneric("doStep"))



#' Set initial values before starting the ODE solver
#'
#' Not all super classes require an init method.
#'
#' @param object a class object
#' @param ... additional parameters
#' @param stepSize size of the step
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
#' # init method in Logistic.R
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



#' setStepSize uses either of two step parameters: stepSize and dt
#' `stepSize`` works for most of the applications
#' `dt`` is used in Pendulum
#'
#' @param object a class object
#' @param ... additional parameters
#' @param stepSize size of the step
#' @rdname setStepSize-method
#' @export
#' @example ./inst/examples/SpringRK4.R
#' @example ./inst/examples/ComparisonRK45App.R
setGeneric("setStepSize", function(object, ...) standardGeneric("setStepSize"))



#' New setState that should work with different methods
#'  "theta", "thetaDot":  used in PendulumApp
#'  "x", "vx", "y", "vy": used in ProjectileApp
#'
#' @param object a class object
#' @param ... additional parameters
#' @export
#' @example ./inst/examples/ProjectileApp.R
#' @example ./inst/examples/PendulumApp.R
setGeneric("setState", function(object, ...) standardGeneric("setState"))



#' Set the tolerance for the solver
#'
#' @param object a class object
#' @param ... additional parameters
#' @param tol tolerance
#' @rdname setTolerance-method
#' @export
#' @example ./inst/examples/ComparisonRK45App.R
#' @example ./inst/examples/KeplerDormandPrince45App.R
setGeneric("setTolerance", function(object, tol, ...)
    standardGeneric("setTolerance"))


#' Get the tolerance for the solver
#'
#' @param object a class object
#' @param ... additional parameters
#' @rdname getTolerance-method
#' @export
setGeneric("getTolerance", function(object, ...) standardGeneric("getTolerance"))


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

#' Enable Runtime Exceptions
#'
#' @rdname DormandPrince45-class
#' @export
#' @examples
#' setMethod("enableRuntimeExceptions", "DormandPrince45", function(object, enable) {
#'     object@enableExceptions <- enable
#' })
setGeneric("enableRuntimeExceptions", function(object, enable, ...)
    standardGeneric("enableRuntimeExceptions"))

#' Get the rate counter
#'
#' How many time the rate has changed with a step
#'
#' @rdname Verlet-class
#' @export
#' @example ./inst/examples/ComparisonRK45App.R
setGeneric("getRateCounter", function(object, ...)
    standardGeneric("getRateCounter"))


#' Get the elapsed time
#'
#' @param object a class object
#' @param ... additional parameters
#' @rdname getTime-method
#' @export
#' @example ./inst/examples/Logistic.R
#' @example ./inst/examples/KeplerEnergy.R
setGeneric("getTime", function(object, ...) standardGeneric("getTime"))



#' Get the calculated energy level
#'
#' @param object a class object
#' @param ... additional parameters
#' @rdname getEnergy-method
#' @export
#' @example ./inst/examples/KeplerEnergy.R
setGeneric("getEnergy", function(object, ...) standardGeneric("getEnergy"))



#' Compare analytical and calculated solutions
#'
#' @param object a class object
#' @param ... additional parameters
#' @param t time ath what we are performing the evaluation
#' @rdname getExactSolution-method
#' @export
#' @example ./inst/examples/ComparisonRK45App.R
#' @example ./inst/examples/ODETest.R
setGeneric("getExactSolution", function(object, t, ...)
    standardGeneric("getExactSolution"))

