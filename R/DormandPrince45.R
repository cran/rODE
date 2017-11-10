#' DormandPrince45 ODE solver class
#'
#' @param object a class object
#' @param ode ODE object
#' @param enable a logical flag
#' @param stepSize size of the step
#' @param tol tolerance
#' @param value step size to set
#' @param ... additional parameters
#'
#' @rdname DormandPrince45-class
#'
#' @include ODEAdaptiveSolver.R ODE.R
#' @example ./inst/examples/KeplerDormandPrince45.R
.DormandPrince45 <- setClass("DormandPrince45", slots = c(
                    error_code       = "numeric",
                    a                = "matrix",
                    b5               = "numeric",
                    er               = "numeric",
                    numStages        = "numeric",
                    stepSize         = "numeric",
                    numEqn           = "numeric",
                    temp_state       = "numeric",
                    k                = "matrix",
                    truncErr         = "numeric",
                    ode              = "ODE",
                    tol              = "numeric",
                    enableExceptions = "logical"
                    ),
                    contains = c("ODEAdaptiveSolver")
                    )

#' DormandPrince45 generic
#'
#' @rdname DormandPrince45-class
#' @export
setGeneric("DormandPrince45", function(ode, ...) standardGeneric("DormandPrince45"))


setMethod("initialize", "DormandPrince45", function(.Object, ode, ...) {
    # initialized the Euler ODE solver
    .Object@error_code <- .Object@NO_ERROR
    .Object@a <- rbind( c(1.0/5.0, 0, 0, 0, 0),
                c(3.0/40.0, 9.0/40.0, 0, 0, 0),
                c(3.0/10.0, -9.0/10.0, 6.0/5.0, 0, 0),
                c(226.0/729.0, -25.0/27.0, 880.0/729.0, 55.0/729.0, 0),
                c(-181.0/270.0, 5.0/2.0, -266.0/297.0, -91.0/27.0, 189.0/55.0))

    .Object@b5 <- c(19.0/216.0, 0.0, 1000.0/2079.0, -125.0/216.0, 81.0/88.0, 5.0/56.0)
    .Object@er <- c(-11.0/360.0, 0.0, 10.0/63.0, -55.0/72.0, 27.0/40.0, -11.0/280.0)
    .Object@numStages <- 6    # number of intermediate rate computations
    .Object@stepSize  <- 0.01
    .Object@numEqn    <- 0
    .Object@ode       <- ode                    # set the ode to ODESolver slot
    .Object@tol       <- 1.0e-6
    .Object@enableExceptions <- FALSE
    return(.Object)
})


#' @rdname DormandPrince45-class
setMethod("init", "DormandPrince45", function(object, stepSize, ...) {
    # inititalize the solver
    object@stepSize <- stepSize
    state <- getState(object@ode)
    if (is.null(state)) {
        stop("state vector not defined")
        return(object)      # state vector not defined.
    }
    if (object@numEqn != length(state)) {
        object@numEqn <- length(state)
        object@temp_state <- vector("numeric", object@numEqn)
        object@k <- matrix(data = 0, nrow = object@numStages, ncol =  object@numEqn)
    }
    object@ode@state <- state
    object
})

#' @rdname DormandPrince45-class
setReplaceMethod("init", "DormandPrince45", function(object,  ..., value) {
    stepSize <- value
    # inititalize the solver
    object@stepSize <- stepSize
    state <- getState(object@ode)
    if (is.null(state)) {
        stop("state vector not defined")
        return(object)      # state vector not defined.
    }
    if (object@numEqn != length(state)) {
        object@numEqn <- length(state)
        object@temp_state <- vector("numeric", object@numEqn)
        object@k <- matrix(data = 0, nrow = object@numStages, ncol =  object@numEqn)
    }
    object@ode@state <- state
    object
})


#' @rdname DormandPrince45-class
setMethod("step", "DormandPrince45", function(object, ...) {
    object@error_code <- object@NO_ERROR
    iterations        <- 10
    currentStep       <- object@stepSize
    error             <- 0
    state             <- getState(object@ode)
    object@k[1,]      <- getRate(object@ode, state)
    # NEW iteration
    repeat  {
        iterations  <- iterations - 1
        currentStep <- object@stepSize
        # compute the k's
        cum <- 0
        for (s in 2:object@numStages) {
            for (i in 1:object@numEqn) {
                object@temp_state[i] <- state[i]
                for (j in 1:(s-1)) {
                    object@temp_state[i] <- object@temp_state[i] +
                        object@stepSize * object@a[s-1, j] * object@k[j, i]
                    cum <- cum + 1    # dummy cum to test how many iterations
                }
            }
            # print k array
            # get the ODE object, state and rate
            # object@ode <- getRate(object@ode, object@temp_state, object@k[s,])
            # assign rate to k vector
            # object@k[s,] <- object@ode@rate    # in Java rate is passed by param

            object@k[s,] <- getRate(object@ode, object@temp_state)   # in Java rate is passed by param


        } # end for loop "s"
        # compute the error
        error <- 0
        for (i in 1:object@numEqn) {
            object@truncErr <- 0
            for (s in 1:object@numStages) {
                object@truncErr <- object@truncErr +
                    object@stepSize * object@er[s] * object@k[s, i]
            }
            error <- max(error, abs(object@truncErr))
        }
        if (error <= 1.4e-45) {          # error too small to be meaningful,
            error <- object@tol / 1.0e5  # increase step size x10
        }
        # find h step for the next try. Update stepSize
        if (error > object@tol) {                     # shrink, no more than x10
            fac <- 0.9 * (error / object@tol)^-0.25
            object@stepSize <- object@stepSize * max(fac, 0.1)
        } else if (error < object@tol / 10.0) {   # grow, but no more than x10
            fac <- 0.9 * (error / object@tol)^-0.2
            if (fac > 1) { # sometimes fac is <1 because error/tol is close to 1
                object@stepSize <- object@stepSize * min(fac, 10)
            }
        }
        if (!((error > object@tol) && (iterations > 0))) break
    }   # end repeat loop
    # advance the state
    for (i in 1:object@numEqn) {
        for (s in 1:object@numStages) {
            state[i] <- state[i] + currentStep * object@b5[s] * object@k[s, i]
        }
    }
    if (iterations == 0) {
        object@error_code <- object@DID_NOT_CONVERGE
        if (object@enableExceptions) {
            warning("DormandPrince45 ODE SOlver did not converge")
        }
    }
    object@ode@state <- state
    return(object)
}
)


#' @rdname DormandPrince45-class
#' @aliases enableRuntimeExceptions,enableRuntimeExceptions-method
setMethod("enableRuntimeExceptions", "DormandPrince45", function(object, enable) {
    object@enableExceptions <- enable
})


#' @rdname DormandPrince45-class
setMethod("setStepSize", "DormandPrince45", function(object, stepSize, ...) {
    object@stepSize <- stepSize
    object
})


#' @rdname DormandPrince45-class
setMethod("getStepSize", "DormandPrince45", function(object, ...) {
    return(object@stepSize)
})


#' @rdname DormandPrince45-class
#' @aliases setTolerance,setTolerance-method
#' @example ./inst/examples/ComparisonRK45ODEApp.R
#' @family adaptive solver methods
setMethod("setTolerance", "DormandPrince45", function(object, tol) {
    object@tol <- abs(tol)
    if (object@tol < 1.0E-12) {
        err_msg = "Error: Dormand-Prince ODE solver tolerance cannot be smaller than 1.0e-12." # $NON-NLS-1$
        if (object@enableExceptions) {
            stop(err_msg)
        }
        cat(err_msg, "\n")
        object@tol <- 1.0e-12
    }
    return(object)
})


#' @rdname DormandPrince45-class
#' @aliases setTolerance,setTolerance-method
# #' @example ./inst/examples/ComparisonRK45ODEApp.R
#' @family adaptive solver methods
setReplaceMethod("setTolerance", "DormandPrince45", function(object, ..., value) {
    tol <- value
    object@tol <- abs(tol)
    if (object@tol < 1.0E-12) {
        err_msg = "Error: Dormand-Prince ODE solver tolerance cannot be smaller than 1.0e-12." # $NON-NLS-1$
        if (object@enableExceptions) {
            stop(err_msg)
        }
        cat(err_msg, "\n")
        object@tol <- 1.0e-12
    }
    return(object)
})


#' @rdname DormandPrince45-class
#' @aliases getTolerance,getTolerance-method
#' @family adaptive solver methods
setMethod("getTolerance", "DormandPrince45", function(object) {
    return(object@tol)
})

#' @rdname DormandPrince45-class
#' @aliases getErrorCode,getErrorCode-method
#' @family adaptive solver methods
setMethod("getErrorCode", "DormandPrince45", function(object) {
    return(object@error_code)
})





#' DormandPrince45 constructor ODE
#'
#' @rdname DormandPrince45-class
#' @importFrom methods new
#' @export
setMethod("DormandPrince45", signature(ode = "ODE"), function(ode, ...) {
    # constructor for DormandPrince45 ODE solver
    dormandPrince45 <- .DormandPrince45(ode = ode)
    dormandPrince45 <- init(dormandPrince45, dormandPrince45@stepSize)                         # diff 5
    return(dormandPrince45)
})
