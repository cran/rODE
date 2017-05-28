context("test Pendulum as app")

source(paste(system.file("examples", package = "rODE"),
             "Pendulum.R", sep ="/"))

# source("./inst/examples/Pendulum.R")                   # this script uses Euler-Richardson


ode <- new("ODE")
pendulum <- Pendulum()

dt <- 0.1
theta <- 0.2
thetaDot <- 0

pendulum@state[3] <- 0      # set time to zero, t = 0

pendulum <- setState(pendulum, theta = theta, thetaDot = thetaDot)
stepSize <- dt
pendulum <- setStepSize(pendulum, dt = stepSize)

state <- c(0.2, 0.0, 0.0)

#
#
expect_equal(pendulum@odeSolver@ode@state, c(0.2, 0.0, 0.0))

expect_equal(getStepSize(pendulum@odeSolver), 0.1) # get default step size

# ----------------------------------------- these two vars hold no values
# expect_true(length(pendulum@state) == 0)
expect_true(length(pendulum@rate)  == 0)

# +++++++++++++++++++++++++++++++++++++++++ values stored here
expect_true(all(pendulum@odeSolver@ode@state == state))
expect_true(length(pendulum@odeSolver@ode@rate) == 3)

pendulum@odeSolver <- init(pendulum@odeSolver, 0.123)
expect_equal(getStepSize(pendulum@odeSolver), 0.123)

expect_equal(pendulum@odeSolver@ode@state, state)
expect_equal(pendulum@odeSolver@numEqn, 3)

pendulum@odeSolver <- setStepSize(pendulum@odeSolver, dt) # set new step size
expect_equal(getStepSize(pendulum@odeSolver), dt)

expect_true(!is.null(step(pendulum@odeSolver))) # step Euler returns a value
expect_true(length(ode@rate)  == 0)

rate <- c(0, 0, 0)
rate <- vector("numeric")
expect_equal(getRate(pendulum@odeSolver@ode, state),
             c(0, -0.596008, 1))

expect_equal(getStepSize(pendulum@odeSolver), dt)

# run until time reaches 100
while (pendulum@state[3] <= 100)    {
    pendulum <- step(pendulum)
    # cat(sprintf("%12f %12f %12f \n",
    #             pendulum@state[1],  # angle
    #             pendulum@state[2],  # derivative of angle
    #             pendulum@state[3])) # time
}

test_that("last vector has these values", {
expect_equal(c(pendulum@state[1], pendulum@state[2], pendulum@state[3]),
             c(-0.129472,     0.314760,   100.100000))
})
