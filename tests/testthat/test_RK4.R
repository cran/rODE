# ######################################################### Test EulerRichardson
# Euler-Richardson class does not implement getState() and getRate()
# as in the case of Euler
#
#########################
library(testthat)

# source("./R/ode_generics.R")
# source("./R/ODE.R")
# source("./R/RK4.R")

state <- c(0, 1, 3)
rate  <- c(0, 0, 0)
stepSize <- 0.1

ode <- new("ODE")

# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ test constructor

# expect_error(RK4(), 'argument ".ode" is missing, with no default')

rk4 <- RK4(ode)

rk4@ode@state <-  state                # set a vector for state


# ----------------------------------------- these two vars hold no values
expect_true(length(ode@state) == 0)
expect_true(length(ode@rate)  == 0)

#                                                  values stored here in ode
expect_true(all(rk4@ode@state == state))
expect_true(length(rk4@ode@rate) == 0)

# +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ test init()
rk4 <- init(rk4, stepSize)

expect_equal(rk4@stepSize, stepSize)      # test stepSize

expect_equal(rk4@ode@state, state)
expect_equal(rk4@numEqn, 3)               # number of equations

expect_true(!is.null(step(rk4)))    # step returns a value not NULL
expect_true(length(ode@rate)  == 0)                     # rate still empty



# expect_equal(getRate(rk4@ode), c(0, 0, 0))  # rate has a vector



test_that("rates match after init", {
    expect_equal(rk4@rate1, c(0, 0, 0))  # rate before step
    expect_equal(rk4@rate2, c(0, 0, 0))  # rate before step
    expect_equal(rk4@rate3, c(0, 0, 0))  # rate before step
    expect_equal(rk4@rate4, c(0, 0, 0))  # rate before step
})

# +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ test step()

#                                rate is zero until we run setState and getRate

# test_that("state, rate match values after step", {
#     rk4 <- step(rk4)
#     expect_equal(rk4@ode@state, state)
#     expect_equal(rk4@ode@rate, rate)
#     expect_equal(rk4@rate1, c(0, 0, 0))  # rate after step
#     expect_equal(rk4@rate2, c(0, 0, 0))  # rate after step
#     expect_equal(rk4@rate3, c(0, 0, 0))  # rate after step
#     expect_equal(rk4@rate4, c(0, 0, 0))  # rate after step
#     expect_equal(rk4@stepSize, 0.1)      # test stepSize
# })
#
#
#
#
#
# rk4 <- step(rk4)
# expect_equal(rk4@ode@state, state)
# expect_equal(rk4@ode@rate, rate)
#
# rk4 <- step(rk4)
# expect_equal(rk4@ode@state, state)
# expect_equal(rk4@ode@rate, rate)
#
# rk4 <- step(rk4)
# expect_equal(rk4@ode@state, state)
# expect_equal(rk4@ode@rate, rate)
