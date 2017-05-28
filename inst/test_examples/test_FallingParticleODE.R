# +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++  + + unit tests
library(testthat)

context("test FallingParticleODE as app")

# source(paste(system.file("examples", package = "rODE"),
#              "FallingParticleODE.R", sep ="/"))

source(file.path(system.file("examples", package="rODE"), "FallingParticleODE.R"))

# source("./inst/examples/FallingParticleODE.R")

initial_y <- 10
initial_v <- 0
dt <- 0.1

ball <- FallingParticleODE(initial_y, initial_v)

expect_true(ball@g == 9.8)
expect_equal(ball@state, c(10, 0, 0))

eusolver <- Euler(ball)
eusolver <- setStepSize(eusolver, dt)

expect_equal(getStepSize(eusolver), dt)
expect_equal(eusolver@stepSize, getStepSize(eusolver))
expect_equal(eusolver@ode@rate, c(0, 0, 0))

eusolver <- step(eusolver)
rate <- eusolver@ode@rate

while (ball@state[1] > 0) {
    eusolver <- step(eusolver)
    ball <- eusolver@ode
    # cat(sprintf("%12f %12f ",  ball@state[1], ball@rate[1] ))
    # cat(sprintf("%12f %12f ",  ball@state[2], ball@rate[2] ))
    # cat(sprintf("%12f %12f\n", ball@state[3], ball@rate[3] ))
    expect_false(all(eusolver@ode@state == c(10, 0, 0)))  # step makes the original state vector to change
}


test_that("match this vector at the end of run with dt = 0.1", {
    expect_equal(getStepSize(eusolver), dt)
    expect_equal(round(ball@state[1], 5), -0.29)      # obtained with dt = 0.1
    expect_equal(c(
                ball@state[1], ball@rate[1],
                ball@state[2], ball@rate[2],
                ball@state[3], ball@rate[3]),
             c(-0.290000,   -13.720000,
               -14.700000,  -9.800000,
               1.500000,     1.000000))
})

