library(testthat)
context("test Kepler as app")

# test_KeplerApp.R
#
# Demostration of the use of ODE solver RK45
#
#

source(paste(system.file("examples", package = "rODE"),
             "Kepler.R", sep ="/"))

# source("./inst/examples/Kepler.R")

# set the orbit into a predefined state.
r <- c(2, 0)
v <- c(0, 0.25)
dt <- 0.1

planet <- Kepler(r, v)
solver <- RK45(planet)

while (planet@state[5] <= 10) {                              # stop at t = 10
    solver <- step(solver)
    planet <- solver@ode
    # cat(sprintf("state[1]=%12f, state[2]= %12f,  state[3]=%12f, state[5]=%12f\n",
    #             planet@state[1],
    #             planet@state[2], planet@state[3], planet@state[5]))
}

expect_equal(c(planet@state[1],  planet@state[2], planet@state[3], planet@state[5]),
c(0.444912, -1.436203, 0.459081,  10.033245), tolerance = 1e-7)
