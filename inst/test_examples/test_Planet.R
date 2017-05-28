context("test Planet")

source(paste(system.file("examples", package = "rODE"),
             "Planet.R", sep ="/"))

# source("./inst/examples/Planet.R")

# x =  1, AU or Astronomical Units. Length of semimajor axis or the orbit of the Earth around the Sun.

x <- 1; vx <- 0; y <- 0; vy <- 6.28; t <- 0
state <- c(x, vx, y, vy, t)
dt <-  0.01

planet <- Planet()

test_that("Have correct slot names", {
    expect_equal(slotNames(planet), c("odeSolver", "GM", "state", "rate"))
    # expect_equal(slotNames(euler@ode), c("state", "rate"))
})

test_that("Class is correct", {
    expect_true(class(planet) == "Planet")
})

planet@odeSolver <- setStepSize(planet@odeSolver, dt)
test_that("Get the default step size", {
    expect_equal(getStepSize(planet@odeSolver), 0.01)        # get default step size
})

planet <- init(planet, initState = state)

test_that("match these values after init", {
    expect_equal(getStepSize(planet@odeSolver), 0.01)
    expect_equal(planet@odeSolver@ode@state, state)
    expect_equal(planet@odeSolver@ode@rate, c(0, 0, 0, 0, 0))
    expect_equal(planet@odeSolver@numEqn, 5)
})

# run infinite loop. stop with ESCAPE.
while (planet@state[5] <= 30) {
    for (i in 1:5) {                 # advances time
        planet <- doStep(planet)
    }
}

# cat(sprintf("%12f %12f %12f %12f %12f \n", planet@state[1], planet@state[2],
#             planet@state[3], planet@state[4], planet@state[5]))

expect_equal(c(planet@state[1], planet@state[2], planet@state[3],
                               planet@state[4], planet@state[5]),
c(3.918366, -0.602274, -0.189784, 3.244568, 30.000000), tolerance = 1e-7)


# planet@state[5] <= 365
    # expect_equal(c(planet@state[1], planet@state[2], planet@state[3],
    #                planet@state[4], planet@state[5]),
    #              c(-1.657601,    -3.002031,     5.996665,    -0.407356,   365.050000 ))
