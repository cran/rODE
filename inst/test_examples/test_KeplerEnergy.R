library(testthat)
context("test KeplerEnergy as app")

#
# Demostration of the use of ODE solver RK45
#

source(paste(system.file("examples", package = "rODE"),
             "KeplerEnergy.R", sep ="/"))

# source("./inst/examples/KeplerEnergy.R")

particle <- KeplerEnergy()

expect_equal(slotNames(particle), c("GM", "odeSolver", "counter", "state", "rate"))
expect_equal(particle@state, c(0,0,0,0,0))
expect_equal(getState(particle), c(0,0,0,0,0))
expect_true(getTime(particle) == 0)
expect_true(getEnergy(particle) == -Inf)
expect_equal(getRate(particle, c(0,0,0,0,0)), c(0, NaN,   0, NaN,   1))


x <- 1
vx <- 0
y <- 0
vy <- 2 * pi
dt <- 0.01
tol <- 1e-3

# cat("state(before)=", particle@state, "\n")
particle <- init(particle, c(x, vx, y, vy, 0))
# cat(" state(after)=", particle@state, "\n")

odeSolver <- Verlet(particle)

# cat("stepSize (before)=", particle@odeSolver@stepSize, "\n")
odeSolver <- init(odeSolver, dt)
# cat("   stepSize(afer)=", particle@odeSolver@stepSize, "\n")

# cat("state(before) =", particle@state, particle@odeSolver@ode@state,"\n")
particle@odeSolver <- odeSolver
# cat(" state(after) =", particle@state, particle@odeSolver@ode@state,"\n")

# odeSolver <- setTolerance(odeSolver, tol)

# particle@odeSolver <- init(particle@odeSolver, dt)



# particle <- init(particle, c(x, vx, y, vy, 0))

expect_equal(slotNames(particle), c("GM", "odeSolver", "counter", "state", "rate"))
expect_equal(particle@state, c(1.000000, 0.000000, 0.000000, 6.283185, 0.000000), tolerance = 1e-6)
expect_equal(getRate(particle, c(0,0,0,0,0)), c(0, NaN,   0, NaN,   1))


# cat(particle@odeSolver@ode@state)

particle <- doStep(particle)

expect_equal(particle@state,
             c(0.99802608, -0.39439339,  0.06283185,  6.27078287,  0.01000000),
             tolerance = 1e-6)


initialEnergy <- getEnergy(particle)
expect_equal(initialEnergy, -19.73921, tolerance = 1e-6)

i <- 0
while (getTime(particle) <= 1.20) {
    particle <- doStep(particle)
    energy <- getEnergy(particle)
    # cat(sprintf("time=%12f energy=%12f state[5]=%12f \n",
    #             getTime(particle), energy, particle@state[5]))
    i <- i + 1
}

expect_equal((c(getTime(particle), energy, particle@state[1], particle@state[2],
      particle@state[3], particle@state[4], particle@state[5])),
c(1.2, -19.73918, 0.3168587, -5.953966, 0.9491895, 1.993771, 1.2),
tolerance = 1e-6
)
