context("test Projectile with loop")

source(paste(system.file("examples", package = "rODE"),
             "Projectile.R", sep ="/"))

# source("./inst/examples/Projectile.R")


x <- 0; vx <- 10; y <- 0; vy <- 10
state <- c(x, vx, y, vy, 0)
dt <- 0.01

projectile <- Projectile()

expect_equal(getStepSize(projectile@odeSolver), 0.1) # get default step size
# ----------------------------------------- these two vars hold no values
expect_true(length(projectile@state) == 0)
expect_true(length(projectile@rate)  == 0)

# +++++++++++++++++++++++++++++++++++++++++ values stored here
expect_true(all(projectile@odeSolver@ode@state == state))
expect_true(length(projectile@odeSolver@ode@rate) == 0)


projectile <- setState(projectile, x, vx, y, vy)
    expect_equal(projectile@odeSolver@ode@state, state)
    expect_equal(projectile@odeSolver@numEqn, 0)
    expect_true(length(projectile@odeSolver@ode@rate) == 0)

projectile@odeSolver <- init(projectile@odeSolver, 0.123)

test_that("match these values after init", {
    expect_equal(getStepSize(projectile@odeSolver), 0.123)
    expect_equal(projectile@odeSolver@ode@state, state)
    # expect_equal(projectile@odeSolver@ode@rate, c(0, 0, 0, 0, 0))
    expect_equal(projectile@odeSolver@numEqn, 5)
})

projectile@odeSolver <- setStepSize(projectile@odeSolver, dt)
    expect_equal(getStepSize(projectile@odeSolver), dt)

# state[5]:           state[1]: x;   # state[3]: y
while (projectile@state[3] >= 0)    {
    projectile <- step(projectile)
    expect_equal(projectile@odeSolver@numEqn, 5)
    expect_equal(getStepSize(projectile@odeSolver), 0.01)

    # cat(sprintf("%12f %12f %12f \n", projectile@state[5],
    #             projectile@state[1], projectile@state[3]))
}

expect_equal(c(projectile@state[5], projectile@state[1], projectile@state[3]),
             c( 2.050000,    20.500000,    -0.0922500 ) )

