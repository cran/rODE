context("test Reaction as app")

source(paste(system.file("examples", package = "rODE"),
             "Reaction.R", sep ="/"))

# source("./inst/examples/Reaction.R")

X <- 1; Y <- 5;
dt <- 0.1

reaction <- Reaction(c(X, Y, 0))

solver <- RK4(reaction)

expect_equal(solver@estimated_state, c(0, 0, 0))
expect_equal(solver@numEqn, 3)

test_that("rates are zero before step", {
    expect_equal(solver@rate1, c(0, 0, 0))
    expect_equal(solver@rate2, c(0, 0, 0))
    expect_equal(solver@rate3, c(0, 0, 0))
    expect_equal(solver@rate4, c(0, 0, 0))
})

solver <- step(solver)
# solver@ode@state
# solver@ode@rate
# solver@ode@
test_that("get these values after one step", {
    expect_equal(solver@ode@state, c(1.342695, 4.641994, 0.1), tolerance = 0.000001)
    expect_equal(solver@estimated_state, c(1.345462, 4.638375, 0.100000), tolerance = 0.000001)
    expect_equal(solver@numEqn, 3)
    expect_equal(solver@rate1, c(2.5, -2.5,  1.0))
    expect_equal(solver@rate2, c(3.232422, -3.357422,  1.000000), tolerance = 0.0000001)
    expect_equal(solver@rate3, c(3.454625, -3.616246,  1.000000), tolerance = 0.0000001)
    expect_equal(solver@rate4, c(4.687590, -5.033052,  1.000000), tolerance = 0.0000001)
})

while (solver@ode@state[3] <= 100.0) {
    # cat(reaction@state[1], reaction@state[2], reaction@state[3], "\n")
    solver <- step(solver)
    # reaction@state <- solver@ode@state
}

test_that("get this vector at the end of the loop", {
    expect_equal(c(solver@ode@state[1], solver@ode@state[2], solver@ode@state[3]),
             c(1.987618, 1.143675, 100.1))
})
