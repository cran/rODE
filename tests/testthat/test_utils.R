library(testthat)
context("test utils")

test_that("Class AbstractODESolver has the correct methods", {
# print(showMethods2("AbstractODESolver"))
    expect_true(all(showMethods2("AbstractODESolver") %in%
             c("getStepSize", "init", "setStepSize", "step", "initialize")))
})

test_that("Class ODE has the correct methods", {
# print(showMethods2("ODE"))
    expect_true(all(showMethods2("ODE") %in%
                    c("AbstractODESolver", "Euler", "EulerRichardson",
                      "getRate", "getState", "RK4", "Verlet", "initialize")))
})
