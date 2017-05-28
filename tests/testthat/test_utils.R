library(testthat)

expect_equal(showMethods2("AbstractODESolver"),
             c("getStepSize", "init", "initialize", "setStepSize", "step"))

# this will not work in test package
    # expect_equal(sort(showMethods2("ODE")),
    #              c("AbstractODESolver", "Euler", "EulerRichardson", "getRate",
    #                "getState", "RK4", "Verlet"))

srcTest <- sort(showMethods2("ODE"))
pkgTest <- sort(showMethods2("ODE"))


expect_true(all(showMethods2("ODE") %in%
                    c("AbstractODESolver", "Euler", "EulerRichardson",
                      "getRate", "getState", "initialize", "RK4", "Verlet")))
