
solver <- new("ODEAdaptiveSolver")
# print(solver)
expect_equal(slotNames(solver), c("NO_ERROR", "DID_NOT_CONVERGE", "BISECTION_EVENT_NOT_FOUND"))

expect_equal(showMethods2("ODEAdaptiveSolver"),
             c("getErrorCode", "getTolerance", "initialize", "setTolerance"))

expect_equal(c(solver@NO_ERROR,
               solver@DID_NOT_CONVERGE,
               solver@BISECTION_EVENT_NOT_FOUND),
             c(0,1,2))


