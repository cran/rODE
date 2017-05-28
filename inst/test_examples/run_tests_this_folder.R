library(testthat)

# test_file("./inst/test_examples/test_zApplications.R", reporter = "tap")

test_dir(system.file("test_examples", package="rODE"), reporter = "tap")

# source(file.path(system.file("examples", package="rODE"), "FallingParticleODE.R"))


# source("./R/Kepler.R")
# test_file("./tests/test_FallingParticleODEApp.R")
# test_file("./tests/test_Pendulum.R", reporter = "tap")
# test_file("./tests/test_PendulumRK4.R", reporter = "tap")
# test_file("./tests/test_PendulumEuler.R", reporter = "tap")
# test_file("./tests/test_Planet.R", reporter = "tap")
# test_file("./tests/test_Projectile.R", reporter = "tap")
# test_file("./tests/test_ReactionApp.R", reporter = "summary")

# test_file("./tests/test_AbstractODESolver.R", reporter = "summary")
# test_file("./tests/test_Euler.R", reporter = "summary")
# test_file("./tests/test_EulerRichardson.R", reporter = "summary")
# test_file("./tests/test_ODESolver.R", reporter = "summary")
# test_file("./tests/test_RK4.R", reporter = "summary")

# RK45 tests
# test_file("./tests/test_DormandPrince45.R", reporter = "summary")
# test_file("./tests/test_RK45.R", reporter = "summary")
# test_file("./tests/test_KeplerApp.R", reporter = "summary")
# test_file("./tests/test_AdaptiveStepApp.R", reporter = "summary")

# test_dir("./tests", reporter = "summary")



