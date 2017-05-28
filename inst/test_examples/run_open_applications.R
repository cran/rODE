# Test by opening all applications under the `examples` folder Get the list of
# appliations by filtering those ending with `App`.
#
# Remove the extension `.R` from each app and loop to call each of the
# applications with `do.call`.
#
# A list contains the expected results that are compared against the result
# coming out from the call to the application.


library(testthat)

# this is where examples live
examples_dir <- system.file("examples", package = "rODE")

# get all the scripts that `App` in them
examples <- list.files(path = examples_dir, pattern = "*App", all.files = FALSE,
           full.names = FALSE, recursive = FALSE, ignore.case = FALSE,
           include.dirs = FALSE, no.. = FALSE)


# loop to open each file
i <- 1
for (app in examples) {
    application <- sub("\\.R$", '', app)
    # cat(sprintf("\n %3d testing ... %30s %25s", i, app, application))
    cat(sprintf("\n %3d testing ... %30s", i, app))
    source(paste(system.file("examples", package = "rODE"),
                 app, sep ="/"))
    if (i == 1) {
        # ComparisonRK45App. tolerance = 1e-6
        cat(sprintf("%25s", application))
        result <- do.call(application, list(FALSE))
        expect_equal(result, list(53.25076, 1.053471e-09, 1.053471e-09, 604),
                     tolerance = 1e-7)
        cat("\t tested")
    }
    if (i == 2) {
        # ComparisonRK45ODEApp. tolerance = 1e-6
        cat(sprintf("%25s", application))
        result <- do.call(application, list(FALSE))
        expect_equal(result, list(51.958888, 9.584591e-08, 9.584591e-08, 286),
                     tolerance = 1e-7)
        cat("\t tested")
    }
    if (i == 3) {
        # FallingParticleApp
        cat(sprintf("%25s", application))
        result <- do.call(application, list(FALSE))
        expect_equal(result,
                     list(-0.090080, -14.014000, -14.112000, -9.800000,
                          1.440000, 1.000000),
                     tolerance = 1e-13)
        cat("\t tested")
    }
    if (i == 4) {
        # KeplerApp
        cat(sprintf("%25s", application))
        result <- do.call(application, list(FALSE))
        expect_equal(result,
                     list(0.444912, -1.436203, 0.459081, 10.033245
                          ),
                     tolerance = 1e-6)
        cat("\t tested")
    }
    if (i == 5) {
        # KeplerDormandPrince45App
        cat(sprintf("%25s", application))
        result <- do.call(application, list(FALSE))
        expect_equal(result,
                     list(1.507215, -19.737875, 1.507215, -0.999051, -0.044948
                     ),
                     tolerance = 1e-5)
        cat("\t tested")
    }

    if (i == 6) {
        # KeplerEnergyApp
        cat(sprintf("%25s", application))
        result <- do.call(application, list(FALSE))
        expect_equal(result,
                     list(1.200000, -19.739182, 1.200000
                     ),
                     tolerance = 1e-5)
        cat("\t tested")
    }

    if (i == 7) {
        # KeplerEulerApp
        cat(sprintf("%25s", application))
        result <- do.call(application, list(FALSE))
        expect_equal(result,
                     list(1.00000, -12.861100, -0.570571, 4.118910, -1.619016,
                          -1.818809, 1.000000
                     ),
                     tolerance = 1e-5)
        cat("\t tested")
    }


    if (i == 8) {
        # PendulumApp
        cat(sprintf("%25s", application))
        result <- do.call(application, list(FALSE))
        expect_equal(result,
                     list(-0.588718, -0.089381, 1000.000000
                     ),
                     tolerance = 1e-5)
        cat("\t tested")
    }


    if (i == 9) {
        # PendulumEulerApp. dt <- 0.01
        cat(sprintf("%25s", application))
        result <- do.call(application, list(FALSE))
        expect_equal(result,
                     list(4920.289096, 9.093423, 1000.010000
                     ),
                     tolerance = 1e-5)
        cat("\t tested")
    }



    if (i == 12) {
        # ProjectileApp
        cat(sprintf("%25s", application))
        result <- do.call(application, list(FALSE))
        expect_equal(result,
                     list(2.050000, 20.500000, 10.000000, -0.092250, -10.090000
                     ),
                     tolerance = 1e-5)
        cat("\t tested")
    }


    if (i == 13) {
        # Reaction
        cat(sprintf("%25s", application))
        result <- do.call(application, list(FALSE))
        expect_equal(result,
                     list(1.987618, 1.143675, 100.1
                     ), tolerance = 1e-6)
        cat("\t tested")
    }


    i <- i + 1
}


# Java results

# ComparisonRK45App. tolerance = 1e-6
# time=49.53332952390886	x1 = 1.0534708814724231E-9 	 error= 1.0534708814708852E-9n=604

# ComparisonRK45ODEApp. tolerance = 1e-6
# time=46.88208701055634	x1 = 9.584591194345978E-8 	 error= 9.584591194343798E-8n=286

# FallingParticleApp: state[1], state[2], state[3]
# -0.090080   -14.112000     1.440000
