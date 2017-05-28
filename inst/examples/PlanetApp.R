# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++  example: PlanetApp.R
# Simulation of Earth orbiting around the SUn using the Euler ODE solver

importFromExamples("Planet.R")      # source the class

PlanetApp <- function(verbose = FALSE) {
    # x =  1, AU or Astronomical Units. Length of semimajor axis or the orbit
    # of the Earth around the Sun.
    x <- 1; vx <- 0; y <- 0; vy <- 6.28; t <- 0
    state <- c(x, vx, y, vy, t)
    dt <-  0.01
    planet <- Planet()
    planet@odeSolver <- setStepSize(planet@odeSolver, dt)
    planet <- init(planet, initState = state)
    rowvec <- vector("list")
    i <- 1
    # run infinite loop. stop with ESCAPE.
    while (planet@state[5] <= 90) {     # Earth orbit is 365 days around the sun
        rowvec[[i]] <- list(t  = planet@state[5],     # just doing 3 months
                            x  = planet@state[1],     # to speed up for CRAN
                            vx = planet@state[2],
                            y  = planet@state[3],
                            vy = planet@state[4])
        for (j in 1:5) {                 # advances time
            planet <- doStep(planet)
        }
        i <- i + 1
    }
    DT <- data.table::rbindlist(rowvec)
    return(DT)
}
# run the application
solution <- PlanetApp()
select_rows <- seq(1, nrow(solution), 10)      # do not overplot
solution <- solution[select_rows,]
plot(solution)

