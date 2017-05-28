# +++++++++++++++++++++++++++++++++++++++++++++++++ application: ProjectileApp.R
#                                                      test Projectile with RK4
#                                                      originally uses Euler

# suppressMessages(library(data.table))

importFromExamples("Projectile.R")      # source the class

ProjectileApp <- function(verbose = FALSE) {
    # initial values
    x <- 0; vx <- 10; y <- 0; vy <- 10
    state <- c(x, vx, y, vy, 0)                        # state vector
    dt <- 0.01

    projectile <- Projectile()
    projectile <- setState(projectile, x, vx, y, vy)
    projectile@odeSolver <- init(projectile@odeSolver, 0.123)
    projectile@odeSolver <- setStepSize(projectile@odeSolver, dt)
    rowV <- vector("list")
    i <- 1
    while (projectile@state[3] >= 0)    {
        rowV[[i]] <- list(t = projectile@state[5],
                          x  = projectile@state[1],
                          vx = projectile@state[2],
                          y  = projectile@state[3],     # vertical position
                          vy = projectile@state[4])
        projectile <- step(projectile)
        i <- i + 1
    }
    DT <- data.table::rbindlist(rowV)
    return(DT)
}


solution <- ProjectileApp()
plot(solution)
