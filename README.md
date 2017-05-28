
<!-- README.md is generated from README.Rmd. Please edit that file -->
rODE
====

The goal of rODE is to explore R and its S4 classes and their differences with Java and Python while exploring simulations by solving differential equations.

`rODE` is based in the extraordinary physics library for computer simulations OpenSourcePhyisics. Tae a look at <http://opensourcephysics.org>.

The ODE solvers implemented in R so far:

-   Euler
-   Euler-Richardson
-   RK4
-   RK45, Dormand-Prince45
-   Verlet

Installation
------------

You can install rODE from github with:

``` r
# install.packages("devtools")
devtools::install_github("AlfonsoRReyes/rODE")
```

Examples
--------

Example scripts are located under the folder `examples` inside the package.

These examples make use of a parent class containing a customized rate calculation as well as the step and startup method. The methods that you would commonly find in the base script or parent class are:

-   getRate()
-   getState()
-   step() or doStep()
-   setStepSize()
-   init(), not the same as the class initialize method
-   initialize(), and
-   the constructor

These methods are defined in the virtual classes `ODE` and `ODESolver`. Two other classes that serve as definition classes for the ODE solvers are: `AbstractODESolver` and `ODEAdaptiveSolver`.

For instance, the application `KeplerApp.R` needs the class `Kepler` located in the `Kepler.R` script, which is called with `planet <- Kepler(r, v)`, an `ODE` object. The solver for the same application is `RK45` called with `solver <- RK45(planet)`. Since `RK45` is an ODE solver, the script `RK45.R` will be located in the folder `./R` in the package.

Vignettes
---------

The vignettes contain examples of the use of the various ODE solvers. For instance, the notebook `Comparison` and `` Kepler` use the ODE solver `RK45`, `FallingParticle` and `Planet` use `Euler ``, `Pendulum` makes use of `EulerRichardson`, `Planet` of `Euler`, `Projectile` and `Reaction` of `RK4`, and `KeplerEnergy` uses the ODE solver `Verlet`.

Tests
-----

There are tests for the core ODE solver classes under tests/testthat, as well as tests for the examples.

The tests for the examples are two: one for the base/parent classes such as `Kepler` or `Planet` or `Projectile`; this test runner is called `run_tests_this_folder.R`. For the applications themselves there is another runner that opens each of the applications as request for a return value. If the hard coded value is not returned, the test will fail. This ensures that any minor change in the core solver classes do not have any impact on the application solutions, and if there is, it must be explained. The name of the test runner is `run_open_applications.R`.
