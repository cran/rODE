#' Get the methods in a class
#'
#' But only those specific to the class
#'
#' @param theClass class to analyze
#'
#' @importFrom methods showMethods
#' @export
showMethods2 <- function(theClass) {
    # get the method printout
    mtext <-  showMethods(classes=theClass, printTo = FALSE )

    # extract only what is between "Function :" and " (pack ... .GlobalEnv)"
    fvec  <- gsub( "Function(\\:\\s|\\s\\\")(.+)(\\s\\(|\\\")(.+$)",
                   "\\2", mtext[grep("^Function: ", mtext)] )

    fvec

    # vector comparison of methods
    # expect_equal(fvec, c("getStepSize", "init", "setStepSize", "step"))
}

#' Run test of all the examples
#'
#' @export
run_test_examples <- function() {
    test_ex_dir  <- system.file("test_examples", package="rODE")
    examples_dir <- system.file("examples", package = "rODE")
    source(paste(test_ex_dir, "run_open_applications.R", sep = "/"))
}


#' Source the R script
#'
#' @param aClassFile a file containing one or more classes
#' @param aFolder    a folder where examples are located
#' @export
importFromExamples <- function(aClassFile, aFolder = "examples") {
    source(paste(system.file(aFolder, package = "rODE"),
                 aClassFile,
                 sep ="/"), echo = FALSE)
}
