# utils.R

#' showMethods2
#'
#' Get the methods in a class. But only those specific to the class
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

#' importFromExamples
#'
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


#' run_test_applications
#'
#' Run test all the examples
#'
#' @export
run_test_applications <- function() {
    test_ex_dir  <- system.file("test_examples", package="rODE")
    examples_dir <- system.file("examples", package = "rODE")
    source(paste(test_ex_dir, "run_test_applications.R", sep = "/"))
}


release_questions <- function() {
    c(
        "Have you add the names for the vignettes?",
        "Do the examples have App in them?"
    )
}



get_list_examples <- function(aPackage) {
    # this is where examples live
    examples_dir <- system.file("examples", package = aPackage)

    # get all the scripts that `App` in them
    list.files(path = examples_dir, pattern = "*App", all.files = FALSE,
                           full.names = FALSE, recursive = FALSE, ignore.case = FALSE,
                           include.dirs = FALSE, no.. = FALSE)
}

