# Purpose: This script is used for parse user input text file for other
# functions in the package
# Date: 2022-12-14
# Version: 0.1.0
# Bugs and Issues:


#' Read a text file and return in a list. This function can be used to format
#' FlyBase ids into a list
#'
#' This function will take the user input text file, and format the input
#' FlyBase IDs to list. The output can be used in other function.
#'
#' @param geneFile a text file with genes separated by new line, the input
#'                 should be the name of the file, and the file should be stored
#'                 in the current working directory for the user.
#'
#' @return A list of genes
#'
#' @export
#'
#' @author {Huilin Niu, \email{ huilin.niu@mail.utoronto.ca}}
#'
#'
parseFile <- function(geneFile) {

  # Check if file exist in current working directory
  if (file.exists(geneFile) == FALSE) {
    stop("Cannot find file in current working directory.")
  }

  # Check if file is a text file
  if (grepl(".txt$", geneFile) == FALSE) {
    stop("Please check the format of the input file.
         You need a text(.txt) file.")
  }

  # Convert file into a list
  geneList <- scan(geneFile, what = character(), sep="\n")

  if (length(geneList) == 0) {
    stop("Please provide a non-empty text file.")
  }

  # Generate correct output
  correctOutput <- rep(x = 11, times = length(geneList))

  if (unique(nchar(geneList)) == unique(nchar(correctOutput))) {
    stop("Please check the input FlyBase ID format. Please type
    help(parseFile) for more details on how to format the text file.")
  }
  return(geneList)
}

# [END]
