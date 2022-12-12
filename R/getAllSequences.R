# Purpose: This script is used for retrieve sequences from FlyBase Application
# Programming Interface(API), and provide a text file for the users.
# Author: Huilin Niu
# Date: 2022-11-14
# Version: 0.1.0
# Bugs and Issues:

# Global Variable accessing the FlyBase API Drosophila Melanogaster sequences
FLYBASESEQ <- "https://api.flybase.org/api/v1.0/sequence/id/"
# Global Variable that listing the sequences types available for retrieval.
SEQUENCETYPE <- c("FBgn", "FBtr", "FBpp", "FBti", "FBtp", "exon", "intron",
                  "five_prime_utr", "three_prime_utr", "CDS")

#' Generate a text file containing all sequences for the FlyBase ids from user
#' input.
#'
#' @param geenList the list of genes for sequence retrieval.
#'
#' @param seqType the type if sequence you want to retrieve, any one choice
#'                from SEQUENCETYPE
#'
#' @examples
#' getAllSequences(geneList = geneListExample, seqType = "FBpp")
#'
#' @export
#'
#' @return NULL
#'
#' @author {Huilin Niu, \email{huilin.niu@mail.utoronto.ca}}
getAllSequences <- function(geneList, seqType) {

  # Check if the user entered a valid seqType
  if (seqType %in% SEQUENCETYPE == FALSE) {
    stop("Please enter a valid sequence type. Type help(getAllSequences) for
         a list of avaiable sequence tyoe.")
  }

  outputFile <- paste0(seqType, "Sequences.txt")
  file.create(outputFile)

  # Loop through all genes, retrieve gene sequences, write to text file
  for (i in seq(along = geneList)) {
    retrieved <- getSequence(seqType, geneList[i])
    if (!is.null(retrieved)) {
      geneName <- paste0(">", geneList[i])
      cat(geneName, file = outputFile, append = TRUE, sep = "\n")
      cat(retrieved, file = outputFile, append = TRUE, sep = "\n")
    }
  }
  finishMessage <- paste("Please check output file", outputFile,
                         "in your current working directory", sep = " ")
  message(finishMessage)

  return(invisible(NULL))
}


#' Read a text file and return in a list. This function can be used to format
#' FlyBase ids into a list
#'
#' This function will take the user input text file, and format the input
#' FlyBase IDs to list. The output can be used in getAllSequences() or
#' getAllExpression() function.
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

  # Check if file exist in current working directionary
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

  # Generate correct output
  correctOutput <- rep(x = 11, times = length(geneList))

  # Check if the length of each FlyBase ID is correct
  checkFormat <- all.equal(nchar(geneList), correctOutput)
  if (checkFormat == FALSE) {
    stop("Please check the input FlyBase ID format. Please type
    help(parseFile) for more details on how to format the text file.")
  }
  return(geneList)
}

#' INTERNAL FUNCTION: Get one sequence from FlyBase API using FlyBase ID and
#' the sequence type specified in the SEQUENCETYPE global variable.
#'
#' @param seqType The sequence type for retrieval specified by a string. The
#' available types include: FBgn, FBtr, FBpp, FBti, FBtp, exon,
#' intron, five_prime_utr, three_prime_utr, CDS. The sequences type that the
#' abbreviation represents are listed below.
#'
#' 1. FBgn: The sequence of the gene span.
#' 2. FBtr: The sequence of transcripts that are typed as messenger RNAs (mRNA).
#' 3. FBpp: The resulting protein sequence from protein coding transcripts.
#' 4. FBti: Insersion sequence.
#' 5. FBtp: Transgenetic construct sequence.
#' 6. exon: Exon sequence split into each exon.
#' 7. intron: Translated intron sequence split into each intron.
#' 8. five_prime_utr: The sequence of 5' untranslated regions.
#' 9. three_prime_utr: The sequence of 3' untranslated regions.
#' 10. CDS: The contiguous protein coding sequence which begins with, and
#' includes, a start codon and ends with, and includes, a stop codon.
#'
#' @param flyID A FlyBase ID that FlyBase uses to identify gene, start with
#' FBgn.
#'
#' @return A list of sequence/sequences
#'
#' @author {Huilin Niu, \email{huilin.niu@mail.utoronto.ca}}
#'
#' @importFrom httr GET status_code
#'
getSequence <- function(seqType, flyID) {

  # Get response from FlyBase server
  response <- httr::GET(paste0(FLYBASESEQ, flyID, "/", seqType, sep = ""))

  seqResult <- NULL

  # Check that status_code is 200 and the content is not NULL.
  if (httr::status_code(response) != 200 || (is.null(content(response)))) {
    warningMessage <- paste0("The ", flyID, " is not avilable in FlyBase")
    warning(warningMessage)
  } else {
    formattdResponse <- httr::content(response, as = "parsed")

    # Retrieve all seq
    numSeq <- formattdResponse$resultset$num_fetched
    seqResult <- character(numSeq)
    for (i in seq(to = numSeq)) {
      seqResult[i] <- formattdResponse$resultset$result[[i]]$sequence
    }
  }
  return(seqResult)
}

# [END]
