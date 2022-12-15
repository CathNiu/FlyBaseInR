
# Global Variable accessing the FlyBase API Drosophila Melanogaster sequences
FLYBASESEQ <- "https://api.flybase.org/api/v1.0/sequence/id/"
# Global Variable that listing the sequences types available for retrieval.
SEQUENCETYPE <- c("FBgn", "FBtr", "FBpp", "FBtp", "exon", "intron",
                  "five_prime_utr", "three_prime_utr", "CDS")

#' Generate a text file containing all sequences for the FlyBase ids from user
#' input.
#'
#' @param geneList the list of genes for sequence retrieval.
#'
#' @param seqType the type if sequence you want to retrieve, any one choice
#'                from SEQUENCETYPE
#'
#' @examples
#' \dontrun{
#' getAllSequences(geneList = geneListExample, seqType = "FBpp")
#' }
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
         a list of avaiable sequence type.")
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
                         "in your current working directory.", sep = " ")
  message(finishMessage)

  return(invisible(NULL))
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
#' 4. FBtp: Transgenetic construct sequence.
#' 5. exon: Exon sequence split into each exon.
#' 6. intron: Translated intron sequence split into each intron.
#' 7. five_prime_utr: The sequence of 5' untranslated regions.
#' 8. three_prime_utr: The sequence of 3' untranslated regions.
#' 9. CDS: The contiguous protein coding sequence which begins with, and
#' includes, a start codon and ends with, and includes, a stop codon.
#'
#' @param flyID A FlyBase ID that FlyBase uses to identify gene, start with
#' FBgn.
#'
#' @return A list of sequence/sequences
#'
#' @author {Huilin Niu, \email{huilin.niu@mail.utoronto.ca}}
#'
#' @importFrom httr GET content status_code
#'
getSequence <- function(seqType, flyID) {

  # Get response from FlyBase server
  response <- httr::GET(paste0(FLYBASESEQ, flyID, "/", seqType, sep = ""))

  seqResult <- NULL

  # Check that status_code is 200 and the content is not NULL.
  if (httr::status_code(response) != 200 || (is.null(httr::content(response)))) {
    warningMessage <- paste0("The ", flyID, " is not valid in FlyBase")
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
