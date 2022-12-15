# Purpose: This script is used for retrieve expression
# data from FlyBase API
# Author: Huilin Niu
# Date: 2022-11-14
# Version: 0.1.0
# Bugs and Issues:

FLYBASEEXP <- "https://api.flybase.org/api/v1.0/expression/proteome/"

#' Generate a text file containing all sequences for the genes ids input to
#' keep record, and return the expression data as a data frame.
#'
#' @param geneList A set of genes represented by FlyBase gene ID for retrival
#' of expression.
#'
#' @examples
#' \dontrun{
#' getAllExpression(geneListExample)
#' }
#'
#' @return a list containing the expression data of all available
#' expressions.Each row contains the expression data from embryogenesis to adult
#' stages, and the name of the gene is specified in the first column of each
#' row.
#'
#' @export
#'
#' @importFrom utils write.table
#'
#' @author {Huilin Niu, \email{huilin.niu@mail.utoronto.ca}}
#'
#' @references
#' R Core Team (2022). R: A language and environment for statistical computing.
#' \emph{R Foundation for Statistical Computing} , Vienna, Austria.
#' \href{https://www.R-project.org/}{link}
#'

getAllExpression <- function(geneList) {

  # Create an output file
  outputFile <- "Expression.txt"
  file.create(outputFile)
  resultdf <- getExpressiondf(geneList)

  # Write output file
  write.table(resultdf, file = outputFile, append = FALSE, sep = " ",
              row.names = FALSE, col.names = TRUE)

  # Print finish message once done.
  finishMessage <- paste("Please check output file", outputFile,
                         "in your current working directory.", sep = " ")
  message(finishMessage)
  return(resultdf)
}


#' INTERNAL FUNCTION: Get expression data from proteomic label-free
#' quantification (LFQ) expression values from FlyBase for a single gene using
#' flyID (FlyBase GeneID).
#'
#' This function Will use FlyBase internal API function to retrieve feature
#' using specified FlyBase gene ID.
#'
#' @param flyID A FlyBase ID that FlyBase uses to identify gene
#'
#' @return A list of expression LFQ intensity and corresponding developmental
#' stages of the gene(by flyID).
#'
#' @author {Huilin Niu, \email{ huilin.niu@mail.utoronto.ca}}
#'
#' @importFrom httr GET content status_code
#'
#' @references
#' Wickham H (2022). _httr: Tools for Working with URLs and HTTP_.
#' R package version 1.4.4, \href{https://CRAN.R-project.org/package=httr}{link}
#'
getExpression <- function(flyID) {

  # Get response from FlyBase API
  response <- httr::GET(paste0(FLYBASEEXP, flyID, sep = ""))

  selectedResponse <- NULL

  # Check that status_code is 200 and the content is not empty.
  resultContentLength <- length(httr::content(response)$resultset$result)
  if (httr::status_code(response) != 200 || resultContentLength == 0) {
    messageInfo <- paste0("The ", flyID, " is not valid in FlyBase",
                             " or does not have FlyBase LFQ expression." )
    message(messageInfo)
  } else {
    # Proceed if there is a successful response
    formattedResponse <- httr::content(response, as = "parsed")

    # Convert json format data to a data frame for easier manipulation
    allResponse <- data.frame(do.call("rbind",
                                      formattedResponse$resultset$result))

    # extract lfq intensity and stages information from the data frame
    intensity <- as.numeric(unlist(allResponse$lfq))
    stages <- unlist(allResponse$name)
    for (i in seq(along = allResponse$stages)) {
      stages[i] <- substr(stages[i], 21, nchar(stages[i]))
    }

    # create new data frame for selected response --- lfq value and stages
    selectedResponse <- data.frame(LFQ = intensity)
    selectedResponse <- t(selectedResponse)

    # format the data for expression analysis
    names(selectedResponse) <- stages
    colnames(selectedResponse) <- stages
    rownames(selectedResponse) <- flyID

  }
  return(selectedResponse)
}

#' INTERNAL FUNCTION: Get a expression data for a list of genes
#' for expression analysis.
#'
#'
#' @param geneList A set of genes for analysis
#'
#' @return A dataframe containing all information for writing the
#' expression table
#'
#' @author {Huilin Niu, \email{ huilin.niu@mail.utoronto.ca}}
#'
#' @import dplyr
#'
#' @references
#' Wickham H, François R, Henry L, Müller K (2022).
#' _dplyr: A Grammar of Data Manipulation_. R package version 1.0.10,
#' \href{https://CRAN.R-project.org/package=dplyr}{link}
#'
getExpressiondf <- function(geneList) {
  dfList <- vector(mode = "list", length = length(geneList))
  newNames <- character(length(geneList))
  # Loop through all genes, retrieve gene expression, write to text file
  countGene <- 1
  for (i in seq(along = geneList)) {
    retrieved <- getExpression(geneList[i])
    if (!is.null(retrieved)) {
      dfList[[countGene]] <- retrieved
      newNames[countGene] <- geneList[i]
      countGene <- countGene + 1
    }
  }

  # format result dataframe for return
  resultdf <- dplyr::bind_rows(dfList)
  namesdf <- data.frame(newNames[1 : countGene - 1])
  names(namesdf) <- c("FlyBaseID")
  resultdf <- dplyr::bind_cols(namesdf, resultdf)
  return(resultdf)
}

# [END]
