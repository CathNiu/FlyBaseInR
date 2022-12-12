# Purpose: This script is used for retrieve and parse sequence and expression
# data from FlyBase API
# Author: Huilin Niu
# Date: 2022-11-14
# Version: 0.1.0
# Bugs and Issues:

FLYBASEEXP <- "https://api.flybase.org/api/v1.0/expression/proteome/"


#' Generate a text file containing all sequences for the genes ids input
#'
#' @param geneFile the list of genes for sequence retrieval as a txt file,
#'                       each gene on a newline.
#'
#' @export
#'
#' @return NULL
#'
#' @importFrom utils write.table
#'
#' @author {Huilin Niu, \email{huilin.niu@mail.utoronto.ca}}
#'

getAllExpression <- function(geneFile) {
  geneList <- parseFile(geneFile)

  outputFile <- "Expression.txt"
  file.create(outputFile)
  resultdf <- getExpressiondf(geneList)
  write.table(resultdf, file = outputFile, append = FALSE, sep = " ",
              row.names = FALSE, col.names = TRUE)
  printFinish(outputFile)
  return(invisible(NULL))
}




#' INTERNAL FUNCTION: Request response warnings
#'
#' This function will take different status code of the request, and print
#' corresponding warning messages to users
#'
#' @param status The status code the request returned
#'
#' @param flyID A FlyBase ID that FlyBase uses to identify gene
#'
#' @return NULL
#'
#' @author {Huilin Niu, \email{huilin.niu@mail.utoronto.ca}}
#'
#'
#'
printWarning <- function(status, flyID) {
  msg1 <- "cannot be retrieved,"
  if (status == 200) {
    errorMessage <- paste(flyID, msg1 ,
                           "you have entered an invalid sequence type.",
                           "Please consult vingette for the proper usage.",
                           sep = " ")


  } else if (status == 501) {
    errorMessage <- paste(flyID, msg1 ,
                           "a server error has occured. Please try later.",
                           sep = " ")
  }
  print(errorMessage)
  return(invisible(NULL))
}


#' INTERNAL FUNCTION: Get expression data from proteomic label-free
#' quantification (LFQ) expression values from FlyBase using
#' flyID (FlyBase GeneID)
#'
#' This function Will use FlyBase internal API function to retrieve feature
#' using specified FlyBase gene ID.
#'
#' @param flyID A FlyBase ID that FlyBase uses to identify gene
#'
#' @return A list of expression LFQ intensity and corresponsing developmental
#' stages of the gene(by flyID).
#'
#' @author {Huilin Niu, \email{ huilin.niu@mail.utoronto.ca}}
#'
#' @importFrom httr GET
#'
getExpression <- function(flyID) {

  # Get response from FlyBase server
  response <- httr::GET(paste0(FLYBASEEXP, flyID, sep = ""))

  # Check if status_code is 200 or if the content is NULL
  # The null content is usually caused by an invalid seqType entry, the FlyBase
  # does not check for this error.
  selectedResponse <- NULL
  if (status_code(response) == 200 && (!is.null(content(response)))) {
    formattedResponse <- httr::content(response, as = "parsed")
    # convert json format data to a data frame for easier manipulation
    allResponse <- data.frame(do.call("rbind",
                                      formattedResponse$resultset$result))
    if (length(formattedResponse$resultset$result) == 0) {
      # If the gene is not found:
      message <- paste0(flyID, " is not found in the dataset.")
      print(message)
    } else {
      # extract lfq intensity and stages information from the data frame
      intensity <- as.numeric(unlist(allResponse$lfq))
      stages <- unlist(allResponse$name)
      for (i in seq(along = allResponse$stages)) {
        stages[i] <- substr(stages[i], 21, nchar(stages[i]))
      }

      #create new data frame for selected response --- lfq value and stages
      selectedResponse <- data.frame(LFQ = intensity)
      selectedResponse <- t(selectedResponse)
      names(selectedResponse) <- stages
      colnames(selectedResponse) <- stages
      rownames(selectedResponse) <- flyID
    }
  } else {
    # If the request is not successful, print warning message to users.
    responseStatus <- status_code(response)
    printWarning(responseStatus, flyID)
  }
  return(selectedResponse)
}

#' INTERNAL FUNCTION: read a text file and return in a list.
#'
#' This function Will use FlyBase internal API function to retrieve feature
#' using specified FlyBase gene ID.
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
  resultdf <- dplyr::bind_rows(dfList)
  namesdf <- data.frame(newNames[1 : countGene - 1])
  names(namesdf) <- c("FlyBaseID")
  resultdf <- dplyr::bind_cols(namesdf, resultdf)
  return(resultdf)
}



# [END]
