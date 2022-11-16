# Purpose: This script is used for draw expression information for a given list
# of genes.
# Author: Huilin Niu
# Date: 2022-11-14
# Version: 0.1.0
# Bugs and Issues:

#' Generate a heatmap containing all expression intensity level for the genes
#' ids input
#'
#' @param geneFile the list of genes for sequence retrieval as a txt file,
#'                       each gene on a newline.
#'
#' @param stage choose from embryogenesis and life cycle
#'
#' @export expression.png
#'
#' @return NULL
#'
#' @export
#'
#' @examples
#' #Example 1:
#' #This will show expression intensity level of all genes ids input in heatmap.
#' drawHeatMap("tests/geneList.txt", "Embryogenesis")
#'
#' @author {Huilin Niu, \email{huilin.niu@mail.utoronto.ca}}
#'
#' @import plotly

drawExpression <- function(geneFile, stage) {
  expressionData <- sortHeatMapData(geneFile, stage)
  intensity <- as.matrix(expressionData)
  figure <- plotly::plot_ly(
    x = names(expressionData), y = expressionData$FlyBaseID,
    z = intensity, type = "heatmap"
  )
  return(figure)
}

#' INTERNAL FUNCTION: Sort the data into embryogenesis data or life cycle data.
#'
#' @param geneFile the list of genes for sequence retrieval as a txt file,
#'                       each gene on a newline.
#'
#' @param stage Stage specifying which part of the list is used for extraction
#'
#' @return Two dataframe with embryogenesis pattern and another with
#' life cycle pattern
#'
#' @examples
#' #Example 1:
#' #This will generate dataset for the drawHeatMap function.
#'
#'
#' @author {Huilin Niu, \email{huilin.niu@mail.utoronto.ca}}
#'
sortHeatMapData <- function(geneFile, stage) {
  geneList <- parseFile(geneFile)
  expressionDataAll <- getExpressiondf(geneList)
  sortedExpressionData <- expressionDataAll[, order(names(expressionDataAll))]
  expressiondf <- NULL
  if (stage == "Embryogenesis") {
    expressiondf <- sortedExpressionData[, 1:15]
  } else if (stage == "Life Cycle") {
    expressiondf <- sortedExpressionData[, 16:31]
  } else {
    ;
  }
  return(expressiondf)
}
