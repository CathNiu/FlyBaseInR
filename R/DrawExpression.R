# Purpose: This script is used for draw expression levels for a given list
# of genes.
# Author: Huilin Niu
# Date: 2022-12-11
# Version: 0.2.0
# Bugs and Issues:

#' Generate a three dimensional heatmap with x axis representing the
#' developmental stages, y axis representing the FlyBase Gene IDs, and z (the
#' color of each unit cell) representing the level of expression(intensity).
#' The expression for each gene is composed of two different types of expression
#' data. One is specific to embryogenesis expression where the data contain gene
#' expression at 1hr, 2hr, 3hr, 4hr, 5hr, 6hr, 8hr, 10hr, 12hr, 14hr, 16hr,
#' 18hr, and 20hr. The other is specific to the life cycle where the data
#' contain gene expression spanning the whole life cycle of the fly, including
#' embryo 0-2hr(E0-2hr), embryo 4-6hr(E4-6hr), embryo 10-12hr(E10-12hr),
#' embryo 18-20hr(E18-20hr), 1st instar larva(L1), 2nd instar larva(L2),
#' 3rd instar larva(L3), late 3rd instar larva(late L3), white prepupa(WPP),
#' pupal stage 2(P2), pupal stage 3(P3), pupal stage 4(P4), pupal stage 5(P5),
#' Adult young female(Ayf), Adult young male(Aym), Adult female(Af), and
#' Adult male(Am). The user can choose which heatmap to draw by changing the
#' type parameter("Embryogenesis" or "LifeCycle").
#'
#' @param expressionData the list of expressionData retrieved from FlyBase API
#' using getAllExpression() function.
#'
#' @param typeGraph The type of expression data the heat map contains. User can
#' choose from "Embryogenesis" or "LifeCycle".
#'
#' @param graphTitle A string used as the title of the heatmap generated
#'
#' @return NULL
#'
#' @examples
#' \dontrun{
#' expressionData <- getAllExpression(geneList = geneListExample)
#'
#' # For generating a heatmap for gene expression specific to embryogenesis.
#' drawExpression(expressionData = expressionData,
#'                typeGraph = "Embryogenesis",
#'                graphTitle = "Embryogenesis Gene Expression")
#'
#' # For generating a heatmap for gene expression specific to life cycle,
#' # including 4 embryo stages, 4 larva stages, 1 prepupal stage, 4 pupal
#' # stages, and 2 adult stages each for male and female flies.
#' drawExpression(expressionData = expressionData,
#'                typeGraph = "LifeCycle",
#'                graphTitle = "Life Cycle Gene Expression")
#' }
#'
#' @export

#' @author {Huilin Niu, \email{huilin.niu@mail.utoronto.ca}}
#'
#' @importFrom plotly plot_ly layout
#'
#' @references
#' Plotly Technologies Inc. Collaborative data science. Montréal, QC, 2015.
#' \href{https://plot.ly.}{link}

drawExpression <- function(expressionData, typeGraph, graphTitle) {

  if (typeGraph != "Embryogenesis" && typeGraph != "LifeCycle" ) {

    # Check user input typegraph is valid
    stop("Please enter Embryogenesis or LifeCycle for typeGraph argument")

  } else {

    extractedData <- sortHeatMapData(expressionData, typeGraph)
    # normalize gene expression data using log2
    normExtractedData <- log2(extractedData[colnames(extractedData) != "FlyBase ID"])
    intensity <- as.matrix(normExtractedData)
    figure <- plotly::plot_ly(
      x = names(extractedData[colnames(extractedData) != "FlyBase ID"]),
      y = extractedData$`FlyBase ID`,
      z = intensity,
      autocolorscale = TRUE,
      showscale = TRUE,
      type = "heatmap"
    )

    plotMargin <- list(l = 50, r = 50,b = 75, t = 75, pad = 4)

    figure <- figure %>% plotly::layout(title = graphTitle,
                          yaxis = list(title = "Gene Names"),
                          autosize = TRUE,
                          margin = plotMargin
                          )
    if (typeGraph == "Embryogenesis") {
      figure <- figure %>% plotly::layout(
        xaxis = list(title = "Embryogenesis(hour)"))
    } else {
      figure <- figure %>% plotly::layout(
        xaxis = list(title = "Life Cycle Stages"))
    }
  }

  return(figure)
}

#' INTERNAL FUNCTION: Extract Embryogenesis or LifeCycle expression data from
#' expressionData gathered from a list of genes.
#'
#' @param expressionData the list of gene expression including embryogenesis
#' and life cycle stages.
#'
#' @param typeData The type of expression data the heat map contains. User can
#' choose from "Embryogenesis" or "LifeCycle".
#'
#' @return A data frame containing only Embryogenesis or LifeCycle expression,
#' specified by the user.
#'
#' @importFrom stats na.omit
#'
#' @author {Huilin Niu, \email{huilin.niu@mail.utoronto.ca}}
#'
#' @references
#' R Core Team (2022). R: A language and environment for statistical computing.
#' \emph{R Foundation for Statistical Computing} , Vienna, Austria.
#' \href{https://www.R-project.org/}{link}
#'
sortHeatMapData <- function(expressionData, typeData) {

  expressiondf <- NULL

  if (typeData == "Embryogenesis") {

    # Extract and clean embryogenesis data
    sortedExpressionData <- expressionData[,
                                           grepl("embryogenesis|FlyBaseID",
                                                 colnames(expressionData))]
    expressiondf <- na.omit(sortedExpressionData)
    colnames(expressiondf) <- c("FlyBase ID", "0", "1", "2", "3", "4", "5",
                                "6", "8", "10", "12", "14", "16", "18", "20")
  } else if (typeData == "LifeCycle") {

    # Extract and clean life cycle  data
    expressiondf <- expressionData[, grepl("life_cycle|FlyBaseID",
                                           colnames(expressionData))]
    colnames(expressiondf) <- c("FlyBase ID", "E0-2hr", "E4-6hr", "E10-12hr",
                                "E18-20hr", "L1", "L2", "Young L3", "Late L3",
                                "WPP", "P2", "P3", "P4", "P5", "Aym", "Ayf",
                                "Am", "Af")
  } else {
    ; # Do Nothing
  }
  return(expressiondf)
}

# [END]
