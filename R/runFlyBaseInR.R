# Purpose: Run Shiny App for FlyBaseInR
# Author: Huilin Niu
# Date: 2022-11-14
# Version: 0.1.0
# Bugs and Issues:
#'  Launch Shiny App for FlyBaseInR
#'
#' A function that launches the Shiny app for FlyBaseInR.
#' The shiny app allows user to analyze and visualize expression levels of
#' specific genes in FlyBase *Drosophila Melanogaster* database via an
#' interactive platform. The code has been placed in
#' \code{./inst/shiny-scripts}.
#'
#' @return No return value but open up a Shiny page.
#'
#' @examples
#' \dontrun{
#' FlyBaseInR::runFlyBaseInR()
#' }
#'
#' @author {Huilin Niu, \email{huilin.niu@mail.utoronto.ca}}
#'
#' @export
#'
#' @importFrom shiny runApp
#'
#' @references
#' Chang W, Cheng J, Allaire J, Sievert C, Schloerke B, Xie Y, Allen J,
#' McPherson J, Dipert A, Borges B (2022). *shiny: Web Application Framework
#' for R*. R package version 1.7.3.9001,
#' \href{https://shiny.rstudio.com/}{link}

runFlyBaseInR <- function() {
  appDir <- system.file("shiny-scripts",
                        package = "FlyBaseInR")
  shiny::runApp(appDir, display.mode = "normal")
  return(invisible(NULL))
}

# [END]
