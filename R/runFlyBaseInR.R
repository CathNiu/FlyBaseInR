#' Launch Shiny App for FlyBaseInR
#'
#' A function that launches the Shiny app for FlyBaseInR.
#' The shiny app allows user to analyze and visualize expression levels of
#' specific genes in FlyBase Drosophila Melanogaster database via an
#' interactive platform.
#'
#' The code has been placed in \code{./inst/shiny-scripts}.
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
#' @references
#' Grolemund, G. (2015). Learn Shiny - Video Tutorials.
#' \href{https://shiny.rstudio.com/tutorial/}{Link}
#'
#' @export
#'
#' @importFrom shiny runApp

runFlyBaseInR <- function() {
  appDir <- system.file("shiny-scripts",
                        package = "FlyBaseInR")
  shiny::runApp(appDir, display.mode = "normal")
  return(invisible(NULL))
}

# [END]
