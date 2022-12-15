# Purpose: This script is used for creating Shiny App
# Date: 2022-12-14
# Version: 0.1.0
# Bugs and Issues:

# This app is adapted from
# Grolemund, G. (2015). Learn Shiny - Wriiten Tutorials.
# URL:https://shiny.rstudio.com/tutorial/written-tutorial/lesson1/


library(shiny)

# Shiny script for UI
ui <- fluidPage(
  # App title
  titlePanel("FlybaseInR"),

  br(),

  sidebarLayout(
    sidebarPanel(

      # Description on file uploads
      tags$div(
        "Please upload a text file end with .txt. Each line should contain one
        FlyBase Gene ID(ex. FBgn0036925). Please see package vignettes for the
        location of testing data.",
        tags$br(),
      ),

      br(),

      # Upload file
      fileInput(inputId = "inputGeneFile",
                label = "Choose a text file.",
                accept = c("text/plain")
                ),


      # Download expression
      uiOutput(outputId = "downloadE"),

      hr(),

      # Add graph title
      textInput(inputId = "graphTitle",
                label = "Graph Title"
      ),

      # Draw expression button
      actionButton(inputId = "drawExpression",
                   label = "Draw Expression",
                   class = "btn-success"
      )



    ),


    mainPanel(

      # Generate tabs for main panel
      tabsetPanel(
        tabPanel(title = "Embryogenesis",
                 plotly::plotlyOutput("Eplot")),
        tabPanel(title = "Life Cycle",
                 plotly::plotlyOutput("Lplot")),

      )
    )
  ),

  uiOutput(outputId = "sequenceDownloader")
)

# Shiny script for server
server <- function(input, output) {

  # Process user input file, check if it is valid
  geneInput <- NULL
  inputExpressionData <- NULL

  # Observe user have uploaded a file
  observeEvent(input$inputGeneFile, {
    userGeneInput <- input$inputGeneFile
    showNotification(ui = "Please wait while processing data!",
                     duration = 10,
                     type = "message")

    # tryCatch errors for parseFile
    tryCatch({
      geneInput <- FlyBaseInR::parseFile(geneFile = userGeneInput$datapath)
    }, error = function(errors) {
      showNotification(ui = paste(errors, sep = "\n"),
                       duration = 10,
                       type = "error")
    })

    # tryCatch errors and messages for getAllExpression
    if (!is.null(userGeneInput)) {
      tryCatch({
        inputExpressionData <- FlyBaseInR::getAllExpression(geneList = geneInput)
      }, message = function(messages) {
        showNotification(ui = paste(messages, sep = "\n"),
                         duration = 10,
                         type = "message")
      }, finally = {
        inputExpressionData <- FlyBaseInR::getAllExpression(geneList = geneInput)
        unlink("Expression.txt")
        showNotification(ui = "Input data processed successfully!",
                         duration = 3)
      })
    } else {
      ; # Do Nothing
    }

    # make download button appear after process data
    output$downloadE <- renderUI({
      downloadButton(inputId = "downloadExpression",
                     outputId = "expressionFile",
                     label = "Download Expression Data")
    })

    # generate output file for download expression
    output$expressionFile <- downloadHandler(
      filename = function() {
        paste0("Expression_", Sys.Date(), ".txt")
      },
      content = function(file) {
        write.table(inputExpressionData, file, append = FALSE, sep = " ",
                    row.names = FALSE, col.names = TRUE)
      },
      contentType = "txt"
    )

    output$sequenceDownloader <- renderUI({
      sidebarLayout(
        sidebarPanel(
          # make download button appear after process data
          downloadButton(inputId = "downloadSeqeunces",
                         label = "Download Sequences",
                         outputId = "sequenceFile")
        ),
        mainPanel(

          # generate column for split layout
          column(
            width = 12,

            # split view for select Input and checkbox input
            splitLayout(cellWidths = c("50%", "50%"),
                        selectInput(inputId = "seqType",
                                    label =
                                      "Select a seqeunce type to download",
                                    choices = c("FBgn", "FBtr", "FBpp",
                                                "FBtp", "exon", "intron",
                                                "five_prime_utr",
                                                "three_prime_utr", "CDS"),
                                    selectize = FALSE,
                                    size = 10
                                    ),
                        checkboxGroupInput(inputId = "checkedID",
                                           label = "Gene IDs to download",
                                           choices = geneInput)

                        )

          )

        )
      )
    })

    # Generate download sequence
    output$sequenceFile <- downloadHandler(
      filename = function() {
        paste0(input$seqType, "_Sequence_", Sys.Date(), ".txt")
      },
      content = function(file) {
        tryCatch({
          getAllSequences(input$checkedID, input$seqType)
        }, warning = function(warnings) {
          showNotification(ui = paste(warnings, sep = "\n"),
                           duration = 5)
        })
        fileName <- paste0(input$seqType, "Sequences.txt")
        file.copy(fileName, file)
        unlink(fileName)
      },
      contentType = "txt"
    )

    # observe clicking on draw expression button
    observeEvent(input$drawExpression, {

      # output embruogenesis plot and check
      output$Eplot <- plotly::renderPlotly({
        showNotification(ui = "Generating graph now....",
                         duration = 3)

        # validate before render plot
        validate(
          need(expr = input$graphTitle,
               message = "Please enter a graph title before drawing!"),
          need(expr = !is.null(input$inputGeneFile),
               message = "Please upload a file with FlyBase Gene IDs!"),
          need(expr = !is.null(geneInput),
               message = "Please check all FlyBase Gene IDs are correct in
             your file!")
        )

        #output embryogenesis plot
        generatePlot(expression = inputExpressionData,
                     plotType = "Embryogenesis",
                     plotTitle = input$graphTitle)
      })

      # output LifeCycle plot
      output$Lplot <- plotly::renderPlotly({
        generatePlot(expression = inputExpressionData,
                     plotType = "LifeCycle",
                     plotTitle = input$graphTitle)
      })
    })
  })
}

# Internal helper function for generate plot
generatePlot <- function(expression, plotType, plotTitle) {
  FlyBaseInR::drawExpression(expressionData = expression,
                             typeGraph = plotType,
                             graphTitle = plotTitle)

}

# Run Shiny App
shinyApp(ui = ui,
         server = server)


# [END]
