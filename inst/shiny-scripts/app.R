# This app is adapted from
# Grolemund, G. (2015). Learn Shiny - Video Tutorials.
# URL:https://shiny.rstudio.com/tutorial/

# The tabset code is adapted from
# RStudio Inc. (2013). Tabsets. Shiny Gallery.
# URL:https://shiny.rstudio.com/gallery/tabsets.html

# The code for hide/show boxes after clicking button is adapted from
# SymbolixAU, June 27, 2017 (22:34p.m.), comment on Yordan Ivanov,
# "Show/hide entire box element in R Shiny, Stack Overflow,
# June 27, 2017 (21:15 p.m.)
# URL: https://stackoverflow.com/questions/44790028/show-hide-entire-box-element-in-r-shiny


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
        FlyBase Gene ID(ex. FBgn0036925). Please see package vingette for the
        location of testing data.",
        tags$br(),
      ),

      br(),

      fileInput(inputId = "inputGeneFile",
                label = "Choose a text file.",
                accept = c("text/plain")
                ),



      uiOutput(outputId = "downloadE"),

      hr(),

      textInput(inputId = "graphTitle",
                label = "Graph Title"
      ),

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
  shinyjs::hide("downloadExpression")
  geneInput <- NULL
  inputExpressionData <- NULL

  observeEvent(input$inputGeneFile, {
    userGeneInput <- input$inputGeneFile
    showNotification(ui = "Please wait while processing data!",
                     duration = 10,
                     type = "message")
    tryCatch({
      geneInput <- FlyBaseInR::parseFile(geneFile = userGeneInput$datapath)
    }, error = function(errors) {
      showNotification(ui = paste(errors, sep = "\n"),
                       duration = 10,
                       type = "error")
    })

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
      ;
    }

    output$downloadE <- renderUI({
      downloadButton(inputId = "downloadExpression",
                     outputId = "expressionFile",
                     label = "Download Expression Data")
    })

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
          downloadButton(inputId = "downloadSeqeunces",
                         label = "Download Sequences",
                         outputId = "sequenceFile")
        ),
        mainPanel(
          column(
            width = 12,
            splitLayout(cellWidths = c("50%", "50%"),
                        selectInput(inputId = "seqType",
                                    label =
                                      "Select a seqeunce type to download",
                                    choices = c("FBgn", "FBtr", "FBpp", "FBti",
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

    observeEvent(input$drawExpression, {
      output$Eplot <- plotly::renderPlotly({
        showNotification(ui = "Generating graph now....",
                         duration = 3)
        validate(
          need(expr = input$graphTitle,
               message = "Please enter a graph title before drawing!"),
          need(expr = !is.null(input$inputGeneFile),
               message = "Please upload a file with FlyBase Gene IDs!"),
          need(expr = !is.null(geneInput),
               message = "Please check all FlyBase Gene IDs are correct in
             your file!")
        )

        generatePlot(expression = inputExpressionData,
                     plotType = "Embryogenesis",
                     plotTitle = input$graphTitle)
      })


      output$Lplot <- plotly::renderPlotly({
        generatePlot(expression = inputExpressionData,
                     plotType = "LifeCycle",
                     plotTitle = input$graphTitle)
      })
    })
  })
}

generatePlot <- function(expression, plotType, plotTitle) {
  FlyBaseInR::drawExpression(expressionData = expression,
                             typeData = plotType,
                             graphTitle = plotTitle)

}

# Run Shiny App
shinyApp(ui = ui,
         server = server)
