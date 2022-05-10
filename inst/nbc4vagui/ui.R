# Richard Wen (rwenite@gmail.com)
# Interface code for shiny app in the nbc4va package.


# (Load) Load required scripts for ui code
source("settings.R", local=TRUE)$value

# (Execute_UI) Execute user interface code
shinyUI(bootstrapPage(
  title="nbc4va - Naive Bayes Classifier for Verbal Autopsy Data",
  theme="bootstrap.min.css",  # simplex theme from bootswatch.com
  tags$head(
    tags$style(type="text/css", "tfoot {display:none;}"),
    tags$style(type="text/css", ".dl-btn {font-size: 10px; padding: 5px;}"),
    tags$style(type="text/css", ".prev-dt {float: left; width:50%; clear: none; padding: 10px;}")
  ),
  navbarPage(
    "nbc4va",
    tabPanel(
      "Run",
      sidebarPanel(
        fileInput(uid$train, "Upload Training File (.csv)", accept=c(".csv", "text/comma-separated-values")),
        hr(),
        fileInput(uid$test, "Upload Testing File (.csv)", accept=c(".csv", "text/comma-separated-values")),
        checkboxInput(uid$known, "Testing Causes Known", value=TRUE),
        conditionalPanel(paste("output", uid$upload, sep="."), hr(), actionButton(uid$start, "Run")),
        conditionalPanel(
          paste("output", uid$run, sep="."),
          #Rtools required for zip to work, eliminate dependency
          #hr(),
          #textInput(uid$header, "Project Name", "nbc4va"),
          #downloadButton(uid$resultsDL, "Download Results"),
          hr(),
          tags$small("Tips:",
                     tags$ul(
                       tags$li("Run additional models by re-uploading training and testing files."),
                       tags$li("Refresh the page to reset the app.")
                     )
          ))
      ),
      mainPanel(
        htmlOutput(uid$guide),
        htmlOutput(uid$runError),
        conditionalPanel(
          paste("output", uid$run, sep="."),
          tabsetPanel(
            tabPanel(aka$overview, br(),
                     "The overview tab provides a summary of the trained NBC model with associated metrics.",
                     br(), br(),
                     downloadButton(uid$metricsDL, paste("Download", aka$metrics), "dl-btn"),
                     hr(),
                     htmlOutput(uid$overview)),
            tabPanel(aka$pred, br(), htmlOutput(uid$predDescribe),
                     br(), downloadButton(uid$predDL, paste("Download", aka$pred), "dl-btn"),
                     hr(),
                     dataTableOutput(uid$predDT),
                     br()),
            tabPanel(
              aka$plotSummary, br(), "The plot shows the causes by predicted CSMF with a footnote of the model details.",
              br(), br(), downloadButton(uid$topPlotDL, paste("Download", aka$plotSummary), "dl-btn"),
              hr(),
              plotOutput(uid$topPlot),
              checkboxInput(uid$topFootnote, "Footnote", value=TRUE),
              br()
            ),
            tabPanel(aka$cause, br(), "The cause metrics table provides NBC model performance metrics by cause.",
                     br(), br(), downloadButton(uid$causeDL, paste("Download", aka$cause), "dl-btn"),
                     hr(),br(),
                     dataTableOutput(uid$causeDT),
                     br()),
            tabPanel(aka$train, br(), "The training table shows the data used for the NBC model to learn cause probabilities given symptoms.", "This data has unknown symptoms removed and is updated on every", tags$i("Training File Upload."),
                     br(), br(), downloadButton(uid$trainDL, paste("Download", aka$train), "dl-btn"),
                     hr(),br(),
                     dataTableOutput(uid$trainDT)),
            tabPanel(aka$test, br(), "The testing table shows the data that was used for the NBC model predictions and performance metrics.", "This data has unknown symptoms removed and is updated on every", tags$i("Testing File Upload."),
                     br(), br(), downloadButton(uid$testDL, paste("Download", aka$test), "dl-btn"),
                     hr(),br(),
                     dataTableOutput(uid$testDT))
          )
        )
      )
    ),
    tabPanel("Info", mainPanel(includeHTML("www/nbc4vagui_info.html")))
  )
))

