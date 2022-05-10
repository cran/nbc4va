# Richard Wen (rwenite@gmail.com)
# Server code for shiny app in the nbc4va package.


# (Load) Load required scripts for server code
source("settings.R", local=TRUE)$value
source("helpers.R", local=TRUE)$value

# (Server_Options) Options for server shiny components
opt <- list()
opt$dtRows <- NULL  # NULL to include all rows
opt$dt <- list(pageLength=10, scrollX=TRUE, scrollY="150px", scrollCollapse=TRUE)  # initial page length for each data table
opt$dlType <- "text/csv"  # download content type
opt$maxFile <- 1000*1024^2  # 1000 mb max file upload request size

# (nbc4va_IO) Names for nbc results dataframes from calling nbc4vaIO()
io <- list()
io$train <- "train"
io$training <- "training"
io$test <- "test"
io$testing <- "testing"
io$summary <- "metrics"
io$causes <- "causes"
io$pred <- "pred"
io$prob <- "prob"

# (Execute_Server) Execute server code
options(shiny.maxRequestSize = opt$maxFile)
shinyServer(function(input, output, session) {

  # (Run_NBC) Execute NBC when both training and testing files are uploaded
  nbcResults <- eventReactive(input[[uid$start]], {
    nbcRun(input[[uid$train]], input[[uid$test]], input[[uid$known]])
  })

  # (Status_Upload) Return the status of the training and testing file uploads
  output[[uid$upload]] <- reactive({return(!is.null(input[[uid$train]]) && !is.null(input[[uid$test]]))})
  outputOptions(output, uid$upload, suspendWhenHidden=FALSE)

  # (Status_NBC) Return the status of the run nbc model
  output[[uid$run]] <- reactive({return(is.list(nbcResults()))})
  outputOptions(output, uid$run, suspendWhenHidden=FALSE)

  # (Status_Error) Display error messages if NBC model is not trained correctly after uploads
  output[[uid$runError]] <- renderUI({nbcError(nbcResults())})

  # (Download_Results) Download the results as raw data
  # Rtools required for zip to work, eliminate dependency
  # output[[uid$resultsDL]] <- nbcDownloadZip(input[[uid$train]], input[[uid$test]], nbcResults(), aka$plotSummary, input[[uid$topFootnote]], input[[uid$header]])

  # (Guide) Display message for guiding uploading of files
  output[[uid$guide]] <- renderUI({
    nbcGuide(input[[uid$train]], input[[uid$test]], input[[uid$start]], known=input[[uid$known]])
  })

  # (Overview) Overview information of NBC run
  output[[uid$overview]] <- renderUI({
    nbcOverview(nbcResults())
  })
  output[[uid$metricsDL]] <- downloadHandler(filename=function() {paste(uid$header, "_", io$summary, ".csv", sep="")},
                                             content=function(csvPath) {
                                               listWriteCSV(io$summary, nbcResults(), csvPath, uid$header)
                                             },
                                             contentType=opt$dlType)

  # (Train_Upload) Table of the training upload
  output[[uid$trainDT]] <- renderDataTable({
    csvDF(input[[uid$train]]$datapath, io$training, nrows=opt$dtRows)
  }, options=opt$dt)
  output[[uid$trainDL]] <- downloadHandler(filename=function() {paste(uid$header, "_", io$train, ".csv", sep="")},
                                           content=function(csvPath) {
                                             listWriteCSV(io$train, nbcResults()$nbc, csvPath, uid$header)
                                           },
                                           contentType=opt$dlType)

  # (Test_Upload) Table of the testing upload
  output[[uid$testDT]] <- renderDataTable({
    uploadTable <- csvDF(input[[uid$test]]$datapath, io$testing, nrows=opt$dtRows)
  }, options=opt$dt)
  output[[uid$testDL]] <- downloadHandler(filename=function() {paste(uid$header, "_", io$test, ".csv", sep="")},
                                          content=function(csvPath) {
                                            listWriteCSV(io$test, nbcResults()$nbc, csvPath, uid$header)
                                          },
                                          contentType=opt$dlType)

  # (Summary_Plot) Create a plot of the top predicted CSMF causes
  output[[uid$topPlot]] <- renderPlot({
    nbcPlot(nbcResults(), aka$plotSummary, input[[uid$topFootnote]])
  })
  output[[uid$topPlotDL]] <- downloadHandler(filename=function() {paste(uid$header, "plot.png", sep="_")},
                                             content=function(plotPath){
                                               png(plotPath)
                                               nbcPlot(nbcResults(), aka$plotSummary, input[[uid$topFootnote]])
                                               dev.off()
                                             })


  # (Causes_Table) Create a cause data table and csv download from NBC run
  output[[uid$causeDT]] <- renderDataTable({
    listDF(io$causes, nbcResults(), nrows=opt$dtRows)
  }, options=opt$dt)
  output[[uid$causeDL]] <- downloadHandler(filename=function() {paste(uid$header, "_", io$causes, ".csv", sep="")},
                                           content=function(csvPath) {
                                             listWriteCSV(io$causes, nbcResults(), csvPath, uid$header)
                                           },
                                           contentType=opt$dlType)

  # (Pred_Table) Create a prediction data table and csv download from NBC run
  output[[uid$predDT]] <- renderDataTable({
    listDF(io$pred, nbcResults(), nrows=opt$dtRows)
  }, options=opt$dt)
  output[[uid$predDL]] <- downloadHandler(filename=function() {paste(uid$header, "_", io$pred, ".csv", sep="")},
                                          content=function(csvPath) {
                                            listWriteCSV(io$pred, nbcResults(), csvPath, uid$header)
                                          },
                                          contentType=opt$dlType)
  output[[uid$predDescribe]] <- renderUI({nbcPredDescribe(nbcResults())})
})

