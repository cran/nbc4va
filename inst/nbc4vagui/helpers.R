# Richard Wen (rwenite@gmail.com)
# Helper functions for shiny app server code in the nbc4va package.


# NBC guide messages in html format
# Helper function to return upload instructions and messages for beginners.
# @param training The shiny file upload object representing the training file.
# @param testing The shiny file upload object representing the testing file.
# @param known Whether the testing causes are known or not.
# @param nbcRan Number of times an nbc model has been run.
# @param nrows The number of rows to preview.
# @return withTags A shiny withTags object for rendering to HTML or NULL if uploads are complete.
nbcGuide <- function(training, testing, nbcRan, known=TRUE, nrows=25, heading=TRUE) {
  if (is.null(training) && is.null(testing)) {
    trainEx <- data.frame(ID=c("a1", "a2", "a3"),
                          Cause=c("HIV", "Stroke", "Cancer"),
                          Symptom1=as.integer(c(1, 0, 1)),
                          Symptom2=as.integer(c(0, 0, 1)),
                          Symptom3=as.integer(c(1, 1, 0)))
    return(
      withTags({
        div(
          if (heading) span(b("Train a Naive Bayes Classifier (NBC) by uploading the training and testing files."), hr()),
          p("The ", HTML("<b>training file</b><sup>1</sup>"), " and ", HTML("<b>testing file</b><sup>2</sup>"), "(.csv) must have the following structure: "),
          ul(li(b("Columns (in order):"), "ID, Cause, Symptoms1..SymptomsN"),
             li(b("ID (text):"), "Case identifiers for each row, must be unique"),
             li(HTML("<b>Cause (text)<sup>a</sup>:</b>"), "Known cause of death for each row"),
             li(HTML("<b>Symptoms (number)<sup>b</sup>:</b>"), "1 for presence and 0 for absence of symptom 1..N"),
             li(i("Example:"), br(), br(), renderTable(trainEx, include.rownames=FALSE))
          ),
          small(sup("1"), "The training file is used for the NBC model to learn probabilities from.",
                br(),
                sup("2"), "The testing file is used to evaluate model performance and obtain predictions.",
                br(),
                sup("a"), "If testing causes are not known, the second column", i("(Cause)"), "from the", b("testing file"), "must be omitted", HTML("with the <b>Testing Causes Known</b> option unchecked."),
                br(),
                sup("b"), "Symptoms that are not equal to 1 or 0 are imputed randomly from the distribution of known 1s and 0s; a symptom column will be removed from training and testing if 1s or 0s do not exist.")
        )
      })
    )
  } else if (is.null(training)) {
    testingData <- read.csv(testing$datapath, nrows=nrows, as.is=TRUE, stringsAsFactors=FALSE)
    return(nbcGuideLoaded("training", "testing", testingData, TRUE))
  } else if(is.null(testing)) {
    trainingData <- read.csv(training$datapath, nrows=nrows, as.is=TRUE, stringsAsFactors=FALSE)
    return(nbcGuideLoaded("testing", "training", trainingData, FALSE))
  } else if (nbcRan < 1 && !is.null(training) && !is.null(testing)) {
    trainingData <- read.csv(training$datapath, nrows=nrows, as.is=TRUE, stringsAsFactors=FALSE)
    testingData <- read.csv(testing$datapath, nrows=nrows, as.is=TRUE, stringsAsFactors=FALSE)
    return(nbcGuideRun(trainingData, testingData, known))
  } else {
    return(NULL)
  }
}


# NBC guide messages for loaded files in html format
# Helper function for creating a preview of the loaded data and producing a message to guide user for consistent upload
# @param notLoaded The name of the file that is not loaded
# @param loaded The name of the file that is loaded
# @param loadedDT A dataframe of the loaded file
# @param omit Whether to let user know that they can omit or include a cause column
# @return withTags A shiny withTags object for rendering to HTML.
nbcGuideLoaded <-function (notLoaded, loaded, loadedDT, omit=TRUE) {
  checked <- nbc4va:::internalCheckNBC(loadedDT[1:2, ], loadedDT)
  loadedCols <- ncol(loadedDT)
  return(
    withTags({
      div(
        p("The", b(notLoaded, HTML("file<sup>1</sup>")), "must be consistent with the", b(loaded, HTML("file<sup>2</sup>:"))),
        ul(
          li("Total of", loadedCols, HTML("columns<sup>a</sup>")),
          li(HTML("Columns must be in the same structure<sup>b</sup>"), " as the ", b(loaded, "file")),
          li(i("Preview of "), b(loaded, "file"), i(": "))
        ),
        renderDataTable(loadedDT, options=list(searching=FALSE, lengthChange=TRUE, columns.searchable=FALSE, scrollX=TRUE, scrollY="150px", paging=FALSE, scrollCollapse=TRUE)), br(),
        if (tolower(notLoaded) == "training") {
          small(sup("1"), "The training file is used by the NBC model to learn probabilities from.")
        } else if (tolower(notLoaded) == "testing") {
          small(sup("1"), "The testing file is used by the NBC model to evaluate performance and make predictions on.")
        },
        br(),
        if (tolower(loaded) == "training") {
          small(sup("1"), "The training file is used by the NBC model to learn probabilities from.")
        } else if (tolower(loaded) == "testing") {
          small(sup("2"), "The testing file is used by the NBC model to evaluate performance and make predictions on.")
        },
        br(),
        small(sup("a"), "If testing causes are not known, the", b(notLoaded, "file"), "must have", if (omit) loadedCols + 1 else loadedCols - 1, "columns instead."),
        br(),
        small(sup("b"), "If testing causes are not known, the", b(notLoaded, "file"), "must",
              if (omit) {
                span("require an additional", i("Cause"), "column as the second column.")
              } else {
                span("omit the second column", HTML("<i>(Cause)</i> with the <b>Testing Causes Known</b> option unchecked."))
              }
        ),
        br(), br(),
        small("Reminder: Symptoms that are not equal to 1 or 0 are imputed randomly from the distribution of known 1s and 0s; symptom columns will be removed from training and testing if 1s or 0s do not exist."),
        br()
      )
    })
  )
}


# NBC guide messages when both files are loaded
# Helper function for creating a confirmation message after loading both train and test files.
# @param trainDT A dataframe of the training data
# @param testDT A dataframe of the testing data
# @return withTags A shiny withTags object for rendering to HTML.
nbcGuideRun <-function (trainDT, testDT, known) {
  checked <- try(nbc4va:::internalCheckNBC(trainDT, testDT, known=known))
  if (is.character(checked)) {
    return(nbcError(checked))
  }
  withTags({
    div(
      "Please proceed by clicking the", b("Run"), "button to train and evaluate a nbc model using the uploaded training and testing files.", hr(),
      HTML("<div class=\"prev-dt\">"),
      i("Preview of Training File:"), br(),
      renderDataTable(trainDT, options=list(searching=FALSE, lengthChange=TRUE, columns.searchable=FALSE, scrollX=TRUE, scrollY="150px", paging=FALSE, scrollCollapse=TRUE)), br(),
      HTML("</div>"),
      HTML("<div class=\"prev-dt\">"),
      i("Preview of Testing File:"), br(),
      renderDataTable(testDT, options=list(searching=FALSE, lengthChange=TRUE, columns.searchable=FALSE, scrollX=TRUE, scrollY="150px", paging=FALSE, scrollCollapse=TRUE)),
      HTML("</div>"),
      br()
    )
  })
}


# NBC error message if incorrect file structure
# Helper function for creating an html based error message if execution of nbc4vaIO() does not complete.
# @param nbcResults The NBC results object from package nbc4va.
# @return withTags A shiny withTags object for rendering to HTML.
nbcError <- function(nbcResults) {
  if (is.character(nbcResults)) {
    return(withTags({
      div(
        HTML("<span class=\"shiny-output-error\">Incorrect file structure. Please review the input file structure specifications.</span>"),
        br(), hr(),
        nbcGuide(NULL, NULL, heading=FALSE),
        br()
      )
    }))
  }
}


# NBC overview after run
# Helper function for creating an html based overview of the run once files are uploaded.
# @param nbcResults The NBC results object from package nbc4va.
# @param inc Whether to let user know that they can omit or include a cause column
# @return withTags A shiny withTags object for rendering to HTML.
nbcOverview <- function(nbcResults) {
  if (is.list(nbcResults)) {
    info <- nbcResults$nbc_summary
    meanProb <- mean(info$prob.causes)
    return(
      withTags({
        div(
          ul(li("A Naive Bayes Classifier model was trained on", info$train.samples, "cases with", length(info$symptoms),
                "symptoms and", length(info$causes.train), "causes."),
             li("The model was tested on", info$test.samples, if(!is.null(info$causes.test)) span("cases with", length(info$causes.test), "causes.") else "cases."),
             li("There were a total of", length(info$causes), "unique causes in both the training and testing data."),
             if(info$test.known) {
               testCauses <- unique(info$test.causes)
               causeDiff <- testCauses[!testCauses %in% unique(info$train.causes)]
               if (length(causeDiff) > 0) {
                 li("Some testing causes were not in the training data:", paste(causeDiff, collapse=", "))
               }
             },
             li("The predictions had", length(info$causes.pred[info$causes.pred %in% info$causes.test]), "causes in the testing data out of the possible", paste(length(info$causes.test), " testing causes.", sep="")),
             if (!is.null(info$metrics.all)) {
               span(
                 li(
                   if (!is.null(info$metrics.all[["CSMFaccuracy"]])) {
                     span(HTML("Performance metrics were calculated for the model: a CSMF Accuracy<sup>1</sup> of"), round(info$metrics.all[["CSMFaccuracy"]], 3))
                   },
                   if (!is.null(info$metrics.all[["Sensitivity"]])) {
                     span(HTML(", a Sensitivity<sup>2</sup> of"), round(info$metrics.all[["Sensitivity"]], 3))
                   },
                   if (!is.null(info$metrics.all[["PCCC"]])) {
                     span(HTML(", a PCCC<sup>3</sup> of"), round(info$metrics.all[["PCCC"]], 3))
                   }
                 ),
                 if (!is.null(info$metrics.all[["TruePositives"]])) {
                   li("There were", info$metrics.all[["TruePositives"]], "true positives, ",
                      info$metrics.all[["TrueNegatives"]], "true negatives, ",
                      info$metrics.all[["FalsePositives"]], "false positives, and",
                      info$metrics.all[["FalseNegatives"]], "false negatives.")
                 }
               )
             }
          ),
          if (!is.null(info$metrics.all)) {
            p(
              if (!is.null(info$metrics.all[["CSMFaccuracy"]])) {
                small(br(), sup("1"), "The Cause Specific Mortality Fraction (CSMF) accuracy has values ranging from 0 (worst possible model for CSMF composition) to 1 (no error in predicted CSMFs)", HTML("<a target=\"_blank\" href=\"http://pophealthmetrics.biomedcentral.com/articles/10.1186/1478-7954-9-28\">(Murray <i>et al</i> 2011)</a>."))
              },
              if (!is.null(info$metrics.all[["Sensitivity"]])) {
                small(br(), sup("2"), "The sensitivity measures a model's ability to correctly predict individual causes of death (higher values are better)", HTML("<a target=\"_blank\" href=\"https://dl.dropboxusercontent.com/u/27743223/201101-Evaluation_JMLT_Postprint-Colour.pdf\">(Powers, 2011)</a>."))
              },
              if (!is.null(info$metrics.all[["PCCC"]])) {
                small(br(), sup("3"), "The Partial Chance Corrected Concordance measures how much better a model is than random cause assignment (higher values are better)", HTML("<a target=\"_blank\" href=\"http://pophealthmetrics.biomedcentral.com/articles/10.1186/1478-7954-9-28\">(Murray <i>et al</i> 2011)</a>."))
              }
            )
          }
        )
      })
    )
  }
}


# Run NBC on training and test data
# Wrapper function to execute the NBC IO on training and testing data when it becomes available.
# @param training The shiny file upload object representing the training file.
# @param testing The shiny file upload object representing the testing file.
# @return out Reactive object from shiny of the nbc4vaIO() output/error or NULL if training or testing is NULL
nbcRun <- function(training, testing, known) {
  if (!is.null(training) && !is.null(testing)) {
    trainingFile <- training$datapath
    testingFile <- testing$datapath
    withProgress(message="Running Naive Bayes Classifier..", {
      return(try(nbc4va::nbc4vaIO(trainingFile, testingFile, known=known, saveFiles=FALSE)))
    })
  } else {
    return(NULL)
  }
}


# Download Zip File from NBC Object
# Helper function for creating a zip download of the NBC results.
# @param training The shiny file upload object representing the training file.
# @param testing The shiny file upload object representing the testing file.
# @param nbcResults The NBC results object from package nbc4va.
# @param plotSummary The name of the summary plot to refer to for logging messages.
# @param plotFootnote Whether to include a footnote in the plot or not
# @param zipName A character value of the file header for the zip file and files inside the zipped file.
# @return downloadHandler A shiny downloadhandler object for the resulting zip file.
# Rtools required for zip to work, eliminate dependency
nbcDownloadZip <- function (training, testing, nbcResults, plotSummary, plotFootnote, zipName="nbc4va") {
  downloadHandler(
    filename=paste(zipName, "zip", sep="."),
    content=function(zipPath) {
      if (is.list(nbcResults)) {
        withProgress(message="Preparing Results Zip..", {

          # (Model_Results) Obtain model results files for zipping
          trainingFile <- training$datapath
          testingFile <- testing$datapath
          resultPaths <- nbc4va::nbc4vaIO(trainingFile, testingFile, fileHeader=zipName)
          resultPaths <- resultPaths[!names(resultPaths) %in% c("dir", "prob")]

          # (Plot_Results) Obtain plot results for zipping
          plotPath <- file.path(dirname(testingFile), paste(zipName, "plot.png", sep="_"))
          png(plotPath)
          nbcPlot(nbcResults, plotSummary, plotFootnote)
          dev.off()
          resultPaths <- c(resultPaths, plotPath)

          # (Zip_Results) Create zip file with all results
          wd <- getwd()
          setwd(dirname(testingFile))
          zip(zipPath, basename(resultPaths))
          setwd(wd)
        })
      }
    },
    contentType = "application/zip"
  )
}

# Provide predicted table description
# @param nbcResults The NBC results object from package nbc4va.
# @return withTags A shiny withTags object for rendering to HTML or NULL if uploads are complete.
nbcPredDescribe <- function(nbcResults) {
  if (is.list(nbcResults)) {
    nbcPred <- nbcResults$pred
    return(
      withTags({
        div(
          "The predictions are formatted as a table with the most probable causes in column",
          b("Prediction1"),
          "to the least probable causes in column",
          b(paste(names(nbcPred)[ncol(nbcPred)], ".", sep=""))
        )
      }))
  }
}


# Plot causes of death by predicted CSMF with progress
# Helper function for plotting the top causes of death by predicted cause specific mortality fraction (CSMF) with a progress message.
# @param nbcResults The NBC results object from package nbc4va.
# @param plotName A character value of the plot name for the progress message.
# @param footnote Set to TRUE to include a footnote or FALSE to exclude.
# @param ... Additional arguments to be passed to plot.
# @return plot A plot object from plotting the nbc results.
nbcPlot <- function(nbcResults, plotName="", footnote=TRUE, ...) {
  if (is.list(nbcResults)) {
    withProgress(message=paste("Updating ", plotName, " Plot..", sep=""), {
      plot(nbcResults$nbc, footnote=footnote, main="Predicted Cause of Death Distribution for Test Data", ...)
    })
  }
}


# Obtain number of unique predicted causes
# Helper function for obtaining the number of unique predicted causes from an nbc object.
# @param nbcResults The NBC results object from package nbc4va.
# @return length The number of unique predicted causes.
nbcPredCauses <- function(nbcResults) {
  if (is.list(nbcResults)) {
    return(length(nbcResults$nbc$causes.pred))
  } else {
    return(1)
  }
}


# Read CSV with progress
# Helper function for reading a csv file with a progress message.
# @param csvPath The path to the csv file.
# @param csvName A character value representing the csv name for progress info.
# @param nrows Maximum number of rows to read.
# @return read.csv A dataframe from the csv file.
csvDF <- function(csvPath, csvName="", nrows) {
  withProgress(message=paste("Updating ", csvName, " Data..", sep=""), {
    if (!is.null(nrows)) {
      return(read.csv(csvPath, nrows=nrows))
    } else {
      return(read.csv(csvPath))
    }
  })
}


# Return Dataframe from list with progress
# Helper function for returning a dataframe from a list of named dataframes with a progress message.
# @param dfName A character value of the dataframe name to extract from the list of named dataframes.
# @param listObj The list object containing the tableName as one of its names.
# @param nrows The maximum number of rows to return.
# @return df A dataframe from calling listObj[[dfName]].
listDF <- function(dfName, listObj, nrows=NULL) {
  if (!is.null(listObj)) {
    withProgress(message=paste("Updating ", dfName, " Data..", sep=""), {
      dfTable <- listObj[[dfName]]
      if (!is.null(nrows)) {
        return(head(dfTable, nrows))
      } else {
        return(dfTable)
      }
    })
  }
}


# Write CSV using a dataframe from a list
# Helper function for writing a csv file from a dataframe in a list of named dataframes.
# @param dfName A character value of the dataframe name in listObj.
# @param listObj The list object containing the dfName as one of its names.
# @param csvPath The path to write the csv file.
# @param csvHeader A character value of the file header for the csv file.
# @return write.csv The result of writing the csv file with write.csv.
listWriteCSV <- function(dfName, listObj, csvPath, csvHeader="nbc4va") {
  if (!is.null(listObj)) {
    withProgress(message=paste("Preparing ", dfName, " CSV..", sep=""), {
      write.csv(listObj[[dfName]], csvPath, row.names=FALSE)
    })
  }
}

