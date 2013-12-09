##############################
# Shiny App: ITOU
# Server
##############################

##### dans transition rate, 1 tab avec effectifs, 1 tab pct en lignes, 1 tab pct
##### en colonnes dans slide plot, surimposer les effectifs pour chaque trait,
##### avec option "show frequencies" tester la fonction seqpcplot

shinyServer(function(input, output, session) {
  
  # functions ----
  
  # load example dataset
  rValues <- reactiveValues(dataSource = NULL)
  
  observe({
    if (input$loadTestData > 0) {
      rValues$dataSource <- "example"
    }
  })
  
  observe({
    if (!is.null(input$file1)){
      rValues$dataSource <- input$file1
    }
  })
  
  # read csv data
  readData <- reactive({
    if(!is.null(rValues$dataSource)) {
      if (rValues$dataSource != "example") {
        mytable <- read.csv(rValues$dataSource$datapath, 
                            header = input$header, 
                            sep = input$sep, 
                            quote = input$quote,
                            stringsAsFactors = FALSE)
        columnList <- unlist(colnames(mytable))
        updateCol(session=session, columns=columnList)
        return(mytable)
      } else {
        data(mvad) # example data set (TraMineR)
        dExample <- mvad[ , c(1, 15:86)]
        dExample <- dExample[ , c(1, seq(4, 66, 12))]
        dExample <- colwise(as.character)(dExample[ , 2:7])
        mytable <- dExample
        columnList <- unlist(colnames(mytable))
        updateExampleCol(session=session, columns=columnList)
        return(mytable)
      }
      
      
    } else {
      return()
    }
  })
  
  observe({
    print(input$timecol)
  })
  
  # update column names
  updateCol <- function(session, columns){
    updateSelectInput(session = session, inputId = "timecol", choices = columns)
    updateSelectInput(session = session, inputId = "factcol1", choices = columns)
    updateSelectInput(session = session, inputId = "factcol2", choices = columns)
  }
  
  updateExampleCol <- function(session, columns){
    print(columns)
    updateSelectInput(session = session, inputId = "timecol", choices = columns, selected= columns)
    updateSelectInput(session = session, inputId = "factcol1", choices = columns)
    updateSelectInput(session = session, inputId = "factcol2", choices = columns)
  }
  
  # select data by factors
  selecData <- reactive({
  fullData <- readData()
  timeCol <- input$timecol
  factCol1 <- input$factcol1
  factCol2 <- input$factcol2
  factMod1 <- input$factmod1
  factMod2 <- input$factmod2
  
  if (!is.null(fullData) && length(timeCol) > 1){
    if (factMod1 == "none") {
      if (factMod2 == "none") {
        return(fullData[, timeCol])
      } else {
        return(fullData[fullData[, factCol2] %in% factMod2, timeCol])
      }
    } else {
      if (factMod2 == "none") {
        return(fullData[fullData[, factCol1] %in% factMod1, timeCol])
      } else {
        return(fullData[((fullData[, factCol1] %in% factMod1) & (fullData[, factCol2] %in% factMod2)), timeCol])
      }
    }
  } else {
    return()
  }
})
  
  # create state sequence object (TraMineR)
  createSts <- reactive({
    if(!is.null(readData()) && length(input$timecol) > 1){
      seqdf <- selecData()
      stsobj <- seqdefinition(seqdf)
    } else {
      return()
    }
  })
  
  
  # outputs ----
  
  # dynamic sliders
  output$sliderthreshold <- renderUI({
    if(!is.null(readData()) && length(input$timecol) > 1){
      flowtab <- selecData()
      splot <- slideplot(flowtab, threshold = 1, mask = input$mask, showfreq = input$showfreq, thickmin = input$thickmin)
      maxfreq <- max(splot$data$FREQ)
      sliderInput(inputId="inSlidersp", label="Threshold", min=0, max=maxfreq - 1, value=round(maxfreq / 4, digits = 0) , step=1)
    } else{
      return()
    }
  })
  
  output$sliderseqi <- renderUI({
    if(!is.null(readData()) && length(input$timecol) > 1){
      nmaxseq <- nrow(selecData())
      sliderInput(inputId="inSliderseqi", label="Index of sequences", min=1, max=nmaxseq, value=c(1, 10) , step=1)
    } else{
      return()
    }
  })
  
  output$slidermod1 <- renderUI({
    if(!is.null(readData()) && length(input$timecol) > 1){
      uniqueModalities1 <- sort(as.character(unique(readData()[,input$factcol1])))
      modalitiesList1 <- c("none", uniqueModalities1)
      selectInput(inputId="factmod1", label="Choose 1st group", choices=modalitiesList1, selected="none", multiple=TRUE)
    } else{
      return()
    }
  })
  
  output$slidermod2 <- renderUI({
    if(!is.null(readData()) && length(input$timecol) > 1){
      uniqueModalities2 <- sort(as.character(unique(readData()[,input$factcol2])))
      modalitiesList2 <- c("none", uniqueModalities2)
      selectInput(inputId="factmod2", label="Choose 2nd group", choices=modalitiesList2, selected="none", multiple=TRUE)
    } else{
      return()
    }
  })
  
  # data summary panel
  output$datasummary <- renderText({
    if(!is.null(readData()) && length(input$timecol) > 1){
      if(isTRUE(any(is.na(selecData())))){
        natext <- "There are missing values in the dataset"
      } else {
        natext <- "There is no missing value in the dataset"
      }
      ndim <- dim(selecData())
      obstext <- paste(ndim[1], "observations", sep = " ")
      vartext <- paste(ndim[2], "variables", sep = " ")
      summarytext <- paste(obstext, "\n", 
                           vartext, "\n",
                           natext, 
                           sep = "")
      return(summarytext)
    } else {
      return()
    }
  })
  
  output$contents <- renderTable({
    if(!is.null(readData()) && length(input$timecol) > 1){
      return(head(selecData(), n = 20))
    } else {
      return()
    }
  })
  
  # slide plot panel
  
  output$slidetext <- renderText({
    if(!is.null(readData()) && length(input$timecol) > 1){
      slideplottext <- "The slide plot draws aggregated trajectories of individuals. \nThe thickness of a segment is proportional to the frequency of transition from one state to another."  
    } else {
      return()
    }
  })
  
  output$slideplot <- renderPlot({
    if(!is.null(readData()) && length(input$timecol) > 1){
      flowtab <- selecData()
      print(slideplot(flowtab, threshold = input$inSlidersp, mask = input$mask, showfreq = input$showfreq, thickmin = input$thickmin))
    } else {
      return()
    }
  })
  
  
  # parallel coordinates plot panel
  
  output$pctext <- renderText({
    if(!is.null(readData()) && length(input$timecol) > 1){
      parcoordtext <- "The parallel coordinates plot for sequence data shows partially aggregated trajectories of individuals."  
    } else {
      return()
    }
  })
  
  output$pcplot <- renderPlot({
    if(!is.null(readData()) && length(input$timecol) > 1){
      seqpcplot(createSts(), ltype="non-embeddable", cex=input$pccex, lwd=input$pclwd, grid.scale=input$pcgrid, embedding="most-frequent", lorder="foreground", lcourse="upwards", order.align="time")
    } else {
      return()
    }
  })
  
  
  # transition rate panel
  
  output$transratetext <- renderText({
    if(!is.null(readData()) && length(input$timecol) > 1){
      tratetext <- "Transition rates are the frequency of transition from one state to another, as observed in the dataset. \nIt is conceived as a row percentage."  
    } else {
      return()
    }
  })
  
  output$transrate <- renderTable({
    if(!is.null(readData()) && length(input$timecol) > 1){
      transitionRate <- as.data.frame(round(seqtrate(createSts()), digits = 2))
    } else {
      return()
    }
  })
  
  # index plot panel
  
  output$indextext <- renderText({
    if(!is.null(readData()) && length(input$timecol) > 1){
      indexplottext <- "The sequence index plot draws a set of individual sequences. \nEach distinct state is filled with a distinct colour."  
    } else {
      return()
    }
  })
  
  output$seqindex <- renderPlot({
    if(!is.null(readData()) && length(input$timecol) > 1){
      seqiplot(createSts(), border = input$borderiplot, tlim = seq(input$inSliderseqi[1], input$inSliderseqi[2], 1))
    } else {
      return()
    }
  })
  
  # frequency plot panel
  
  output$freqtext <- renderText({
    if(!is.null(readData()) && length(input$timecol) > 1){
      freqplottext <- "The sequence frequency plot draws the ten most frequent sequences. \nThe vertical axis indicates the cumulative percentage of these ten sequences."  
    } else {
      return()
    }
  })
  
  output$seqfreq <- renderPlot({
    if(!is.null(readData()) && length(input$timecol) > 1){
      seqfplot(createSts(), border = input$borderfplot)
    } else {
      return()
    }
  })
  
  # distribution plot panel
  
  output$distrtext <- renderText({
    if(!is.null(readData()) && length(input$timecol) > 1){
      distrplottext <- "The sequence distribution plot draws the whole dataset with transversal aggregation . \nIt shows the distribution of distinct states at each time step."  
    } else {
      return()
    }
  })
  
  output$seqdistr <- renderPlot({
    if(!is.null(readData()) && length(input$timecol) > 1){
      seqdplot(createSts(), border = input$borderdplot)
    } else {
      return()
    }
  })
  
  
  # Downloads ----
  
  output$downloadsp <- downloadHandler(
    filename = "SlidePlot.svg",
    content = function(file) {
      svg(file, width = input$widthslide / 2.54, height = input$heightslide / 2.54, pointsize = 8)
      if(!is.null(readData()) && length(input$timecol) > 1){
        flowtab <- selecData()
        print(slideplot(flowtab, threshold = input$inSlidersp, mask = input$mask, showfreq = input$showfreq, thickmin = input$thickmin))
      } else {
        return()
      }
      dev.off()
    })
  
  
  output$downloadpc <- downloadHandler(
    filename = "ParallelCoordPlot.svg",
    content = function(file) {
      svg(file, width = input$widthseqpc / 2.54, height = input$heightseqpc / 2.54, pointsize = 8)
      if(!is.null(readData()) && length(input$timecol) > 1){
        seqpcplot(createSts(), ltype="non-embeddable", cex=input$pccex, lwd=input$pclwd, grid.scale=input$pcgrid, embedding="most-frequent", lorder="foreground", lcourse="upwards", order.align="time")
      } else {
        return()
      }
      dev.off()
    })
  
  
  output$downloadip <- downloadHandler(
    filename = "IndexPlot.svg",
    content = function(file) {
      svg(file, width = input$widthseqi / 2.54, height = input$heightseqi / 2.54, pointsize = 8)
      if(!is.null(readData()) && length(input$timecol) > 1){
        seqiplot(createSts(), border = input$borderiplot, tlim = seq(input$inSliderseqi[1], input$inSliderseqi[2], 1))
      } else {
        return()
      }
      dev.off()
    })
  
  
  output$downloadfp <- downloadHandler(
    filename = "FrequencyPlot.svg",
    content = function(file) {
      svg(file, width = input$widthseqf / 2.54, height = input$heightseqf / 2.54, pointsize = 8)
      if(!is.null(readData()) && length(input$timecol) > 1){
        seqfplot(createSts(), border = input$borderfplot)
      } else {
        return()
      }
      dev.off()
    })
  
  output$downloaddp <- downloadHandler(
    filename = "DensityPlot.svg",
    content = function(file) {
      svg(file, width = input$widthseqd / 2.54, height = input$heightseqd / 2.54, pointsize = 8)
      if(!is.null(readData()) && length(input$timecol) > 1){
        seqdplot(createSts(), border = input$borderiplot)
      } else {
        return()
      }
      dev.off()
    })
  
  output$downloadfct <- downloadHandler(
    filename = "SlideplotFunction.R",
    content = function(file) {
      dump("slideplot", file=file)
    }    
  )
  
})
