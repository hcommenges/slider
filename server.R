##############################
# Shiny App: SLIDER - Software for LongItudinal Data Exploration with R
# Server
##############################


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
  
  
  # update column names
  updateCol <- function(session, columns){
    updateSelectInput(session = session, inputId = "timecol", choices = columns)
  }
  
  updateExampleCol <- function(session, columns){
    updateSelectInput(session = session, inputId = "timecol", choices = columns, selected = columns)
  }
  
  # select data by factors
  selecData <- reactive({
    fullData <- readData()
    timeCol <- input$timecol
    factCol1 <- input$factcol1
    factCol2 <- input$factcol2
    factMod1 <- input$factmod1
    factMod2 <- input$factmod2
    wgtCol <- input$weightcol
    
    if (!is.null(fullData) && length(timeCol) > 1){
      if (is.null(factMod1)) {
        if (is.null(factMod2)) {
          timeSteps <- fullData[, timeCol]
        } else {
          timeSteps <- fullData[fullData[, factCol2] %in% factMod2, timeCol]
        }
      } else {
        if (is.null(factMod2)) {
          timeSteps <- fullData[fullData[, factCol1] %in% factMod1, timeCol]
        } else {
          timeSteps <- fullData[((fullData[, factCol1] %in% factMod1) & (fullData[, factCol2] %in% factMod2)), timeCol]
        }
      }
    } else {
      timeSteps <- NULL
    }
    
    if(is.character(timeSteps[ , 1]) == TRUE){
      uniqueVal <- sort(unique(unlist(timeSteps)))
      timeSteps <- colwise(factor, levels = uniqueVal, labels = uniqueVal)(timeSteps)
    } else {
      uniqueVal <- sort(unique(unlist(timeSteps)))
      timeSteps <- colwise(factor, levels = uniqueVal, labels = paste("CAT_", uniqueVal, sep = ""))(timeSteps)
    }
    return(timeSteps)
  })
  
  # select weight variable
  selecWgt <- reactive({
    if(!is.null(input$weightcol)){
      return <- as.vector(readData()[row.names(selecData()), input$weightcol])
    } else {
      return(NULL)
    }
  })
  
  # create table of flows for the slide plot
  createFlowsTab <- reactive({
    df <- selecData()
    getFlows <- GetCrossFlows(df = df, wgtvar = selecWgt())
    return(getFlows)
  })

  
  # create state sequence object (TraMineR)
  createSts <- reactive({
    if(!is.null(readData()) && length(input$timecol) > 1){
      seqdf <- selecData()
      stsobj <- seqdefinition(seqdf, wgt = selecWgt())
    } else {
      return()
    }
  })
  
  
  # outputs ----
  
  # dynamic sliders
  output$sliderthreshold <- renderUI({
    if(!is.null(readData()) && length(input$timecol) > 1){
      flowstab <- createFlowsTab()
      splot <- slideplot(listFlows = flowstab, threshold = 1, mask = input$mask, showfreq = input$showfreq, thickmin = input$thickmin)
      maxfreq <- max(splot$data$FREQ)
      sliderInput(inputId = "inSlidersp", label = "Threshold", min = 0, max = maxfreq - 1, value = round(maxfreq / 4, digits = 0) , step = 1)
    } else{
      return()
    }
  })
  
  output$sliderseqi <- renderUI({
    if(!is.null(readData()) && length(input$timecol) > 1){
      nmaxseq <- nrow(selecData())
      sliderInput(inputId = "inSliderseqi", label = "Index of sequences", min = 1, max = nmaxseq, value = c(1, 10) , step = 1)
    } else{
      return()
    }
  })
    
  output$selectwgt <- renderUI({
    if(!is.null(readData()) && length(input$timecol) > 1){
      colNames <- colnames(readData())
      selectInput(inputId = "weightcol", label = "Choose weighting variable (optional)", choices = colNames, multiple = TRUE)
    } else {
      return()
    }
  })
  
  output$selectfac1 <- renderUI({
    if(!is.null(readData()) && length(input$timecol) > 1){
      colNames <- colnames(readData())
      selectInput(inputId = "factcol1", label = "Choose 1st factor (optional)", choices = colNames, multiple = TRUE)
    } else {
      return()
    }
  })
  
  output$selectfac2 <- renderUI({
    if(!is.null(readData()) && length(input$timecol) > 1){
      colNames <- colnames(readData())
      selectInput(inputId = "factcol2", label = "Choose 2nd factor (optional)", choices = colNames, multiple = TRUE)
    } else {
      return()
    }
  })
  
  output$selectmod1 <- renderUI({
    if(!is.null(readData()) && length(input$timecol) > 1){
      uniqueModalities1 <- sort(as.character(unique(readData()[,input$factcol1])))
      modalitiesList1 <- uniqueModalities1
      selectInput(inputId = "factmod1", label = "Choose 1st group", choices = modalitiesList1, multiple = TRUE)
    } else {
      return()
    }
  })
  
  output$selectmod2 <- renderUI({
    if(!is.null(readData()) && length(input$timecol) > 1){
      uniqueModalities2 <- sort(as.character(unique(readData()[,input$factcol2])))
      modalitiesList2 <- uniqueModalities2
      selectInput(inputId = "factmod2", label = "Choose 2nd group", choices = modalitiesList2, multiple = TRUE)
    } else {
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
  
  output$contents <- renderDataTable({
    if(!is.null(readData()) && length(input$timecol) > 1){
      return(selecData())
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
      flowstab <- createFlowsTab()
      print(slideplot(listFlows = flowstab, threshold = input$inSlidersp, mask = input$mask, showfreq = input$showfreq, thickmin = input$thickmin))
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
      seqpcplot(createSts(), 
                ltype = "non-embeddable",
                cex = input$pccex, 
                lwd = input$pclwd, 
                grid.scale = input$pcgrid, 
                embedding = "most-frequent", 
                lorder = "foreground", 
                lcourse = "upwards", 
                order.align = "time")
    } else {
      return()
    }
  })
  
  
  # transition rate panel
  
  output$transratetext <- renderText({
    if(!is.null(readData()) && length(input$timecol) > 1){
      tratetext <- "Transition rates are the frequency of transition from one state to another, as observed in the dataset. \nIt may be read as an origin-detination matrix, with absolute or relative frequency."  
    } else {
      return()
    }
  })
  
  output$transrate <- renderTable({
    if(!is.null(readData()) && length(input$timecol) > 1){
      if(!is.null(input$weightcol)){
        rowPercent <- seqtrate(createSts(), weighted = TRUE)
        oriStates <- selecData()[ , -ncol(selecData())]
        tabCont <- colwise(TabWgt, wgt = selecWgt())(oriStates)
        tabContAggreg <- apply(tabCont, 1, sum, na.rm = TRUE)
        absFreq <- rowPercent * tabContAggreg
        mode(absFreq) <- "integer"
        colPercent <- t(t(absFreq) / apply(absFreq, 2, sum))
      } else {
        rowPercent <- seqtrate(createSts(), weighted = FALSE)
        tabCont <- as.vector(table(as.matrix(selecData()[ , -ncol(selecData())])))
        absFreq <- rowPercent * tabCont
        mode(absFreq) <- "integer"
        colPercent <- t(t(absFreq) / apply(absFreq, 2, sum))
      }

      if(input$transparameter == "absfreq"){
        return(as.data.frame(absFreq))
      }
      if(input$transparameter == "rowpct"){
        return(as.data.frame(round(rowPercent, digits = 2)))
      }
      if(input$transparameter == "colpct"){
        return(as.data.frame(round(colPercent, digits = 2)))
      }
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
      seqiplot(createSts(), border = input$borderiplot, tlim = seq(input$inSliderseqi[1], input$inSliderseqi[2], 1), withlegend = "right")
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
      seqfplot(createSts(), border = input$borderfplot, withlegend = "right")
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
      seqdplot(createSts(), border = input$borderdplot, withlegend = "right")
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
        flowstab <- createFlowsTab()
        print(slideplot(listFlows = flowstab, threshold = input$inSlidersp, mask = input$mask, showfreq = input$showfreq, thickmin = input$thickmin))
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
    filename = "DistributionPlot.svg",
    content = function(file) {
      svg(file, width = input$widthseqd / 2.54, height = input$heightseqd / 2.54, pointsize = 8)
      if(!is.null(readData()) && length(input$timecol) > 1){
        seqdplot(createSts(), border = input$borderdplot)
      } else {
        return()
      }
      dev.off()
    })
  
  
})
