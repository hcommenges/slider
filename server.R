##############################
# Shiny App: SLIDER - Software for LongItudinal Data Exploration with R
# Server
##############################


shinyServer(function(input, output, session) {
  
  # load data ----
  
  baseData <- reactiveValues(df = NULL)
  data(mvad)
  
  observe({
    req(input$fileInput$datapath)
    oriData <- read.csv(file = input$fileInput$datapath,
                        sep = input$sepcol,
                        quote = input$quote,
                        dec = input$sepdec,
                        encoding = input$encodtab,
                        stringsAsFactor = FALSE,
                        check.names = FALSE)
    baseData$df <- oriData
  })
  
  observeEvent(input$loadTestData, {
    dataTest <- mvad[ , c(1, 15:86)]
    dataTest <- dataTest[ , c(1, seq(4, 66, 12))]
    dataTest <- colwise(as.character)(dataTest[ , 2:7])
    baseData$df <- dataTest
  })
  
  
  # update fields ----
  
  observe({
    columnList <- c(colnames(baseData$df))
    columnListFirst <- c("", columnList)
    
    updateSelectInput(session = session,
                      inputId = "timecol",
                      choices = columnList)
    updateSelectInput(session = session,
                      inputId = "weightcol",
                      choices = columnListFirst)
    updateSelectInput(session = session,
                      inputId = "factcol1",
                      choices = columnListFirst)
    updateSelectInput(session = session,
                      inputId = "factcol2",
                      choices = columnListFirst)
    
  })
  
  
  observe({
    req(baseData$d, input$timecol)
    if(length(input$timecol) > 1){
      maxFreq <- RoundAccur(val = max(table(c(unlist(selecData()$TBL)))) / (ncol(selecData()$TBL) + 1), acc = 10)
      updateSliderInput(session = session,
                        inputId = "sliderthreshold",
                        value = 10,
                        min = 0,
                        max = maxFreq,
                        step = 1)
    } else {
      return() 
    }
  })
  
  
  # select data by factors ----
  
  selecData <- reactive({
    req(baseData$df, input$timecol)
    if(length(input$timecol) > 1){
      # filter rows and select columns
      if (!is.null(input$factmod1) & is.null(input$factmod2)) {
        dataSel <- baseData$df[baseData$df[, input$factcol1] %in% input$factmod1, ]
      } else if (is.null(input$factmod1) & !is.null(input$factmod2)){
        dataSel <- baseData$df[baseData$df[, input$factcol2] %in% input$factmod2, ]
      } else if (!is.null(input$factmod1) & !is.null(input$factmod2)){
        dataSel <- baseData$df[((baseData$df[, input$factcol1] %in% input$factmod1) & (baseData$df[, input$factcol2] %in% input$factmod2)), ]
      } else {
        dataSel <- baseData$df
      }
      timeSteps <- dataSel[, input$timecol]
      
      # select weights
      if(input$weightcol == ""){
        wgtVec <- rep(1, times = nrow(dataSel))
      } else {
        wgtVec <- as.vector(dataSel[, input$weightcol])
      }
      # convert into factor
      if(is.character(timeSteps[ , 1]) == TRUE){
        uniqueVal <- sort(unique(unlist(timeSteps)))
        timeSteps <- colwise(factor, levels = uniqueVal, labels = uniqueVal)(timeSteps)
      } else {
        uniqueVal <- sort(unique(unlist(timeSteps)))
        timeSteps <- colwise(factor, levels = uniqueVal, labels = paste("CAT_", uniqueVal, sep = ""))(timeSteps)
      }
      return(list(TBL = timeSteps, WGT = wgtVec))
    } else {
      return()
    }
  })
  
  
  # create table of flows for the slide plot
  createFlowsTab <- reactive({
    req(baseData$df, input$timecol)
    df <- selecData()
    getFlows <- GetCrossFlows(df = df$TBL, wgtvar = df$WGT)
    return(getFlows)
  })
  
  
  # create state sequence object (TraMineR)
  createSts <- reactive({
    req(baseData$df, input$timecol)
    df <- selecData()
    stsobj <- CreateSeq(df$TBL, wgt = df$WGT)
  })
  
  
  # outputs ----
  
  # dynamic sliders
  
  output$selectmod1 <- renderUI({
    req(input$factcol1)
    uniqueModalities1 <- sort(as.character(unique(baseData$df[,input$factcol1])))
    modalitiesList1 <- uniqueModalities1
    selectInput(inputId = "factmod1", label = "Choose 1st group", choices = modalitiesList1, multiple = TRUE)
  })
  
  output$selectmod2 <- renderUI({
    req(input$factcol2)
    uniqueModalities2 <- sort(as.character(unique(baseData$df[,input$factcol2])))
    modalitiesList2 <- uniqueModalities2
    selectInput(inputId = "factmod2", label = "Choose 2nd group", choices = modalitiesList2, multiple = TRUE)
  })
  
  
  # data summary panel
  output$datasummary <- renderText({
    req(baseData$df, input$timecol)
    if(length(input$timecol) > 1){
      if(isTRUE(any(is.na(unlist(selecData()$TBL))))){
        natext <- "There are missing values in the dataset"
      } else {
        natext <- "There is no missing value in the dataset"
      }
      ndim <- dim(selecData()$TBL)
      obstext <- paste(ndim[1], "observations", sep = " ")
      vartext <- paste(ndim[2], "variables", sep = " ")
      summarytext <- paste(obstext, "\n", 
                           vartext, "\n",
                           natext, 
                           sep = "")
      return(summarytext)
    } else {return()}
  })
  
  output$contents <- renderDataTable({
    req(baseData$df, input$timecol)
    return(selecData()$TBL)
  })
  
  # slide plot panel
  
  output$slidetext <- renderText({
    req(baseData$df, input$timecol)
    slideplottext <- "The slide plot draws aggregated trajectories of individuals. \nThe thickness of a segment is proportional to the frequency of transition from one state to another."  
  })
  
  output$slideplot <- renderPlot({
    req(baseData$df, input$timecol)
    print(SlidePlot(listFlows = createFlowsTab(), threshold = input$sliderthreshold, mask = input$mask, showfreq = input$showfreq, thickmin = input$thickmin))
  })
  
  
  # parallel coordinates plot panel
  
  output$pctext <- renderText({
    req(baseData$df, input$timecol)
    parcoordtext <- "The parallel coordinates plot for sequence data shows partially aggregated trajectories of individuals."  
  })
  
  output$pcplot <- renderPlot({
    req(baseData$df, input$timecol)
    seqpcplot(createSts(), 
              ltype = "non-embeddable",
              cex = input$pccex, 
              lwd = input$pclwd, 
              grid.scale = input$pcgrid, 
              embedding = "most-frequent", 
              lorder = "foreground", 
              lcourse = "upwards", 
              order.align = "time")
  })
  
  
  # transition rate panel
  
  output$transratetext <- renderText({
    req(baseData$df, input$timecol)
    tratetext <- "Transition rates are the frequency of transition from one state to another, as observed in the dataset. \nIt may be read as an origin-detination matrix, with absolute or relative frequency."  
  })
  
  output$transrate <- renderTable({
    req(baseData$df, input$timecol)
    wgtBool <- is.null(input$weightcol)
    resTrans <- CreateTransRate(stsobj = createSts(), wgtbool = wgtBool, df = selecData()$TBL, wgtvec = selecData()$WGT)
    
    if(input$transparameter == "absfreq"){
      return(as.data.frame(resTrans$ABSFREQ))
    }
    if(input$transparameter == "rowpct"){
      return(as.data.frame(round(resTrans$ROWPCT, digits = 2)))
    }
    if(input$transparameter == "colpct"){
      return(as.data.frame(round(resTrans$COLPCT, digits = 2)))
    }
    
  })
  
  # index plot panel
  
  output$indextext <- renderText({
    req(baseData$df, input$timecol)
    indexplottext <- "The sequence index plot draws a set of individual sequences. \nEach distinct state is filled with a distinct colour."  
  })
  
  output$seqindex <- renderPlot({
    req(baseData$df, input$timecol)
    seqiplot(createSts(), border = input$borderiplot, tlim = seq(input$sliderseqi[1], input$sliderseqi[2], 1), withlegend = "right")
  })
  
  # frequency plot panel
  
  output$freqtext <- renderText({
    req(baseData$df, input$timecol)
    freqplottext <- "The sequence frequency plot draws the ten most frequent sequences. \nThe vertical axis indicates the cumulative percentage of these ten sequences."  
  })
  
  output$seqfreq <- renderPlot({
    req(baseData$df, input$timecol)
    seqfplot(createSts(), border = input$borderfplot, withlegend = "right")
  })
  
  # distribution plot panel
  
  output$distrtext <- renderText({
    req(baseData$df, input$timecol)
    distrplottext <- "The sequence distribution plot draws the whole dataset with transversal aggregation . \nIt shows the distribution of distinct states at each time step."  
  })
  
  output$seqdistr <- renderPlot({
    req(baseData$df, input$timecol)
    seqdplot(createSts(), border = input$borderdplot, withlegend = "right")
  })
  
  
  # Downloads ----
  
  output$downloadsp <- downloadHandler(
    filename = "SlidePlot.svg",
    content = function(file) {
      svg(file, width = input$widthslide / 2.54, height = input$heightslide / 2.54, pointsize = 8)
      req(baseData$df, input$timecol)
      print(SlidePlot(listFlows = createFlowsTab(), threshold = input$sliderthreshold, mask = input$mask, showfreq = input$showfreq, thickmin = input$thickmin))
      dev.off()
    })
  
  
  output$downloadpc <- downloadHandler(
    filename = "ParallelCoordPlot.svg",
    content = function(file) {
      svg(file, width = input$widthseqpc / 2.54, height = input$heightseqpc / 2.54, pointsize = 8)
      req(baseData$df, input$timecol)
      seqpcplot(createSts(), ltype="non-embeddable", cex=input$pccex, lwd=input$pclwd, grid.scale=input$pcgrid, embedding="most-frequent", lorder="foreground", lcourse="upwards", order.align="time")
      dev.off()
    })
  
  
  output$downloadip <- downloadHandler(
    filename = "IndexPlot.svg",
    content = function(file) {
      svg(file, width = input$widthseqi / 2.54, height = input$heightseqi / 2.54, pointsize = 8)
      req(baseData$df, input$timecol)
      seqiplot(createSts(), border = input$borderiplot, tlim = seq(input$sliderseqi[1], input$sliderseqi[2], 1))
      dev.off()
    })
  
  
  output$downloadfp <- downloadHandler(
    filename = "FrequencyPlot.svg",
    content = function(file) {
      svg(file, width = input$widthseqf / 2.54, height = input$heightseqf / 2.54, pointsize = 8)
      req(baseData$df, input$timecol)
      seqfplot(createSts(), border = input$borderfplot)
      dev.off()
    })
  
  output$downloaddp <- downloadHandler(
    filename = "DistributionPlot.svg",
    content = function(file) {
      svg(file, width = input$widthseqd / 2.54, height = input$heightseqd / 2.54, pointsize = 8)
      req(baseData$df, input$timecol)
      seqdplot(createSts(), border = input$borderdplot)
      dev.off()
    })
  
  
})
