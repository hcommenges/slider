##############################
# Shiny App: SLIDER - Software for LongItudinal Data Exploration with R
# Packages and functions
##############################


# load packages ----

library(plyr)
library(reshape2)
library(RColorBrewer)
library(ggplot2)
library(TraMineR)
data(mvad) # example data set (TraMineR)
library(RColorBrewer)


# slideplot function ----

slideplot <- function(listFlows, threshold, mask, showfreq, thickmin)
{
  # get table of flows (cross-classification)
  flowsTable <- listFlows$FLOWS
  timeLabels <- listFlows$LABELS
  stateAlphabet <- listFlows$ALPHABET
  
  # get min and max vales
  scaleValue <- thickmin * max(flowsTable$FREQ) / (threshold + 1)
  
  # set treshold fields
  flowsTable$FREQUPON <- ifelse(flowsTable$FREQ > threshold, flowsTable$FREQ, NA)
  flowsTable$FREQBELOW <- as.numeric(ifelse(flowsTable$FREQ > threshold, NA, (threshold + 1)))
  
  # position text labels 
  flowsTable$XTEXT <- apply(flowsTable[ , c("XORI", "XDES")], 1, mean)
  flowsTable$YTEXT <- apply(flowsTable[ , c("YORI", "YDES")], 1, mean)
  flowsTable$SLOPE <- with(flowsTable, (YDES - YORI) / (XDES - XORI))
  flowsTable$INTERCEPT <- with(flowsTable, (YTEXT - SLOPE * XTEXT))
  flowsTable$YTEXTCORR <- with(flowsTable, ifelse(SLOPE == 0, YTEXT, 
                                                  ifelse(SLOPE < 0, YTEXT - 0.1 * abs(SLOPE), YTEXT + 0.1 * abs(SLOPE))))
  flowsTable$XTEXTCORR <- with(flowsTable, ifelse(SLOPE == 0, XTEXT, 
                                                  ifelse(SLOPE < 0, (YTEXTCORR - INTERCEPT) / SLOPE - 0.1, (YTEXTCORR - INTERCEPT) / SLOPE + 0.1)))
  
  
  # plot flows
  if(mask == FALSE){
    slidePlot <- ggplot(flowsTable) + 
      geom_segment(aes(x = XORI, y = YORI, xend = XDES, yend = YDES, size = FREQBELOW), colour = "lightgrey", lineend = "round") +
      geom_segment(aes(x = XORI, y = YORI, xend = XDES, yend = YDES, size = FREQUPON), colour = "black", lineend = "round") +
      scale_x_continuous("Timeline", breaks = seq(1, length(timeLabels), 1), labels = timeLabels) +
      scale_y_continuous("Categories", breaks = seq(1, length(timeLabels), 1), labels = stateAlphabet) +
      scale_size_continuous("Frequencies", range = c(thickmin, scaleValue)) +
      theme_bw() + theme(panel.grid.major = element_line(colour = NA), panel.grid.minor = element_line(colour = NA))
  }
  else if(mask == TRUE){
    slidePlot <- ggplot(flowsTable) + 
      geom_segment(aes(x = XORI, y = YORI, xend = XDES, yend = YDES, size = FREQBELOW), colour = "white", lineend = "round") +
      geom_segment(aes(x = XORI, y = YORI, xend = XDES, yend = YDES, size = FREQUPON), colour = "black", lineend = "round") +
      scale_x_continuous("Timeline", breaks = seq(1, length(timeLabels), 1), labels = timeLabels) +
      scale_y_continuous("Categories", breaks = seq(1, length(timeLabels), 1), labels = stateAlphabet) +
      scale_size_continuous("Frequencies", range = c(thickmin, scaleValue)) +
      theme_bw() + theme(panel.grid.major = element_line(colour = NA), panel.grid.minor = element_line(colour = NA))
  }
  
  if(showfreq == TRUE) {slidePlot  <-  slidePlot + geom_text(aes(x = XTEXTCORR, y = YTEXTCORR, label = FREQUPON), colour = "#CD3700", fontface = "bold")}
  
  return(slidePlot)  
}


# TraMineR function : sequence definition ----

seqdefinition <- function(df, wgt){
  stateAlphabet <- levels(unlist(df))
  timeLabels <- colnames(df)
  nbCol <- length(timeLabels)
  colorPal <- brewer.pal(length(stateAlphabet), "Set3")
  stsObject <- seqdef(df, seq(1, nbCol, 1), states = stateAlphabet, labels = stateAlphabet, cpal = colorPal, weights = wgt)
  
  return(stsObject)
}


# Internal functions ----

MakeCouples <- function(fac)
{
  coupleList <- vector()
  for(i in 1:(length(fac)-1)){
    coupleItem <- paste(
      paste(i, as.numeric(fac[i]), sep = "-"), 
      paste(i + 1, as.numeric(fac[i + 1]), sep = "-"),
      sep = "_")
    coupleList <- append(coupleList, coupleItem)
  }
  return(coupleList)
}

TabWgt <- function(fac, wgt){
  return(tapply(wgt, fac, sum, na.rm = TRUE))
}

GetCrossFlows <- function(df, wgtvar = NULL)
{
  stateAlphabet <- levels(unlist(df))
  timeLabels <- colnames(df)
  nbCol <- length(timeLabels)
  
  # create flows
  dfList <- split(df, f = row.names(df), drop = FALSE)
  sortedList <- dfList[order(as.integer(names(dfList)))]
  identList <- lapply(sortedList, FUN = unlist)
  oriDesCouples <- lapply(identList, MakeCouples)
  oriDesTab <- as.data.frame(do.call("rbind", oriDesCouples), stringsAsFactors = FALSE)
  if(!is.null(wgtvar)) {oriDesTab$WEIGHT <- wgtvar} else {oriDesTab$WEIGHT <- 1}
  moltenFlows <- melt(oriDesTab, id.vars = "WEIGHT", measure.vars = colnames(oriDesTab)[1:(ncol(oriDesTab)-1)])
  tabCont <- tapply(moltenFlows$WEIGHT, moltenFlows$value, sum, na.rm = TRUE)
  flowsTable <- data.frame(FLOWID = names(tabCont), FREQ = unname(tabCont))
  
  # split coordinates
  flowsTable$FLOWID <- as.character(flowsTable$FLOWID)
  coordFlows <- colsplit(flowsTable$FLOWID, pattern = "_", names = c("COORDORI", "COORDDES"))
  coordOri <- colsplit(coordFlows$COORDORI, pattern = "-", names = c("XORI", "YORI"))
  coordDes <- colsplit(coordFlows$COORDDES, pattern = "-", names = c("XDES", "YDES"))
  flowsTable <- cbind(flowsTable, coordOri, coordDes)
  
  return(list(FLOWS = flowsTable, LABELS = timeLabels, ALPHABET = stateAlphabet))
}



