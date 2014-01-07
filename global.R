##############################
# Shiny App: ITOU
# Packages and functions
##############################


# load packages ----

require(plyr)
require(reshape2)
require(ggplot2)
require(TraMineR)


# slideplot function ----

slideplot <- function(df, threshold, mask, showfreq, thickmin)
{
  if(is.character(df[ , 1]) == TRUE){
    vUniqueVal <- sort(unique(unlist(df)))
    df <- colwise(factor, levels = vUniqueVal, labels = vUniqueVal)(df)
  } else {
    vUniqueVal <- sort(unique(unlist(df)))
    df <- colwise(factor, levels = vUniqueVal, labels = letters[seq(1, length(vUniqueVal), 1)])(df)
  }
  
  vAlphabet <- levels(unlist(df))
  vTimeLabel <- colnames(df)
  vNbColInit <- length(vTimeLabel)
  
  # create flows
  lFlows <- list()
  for(i in 1:(vNbColInit - 1)){
    vFlows <- paste(paste(i, as.integer(df[ , i]), sep = "-"), paste(i + 1, as.integer(df[ , i + 1]), sep = "-"), sep = "_")
    lFlows[[length(lFlows) + 1]] <- vFlows
  }
  vConcatFlows <- unlist(lFlows)
  dFlows <- as.data.frame(table(vConcatFlows))
  colnames(dFlows) <- c("FLOWID", "FREQ")
  
  # split coordinates
  dFlows$FLOWID <- as.character(dFlows$FLOWID)
  dCoords <- colsplit(dFlows$FLOWID, pattern = "_", names = c("COORDORI", "COORDDES"))
  dCoordsOri <- colsplit(dCoords$COORDORI, pattern = "-", names = c("XORI", "YORI"))
  dCoordsDes <- colsplit(dCoords$COORDDES, pattern = "-", names = c("XDES", "YDES"))
  dFlows <- cbind(dFlows, dCoordsOri, dCoordsDes)
  
  # get min and max vales
  vScaleVal <- thickmin * max(dFlows$FREQ) / (threshold + 1)
  
  # set treshold fields
  dFlows$FREQBLACK <- ifelse(dFlows$FREQ > threshold, dFlows$FREQ, NA)
  dFlows$FREQGREY <- as.numeric(ifelse(dFlows$FREQ > threshold, NA, (threshold + 1)))
  dFlows$FREQTEXT <- ifelse(dFlows$FREQ > threshold, dFlows$FREQ, NA)

  dFlows$XTEXT <- apply(dFlows[ , c("XORI", "XDES")], 1, mean)
  dFlows$YTEXT <- apply(dFlows[ , c("YORI", "YDES")], 1, mean)
  dFlows$SLOPE <- with(dFlows, (YDES - YORI) / (XDES - XORI))
  dFlows$INTERCEPT <- with(dFlows, (YTEXT - SLOPE * XTEXT))
  dFlows$YTEXTCORR <- with(dFlows, ifelse(SLOPE == 0, YTEXT, 
                                          ifelse(SLOPE < 0, YTEXT - 0.1 * abs(SLOPE), YTEXT + 0.1 * abs(SLOPE))))
  dFlows$XTEXTCORR <- with(dFlows, ifelse(SLOPE == 0, XTEXT, 
                                          ifelse(SLOPE < 0, (YTEXTCORR - INTERCEPT) / SLOPE - 0.1, (YTEXTCORR - INTERCEPT) / SLOPE + 0.1)))
    
  
  # plot flows
  if(showfreq == TRUE){
    if(mask == FALSE){
      pSlidePlot <- ggplot(dFlows) + 
        geom_segment(aes(x = XORI, y = YORI, xend = XDES, yend = YDES, size = FREQGREY), colour = "lightgrey", lineend = "round") +
        geom_segment(aes(x = XORI, y = YORI, xend = XDES, yend = YDES, size = FREQBLACK), colour = "black", lineend = "round") +
        geom_text(aes(x = XTEXTCORR, y = YTEXTCORR, label = FREQTEXT), colour = "#CD3700", fontface = "bold") +
        scale_x_discrete("Timeline", labels = vTimeLabel) +
        scale_y_discrete("Categories", labels = vAlphabet) +
        scale_size_continuous("Frequencies", range = c(thickmin, vScaleVal)) +
        theme_bw() + theme(panel.grid.major = element_line(colour = NA), panel.grid.minor = element_line(colour = NA))
    }
    
    if(mask == TRUE){
      pSlidePlot <- ggplot(dFlows) + 
        geom_segment(aes(x = XORI, y = YORI, xend = XDES, yend = YDES, size = FREQGREY), colour = "white", lineend = "round") +
        geom_segment(aes(x = XORI, y = YORI, xend = XDES, yend = YDES, size = FREQBLACK), colour = "black", lineend = "round") +
        geom_text(aes(x = XTEXTCORR, y = YTEXTCORR, label = FREQTEXT), colour = "#CD3700", fontface = "bold") +
        scale_x_discrete("Timeline", labels = vTimeLabel) +
        scale_y_discrete("Categories", labels = vAlphabet) +
        scale_size_continuous("Frequencies", range = c(thickmin, vScaleVal)) +
        theme_bw() + theme(panel.grid.major = element_line(colour = NA), panel.grid.minor = element_line(colour = NA))
    }
  }
  
  if(showfreq == FALSE){
    if(mask == FALSE){
      pSlidePlot <- ggplot(dFlows) + 
        geom_segment(aes(x = XORI, y = YORI, xend = XDES, yend = YDES, size = FREQGREY), colour = "lightgrey", lineend = "round") +
        geom_segment(aes(x = XORI, y = YORI, xend = XDES, yend = YDES, size = FREQBLACK), colour = "black", lineend = "round") +
        scale_x_discrete("Timeline", labels = vTimeLabel) +
        scale_y_discrete("Categories", labels = vAlphabet) +
        scale_size_continuous("Frequencies", range = c(thickmin, vScaleVal)) +
        theme_bw() + theme(panel.grid.major = element_line(colour = NA), panel.grid.minor = element_line(colour = NA))
    }
    
    if(mask == TRUE){
      pSlidePlot <- ggplot(dFlows) + 
        geom_segment(aes(x = XORI, y = YORI, xend = XDES, yend = YDES, size = FREQGREY), colour = "white", lineend = "round") +
        geom_segment(aes(x = XORI, y = YORI, xend = XDES, yend = YDES, size = FREQBLACK), colour = "black", lineend = "round") +
        scale_x_discrete("Timeline", labels = vTimeLabel) +
        scale_y_discrete("Categories", labels = vAlphabet) +
        scale_size_continuous("Frequencies", range = c(thickmin, vScaleVal)) +
        theme_bw() + theme(panel.grid.major = element_line(colour = NA), panel.grid.minor = element_line(colour = NA))
    }
  }
  
  return(pSlidePlot)  
}


# TraMineR functions ----

# sequence definition

seqdefinition <- function(df){
  if(is.character(df[ , 1]) == TRUE){
    vUniqueVal <- sort(unique(unlist(df)))
    df <- colwise(factor, levels = vUniqueVal, labels = vUniqueVal)(df)
  } else {
    vUniqueVal <- sort(unique(unlist(df)))
    df <- colwise(factor, levels = vUniqueVal, labels = letters[seq(1, length(vUniqueVal), 1)])(df)
  }
  vAlphabet <- levels(unlist(df))
  vTimeLabel <- colnames(df)
  vNbColInit <- length(vTimeLabel)
  
  vColors <- brewer.pal(length(vAlphabet), "Set3")
  
  stsobject <- seqdef(df, seq(1, vNbColInit, 1), states = vAlphabet, labels = vAlphabet, cpal = vColors)
  return(stsobject)
  
}






