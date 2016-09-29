##############################
# Shiny App: SLIDER - Software for LongItudinal Data Exploration with R
# User interface
##############################

shinyUI(fluidPage(
  theme = shinytheme("united"),
  titlePanel("SLIDER: Software for LongItudinal Data Exploration with R",
             tags$head(tags$link(rel = "icon", type = "image/png", href = "favicon.png"),
                       tags$title("SLIDER: Software for LongItudinal Data Exploration with R"))
  ),
  
  tabsetPanel(
    tabPanel("User guide", 
             fluidRow(
               column(2),
               column(7, includeMarkdown("README_app.md")),
               column(3))),
    
    tabPanel("Data summary",
             fluidRow(
               column(3, wellPanel(
                 tags$h4("Upload example data"),
                 actionButton(inputId = 'loadTestData', label = "Load example data"),
                 tags$h4("Upload your own data"),
                 checkboxInput("csvSettings", "CSV Options", FALSE),
                 conditionalPanel(
                   condition = "input.csvSettings == true",
                   checkboxInput("header", "Header", TRUE),
                   
                   radioButtons("sep", "Separator",
                                c(Comma = ",",
                                  Semicolon = ";",
                                  Tab = "\t"),
                                ","),
                   
                   radioButtons("quote", "Quote",
                                c(None = "",
                                  "Double Quote" = '"',
                                  "Single Quote" = "'"),
                                "")
                 ),
                 fileInput("fileInput", "Choose CSV File",
                           accept=c("text/csv", "text/comma-separated-values,text/plain", ".csv")),
                 
                 # sliders
                 tags$h4("Select variables"),
                 selectInput(inputId = "timecol", 
                             label = "Choose time steps (mandatory)", 
                             choices = "", 
                             selected = "", 
                             multiple = TRUE, 
                             selectize = TRUE),
                 selectInput(inputId = "weightcol", 
                             label = "Choose weighting variable (optional)", 
                             choices = "",
                             selected = "", 
                             multiple = FALSE, 
                             selectize = TRUE),
                 selectInput(inputId = "factcol1", 
                             label = "Choose 1st factor (optional)", 
                             choices = "",
                             selected = "", 
                             multiple = FALSE, 
                             selectize = TRUE),
                 uiOutput("selectmod1"),
                 selectInput(inputId = "factcol2", label = "Choose 2nd factor (optional)",
                             choices = "",
                             selected = "", 
                             multiple = FALSE, 
                             selectize = TRUE),
                 uiOutput("selectmod2")
               )),
               column(9, 
                      verbatimTextOutput("datasummary"),
                      dataTableOutput("contents")))),
    
    tabPanel("Transition rate",
             fluidRow(
               column(3, wellPanel(
                 tags$h4("Absolute or relative frequency"),
                 radioButtons(inputId = "transparameter", label = "Choose content:",
                              c("Absolute frequencies" = "absfreq",
                                "Row percentages" = "rowpct",
                                "Column percentages" = "colpct"),
                              selected = "absfreq")
               )),
               column(9, 
                      verbatimTextOutput("transratetext"),
                      tableOutput("transrate")))),
    
    tabPanel("Distribution plot",
             fluidRow(
               column(3, wellPanel(
                 tags$h4("Set graphical parameters"), 
                 checkboxInput(inputId = "borderdplot", label = "Draw borders", value = FALSE),
                 tags$h4("Download your plot"),
                 downloadButton("downloaddp", "Download plot"),
                 numericInput(inputId = "widthseqd", label = "Width (cm)", value = 20, min = 1, max = 30),
                 numericInput(inputId = "heightseqd", label = "Height (cm)", value = 15, min = 1, max = 30)
               )),
               column(9, 
                      verbatimTextOutput("distrtext"),
                      plotOutput("seqdistr", width = "100%", height = "600px")))),
    
    tabPanel("Index plot",
             fluidRow(
               column(3, wellPanel(
                 tags$h4("Set graphical parameters"), 
                 checkboxInput(inputId = "borderiplot", label = "Draw borders", value = FALSE),
                 sliderInput(inputId = "sliderseqi", label = "Index of sequences", min = 1, max = 50, value = c(1, 10) , step = 1),
                 tags$h4("Download your plot"),
                 downloadButton("downloadip", "Download plot"),
                 numericInput(inputId = "widthseqi", label = "Width (cm)", value = 20, min = 1, max = 30),
                 numericInput(inputId = "heightseqi", label = "Height (cm)", value = 15, min = 1, max = 30)
               )),
               column(9, 
                      verbatimTextOutput("indextext"),
                      plotOutput("seqindex", width = "100%", height = "600px")))),
    
    tabPanel("Frequency plot",
             fluidRow(
               column(3, wellPanel(
                 tags$h4("Set graphical parameters"), 
                 checkboxInput(inputId = "borderfplot", label = "Draw borders", value = FALSE),
                 tags$h4("Download your plot"),
                 downloadButton("downloadfp", "Download plot"),
                 numericInput(inputId = "widthseqf", label = "Width (cm)", value = 20, min = 1, max = 30),
                 numericInput(inputId = "heightseqf", label = "Height (cm)", value = 15, min = 1, max = 30)
               )),
               column(9, 
                      verbatimTextOutput("freqtext"),
                      plotOutput("seqfreq", width = "100%", height = "600px")))),
    
    tabPanel("Parallel coordinates plot",
             fluidRow(
               column(3, wellPanel(
                 tags$h4("Set graphical parameters"), 
                 sliderInput(inputId = "pccex", label = "Squared symbol size", min = 0, max = 2, value = 1, step = 0.1),
                 sliderInput(inputId = "pclwd", label = "Line width", min = 0, max = 2, value = 1, step = 0.1),
                 sliderInput(inputId = "pcgrid", label = "Translation zone", min = 0, max = 1, value = 0.5, step = 0.1),
                 tags$h4("Download your plot"),
                 downloadButton("downloadpc", "Download plot"),
                 numericInput(inputId = "widthseqpc", label = "Width (cm)", value = 20, min = 1, max = 30),
                 numericInput(inputId = "heightseqpc", label = "Height (cm)", value = 15, min = 1, max = 30)
               )),
               column(9, 
                      verbatimTextOutput("pctext"),
                      plotOutput("pcplot", width = "100%", height = "600px")))),
    
    tabPanel("Slide plot",
             fluidRow(
               column(3, wellPanel(
                 tags$h4("Set graphical parameters"), 
                 sliderInput(inputId = "sliderthreshold", label = "Threshold", min = 0, max = 10, value = 1, step = 1),
                 sliderInput(inputId = "thickmin", label = "Minimal thickness", min = 0, max = 2, value = 0.5, step = 0.1),
                 checkboxInput(inputId = "mask", label = "Mask values under threshold", value = FALSE),
                 checkboxInput(inputId = "showfreq", label = "Show frequencies", value = FALSE),
                 tags$h4("Download your plot"),
                 downloadButton("downloadsp", "Download plot"),
                 numericInput(inputId = "widthslide", label = "Width (cm)", value = 20, min = 1, max = 30),
                 numericInput(inputId = "heightslide", label = "Height (cm)", value = 15, min = 1, max = 30)
               )),
               column(9, 
                      verbatimTextOutput("slidetext"),
                      plotOutput("slideplot", width = "100%", height = "600px"))))
  )
)
)


