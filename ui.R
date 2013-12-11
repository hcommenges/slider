##############################
# Shiny App: ITOU
# User interface
##############################

shinyUI(pageWithSidebar(
 headerPanel("SLIDER: Software for LongItudinal Data Exploration with R",
 tags$head(tags$link(rel="icon", type="image/png", href="favicon.png"),
tags$title("SLIDER: Software for LongItudinal Data Exploration with R"))
),
# headerPanel("",
 #               tags$head(
 #                 tags$link(rel="icon", type="image/png", href="favicon.png"),
 #                 tags$title("SLIDER: Software for LongItudinal Data Exploration with R"),
 #                 h1("SLIDER: Software for LongItudinal Data Exploration with R"),
 #               )
# ),

  sidebarPanel(
    actionButton(inputId='loadTestData', label="Load example data"),
    tags$hr(),
    
    # csv options
    checkboxInput("csvSettings", "CSV Options", FALSE),
    conditionalPanel(
      condition="input.csvSettings == true",
      checkboxInput("header", "Header", TRUE),
      
      radioButtons("sep", "Separator",
                   c(Comma=",",
                     Semicolon=";",
                     Tab="\t"),
                   "Comma"),
      
      radioButtons("quote", "Quote",
                   c(None="",
                     "Double Quote"='"',
                     "Single Quote"="'"),
                   "Double Quote")
    ),
    tags$hr(),
    # file input
    
    fileInput("file1", "Choose CSV File",
              accept=c("text/csv", "text/comma-separated-values,text/plain", ".csv")),
    
    tags$hr(),
    
    # sliders
    selectInput(inputId="timecol", label="Choose time steps", choices="", selected="", multiple=TRUE),
    selectInput(inputId="factcol1", label="Choose 1st factor", choices="", selected="", multiple=FALSE),
    uiOutput("slidermod1"),
    selectInput(inputId="factcol2", label="Choose 2nd factor", choices="", selected="", multiple=FALSE),
    uiOutput("slidermod2"),
    tags$hr(),
    downloadButton("downloadfct", "Download slide plot function")
  ),
  
  mainPanel(
    tabsetPanel(
      tabPanel("Data summary", 
               verbatimTextOutput("datasummary"),
               #tableOutput("contents")
               dataTableOutput('contents')
      ),
      
      tabPanel("Slide plot", 
               verbatimTextOutput("slidetext"),
               plotOutput("slideplot"),
               tags$hr(),
               uiOutput("sliderthreshold"),
               tags$hr(),
               checkboxInput(inputId="mask", label="Mask values under threshold", value=FALSE),
               checkboxInput(inputId="showfreq", label="Show frequencies", value=FALSE),
               tags$hr(), 
               sliderInput(inputId="thickmin", label="Minimal thickness", min=0, max=2, value=0.5, step=0.1),
               tags$hr(),
               downloadButton("downloadsp", "Download plot"),
               numericInput(inputId="widthslide", label="Width (cm)", value=20, min=1, max=30),
               numericInput(inputId="heightslide", label="Height (cm)", value=15, min=1, max=30)
      ),
      
      tabPanel("Parallel coordinates plot",
               verbatimTextOutput("pctext"),
               plotOutput("pcplot"),
               tags$hr(), 
               sliderInput(inputId="pccex", label="Squared symbol size", min=0, max=2, value=1, step=0.1),
               tags$hr(), 
               sliderInput(inputId="pclwd", label="Line width", min=0, max=2, value=1, step=0.1),
               tags$hr(), 
               sliderInput(inputId="pcgrid", label="Translation zone", min=0, max=1, value=0.5, step=0.1),
               tags$hr(),
               downloadButton("downloadpc", "Download plot"),
               tags$hr(),
               numericInput(inputId="widthseqpc", label="Width (cm)", value=20, min=1, max=30),
               numericInput(inputId="heightseqpc", label="Height (cm)", value=15, min=1, max=30)
      ),
      
      
      tabPanel("Transition rate",
               verbatimTextOutput("transratetext"),
               tableOutput("transrate")
      ),
      
      tabPanel("Index plot",
               verbatimTextOutput("indextext"),
               plotOutput("seqindex"),
               tags$hr(),
               selectInput(inputId="borderiplot", label="Draw border", choices=list("TRUE"=1, "FALSE"="NA")),
               tags$hr(),
               uiOutput("sliderseqi"),
               tags$hr(),
               downloadButton("downloadip", "Download plot"),
               tags$hr(),
               numericInput(inputId="widthseqi", label="Width (cm)", value=20, min=1, max=30),
               numericInput(inputId="heightseqi", label="Height (cm)", value=15, min=1, max=30)
      ),
      
      tabPanel("Frequency plot",
               verbatimTextOutput("freqtext"),
               plotOutput("seqfreq"),
               tags$hr(),
               selectInput(inputId="borderfplot", label="Draw border", choices=list("TRUE"=1, "FALSE"="NA")),
               tags$hr(),
               downloadButton("downloadfp", "Download plot"),
               tags$hr(),
               numericInput(inputId="widthseqf", label="Width (cm)", value=20, min=1, max=30),
               numericInput(inputId="heightseqf", label="Height (cm)", value=15, min=1, max=30)
      ),
      
      tabPanel("Distribution plot",
               verbatimTextOutput("distrtext"),
               plotOutput("seqdistr"),
               tags$hr(),
               selectInput(inputId="borderdplot", label="Draw border", choices=list("TRUE"=1, "FALSE"="NA")),
               tags$hr(),
               downloadButton("downloaddp", "Download plot"),
               tags$hr(),
               numericInput(inputId="widthseqd", label="Width (cm)", value=20, min=1, max=30),
               numericInput(inputId="heightseqd", label="Height (cm)", value=15, min=1, max=30)
      ),
      
      tabPanel("User guide", 
               includeMarkdown("README.md"))
    )
  )
))


