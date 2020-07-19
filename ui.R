####################################################
###### ui.R web app for DSSAT file generator #######
####################################################

library(shiny)
library(shinyjs)
shinyUI(
  pageWithSidebar(
  
  headerPanel("DSSAT File Generator"),   #1
  
  sidebarPanel(                  #2
    h5("Input data in .csv format"),
    h6("warning: This version does not accept gaps in data"),
          #checkboxInput(inputId = "datafile", label = "Load csv data", value = FALSE),
          #conditionalPanel(
          #  condition = "input.datafile",
          fileInput('dat', 'Choose data File',
                    accept=c('text/csv', 'text/comma-separated-values,text/plain', '.csv')),
          #),
    h6("First 100 rows of data will be shown below"),
    br(), 
    br(),
    br(),
    h6("            version 5th April 2020, Ebrahim Jahanshiri")
          ),
  mainPanel(
    useShinyjs(),
    
    tabsetPanel(
      tabPanel("Weather", 
    column(3,
           h3("Select variables"),
    htmlOutput("selectUIdatYear"),
    htmlOutput("selectUIdatDOY"),
    htmlOutput("selectUIdatTmax"),
    htmlOutput("selectUIdatTmin"),
    htmlOutput("selectUIdatRF"),
    htmlOutput("selectUIdatSolar"),
    #htmlOutput("SunHr")
    checkboxInput('SH', 'Sunshine hours?'),
    conditionalPanel(
      condition = "input.LAT <= 35 && input.SH == 1",
      h6("Angstrom-Prescott coeficients (for locations outside Europ)"),
      numericInput("A", "A", value = 0.29),
      numericInput("B", "B", value = 0.39)
    )
    ),
    column(4,
           h3("Model Parameters"),

          textInput("INSI", "INSI", "CZCH"),
          numericInput("LAT", "Latitude", value = 50),
          numericInput("LONG", "Longitude", value = 17),
          numericInput("ELEV", "Elevation", value = 319),
          numericInput("TAV", "TAV", value = 7.7),
          numericInput("AMP", "AMP", value = 23.6),
          numericInput("REFHT", "-REFHT", value = -99.0),
          numericInput("WNDHT", "-WNDHT", value = -99.0)
     ),
    conditionalPanel(
      "false", # always hide the download button
      downloadButton("downloadData", "Download")
    ),
    actionButton("do", "Generate"), 
    tableOutput('table')),
    tabPanel("Soil"),
    tabPanel("API") 
    )
                )))