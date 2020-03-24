####################################################
###### ui.R web app for DSSAT file generator #######
####################################################

library(shiny)
shinyUI(pageWithSidebar(
        
  headerPanel("DSSAT File Generator"),   #1
  
  sidebarPanel(                  #2
    h5("Input data in .csv format"),
    h6("warning: This version does not accept gaps in data"),
          checkboxInput(inputId = "datafile", label = "Load csv data", value = FALSE),
          conditionalPanel(
            condition = "input.datafile",
          fileInput('dat', 'Choose data File',
                    accept=c('text/csv', 'text/comma-separated-values,text/plain', '.csv'))),
    br(), 
    br(),
    br(),
    h6("            2020, Ebrahim Jahanshiri")
          ),
  mainPanel(
    tabsetPanel(
      tabPanel("Weather"),
      tabPanel("Soil"),
      tabPanel("API") 
        ), 
    column(3,
           h3("Select variables"),
    htmlOutput("selectUIdatYear"),
    htmlOutput("selectUIdatDOY"),
    htmlOutput("selectUIdatTmax"),
    htmlOutput("selectUIdatTmin"),
    htmlOutput("selectUIdatRF"),
    htmlOutput("selectUIdatSolar"),
    #htmlOutput("SunHr")
    checkboxInput('SH', 'Sunshine hours?')
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
          numericInput("WNDHT", "-WNDHT", value = -99.0),
          
          actionButton("do", "Generate")
    ),
    downloadLink("downloadData", "Download")
            )))