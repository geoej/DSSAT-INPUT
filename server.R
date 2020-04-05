#########################################################
###### server.R web app for DSSAT file generator  #######
###### A simiple proof of concept   #####################
###### application. Accept a CSV    #####################
###### file, ask user what are the  #####################
###### variables and generate DSSAT compaible files  ####
###### Current version only generates weather file ######
#########################################################

library(shiny)
library(sirad)
library(shinyjs)

# weather <- function(){
#   ## sorting out the data
#   
#   weather <- data()
#   #weather <- weather[complete.cases(weather),] # making sure we dont have NAs
#   wd <- data.frame(Year = data[[input$Year]],
#                    DOY = data[[input$DOY]],
#                    Tmax = data[[input$Tmax]],
#                    Tmin = data[[input$Tmin]],
#                    RF = data[[input$RF]],
#                    Solar = data[[input$Solar]]
#   )
#   
#   ## calculating the SRAD from SH
#   if ('sunshine checkbox is checked'){
#     # 'run the code for calculating the sunshine hours'
#     dat <- paste0(as.character(wd$Year), "-", sprintf("%03d",wd$DOY))
#     dat <- as.Date(dat, format = "%Y-%j")
#     
#     if ('the lat and long are outside the map'){
#       # 'run a simple regression for determining a and b'
#       wd$Solar <- ap(dat,as.numeric(LAT),
#                      as.numeric(LONG), extraT=NULL,A=NA,B=NA,wd$SH)
#       }
#   }
#   ## make 
#   # building the text file
#   
#   txt <- sprintf(paste0("*WEATHER DATA : MLSA\n",
#                         "\n@ INSI      LAT     LONG  ELEV   TAV   AMP REFHT WNDHT\n",
#                         "  %s    %3s  %3s   %1s  %1s  %1s %1s %1s\n",
#                         "@DATE  SRAD  TMAX  TMIN  RAIN"),INSI,      LAT,     LONG,  ELEV,   TAV,   AMP, REFHT, WNDHT)
#   
#   txt2 = character()
#   for (i in 1:nrow(wd)){
#     txt2<-  append(txt2,paste(paste0(do.call(paste0,as.list(strsplit(as.character(wd[i,"Year"]),"")[[1]][3:4])), as.character(sprintf("%03d", wd[i,"DOY"]))),
#                               sprintf("%4s",wd[i,"Solar"]), # if the data is sunshine hours, use wd[i,7]
#                               sprintf("%4s",wd[i,"Tmax"]),
#                               sprintf("%4s",wd[i,"Tmin"]),
#                               sprintf("%4s",wd[i,"RF"],"\n"), sep = "  "))
#   }
#   # writing the output
#   
#   # # Downloadable csv of selected dataset ----
#   # output$downloadData <- downloadHandler(
#   #   content = function(file) {
#   #     writeLines(c(txt,txt2), "outfile.WTH")
#   #   }
#   # )
#   
# 
#    
#   runjs("$('#downloadData')[0].click();")
#   
#   message("running code...")
#   return("some output")
#   
# }

shinyServer(function(input, output, session){
  
  data <- reactive({
   dat <- input$dat
   if (is.null(dat))
    return(NULL)
  read.csv(dat$datapath) # removed unneccesary arguments, header=input$header, sep=input$sep, quote=input$quote
  })
  output$table <- renderTable(head(data()))
  
  output$selectUIdatYear <- renderUI({ 
    if (is.null(data())) return(NULL)
    selectInput("Year", "Year", as.list(names(data())))
 
  })
  output$selectUIdatDOY <- renderUI({ 
    if (is.null(data())) return(NULL)
    
    selectInput("DOY", "DOY", as.list(names(data())), selected = as.list(names(data()))[2] )
   
  })
  output$selectUIdatTmax <- renderUI({ 
    if (is.null(data())) return(NULL)
    
    selectInput("Tmax", "Tmax", as.list(names(data())), selected = as.list(names(data()))[3] )
    
  })
  output$selectUIdatTmin <- renderUI({ 
    if (is.null(data())) return(NULL)
    
    selectInput("Tmin", "Tmin", as.list(names(data())), selected = as.list(names(data()))[4] )
    
  })
  output$selectUIdatRF <- renderUI({ 
    if (is.null(data())) return(NULL)
    
    selectInput("RF", "RF", as.list(names(data())), selected = as.list(names(data()))[5] )
    
  })
  output$selectUIdatSolar <- renderUI({ 
    if (is.null(data())) return(NULL)
    
    selectInput("Solar", "Solar", as.list(names(data())), selected = as.list(names(data()))[6] )
    
  })

  # core function

    vals <- reactiveValues()

    observeEvent(input$do, {
      
      
      weather <- function(){
        ## sorting out the data
        
        weather <- data()
        #weather <- weather[complete.cases(weather),] # making sure we dont have NAs
        wd <- data.frame(Year = weather[[input$Year]],
                         DOY = weather[[input$DOY]],
                         Tmax = weather[[input$Tmax]],
                         Tmin = weather[[input$Tmin]],
                         RF = weather[[input$RF]],
                         Solar = weather[[input$Solar]]
        )
        print(names(wd))
        ## calculating the SRAD from SH
        if (input$'SH'){
          # 'run the code for calculating the sunshine hours'
          dat <- paste0(as.character(wd$Year), "-", sprintf("%03d",wd$DOY))
          dat <- as.Date(dat, format = "%Y-%j")
          
          if (as.numeric(input$LAT) > 35){
            # This works only for EU
            wd$Solar <- ap(dat,as.numeric(input$LAT),
                           as.numeric(input$LONG), extraT=NULL,A=NA,B=NA,wd$Solar)
            
          }else {
            wd$Solar <- ap(dat,as.numeric(input$LAT), # !!!! Needs to be asked from the user
                           as.numeric(inputLONG), extraT=NULL,A=as.numeric(input$A),B=as.numeric(input$B),wd$Solar)
          }
        }
        ## make 
        # building the text file
        
        txt <- sprintf(paste0("*WEATHER DATA : ",input$INSI," \n",
                              "\n@ INSI      LAT     LONG  ELEV   TAV   AMP REFHT WNDHT\n",
                              "  %s%9.3f%9.3f%6s%6.1f%6.1f%6.1f%6.1f\n",
                              "@DATE  SRAD  TMAX  TMIN  RAIN"),
                       input$INSI,
                       input$LAT,
                       input$LONG,
                       input$ELEV,   
                       input$TAV,   
                       input$AMP,
                       input$REFHT, 
                       input$WNDHT)
        
        txt2 = character()
        
        withProgress(message = 'Generating data', value = 0,{
          # Number of times we'll go through the loop
          n <- 10 
          
        for (i in 1:nrow(wd)){
          txt2<-  
            append(txt2,
                   paste(
                     paste0(do.call(paste0,as.list(strsplit(as.character(wd[i,"Year"]),"")[[1]][3:4])), as.character(sprintf("%03d", wd[i,"DOY"]))),
                     as.character(sprintf("%6.1f%6.1f%6.1f%6.1f",wd[i,"Solar"],wd[i,"Tmax"],wd[i,"Tmin"],wd[i,"RF"]))
                   ,sep="")
            )
          incProgress(1/n, detail = paste("Row number", i))
          # Pause for 0.1 seconds to simulate a long computation.
          #Sys.sleep(0.1)
        }
        })
        # for (i in 1:nrow(wd)){
        #   txt2<-  append(txt2,paste(paste0(do.call(paste0,as.list(strsplit(as.character(wd[i,"Year"]),"")[[1]][3:4])), as.character(sprintf("%03d", wd[i,"DOY"]))),
        #                             sprintf("%4.1f",wd[i,"Solar"]), 
        #                             sprintf("%04.1f ",wd[i,"Tmax"]),
        #                             sprintf("%04.1f",wd[i,"Tmin"]),
        #                             sprintf("%4.1f",wd[i,"RF"],"\n"), sep = "  "))
        # }
        # writing the output
        
        writeLines(txt)
        #writeLines(txt2)
        vals$final <- c(txt,txt2)
         
        message("running code...")
      }
      
      vals$long_code_out <- weather()
      runjs("$('#downloadData')[0].click();")
      
      #showModal(modalDialog("calculation finished!"))
      # Downloadable csv of selected dataset ----
      
    })

    output$downloadData <- downloadHandler(
      filename = function() {
        paste(input$INSI, Sys.time(), ".WTH", sep="")
      },
      
      content = function(file) {
        #writeLines(c(txt,txt2), file)
        writeLines(vals$final, file)
        
        message("reached here")
      }
    )
    
    
  
  
  
  # output$ESDAPlots <- renderPlot({
  #   library(ggplot2)
  #   library(grid)
  #   library(gridExtra)
  #   
  #   if (is.null(data())) return(NULL)
  #   if (is.null(grid())) return(NULL)
  #   
  #   data <- data()
  #   grid <- grid()
  #   
  #   df <- data.frame(Xvar = data[[input$X]], Yvar = data[[input$Y]], Zvar = data[[input$Z]])
  #   
  #   plot <- ggplot(df, aes(Zvar)) 
  #   plot1 <- ggplot(df, aes(Zvar, Zvar))
  #   plot2 <- ggplot(df, aes(Xvar, Yvar, Zvar))
  #   
  #   theHisto <- plot + geom_bar(fill="white", colour="darkgreen")+ xlab(names(data[input$Z])) +
  #                               ggtitle(paste("Histogram for", (names(data[input$Z])), sep = " "))
  #   
  #   theScatter <- plot2 + geom_point(aes(alpha = Zvar))+ theme_bw()+
  #                         xlab("Easting") +ylab("Northing")+  labs(colour = names(data[input$Z])) +
  #                         ggtitle(paste("Scatterplot for", (names(data[input$Z])), "ppm",sep = " "))
  #   
  #   theBoxplot <- plot1 + geom_boxplot(notch = T, outlier.colour = "green", outlier.size = 3)+
  #                               xlab(names(data[input$Z])) + coord_flip() +
  #                               ggtitle(paste("Notch boxplot for", (names(data[input$Z])), sep = " "))
  #   
  #   
  #   theGraph <- grid.arrange(theHisto, theBoxplot, theScatter,ncol = 2)
  #   print(theGraph)                                                          
  # })
  # 
  # output$MapIdwE1 <- renderPlot({
  #  
  #          library(gstat)
  #          
  #          if (is.null(data())) return(NULL)
  #          if (is.null(grid())) return(NULL)
  #          data <- data()
  #          grid <- grid()
  #          # Preparing for interpolation
  #          coordinates(data) <- data[c(input$X, input$Y)]
  #          coordinates(grid) <- grid[c("x" , "y")]
  #          var <- data[[input$Z]]
  #          e1.idw <- krige(var ~ 1, data, grid)
  #          TheGraph <- spplot(e1.idw["var1.pred"], main = 
  #                               paste((names(data[input$Z]@data)) ,  "IDW Interpolated Map", sep =" "))
  #          print(TheGraph)
  #   })
  # 
  # output$MapOKE1 <- renderPlot({
  #   # if (inFile1 & inFile2) {
  #   library(gstat)
  #   library(grid)
  #   library(ggplot2)
  #   library(gridExtra)
  #   
  #   if (is.null(data())) return(NULL)
  #   if (is.null(grid())) return(NULL)
  #   data <- data()
  #   grid <- grid()
  #   # Preparing for interpolation
  #   coordinates(data) <- data[c(input$X, input$Y)]
  #   coordinates(grid) <- grid[c("x" , "y")]
  #   var <- data[[input$Z]]
  #   vgm <- variogram(var~1,data)
  #   fit <- fit.variogram(vgm,model=vgm(1, "Sph", 900, 1))
  #   kriged <- krige(var~1, data, grid, model=fit)
  #   
  #   df <- as.data.frame(kriged)
  #   br <- seq(min(df$var1.pred), max(df$var1.pred), len=8)
  #   
  #   theMap <- ggplot(data=df) + theme_bw()+
  #     geom_tile(aes(x, y, fill=var1.pred)) + 
  #     scale_fill_gradient(low="red", high="green", 
  #                         breaks=br, labels=sprintf("%.02f", br), 
  #                         guide=guide_colorbar(title=NULL, nbin=100, barheight=unit(0.75, "npc"), label.hjust=1)) + 
  #     scale_x_continuous(expand=c(0,0)) + 
  #     scale_y_continuous(expand=c(0,0)) + 
  #     ggtitle(paste("Kriging map for", names(data[input$Z]), sep= " "))
  #   
  #   theVario <- plot(vgm, fit, 
  #                    main= paste("Omnidirectional variogram for", names(data[input$Z]), "ppm", sep= " "))
  #   
  #   theGraph <- grid.arrange(theMap, theVario,ncol = 2)
  #   
  #   print(theGraph)   
  # 
  # })
})