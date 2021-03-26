#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library("shiny")
library("shinythemes")
library("forecast")
library("tsoutliers")
library("forecast")
library("tseries")
library("readxl")
library("tidyverse")
library("readr")



# Define UI for application that draws a histogram
ui <- navbarPage(theme= shinytheme("united"),
                 "Time Series app",
                 id = "form",
                 # First Panel
                 tabPanel("Instructions",
                          fluidRow(
                              column(10,
                                     includeMarkdown("instructions.Rmd")
                                     )
                              
                          )
                     
                 ),
                 # Second Panel
                 tabPanel("Plot, ACF and PACF",
                          fluidPage(
                              sidebarLayout(
                                  sidebarPanel(
                                      shinyjs::useShinyjs(),
                                      div(id = "myapp",
                                      #fileInput(inputId = "filedata",
                                      #          label = "Upload data. Choose csv file",
                                      #          accept = c(".csv")),
                                      radioButtons(
                                          "fileType_Input",
                                          label = h4("Choose File type"),
                                          choices = list(".csv/txt" = 1, ".xlsx" = 2),
                                          selected = 1,
                                          inline = TRUE
                                      ),
                                      fileInput(
                                          'file1',
                                          h4('Upload Items List'),
                                          accept = c(
                                              'text/csv',
                                              'text/comma-separated-values,text/plain',
                                              '.csv',
                                              '.xlsx'
                                          ),
                                      ),
                                      numericInput("init","Choose the beggining", 1900, min = 1, max = 2030),
                                      sliderInput("month", "What month is the first one? (If applies)", 1, min=1,max=12),
                                      sliderInput("day", "What month is the first day? (If applies)", 1, min=1,max=31),
                                      numericInput("freq","Choose the frequency", 1, min = 1,max = 12),
                                      selectInput("pl", "Select a plot:",
                                                  choices = c('Select','Actual','Differenced','log','log_and_diff', "second_diff","log_and_second_diff"),
                                                  selected = 'Select'),
                                      selectInput("acfp", "Select a correlation function:",
                                                  choices = c('Select','PACF-Actual','PACF-Differenced','ACF-Actual','ACF-Differenced',"ACF-Sec-Diff","PACF-Sec-Diff"),
                                                  selected = 'Select'),
                                      numericInput("lags","Choose the lags", 1, min = 1, max =48),
                                      
                                        )),  
                                      mainPanel(
                                          tags$style(type="text/css",
                                                     ".shiny-output-error { visibility: hidden; }",
                                                     ".shiny-output-error:before { visibility: hidden; }"
                                          ),
                                    
                                          actionButton("reset", "Reset all"),
                                          plotOutput(outputId = "plot1"),
                                          plotOutput(outputId = "plot2"),
                                      )
                                  )
                              )
                          ),
                 tabPanel("Estimation and Diagnostics",
                          tabsetPanel(
                              tabPanel("Estimation with Outliers", value=0,verbatimTextOutput("Summary")),
                              tabPanel("Estimation without Outliers", value=1,verbatimTextOutput("Summaryy")),
                              tabPanel("Residual Diagnostics", value=2,
                                       radioButtons("radio", 
                                                    label = HTML('<FONT color="red"><FONT size="5pt">Output Selection</FONT></FONT><br> <b>Choose a diagnostic</b>'),
                                                    choices = list("Estimation vs. Original Plot" = 1, "Plot of Residuals" = 2, "ACF of Residuals" = 3, "PACF of Residuals" = 4, "QQ Plot of Residuals" = 5, "Jarque-Bera Test" = 6, "Ljung- Box Test" = 7),
                                                    selected = 1),      
                                       plotOutput("diag"),
                                       fluidRow(
                                           column(10,
                                                  verbatimTextOutput("test"),
                                           )),
                              )
                              ),

                          ),
                              
                              tabPanel("Series Decomposition", value=3, plotOutput("Structure")), 

                                       
                tabPanel("Forecast",
                                  fluidPage(
                                      sidebarLayout(
                                          sidebarPanel(
                                              # Input: Enter P,D,Q value ----
                                             numericInput("pval", "p", value=""),
                                             numericInput("dval", "d", value=""),
                                             numericInput("qval", "q", value=""),
                                             numericInput("Pval", "P", value=""),
                                             numericInput("Dval", "D", value=""),
                                             numericInput("Qval", "Q", value=""),
                                             numericInput("ndays","Number of periods to forecast",1, min = 1, max = 200),
                                             # Horizontal line ----
                                             tags$hr(),
                                             # Input: Forecast ----
                                             selectInput("forecast", "Select to Forecast:",
                                                         choices = c('Select','Forecast-Plot','Forecast-Values'), 
                                                         selected = 'Select')
                                          ),  
                                          mainPanel(
                                              plotOutput("forecastPlot"),
                                              tableOutput('forecastTable')
                                          ),
                                      )
                                 
                                  )
                         
                         ))

    
                        

# Define server logic required to draw a histogram
server <- function(input, output) {

    #Read the csv
    
    #data <- reactive({read.csv(input$filedata$datapath)})
    
##########################################
  # intento de excel y csv
    
      data <- reactive({
    inFile <- input$file1
        
        if (is.null(inFile)) {
            return(NULL) }
        
        if (input$fileType_Input == "1") {
            read.csv(inFile$datapath)
        } else {
            read_excel(inFile$datapath)
        }
    })
###############################################################
        
   series <- reactive({
       serie <- ts(data(), start=c(year=input$init, month=input$month, day=input$day), frequency = input$freq)
       
   })

    output$plot1 <- renderPlot({
        differncedSeries <<- diff(series(), differences=1)
        logseries <<- log(series())
        secdiff <<- diff(series(),differences=2)
        
        
        if(input$pl == 'Actual'){
            plot.ts(series(),main="Time Series Plot")
        } else if(input$pl == 'Differenced'){
            plot.ts(differncedSeries,col = "blue", main="Dif Time Series")
        } else if(input$pl == 'log'){
           plot.ts(logseries,col = "blue",main=)
        } else if(input$pl == 'log_and_diff'){
            plot.ts(log(differncedSeries),col = "blue")
        }
        else if(input$pl == 'second_diff'){
            plot.ts((secdiff),col = "blue")
        }
        else if(input$pl == 'log_and_second_diff'){
            plot.ts(log(secdiff),col = "blue")
        }
    })
    
    output$plot2 <- renderPlot({
        if(input$acfp == 'PACF-Actual'){
            pacf(series(), lag.max=input$lags)
        } else if(input$acfp == 'PACF-Differenced'){
            pacf(differncedSeries, lag.max=input$lags)
        } else if(input$acfp == 'ACF-Actual'){
            acf(series(), lag.max=input$lags)
        } else if(input$acfp == 'ACF-Differenced'){
            pacf(differncedSeries, lag.max=input$lags)
        } else if(input$acfp == 'ACF-Sec-Diff'){
            pacf(secdiff, lag.max=input$lags)
        } else if(input$acfp == 'PACF-Sec-Diff'){
            pacf(secdiff, lag.max=input$lags)
        }
    })

    
    
    output$Summary <- renderPrint({
        tso(series(),cval=3.6,types=c("AO","LS","TC"))
    })
    
    output$Summaryy <- renderPrint({
        auto.arima(series())
    })
    
    

    output$Filtered_Table <- renderPlot({
        dataComp <<- decompose(series())
        plot(dataComp)
            })

    
    arimafunc <- function(){
        M_arima <- Arima(series(), order=c(input$pval,input$dval,input$qval),
                         seasonal = list(order = c(input$Pval,input$Dval,input$Qval)))
        return(M_arima)
    }
    
    forecastfunc <- function(){
        amv <<- arimafunc()
        Mforecasts <- forecast(amv, h = input$ndays)
        return(Mforecasts)
    }
    
    
    output$forecastPlot <- renderPlot({
        if(input$forecast == 'Forecast-Plot'){
            if(input$ndays == 0){
                error()
            } else {
                fc <<- forecastfunc()
                plot(fc, col = "darkgreen")
            }
        }
    })
    
    output$forecastTable <- renderTable({
        if(input$forecast == 'Forecast-Values'){
            if(input$ndays == 0){
                error()
            } else {
                yy <- fc$mean
                cd <- data.frame(ForecastedPrice = c(yy))
                return(cd)
            }
        }
    })
    
  
    output$test <- renderPrint({
        f <- auto.arima(series())
        resid<-f$residuals
    if (input$radio == 6){
        jarque.bera.test(resid)
    }
        else if (input$radio == 7){
        Box.test(resid,type="Ljung-Box")
    }
    })

    output$diag <- renderPlot({
        f <- auto.arima(series())
        resid<-f$residuals
        if(input$radio == 1){
            yest<-series()-resid
            plot.ts(series())
            lines(yest,col="red")
        }
        else if  (input$radio == 2){
            par(mfrow=c(2,1))
            plot(resid)
        }
        else if ( input$radio == 3){
            acf(resid,lag=36)
        }
        else if ( input$radio == 4){
            pacf(resid,lag=36)
            
        }
        else if ( input$radio == 5){
        par(mfrow=c(1,1))
        qqnorm(resid)
        qqline(resid)
        }

    })
    
    output$Structure <- renderPlot({
        if (input$freq == 1){
        showNotification("You can not see this decomposition, as your data is annual",duration=NULL,closeButton = FALSE)
        }
        else {
        birthsComp <- decompose(series())
        plot(birthsComp)
        }
        
    })
    
    observeEvent(input$reset, {
        shinyjs::reset("myapp")
    })    
    
    

}

# Run the application 
shinyApp(ui = ui, server = server)
