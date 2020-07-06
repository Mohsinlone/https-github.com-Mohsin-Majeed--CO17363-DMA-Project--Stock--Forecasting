
library(shiny)
library(shinydashboard)
library(ggplot2)
library(tseries)
library(forecast)
library(magrittr)
library(timeDate)
library(dplyr)
library(zoo)
header <- dashboardHeader(title = "Basic Dashboard")
sidebar <- dashboardSidebar(
    sidebarMenu(
        menuItem("Dashboard", tabName = "dashboard", icon = icon("dashboard")),
        selectInput("ohlcv", "Select a column:",
                    choices = c('select','open','high','low','close','volume'),
                    selected = 'select'),
        
        selectInput("graphs", "Select a graph:",
                    choices = c('Select','TimeSeries','DecomposedSeries','DifferencedSeries','HoltWinter'),
                    selected = 'Select'),
        selectInput("acfpcfval", "Select VALUES:",
                    choices = c('Select','ACF','PACF'),
                    selected = 'Select'),
        numericInput("pval", "P", value=""),
        numericInput("dval", "D", value=""),
        numericInput("qval", "Q", value=""),
        selectInput("arima", "Select a model:",
                    choices = c('Select','ARIMA','Auto Arima'),
                    selected = 'Select'),
        
        numericInput("ndays", "Number of days to forecast:", value=0),
        
        selectInput("forecast", "Select to Forecast:",
                    choices = c('Select','Forecast-Plot','Forecast-Values'), 
                    selected = 'Select'),
        
        selectInput('holtwinterforecast','select year:',
                    choices = c('select','1','2','3','4','5'),
                    selected='select')
        
        ))

frow1 <- fluidRow(
    valueBoxOutput("value1"),
    valueBoxOutput("value2"),
    valueBoxOutput("value3")
)
frow2 <- fluidRow( 
    box(
        title = "Diffrents Graphs"
        ,status = "primary"
        ,solidHeader = TRUE 
        ,collapsible = TRUE, 
        tabBox(id="firsttab",title = "GRAPHS",width = 100,
               div(style = 'height:50vh ;overflow-x:auto;overflow-y:auto',
                   tabPanel(plotOutput("graphs"),title="Graphs"))
        )),
    
    box(
      title = "ACF/PACF Graphs"
      ,status = "primary"
      ,solidHeader = TRUE 
      ,collapsible = TRUE,
      div(
        tabBox(
          id = "acfval",
          side = "right",
          title = "Graphs",
          width = 100,
          tabPanel(id="acff",plotOutput("calval"),title="ACF Graph"),
          tabPanel(id="pacff",plotOutput("calvall"),title="PACF Graph")
        ))),
    box(
        title = "Arima model Values"
        ,status = "primary"
        ,solidHeader = TRUE 
        ,collapsible = TRUE, 
        verbatimTextOutput('arimaPlot'),
        verbatimTextOutput('arimaPlott')
    ),
    box(
      title = "Forecasting Graphs Graphs"
      ,status = "primary"
      ,solidHeader = TRUE 
      ,collapsible = TRUE,
      div(
        tabBox(
          id = "acfval",
          side = "right",
          title = "Graphs",
          width = 100,
          tabPanel(id="acff",plotOutput("forecastPlot"),title="Arima Graph"),
          tabPanel(id="pacff",plotOutput("fautoarimaaa"),title="Auto Arima Graph")
        )))
)
recommendation <- read.csv('/Users/jaswinder/Desktop/ashokstock.csv',stringsAsFactors = F,header=T)
body <- dashboardBody(frow1,frow2)
ui <- dashboardPage(title = 'This is my Page title', header, sidebar,body,skin="blue") 

server <- function(input, output) {
  
  maxopen=max(openn)
  minopen=min(openn)
  maxclose=max(closee)
  minclose=min(closee)
  output$value1 <- renderValueBox({
    valueBox(
      formatC(maxopen, format="d", big.mark=',',width = 1)
      ,paste("Max ?"," Min Open :",minopen)
      ,icon = icon("stats",lib='glyphicon')
      ,color = "purple")  
  })
  output$value2 <- renderValueBox({
    valueBox(
      formatC(maxclose, format="d", big.mark=',',width = 1)
      ,paste("Max ?"," Min Close :",minclose)
      ,icon = icon("gbp",lib='glyphicon')
      ,color = "green")  
  })
  output$value3 <- renderValueBox({
    valueBox(
      formatC(paste(round(recommendation$high[263]),"-",round(recommendation$low[263])), format="d", big.mark=',',width = 1)
      ,paste('Last week High and Low Value')
      ,icon = icon("menu-hamburger",lib='glyphicon')
      ,color = "yellow")   
  })
    output$graphs <- renderPlot ({ 
        
        if(input$graphs == 'TimeSeries'){
          openn=c(recommendation$open)
          tym=ts(openn,start = c(2014,7), end =c(2019,7) , frequency = 7)
          plot.ts(tym, main = "Time-Series plot", col = "blue")
          }
         else if(input$graphs == 'DecomposedSeries'){
            stldecomp = stl(tym, s.window="periodic")
            plot(stldecomp)
        } else if(input$graphs == 'DifferencedSeries'){
            #plot.ts(differncedSeries,col = "blue")
        }else if(input$graphs=='HoltWinter')
        {
          if(input$holtwinterforecast=='1')
          {
            a_ts_hw1=HoltWinters(tym,beta = F,gamma= F)
            a_ts_hw_fcst1= forecast:::forecast.HoltWinters(a_ts_hw1,h=1)
            plot(a_ts_hw_fcst1)
          }else if(input$holtwinterforecast=='2')
          {
            a_ts_hw2=HoltWinters(tym,beta = F,gamma= F)
            a_ts_hw_fcst2= forecast:::forecast.HoltWinters(a_ts_hw2,h=2)
            plot(a_ts_hw_fcst2) 
          }else if(input$holtwinterforecast=='3')
          {
            a_ts_hw3=HoltWinters(tym,beta = F,gamma= F)
            a_ts_hw_fcst3= forecast:::forecast.HoltWinters(a_ts_hw3,h=3)
            plot(a_ts_hw_fcst3)
          }else if(input$holtwinterforecast=='4')
          {
            a_ts_hw4=HoltWinters(tym,beta = F,gamma= F)
            a_ts_hw_fcst4= forecast:::forecast.HoltWinters(a_ts_hw4,h=4)
            plot(a_ts_hw_fcst4)
          }else if(input$holtwinterforecast=='5')
          {
            a_ts_hw5=HoltWinters(tym,beta = F,gamma= F)
            a_ts_hw_fcst5= forecast:::forecast.HoltWinters(a_ts_hw5,h=5)
            plot(a_ts_hw_fcst5)
          }}
  
    })
    
    output$calval <- renderPlot({
      if(input$acfpcfval=='ACF'){
        acff=acf(tym,lag.max = 20)
      }
      
      
    })
    output$calvall <- renderPlot({ 
      if(input$acfpcfval=='PACF'){
        pacff=pacf(tym,lag.max = 20)
      }
      
    })
    output$value <- renderText({ input$pval })
    output$value <- renderText({ input$qval })
    output$value <- renderText({ input$dval })
    
    output$arimaPlot <- renderText({ 
      if(input$arima == 'ARIMA'){
        amv <<- arimafunc()
        paste("AIC:",amv$aic," AICc:",amv$aicc," BIC:",amv$bic)
      }
      else if(input$arima == 'Auto Arima'){
        am <<- autoarimaa()
        paste("AIC:",am$aic," AICc:",am$aicc," BIC:",am$bic)
      }
    })
    output$arimaPlott <- renderText({ 
       if(input$arima == 'Auto Arima'){
        am <<- autoarimaa()
        paste("AIC:",am$aic," AICc:",am$aicc," BIC:",am$bic)
      }
    })
    arimafunc <- function(){
      M_arima <- Arima(tym, order=c(input$pval,input$dval,input$qval))
      return(M_arima)
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
    output$fautoarimaaa <- renderPlot({
      if(input$forecast == 'Forecast-Plot'){
        if(input$ndays == 0){
          error()
        } else {
          af <<- forecastfun()
          plot(af,col = "darkgreen")
        }
      }
    })
    
    forecastfunc <- function(){
      Mforecasts <- forecast(amv, h = input$ndays)
      return(Mforecasts)
    }
    forecastfun <- function(){
      forcast <- forecast(am, h = input$ndays)
      return(forcast)
    }
    
    autoarimaa <- function(){
      aarima <- auto.arima(tym)
      return(aarima)
      
    }
    
    getColOpt <- function(cv){
        if(cv == 'select'){
            print("Choose a column")
        } else if(cv == 'open'){
            return("open")
        } else if(cv == 'high'){
            return("high")
        } else if(cv == 'low'){
            return("low")
        } else if(cv == 'close'){
            return("close")
        } else if(cv == 'volume'){
            return("volume")
        }
    }
    # output$tp <- function(){
    #   datee=c(recommendation$date)
    #   op=c(recommendation$open)
    #   val=lm(op ~ datee)
    #   a<-data.frame(date="2024-07-19")
    #   res=as.integer(predict.lm(val,a,data=recommendation))
    #   return(res)
    #   
    # }
    
}



shinyApp(ui = ui, server = server)

