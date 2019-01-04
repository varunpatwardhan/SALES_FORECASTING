require('shiny')
require('dplyr')
require('data.table')
require('dygraphs')
require('prophet')
require('data.table')
require('tidyr')
data <-
  read.csv(
    'Sales by Store - Department.csv',
    sep = ",",
    header = T,
    strip.white = T,
    na.strings = c("NA", "NaN", "", "?")
  )
ui <-  fluidPage(
  titlePanel("SALES FORECAST"),
  
  fluidRow(
    sidebarPanel(
      selectInput(
        inputId = "unit" ,
        label =  "Enter the Store Number here",
        choices = sort(unique(data$Store)),
        selected = NULL
      )
      
      ,
      actionButton(inputId = "go", label = "Get Departments")
      
      ,
      selectInput(
        inputId = "dept" ,
        label =  "Department:",
        choices = NULL,
        selected = NULL
      )
      ,
      dateRangeInput(
        inputId = "daterange",
        label = "Pick the date range for training data",
        start  = '2015-10-01',
        end  = Sys.Date()
      )
      
      ,
      actionButton(inputId = "go2", label = "Forecast")
    )
  )
  ,
  fluidRow(dygraphOutput("hist")
           
           ,
           plotOutput("comp"))
  
)
server <- function(input, output, session) {
  observeEvent(input$go, {
    print('Updating Departments and date ranges')
    df <- data[data$Store == input$unit, ]
    df <- df %>%
      mutate(Date = as.Date(Date)) %>%
      complete(Date = seq.Date(min(Date), max(Date), by =
                                 "day"))
    updateSelectInput(
      session = session,
      inputId = "dept",
      label = "Department:",
      choices = c('ALL DEPARTMENTS', sort(df$Dept)),
      selected = sort(df$Dept)[1]
    )
    updateDateRangeInput(
      session = session,
      inputId = "daterange",
      start = min(df$Date),
      end = max(df$Date)
    )
    
    
  })
  
  
  
  
  
  model <- eventReactive(ignoreInit = T , input$go2,
                         {
                           #print('I am working')
                           #print(dim(df))
                           #print(input$unit)
                           #print(input$dept)
                           #df <- read.csv('Sales by Store - Department.csv',sep=",", header=T, strip.white = T, na.strings = c("NA","NaN","","?"))
                           df <- data[data$Store == input$unit, ]
                           
                           
                           if (input$dept == 'ALL DEPARTMENTS') {
                             df <- df %>%
                               group_by(Store, Date, IsHoliday) %>%
                               summarise_at(vars(Weekly_Sales), funs(sum)) %>% ungroup()
                             # df %>%
                             #   group_by(Store,Date, IsHoliday) %>%
                             #   summarise_at(vars(Weekly_Sales),funs(sum))
                           }
                           else{
                             df = df[df$Dept == input$dept, ]
                           }
                           df <- df %>%
                             mutate(Date = as.Date(Date)) %>%
                             complete(Date = seq.Date(min(Date), max(Date), by =
                                                        "day"))
                           df <- df[df$Date >= input$daterange[1], ]
                           df <- df[df$Date <= input$daterange[2], ]
                           
                           setnames(
                             df,
                             old = c('Weekly_Sales', 'Date', 'IsHoliday'),
                             new = c('y', 'ds', 'holiday')
                           )
                           print(dim(df))
                           m <- prophet()
                           
                           #m <- add_country_holidays(m, country_name = 'US') # added in prophet 0.4
                           m <- add_regressor(m, 'holiday')
                           m <- fit.prophet(m, df)
                           future <-
                             make_future_dataframe(m, periods = 52, freq = 'week')
                           future$holiday <-
                             is.element(week(future$ds), c(47, 52, 6, 36)) # Holiday week numbers for next 52 weeks calculated
                           forecast <- predict(m, future)
                           
                           z = list()
                           z$m <- m
                           z$forecast <- forecast
                           z
                           
                         })
  output$hist <- renderDygraph({
    #tail(forecast[c('ds', 'yhat', 'yhat_lower', 'yhat_upper')])
    #plot(2,3)
    #prophet_plot_components(m, forecast)
    #prophet_plot_components(m, forecast)
    #plot(m, forecast)
    dyplot.prophet(model()$m, model()$forecast)
    
  })
  
  output$comp <- renderPlot({
    prophet_plot_components(model()$m, model()$forecast)
  })
}





app <- shinyApp(server = server, ui = ui)
#runApp(app, host = '0.0.0.0',port=3045)
