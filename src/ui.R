library(shinyjs)
library(DT)
library(shinythemes)
library(dygraphs)

# Close shiny app window
jscode <- "shinyjs.closeWindow = function() { window.close(); }"

ui = fluidPage(
  
  titlePanel(h2('Time series forecasting')),
  theme = shinythemes::shinytheme('simplex'),
  sidebarLayout(
    sidebarPanel(
      actionButton('random_ts_selection',"Select time series at random"),
      selectInput('forecasting_method', 'Forecasting method', choices=c('Naive','Seasonal Naive',
                                                                        'Random Walk with Drift',
                                                                        'Mean','ETS','ARIMA',
                                                                        'Neural Net'),
                  selected='ETS'),
      sliderInput('forecasting_horizon', '\nForecasting horizon', min=1, max=12, value=1),
      radioButtons('confidence_level', 'Confidence level', 
                   choices = list("80" = 80, "90" = 90, "95" = 95), selected = 90),
      actionButton('forecast', 'Forecast!')
    ),
    mainPanel(
      tabsetPanel(
        tabPanel('Predictions plot', dygraphOutput('time_series_predictions')),
        tabPanel('Residuals plot', dygraphOutput('time_series_residuals'))
        
      ),  
      tabsetPanel(
        tabPanel('Forecasts and prediction intervals table', DT::DTOutput('predictions_table')),
        tabPanel('Prediction accuracy table', DT::DTOutput('accuracy_table'))
      )
    )
  ),
  
  # Close shiny app window
  useShinyjs(),
  extendShinyjs(text = jscode, functions = c("closeWindow")),
  actionButton("close", "Close window")
)