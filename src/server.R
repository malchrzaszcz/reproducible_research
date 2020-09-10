library(shiny)
library(dplyr)
library(forecast)
library(DT)
library(dygraphs)
library(Mcomp)


server = function(input, output, session) {
  
  # Predictions table
  output$predictions_table <- renderDT({
    data.frame(predicted()[[1]],method = input$forecasting_method) %>% tail(isolate(input$forecasting_horizon))
  })
  
  # Accuracy table
  output$accuracy_table <- renderDT({
    horizon = isolate(input$forecasting_horizon)
    df = predicted()[[1]] %>% as.data.frame() %>% tail(horizon)
    df = df[,c("actuals","point_forecast")]
    df$error = df$actuals - df$point_forecast
    rmse = sqrt(mean((df$error)^2))
    se = sum(df$error)
    sae = sum(abs(df$error))
    mae = mean(abs(df$error))
    method = input$forecasting_method
    accuracy = data.frame(round(cbind(se,sae,rmse,mae,horizon),3),method)
    names(accuracy) = c("Sum of errors", "Sum of absolute errors", 
                        "Root mean squared error", "Mean absolute error", 
                        "Horizon","Method")
    accuracy
  })
  
  # Select time series at random
  random_item <- eventReactive(input$random_ts_selection, {
    
    random_ts_index = sample(1:length(M3),1)
    random_ts = c(M3[[random_ts_index]]$x,M3[[random_ts_index]]$xx)
    ts(random_ts)
  })
  
  # Make predictions 
  predicted <- eventReactive(input$forecast, {
    
    time_series_1 = random_item()
    level = as.numeric(input$confidence_level)
    horizon=input$forecasting_horizon
    time_series_1_cut = time_series_1 %>% tail(-horizon)
    
    if ('ETS' %in% input$forecasting_method) {
      forecast <- ets(time_series_1_cut) %>% forecast(h=horizon, level=level)   
    } else if ('ARIMA' %in% input$forecasting_method) {
      forecast <- auto.arima(time_series_1_cut) %>% forecast(h=horizon, level=level)   
    } else if ('Naive' %in% input$forecasting_method) {
      forecast <- naive(time_series_1_cut, h=horizon, level=level)
    } else if ('Seasonal Naive' %in% input$forecasting_method) {
      forecast <- snaive(time_series_1_cut, h=horizon, level=level)
    } else if ('Random Walk with Drift' %in% input$forecasting_method) {
      forecast <- rwf(time_series_1_cut, h=horizon,  level=level, drift=TRUE)
    } else if ('Mean' %in% input$forecasting_method) {
      forecast <- meanf(time_series_1_cut, h=horizon, level=level)
    } else if ('Neural Net' %in% input$forecasting_method) {
      forecast <- nnetar(time_series_1_cut) %>% forecast(h=horizon, PI=TRUE, level=level)
    }
    
    actuals <- ts(as.vector(time_series_1_cut), start=1)
    point_forecast <- ts(as.vector(forecast$mean),start=length(actuals)-horizon+1)
    lower <- ts(data.frame(forecast)[,2],start=length(actuals)-horizon+1)
    upper <- ts(data.frame(forecast)[,3],start=length(actuals)-horizon+1)
    residuals <- ts(as.vector(forecast$res), start=1)
    forecast_dygraphs <- cbind(lower, actuals, point_forecast, upper)
    list(round(forecast_dygraphs,3), residuals)
  })
  
  # Plot predictions 
  output$time_series_predictions <- renderDygraph({
    
    dygraph(predicted()[[1]], main = paste('Forecast for', 
                                           isolate(input$forecasting_horizon),
                                           'period(s) produced with', isolate(input$forecasting_method))) %>% 
      dySeries(name = "actuals", label = "Actual") %>%
      dySeries(c("lower","point_forecast","upper"), label = "Predicted") %>% 
      dyOptions(includeZero = TRUE, 
                axisLineColor = "navy", 
                gridLineColor = "lightblue")
  })
  
  # Plot residuals 
  output$time_series_residuals <- renderDygraph({
    
    dygraph(predicted()[[2]], main = paste('Residuals from', input$forecasting_method)) %>% 
      dySeries(label = "Residuals") %>%
      dyOptions(includeZero = TRUE, 
                axisLineColor = "navy", 
                gridLineColor = "lightblue")
  })
  
  # Close shiny app window
  observeEvent(input$close, {
    js$closeWindow()
    stopApp()
  })
}