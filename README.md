# Covid-19-Time-Series-Modelling
#Time series modelling
library(lubridate) # Simplifies Date and Time manipulation
library(forecast) # Analyzing and Forecasting 

# Create sample COVID-19 cases data
cases <- c(580, 7813, 28266, 59287, 75700,
           87820, 95314, 126214, 218843, 471497,
           936851, 1508725, 2072113)

# Convert to time series object
covid_ts <- ts(cases, 
               start = decimal_date(ymd("2020-01-22")), 
               frequency = 365.25 / 7)
#Check the ACF and PACF plots
Acf(diff(covid_ts)) # ACF plot
Pacf(diff(covid_ts)) # PACF plot
# Plot the time series
plot.ts(covid_ts, xlab = "Weekly Data", ylab = "Total Positive Cases",
        main = "COVID-19 Pandemic")
# Create Simple moving average model
ma_model <- ma(covid_ts, order = 3, centre = FALSE)
#Forecasting 5 steps ahead
forecast_ma <- forecast(ma_model, h = 5)
print(forecast_ma) # display the forecast
#Simple Exponential smoothing 
ses_model <- HoltWinters(covid_ts, beta = FALSE, gamma = FALSE)
# Double Exponential Smoothing (Holt's method)
des_model <- HoltWinters(covid_ts, gamma = FALSE)
print(des_model) # results
# Triple Exponential Smoothing (Holt-Winters)
# Automatically chooses the right exponential smoothing model
tes_model <- ets(covid_ts)
# Forecast 5 steps ahead
forecast_tes <- forecast(tes_model, h = 5)
# Plot
plot(forecast_tes)
# Automatic ARIMA selection
auto_arima_model <- auto.arima(covid_ts)
# Manual ARIMA specification,
#AR1, differencing and MA1 with MAximum likelihood estimate
manual_arima <- Arima(covid_ts, order = c(1,1,1),method='ML')
print(manual_arima) # Arima result
# Plot the time series
plot.ts(covid_ts, 
        xlab = "Weekly Data", 
        ylab = "Total Positive Cases",
        main = "COVID-19 Pandemic (Weekly Cases Over Time)")

# Forecast 5 steps ahead
forecast_arima <- forecast(manual_arima, h = 5)

# Plot ARIMA forecast
plot(forecast_arima,
     main = "ARIMA(1,1,1) Forecast of COVID-19 Cases",
     xlab = "Weekly Data",
     ylab = "Predicted Cases")
