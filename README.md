# Covid-19-Time-Series-Modelling
#Time series modelling
> library(lubridate) # Simplifies Date and Time manipulation
> library(forecast) # Analyzing and Forecasting 
> 
> # Create sample COVID-19 cases data
> cases <- c(580, 7813, 28266, 59287, 75700,
+            87820, 95314, 126214, 218843, 471497,
+            936851, 1508725, 2072113)
> 
> # Convert to time series object
> covid_ts <- ts(cases, 
+                start = decimal_date(ymd("2020-01-22")), 
+                frequency = 365.25 / 7)
> #Check the ACF and PACF plots
> Acf(diff(covid_ts)) # ACF plot
> Pacf(diff(covid_ts)) # PACF plot
> # Plot the time series
> plot.ts(covid_ts, xlab = "Weekly Data", ylab = "Total Positive Cases",
+         main = "COVID-19 Pandemic")
> # Create Simple moving average model
> ma_model <- ma(covid_ts, order = 3, centre = FALSE)
> #Forecasting 5 steps ahead
> forecast_ma <- forecast(ma_model, h = 5)
Warning message:
In ets(object, model = "ZZN", lambda = lambda, biasadj = biasadj,  :
  Missing values encountered. Using longest contiguous portion of time series
> print(forecast_ma) # display the forecast
         Point Forecast      Lo 80   Hi 80      Lo 95    Hi 95
2020.287        1505843  -103131.6 3114817  -954871.2  3966557
2020.307        1505843 -1135420.3 4147106 -2533620.5  5545306
2020.326        1505843 -2290602.9 5302288 -4300319.8  7312005
2020.345        1505843 -3692060.0 6703746 -6443663.6  9455349
2020.364        1505843 -5450010.4 8461696 -9132216.6 12143902
> #Simple Exponential smoothing 
> ses_model <- HoltWinters(covid_ts, beta = FALSE, gamma = FALSE)
> # Double Exponential Smoothing (Holt's method)
> des_model <- HoltWinters(covid_ts, gamma = FALSE)
> print(des_model) # results
Holt-Winters exponential smoothing with trend and without seasonal component.

Call:
HoltWinters(x = covid_ts, gamma = FALSE)

Smoothing parameters:
 alpha: 1
 beta : 1
 gamma: FALSE

Coefficients:
     [,1]
a 2072113
b  563388
> # Triple Exponential Smoothing (Holt-Winters)
> # Automatically chooses the right exponential smoothing model
> tes_model <- ets(covid_ts)
> # Forecast 5 steps ahead
> forecast_tes <- forecast(tes_model, h = 5)
> # Plot
> plot(forecast_tes)
> # Automatic ARIMA selection
> auto_arima_model <- auto.arima(covid_ts)
> # Manual ARIMA specification,
> #AR1, differencing and MA1 with MAximum likelihood estimate
> manual_arima <- Arima(covid_ts, order = c(1,1,1),method='ML')
> print(manual_arima) # Arima result
Series: covid_ts 
ARIMA(1,1,1) 

Coefficients:
         ar1     ma1
      0.9601  1.0000
s.e.  0.0519  0.2592

sigma^2 = 2.912e+09:  log likelihood = -149.88
AIC=305.75   AICc=308.75   BIC=307.21
> # Plot the time series
> plot.ts(covid_ts, 
+         xlab = "Weekly Data", 
+         ylab = "Total Positive Cases",
+         main = "COVID-19 Pandemic (Weekly Cases Over Time)")
> 
> # Forecast 5 steps ahead
> forecast_arima <- forecast(manual_arima, h = 5)
> 
> # Plot ARIMA forecast
> plot(forecast_arima,
+      main = "ARIMA(1,1,1) Forecast of COVID-19 Cases",
+      xlab = "Weekly Data",
+      ylab = "Predicted Cases")
> 
