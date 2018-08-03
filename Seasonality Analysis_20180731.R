library(ggplot2)
library(forecast)
library(tseries)
library(readxl)

setwd('C:....')
daily_data = read_excel('.....xlsx', sheet = 1)
daily_data$Date = as.Date(daily_data$Date,"%Y/%m/%d")           # convert POSIXct datetime to date for forthcomming functions
daily_data[is.na(daily_data)] <- 0                              # fill na in all columns with 0

ggplot(daily_data, aes(Date, Amt)) + geom_line() + scale_x_date('month')  + ylab("Daily Cash Flow") +
  xlab("")

cashFlow_ts = ts(daily_data[, c('Amt')])
daily_data$clean_cashFlow = tsclean(cashFlow_ts)                # remove anomaly


ggplot() +
  geom_line(data = daily_data, aes(x = Date, y = clean_cashFlow)) + ylab('Cleaned Cash Flow')

# Investigate Seasonality
Acf(daily_data$clean_cashFlow, main='')

# decompostion
clean_cashFlow = ts(na.omit(daily_data$clean_cashFlow), frequency=7)
decomp = stl(clean_cashFlow, s.window="periodic")
deseasonal_cashFlow <- seasadj(decomp)
plot(decomp)


# Investigate remaining Seasonality
Acf(deseasonal_cashFlow, lag.max=45)

deseason_cashFlow = ts(deseasonal_cashFlow, frequency=30.5)
decomp1 = stl(deseason_cashFlow, s.window="periodic")
de_deseasonal_cashFlow <- seasadj(decomp1)
plot(decomp1)

Acf(de_deseasonal_cashFlow, lag.max=45)




deseason_cashFlow = ts(deseasonal_cashFlow, frequency=33)
decomp1 = stl(deseason_cashFlow, s.window="periodic")
de_deseasonal_cashFlow <- seasadj(decomp1)
plot(decomp1)

Acf(de_deseasonal_cashFlow, lag.max=45)


# Augmented Dickey-Fuller Test, test if the time-series is stationary. 
adf.test(clean_cashFlow, alternative = "stationary")  
    # ARIMA model can be used if it's stationary
    # The null hypo is the data is not stationary
    # p value is small here, means the null hypo can be rejected, i.e. it's stationary

adf.test(deseason_cashFlow, alternative = "stationary")  
    # p value is small here, means the null hypo can be rejected, i.e. it's stationary

adf.test(de_deseasonal_cashFlow, alternative = "stationary")  
    # p value is small here, means the null hypo can be rejected, i.e. it's stationary


# Timeseries forcasting
# a <- data.frame(Y = as.matrix(clean_cashFlow), x=time(clean_cashFlow)) # Converting time series data into dataframe
fit <- auto.arima(clean_cashFlow, seasonal=TRUE)
tsdisplay(residuals(fit), lag.max=7)  # plot model diagnostics

forecasted <- forecast(fit, h=14)
plot(forecasted)


