
### TASK 3.2 VISUALIZE ENERGY DATA

#install.packages("ggfortify")
install.packages("forecast")

library(RMySQL) # MySQL for R
library(dplyr) # Basic data manipulation
library(ggplot2) # Data visualization
library(lubridate) # To work with dates and times
library(plotly) # Interactive graphs in R
library(ggfortify) #ggplot extension
library(forecast) # Forecast

## STEP 0. LOAD THE DATA
con = dbConnect(
  MySQL(),
  user = 'deepAnalytics',
  password = 'Sqltask1234!',
  dbname = 'dataanalytics2018',
  host = 'data-analytics-2018.cbrosir2cswx.us-east-1.rds.amazonaws.com'
)

# power_2006 <- dbGetQuery(con, "SELECT * FROM yr_2006") # excluded because it's incomplete (Task 3.1)
power_2007 <- dbGetQuery(con, "SELECT * FROM yr_2007")
power_2008 <- dbGetQuery(con, "SELECT * FROM yr_2008")
power_2009 <- dbGetQuery(con, "SELECT * FROM yr_2009")
#power_2010 <- dbGetQuery(con, "SELECT * FROM yr_2010") # excluded because it's incomplete (Task 3.1)

# 1.STEP 1. VISUALIZE THE DATA

plot(power_df$Sub_metering_1) 
      # Plots all the data of sub-meter 1 (note:takes time to plot).Unuseful to visualize.

power_df <-  bind_rows(power_2007, power_2008, 
      power_2009) # "bind_rows()": combine the 3 dataframes

power_df <-cbind(power_df,paste(power_df$Date,power_df$Time),
      stringsAsFactors=FALSE) 
      # "cbind" combines columns, like concatenate in excel. 

colnames(power_df)[11] <-"DateTime" 
      # "colnames" change name of Datetimecolumn; 
      # checked by double clicking variable in global environment.

power_df <- power_df[,c(ncol(power_df), 
      1:(ncol(power_df)-1))] # moving column to the first column for convenience

power_df$DateTime <- as.POSIXct(power_df$DateTime, 
      tz="Europe/Paris", format="%Y-%m-%d %H:%M:%S")
attr(power_df$DateTime, "tzone") <- "Europe/Paris"
      #POSIXct class is the usual choice for storing dates in R. "tz"= timezone

power_df$year <- year(power_df$DateTime) # year column created at the end right, for analysis
power_df$quarter <- quarter(power_df$DateTime) # quarter column created at the end right, for analysis
power_df$month <- month(power_df$DateTime) # month column created at the end right, for analysis
power_df$week <- week(power_df$DateTime) # week column created at the end right, for analysis
power_df$weekday <- wday(power_df$DateTime, week_start=1) 
  # quarter column created at the end right, for analysis; redefinition of weekday starter.
power_df$day <- day(power_df$DateTime) # day column created at the end right, for analysis.
power_df$hour <- hour(power_df$DateTime) # hour column created at the end right, for analysis.
power_df$minute <- minute(power_df$DateTime) # minute column created at the end , for analysis.

houseWeek <- filter(power_df, year == 2008 & week == 2)
  # create variable "houseWeek" which extracts week 2 from 2008. "filter()" function lubridate.
plot(houseWeek$Sub_metering_1) 
  # plot from this dataframe the submetering_1 (kitchen); over 10,000 observations; no patterns.

houseDay <- filter(power_df, year == 2009 & month == 1 & day == 25 ) # plot of 1 day submeter
plot(houseDay$Sub_metering_2)

plot_ly(houseDay, x = ~houseDay$Time, y = ~houseDay$Sub_metering_2, # plot scatter line
      name = 'Laundry', type = 'scatter', mode = 'lines', color = "orange") %>%
  layout(title = "Power Consumption Laundry January 25Th, 2009",
      xaxis = list(title = "Time"),
      yaxis = list (title = "Power (watt-hours)"))

houseDay30 <- filter(power_df, year == 2009 & month == 1 & day == 25 & # new variable; redefine 10min range
      (minute == 0 | minute == 30 ))

plot_ly(houseDay30, x = ~houseDay30$DateTime, y = ~houseDay30$Sub_metering_1, 
        name = 'Kitchen', type = 'scatter', mode = 'lines') %>%
  add_trace(y = ~houseDay30$Sub_metering_2, name = 'Laundry Room', mode = 'lines') %>%
  add_trace(y = ~houseDay30$Sub_metering_3, name = 'Water Heater & AC', mode = 'lines') %>%
  layout(title = "Total Power Consumption January 25Th, 2009",
         xaxis = list(title = "Time"),
         yaxis = list (title = "Power (watt-hours)"))

houseWeek4 <- power_df %>%
  filter(year == 2009 & week == 20 & (hour == 0 | hour == 4| hour == 6
  | hour == 8|hour == 12| hour == 16| hour == 20))
plot_ly(houseWeek4, x = ~houseWeek4$DateTime, y = ~houseWeek4$Sub_metering_1, 
        name = 'Kitchen', type = 'scatter', mode = 'lines') %>%
  add_trace(y = ~houseWeek4$Sub_metering_2, name = 'Laundry Room', mode = 'lines') %>%
  add_trace(y = ~houseWeek4$Sub_metering_3, name = 'Water Heater & AC', mode = 'lines') %>%
  layout(title = "Power Consumption Week 20 May 14 to 20th, 2009",
         xaxis = list(title = "Time"),
         yaxis = list (title = "Power (watt-hours)"))

## Subset April 2009 - 6 hour frequency
houseMonth12 <- power_df %>%
  filter(year == 2009 & month == 12 & (hour == 0 | hour == 12))

## Plot sub-meter 1, 2 and 3 with title, legend and labels - 3 hour frequency
plot_ly(houseMonth12, x = ~houseMonth12$DateTime, y = ~houseMonth12$Sub_metering_1, 
  name = 'Kitchen', type = 'scatter', mode = 'lines') %>%
  add_trace(y = ~houseMonth12$Sub_metering_2, name = 'Laundry Room', mode = 'lines') %>%
  add_trace(y = ~houseMonth12$Sub_metering_3, name = 'Water Heater & AC', mode = 'lines') %>%
  layout(title = "Power Consumption for December, 2009",
         xaxis = list(title = "Time"),
         yaxis = list (title = "Power (watt-hours)"))

# STEP 2. PREPARE TO ANALYZE THE DATA

house070809weekly <- filter(power_df, weekday == 3 & hour == 13 & minute == 1)
    # In random day of the week, hour and minute, extract the data
tsSM1_070809weekly <- ts(house070809weekly$Sub_metering_1, frequency=52, 
    start=c(2007,1)) # definition of variable with data
autoplot(tsSM1_070809weekly, colour = 'blue', xlab = "Time", ylab = "Watt-Hours", 
    main = "Kitchen", lwd=1) # plot in consistency with the rest of the graphs

tsSM2_070809weekly <- ts(house070809weekly$Sub_metering_2, frequency=52, 
                           start=c(2007,1)) # definition of variable with data
autoplot(tsSM2_070809weekly, colour = 'orange', xlab = "Time", ylab = "Watt-Hours", 
           main = "Laundry", lwd=1) 

tsSM3_070809weekly <- ts(house070809weekly$Sub_metering_3, frequency=52, 
                         start=c(2007,1)) # definition of variable with data
autoplot(tsSM3_070809weekly, colour = 'dark green', xlab = "Time", ylab = "Watt-Hours", 
         main = "Water Heater & AC", lwd=1) 

# STEP 3. FORECASTING A TIME SERIES
fitSM1 <- tslm(tsSM1_070809weekly ~ trend + season) 
# tslm() fit linear regresion models to time series including trend and seasonality.
summary(fitSM1) # summary of predictions and performance metrics
forecastfitSM1c <- forecast(fitSM1, h=20, level=c(80,90)) 
# h: height, level, define confidence levels
plot(forecastfitSM1c, ylim = c(0, 20), ylab= "Watt-Hours", 
     xlab="Time", main = "Kitchen")

fitSM2 <- tslm(tsSM2_070809weekly ~ trend + season) # application to submeter 2
summary(fitSM2)
forecastfitSM2c <- forecast(fitSM2, h=20, level=c(80,90)) 
plot(forecastfitSM2c, ylim = c(0, 20), ylab= "Watt-Hours", 
     xlab="Time", main = "Laundry")

fitSM3 <- tslm(tsSM3_070809weekly ~ trend + season) # application to submeter 3
summary(fitSM3)
forecastfitSM3c <- forecast(fitSM3, h=20, level=c(80,90)) 
plot(forecastfitSM3c, ylim = c(0, 20), ylab= "Watt-Hours", 
     xlab="Time", main = "Water Heater A/C")

# STEP 4. DECOMPOSING SEASONAL TIME SERIES

components070809SM1weekly <- decompose(tsSM1_070809weekly) 
  # decompose(): R decomposes a time series in seasonality, trend and irregular components
autoplot(components070809SM1weekly, main="Kitchen: Decomposition of additive time series")
print("Seasonal") ## Summary statistics for decomposed
summary(components070809SM1weekly$seasonal)
print("Trend")
summary(components070809SM1weekly$trend)
print("Random")
summary(components070809SM1weekly$random)

components070809SM2weekly <- decompose(tsSM2_070809weekly) # Decomposition for submeter 2
autoplot(components070809SM2weekly, main="Laundry: Decomposition of additive time series")
print("Seasonal")
summary(components070809SM2weekly$seasonal)
print("Trend")
summary(components070809SM2weekly$trend)
print("Random")
summary(components070809SM2weekly$random)

components070809SM3weekly <- decompose(tsSM3_070809weekly) # Decomposition for submeter 3
autoplot(components070809SM3weekly, main="Water heater & A/C: Decomposition of additive time series")
print("Seasonal")
summary(components070809SM3weekly$seasonal)
print("Trend")
summary(components070809SM3weekly$trend)
print("Random")
summary(components070809SM3weekly$random)

# STEP 5. HOLT-WINTERS FORECASTING

tsSM1_070809Adjusted <- tsSM1_070809weekly - components070809SM1weekly$seasonal # excl.seasonality
tsSM1_HW070809 <- HoltWinters(tsSM1_070809Adjusted, beta=FALSE, gamma=FALSE) # smoothing
tsSM1_HW070809for <- forecast(tsSM1_HW070809, h=25)
plot(tsSM1_HW070809for, ylim = c(0, 20), ylab= "Watt-Hours", xlab="Time", 
    main="Kitchen Forecast Holt-Winters")
tsSM1_HW070809forC <- forecast(tsSM1_HW070809, h=25, level=c(10,25)) # w/lower confidence levels
plot(tsSM1_HW070809forC, ylim = c(0, 9), ylab= "Watt-Hours", # only forecast plotted
     xlab="Time", start(2010), main = "Kitchen Forecast Holt-Winters")

 ## Submeter 2
tsSM2_070809Adjusted <- tsSM2_070809weekly - components070809SM2weekly$seasonal 
tsSM2_HW070809 <- HoltWinters(tsSM2_070809Adjusted, beta=FALSE, gamma=FALSE)
tsSM2_HW070809for <- forecast(tsSM2_HW070809, h=25)
plot(tsSM2_HW070809for, ylim = c(0, 20), ylab= "Watt-Hours", xlab="Time", 
     main="Laundry Forecast Holt-Winters")

tsSM2_HW070809forC <- forecast(tsSM2_HW070809, h=25, level=c(10,25)) # w/lower confidence levels
plot(tsSM2_HW070809forC, ylim = c(0, 9), ylab= "Watt-Hours", # only forecast plotted
     xlab="Time", start(2010), main = "Laundry Forecast Holt-Winters")

## Submeter 3
tsSM3_070809Adjusted <- tsSM3_070809weekly - components070809SM3weekly$seasonal 
tsSM3_HW070809 <- HoltWinters(tsSM3_070809Adjusted, beta=FALSE, gamma=FALSE)
tsSM3_HW070809for <- forecast(tsSM3_HW070809, h=25)
plot(tsSM3_HW070809for, ylim = c(0, 20), ylab= "Watt-Hours", xlab="Time", 
     main="Water-Heater A/C Forecast Holt-Winters")

tsSM3_HW070809forC <- forecast(tsSM3_HW070809, h=25, level=c(10,25)) # w/lower confidence levels
plot(tsSM3_HW070809forC, ylim = c(0, 9), ylab= "Watt-Hours", # only forecast plotted
     xlab="Time", start(2010), main = "Water-Heater A/C Forecast Holt-Winters")

