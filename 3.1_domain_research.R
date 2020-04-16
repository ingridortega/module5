# install.packages("RMySQL")
# install.packages("hrbrthemes")
# install.packages("extrafontdb", "Rttf2pt1")
# install.packages("Rttf2pt1")

library(RMySQL)
library(dplyr)
library(ggplot2)
library(lubridate)
library(hrbrthemes)
library(viridis)

## Create a database connection
con = dbConnect(
  MySQL(),
  user = 'deepAnalytics',
  password = 'Sqltask1234!',
  dbname = 'dataanalytics2018',
  host = 'data-analytics-2018.cbrosir2cswx.us-east-1.rds.amazonaws.com'
)

## List the tables contained in the database
dbListTables(con)

## Lists attributes contained in a table
dbListFields(con, 'yr_2010')

## Use asterisk to specify all attributes for download
power_2006 <- dbGetQuery(con, "SELECT * FROM yr_2006")
power_2007 <- dbGetQuery(con, "SELECT * FROM yr_2007")
power_2008 <- dbGetQuery(con, "SELECT * FROM yr_2008")
power_2009 <- dbGetQuery(con, "SELECT * FROM yr_2009")
power_2010 <- dbGetQuery(con, "SELECT * FROM yr_2010")

str(power_2006)
str(power_2007)
str(power_2008)
str(power_2009)
str(power_2010)

summary(power_2006)
summary(power_2007)
summary(power_2008)
summary(power_2009)
summary(power_2010)

dim(power_2006) # it has too few records: aprox 22k records
dim(power_2007) # approx 521k records
dim(power_2008) # approx 526k records
dim(power_2009) # approx 521k records
dim(power_2010) # approx 457k records, missing around 60k records.

head(power_2006)
tail(power_2006)

head(power_2007)
tail(power_2007)

head(power_2009)
tail(power_2009)

# Fix datatypes
power_2006$Date <- ymd(power_2006$Date)
#power_2006$Time <- hms(power_2006$Time)
str(power_2006)
glimpse(power_2006)

power_2007$Date <- ymd(power_2007$Date)
power_2008$Date <- ymd(power_2008$Date)
power_2009$Date <- ymd(power_2009$Date)
power_2010$Date <- ymd(power_2010$Date)

summary(power_2006$Date)
summary(power_2007$Date)
summary(power_2008$Date)
summary(power_2009$Date)
summary(power_2010$Date)

## 2006 only has the last 2 weeks of December
## 2007, 2008, 2009 have the whole year it seems
## 2010 has the first 11 months almost complete. Missing from Nov-27 forward

summary(power_2006$Sub_metering_1)
summary(power_2007$Sub_metering_1)
summary(power_2008$Sub_metering_1)
summary(power_2009$Sub_metering_1)
summary(power_2010$Sub_metering_1)


summary(power_2006$Sub_metering_2)
summary(power_2007$Sub_metering_2)
summary(power_2008$Sub_metering_2)
summary(power_2009$Sub_metering_2)
summary(power_2010$Sub_metering_2)

summary(power_2006$Sub_metering_3)
summary(power_2007$Sub_metering_3)
summary(power_2008$Sub_metering_3)
summary(power_2009$Sub_metering_3)
summary(power_2010$Sub_metering_3)

## Check for NA values
anyNA(power_2006)
anyNA(power_2007)
anyNA(power_2008)
anyNA(power_2009)
anyNA(power_2010)

# Merge datasets
power_all <-
  bind_rows(power_2006, power_2007, power_2008, power_2009, power_2010)

glimpse(power_all)
str(power_all)
# Fix time datatype (it has to be done after the merge)
power_all$Time <- hms(power_all$Time)

# Create a year column
power_all <- power_all %>%
  arrange(Date, Time) %>%
  mutate(year = year(Date))

# Create a month column
power_all <- power_all %>%
  mutate(month = month(Date), week_day = wday(Date, label = TRUE))

# Create hour and miunte columns
power_all <- power_all %>%
  mutate(hour = hour(Time), minute = minute(Time))


# Convert year and month to factor columns.
# power_all$year <- factor(power_all$year, levels = c(2006:2010), ordered = T)
# power_all$month <- factor(power_all$month, levels = c(1:12), ordered = T)

str(power_all)

sum_year <- power_all %>%
  filter(year > 2006) %>%
  group_by(year, week_day) %>%
  summarize(
    median_gap = median(Global_active_power),
    total_gap = sum(Global_active_power)
  )

sum_year

ggplot(sum_year, aes(y = total_gap, x = week_day, color = week_day)) +
  geom_boxplot()

by_hour <- power_all %>%
  filter(year > 2006) %>%
  group_by(week_day, hour) %>%
  summarize(
    median_gap = median(Global_active_power),
    total_gap = sum(Global_active_power)
  )

ggplot(by_hour,
       aes(
         y = week_day,
         x = hour,
         size = total_gap,
         color = total_gap
       )) +
  geom_point() +
  scale_color_gradient(low = "yellow", high = "red")

########### Submeters behavior ###########
sub_meters <- power_all %>%
  filter(year > 2006) %>%
  group_by(year) %>%
  mutate(
    Sub_metering_rest = (Global_active_power * 1000 / 60) - Sub_metering_1 - Sub_metering_2 - Sub_metering_3
  ) %>%
  summarize(
    totalMetering_1 = sum(Sub_metering_1),
    totalMetering_2 = sum(Sub_metering_2),
    totalMetering_3 = sum(Sub_metering_3),
    totalMetering_rest = sum(Sub_metering_rest)
  )
sub_meters

# Consumption per year
ggplot(sub_meters, aes(x = year)) +
  geom_line(aes(y = totalMetering_1), color = "red") +
  geom_line(aes(y = totalMetering_2), color = "orange") +
  geom_line(aes(y = totalMetering_3), color = "gray") +
  geom_line(aes(y = totalMetering_rest), color = "black")

###########

## Consumption of A/C, Heater per month ###
## energy sub-metering No. 3 
ac_per_month <- power_all %>%
  group_by(month) %>%
  summarize(Total_Submeter_3 = sum(Sub_metering_3))

ac_per_month$month_factor <- factor(ac_per_month$month, levels = c(1:12), ordered = T)

ggplot(ac_per_month, aes(x=month_factor, y=Total_Submeter_3)) +
  geom_bar(stat="identity", color="yellow", fill="gray") + 
  scale_x_discrete(name ="Month", limits=c(1:12)) +
  scale_fill_viridis(discrete = TRUE) +
  theme(legend.position="none") +
  theme_ipsum()  +
  theme(legend.position="none") +
  ggtitle("Monthly Power-Consumption Submeter3 (WaterHeater, A/C)")


## Consuption ofhe kitchen, containing mainly a dishwasher, 
## an oven and a microwave (hot plates are not electric but gas powered).  
## energy sub-metering No. 1 
kitchen_per_month <- power_all %>%
  group_by(month) %>%
  summarize(Total_Submeter_1 = sum(Sub_metering_1))

kitchen_per_month$month_factor <- factor(kitchen_per_month$month, levels = c(1:12), ordered = T)

ggplot(kitchen_per_month, aes(x=month_factor, y=Total_Submeter_1)) +
  geom_bar(stat="identity", color="yellow", fill="red") + 
  scale_x_discrete(name ="Month", limits=c(1:12)) +
  labs(y="Total power consumed")+
  scale_fill_viridis(discrete = TRUE) +
  theme(legend.position="none") +
  theme_ipsum()  +
  theme(legend.position="none") +
  ggtitle("Monthly Power-Consumption Submeter1 (Kitchen)")


## Consuption of the laundry room, containing a washing-machine,
## a tumble-drier, a refrigerator and a light. 
## energy sub-metering No. 2 
laundry_per_month <- power_all %>%
  group_by(month) %>%
  summarize(Total_Submeter_2 = sum(Sub_metering_2))

laundry_per_month$month_factor <- factor(laundry_per_month$month, levels = c(1:12), ordered = T)

ggplot(laundry_per_month, aes(x=month_factor, y=Total_Submeter_2)) +
  geom_bar(stat="identity", color="yellow", fill="orange") + 
  scale_x_discrete(name ="Month", limits=c(1:12)) +
  scale_fill_viridis(discrete = TRUE) +
  theme(legend.position="none") +
  theme_ipsum()  +
  labs(y="Total power consumed")+
  theme(legend.position="none") +
  ggtitle("Monthly Power-Consumption Submeter2 (Laundry)")
