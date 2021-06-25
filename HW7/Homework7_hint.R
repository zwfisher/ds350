# Homework 7
# Hint

setwd('c:/users/zfishe1/Desktop/DS350/HW7/')


##-----Load Libraries-----
library(dplyr)
library(data.table)
library(logging)

logReset()
file_log_handler = getLogger()
setLevel("INFO", file_log_handler)
basicConfig(level="INFO")
addHandler(writeToFile, file = "hw7.log", level = "INFO", logger = file_log_handler)

##-----Load Data-----
headcount = read.csv('JitteredHeadCount.csv', stringsAsFactors = FALSE)
weather = read.csv('las_vegas_hourly_weather.csv', stringsAsFactors = FALSE)


##-----Format Data----
headcount$DateFormat = as.Date(headcount$DateFormat, format="%m/%d/%Y")
names(weather) = c('time','temp','dew_pt','humidity','pressure',
                   'visibility','wind_dir','wind_speed','gust_speed',
                   'precipitation','events','conditions',
                   'wind_dir_deg','date')

weather$datetime = paste(weather$date,weather$time)
weather$datetime = strptime(weather$datetime, format="%Y-%m-%d %I:%M %p")
weather$Hour = as.numeric(format(round(weather$datetime, units="hours"), format="%H"))

##----Drop Duplicates----
weather = weather[!duplicated(weather[c("date", 'Hour')]),]


##----Merge Data-----
weather$DateFormat = weather$date
weather$date = NULL
weather$DateFormat = as.Date(weather$DateFormat, format="%Y-%m-%d")

headcount = merge(headcount, weather, all.x=TRUE, by=c("DateFormat","Hour"))

##----Imputation for NAs in weather-----
numeric_cols = c(11:15, 17:19, 22)
# Linear Interpolation:
headcount[,numeric_cols] = apply(headcount[,numeric_cols], 2, function(x) approx(x, xout=1:length(x), rule=2)$y)

##---Drop character columns----
headcount$wind_dir = NULL
headcount$time = NULL
headcount$datetime = NULL

##-----Deal with events/conditions----
headcount$events[headcount$events == ""] = "None"
headcount$events[is.na(headcount$events)] = "None"
headcount$conditions[is.na(headcount$conditions)] = "None"

##----Format Data for Time Series Exploration-----
headcount$DayNumber = as.numeric(headcount$DayNumber)
headcount$week_count = floor(headcount$DayNumber/7.0)
headcount$month_count = floor(headcount$DayNumber/30.5)
headcount$year_count = as.numeric(format(headcount$DateFormat, format="%Y")) - 2011

headcount$month = as.numeric(format(headcount$DateFormat, format="%m"))
headcount$season = floor((headcount$month-1) / 3)
headcount$week = as.numeric(format(headcount$DateFormat, format = "%W"))

##----Linear Model----
hc_model <- lm(HeadCount ~ . - DateFormat, data = headcount)
summary(hc_model)

loginfo(paste("Residual Standard Error:",sqrt(sum(hc_model$residuals**2)/hc_model$df.residual)))
loginfo("This means that if our residuals are normally distributed the standard deviation around the mean of that distribution is estimated to be 3.602")
  
plot(headcount$DateFormat, headcount$HeadCount, type="l", lwd=2, main="Las Vegas Headcount", xlab="Date", ylab="headcount")
lines(headcount$DateFormat, hc_model$fitted.values, lwd=2, lty=8, col="red")

plot(headcount$DateFormat, headcount$HeadCount, type="l", lwd=2, 
     main="Las Vegas Headcount", xlab="Date", ylab="headcount",
     xlim=c(as.Date("2011-11-01"), as.Date("2011-12-31"))
)
lines(headcount$DateFormat, hc_model$fitted.values, lwd=2, lty=8, col="red")
