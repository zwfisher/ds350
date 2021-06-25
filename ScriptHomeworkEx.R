##--------------------------------------------
##
## R Review Homework Headstart
##
## Class: PCE Data Science Methods Class
##
##--------------------------------------------

##-----Set working directory-----
setwd('E:/Work/Teaching/PCE_Data_Science/1_Intro_Lecture/')

##-----Load Libraries-----
library(dplyr)
library(data.table)
library(ggplot2)

# Load jittered Data
headcount = read.csv('JitteredHeadCount.csv', stringsAsFactors = FALSE)
headcount$DateFormat = as.Date(headcount$DateFormat, format="%m/%d/%Y")

# Check for duplicates!
anyDuplicated(headcount[c("DateFormat", "Hour","GameCode")])

# Use data.table
headcount = data.table(headcount)

# Create an aggregate by day
daily_headcount = headcount[, list(TablesOcc_avg=mean(TablesOcc),
                                   TablesOcc_max=max(TablesOcc),
                                   HeadCount_avg=mean(HeadCount),
                                   HeadCount_max=max(HeadCount),
                                   HeadCount_total=sum(HeadCount)),
                            by=list(DateFormat, GameCode)]

daily_headcount$DayOfWeek = as.numeric(format(daily_headcount$DateFormat, format='%w'))+1

# Look at game code 'S6' by day-average
plot(daily_headcount$HeadCount_total[daily_headcount$GameCode=='S6'])

# Not that enlightening.  Look at S6 total headcount by weekday type
# I.e., we look at the average total daily headcount for each day-type
day_average = daily_headcount[,list(HeadCount_avg=mean(HeadCount_total)),
                              by=list(DayOfWeek, GameCode)]
# Not necessarily in the right order (days 1 -> 7)
s6_days = day_average$DayOfWeek[day_average$GameCode=="S6"]
s6_hc_avg = day_average$HeadCount_avg[day_average$GameCode=="S6"]
day_ordering = sort(s6_days, index.return=TRUE)

# Plot
plot(s6_days[day_ordering$ix],
     s6_hc_avg[day_ordering$ix],
     main="Day-of-Week Headcount Average",
     xlab="Day-of-Week", ylab="Total Headcount Average",
     type='l')

# Hourly headcount - aggregate
hourly_headcount = headcount[, list(HeadCount_avg=mean(HeadCount),
                                   HeadCount_max=max(HeadCount),
                                   HeadCount_total=sum(HeadCount)),
                            by=list(Hour, GameCode)]

ggplot(data = hourly_headcount, aes(x=Hour, y=HeadCount_avg)) + 
  geom_bar(stat="identity") + 
  labs(title="Average Headcount by Hour", x="Hour", y="Head Count")
ggplot(data = hourly_headcount, aes(x=Hour, y=HeadCount_avg)) + 
  geom_bar(aes(fill=GameCode), stat="identity", position="stack") + 
  labs(title="Average Headcount by Hour per Game", x="Hour", y="Head Count")

# Hourly headcount - at a given hour
noon_headcount <- hourly_headcount[hourly_headcount$Hour==12]
sixpm_headcount <- hourly_headcount[hourly_headcount$Hour==18]
midnight_headcount <- hourly_headcount[hourly_headcount$Hour==0]

given_hour_data <- data.table(gamecode=noon_headcount$GameCode, noon=noon_headcount$HeadCount_avg, sixpm=sixpm_headcount$HeadCount_avg, midnight=midnight_headcount$HeadCount_avg)
ghd <- melt(given_hour_data, 
           noon=given_hour_data$noon, 
           sixpm=given_hour_data$sixpm, 
           midnight=given_hour_data$midnight, 
           gamecode=given_hour_data$gamecode, 
           variable.name = "time")

ggplot(ghd, aes(x=gamecode, y=value, fill=time)) + 
  geom_bar(position="dodge", stat="identity") +
  labs(title="Headcount at Given Times", x="Game Code", y="Head Count")

# Seasonal headcount - aggregate and blackjack

# Create monthly set of data
monthly_headcount = headcount[, list(HeadCount_total=sum(HeadCount)),
                            by=list(DateFormat)]

ggplot(monthly_headcount, aes(DateFormat, HeadCount_total)) + 
  geom_line() +
  scale_x_date(date_breaks = "2 months") + 
  labs(x="", y="Head Count", title="Daily Traffic")

# Collect blackjack variants
monthly_headcount = headcount[, list(HeadCount_total=sum(HeadCount)),
                              by=list(DateFormat, GameCode)]
monthly_headcount$month = as.numeric(format(monthly_headcount$DateFormat, format='%m'))
blackjack_variants <- c("C4","DH","MQ","ND","SH","TH","TL")
setkey(monthly_headcount, GameCode)

monthly_blackjack <- monthly_headcount[blackjack_variants]
monthly_blackjack_totals <- monthly_blackjack %>% select(month, HeadCount_total) %>% group_by(month) %>% summarise(total=sum(HeadCount_total))

ggplot(monthly_blackjack_totals, aes(month, total)) + 
  geom_bar(stat="identity") +
  labs(x="Month", y="Head Count", title="Monthly Total Blackjack Traffic")

# Non-card Games - seasonality vs card games or demand vs card games
non_card_games <- c("CR", "PA", "RO", "RR", "S6")
monthly_headcount$GameType <- NA
monthly_headcount$GameType <- rep("card", each=7328)

monthly_headcount[GameCode %in% non_card_games, GameType := "non-card"]

ggplot(monthly_headcount, aes(x=month, y=HeadCount_total, fill=GameType)) + 
  geom_bar(position = "dodge", stat = "identity") +
  labs(title="Headcount for Card Games vs Non-Card Games", x="Month", y="Head Count")

