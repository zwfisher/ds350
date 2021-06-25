library(dplyr)
library(ggplot2)
library(reshape2)
library(mice)

setwd("c:/users/zfishe1/desktop/ds350/project")

acled_data <- read.csv("ACLED Version 6 All Africa 1997-2015_csv_dyadic.csv", stringsAsFactors = FALSE)


# Look at overall trend of violence

yearly_violence <- acled_data %>% select(YEAR, FATALITIES)
yearly_violence <- yearly_violence %>% group_by(YEAR)
yearly_violence <- summarise(yearly_violence, fats = sum(FATALITIES))

plot(yearly_violence)

# Large amount of violence early on, not expected
# Examining if more records are present in the dataset for early years

num_records_per_yr <- acled_data %>% group_by(YEAR)
num_records_per_yr <- summarise(num_records_per_yr, num_records = n())

plot(num_records_per_yr)

# Actually more records for the later years which is to be expected (more awareness, better reporting)
# Maybe cut data to get rid of outlier years

yearly_violence <- filter(yearly_violence, YEAR > 1999)
num_records_per_yr <- filter(num_records_per_yr, YEAR > 1999)

# Now check out avg fatalities per record

avg_fat_per_rec <- yearly_violence/num_records_per_yr
avg_fat_per_rec$YEAR <- yearly_violence$YEAR
plot(avg_fat_per_rec)

# Still see higher avg deaths/record early on. Going to manually look at data.
# Looks like 1999 has several event types related to battle
# Look at type of event to try to explain higher avg deaths/record

event_type_data <- acled_data %>% select(YEAR, EVENT_TYPE, FATALITIES)
event_type_data <- event_type_data %>% group_by(YEAR, EVENT_TYPE)
event_type_data <- summarise(event_type_data, fats=sum(FATALITIES))
event_type_data

p <- ggplot(event_type_data, aes(x=YEAR, y=fats, fill=EVENT_TYPE))
p + geom_bar(stat = "identity", position = "dodge") + labs(
  x = "Year",
  y = "Fatalities",
  title = "Number of fatalities per year by event type"
)

# As expected, a large number of fatalities due to Battle
# Looks like the Second Congo War occured during this time involving many nations and a large number of casualties

riot_data <- event_type_data %>% filter(EVENT_TYPE %in% c("Riots/Protests"))
                           
p <- ggplot(riot_data, aes(x=YEAR, y=fats, fill=EVENT_TYPE))
p + geom_bar(stat = "identity", position = "dodge") + labs(
  x = "Year",
  y = "Fatalities",
  title = "Number of fatalities per year in riots/protests"
) 

# Number of deaths due to riots has increased but those are not a large share of the overall total
# Going to look at pure number of events

event_counts <- acled_data %>% select(YEAR, EVENT_TYPE)
event_counts <- event_counts %>% group_by(YEAR, EVENT_TYPE)                           
event_counts <- summarise(event_counts, count=n())
event_counts

p <- ggplot(event_counts, aes(x=YEAR, y=count, fill=EVENT_TYPE))
p + geom_bar(stat = "identity", position = "dodge") + labs(
  x = "Year",
  y = "Number of Incidents",
  title = "Number of violent incidents per year by event type"
) 

# Interested to see share of violent vs non-violent
violent_nonviolent <- acled_data %>% select(YEAR, FATALITIES)

nv <- violent_nonviolent %>% filter(FATALITIES == 0) %>% mutate(class = "nv")
v <- violent_nonviolent %>% filter(FATALITIES != 0) %>% mutate(class = "v")

nv <- nv %>% group_by(YEAR)
nv <- summarise(nv, count=n())
nv$class <- "nv"

v <- v %>% group_by(YEAR)
v <- summarise(v, count=n())
v$class <- "v"

v_nv <- data.frame(yr=v$YEAR, v=v$count, nv=nv$count)

ggplot(v_nv, aes(yr)) + geom_line(aes(y = v, colour = "v")) + geom_line(aes(y = nv, colour = "nv"))  + labs(
  x = "Year",
  y = "Number of Incidents",
  title = "Number of violent and non-violent incidents per year"
)

acled_countries <- unique(acled_data$COUNTRY)

# Get and explore temperature data
# NASA data avaialble in global, hemisphere, and zonal avg. per year
# Did some manual work to segregate african countries into zones

merged_data <- acled_data %>% select(YEAR, FATALITIES, COUNTRY)
countryZones <- read.csv("countryZones.csv", stringsAsFactors = FALSE)
countryZones <- data.frame(countryZones)
names(countryZones) <- c("COUNTRY","ZONE")

merged_countries <- merged_data %>% distinct(COUNTRY)

setdiff(merged_countries$COUNTRY,countryZones$COUNTRY)

merged_country_zones <- merge(countryZones, merged_data, by="COUNTRY")
str(merged_country_zones)

yearly_zonal_violence <- merged_country_zones %>% group_by(YEAR, ZONE)
yearly_zonal_violence <- summarise(yearly_zonal_violence, count=n())
yearly_zonal_violence$ZONE <- as.factor(yearly_zonal_violence$ZONE)

ggplot(yearly_zonal_violence, aes(YEAR)) + geom_line(aes(y = count, colour = ZONE))  + labs(
  x = "Year",
  y = "Number of Incidents",
  title = "Number of incidents per year by climate zone"
)

# From NASA:
# The following are plain-text files in tabular format of temperature anomalies, 
# i.e. deviations from the corresponding 1951-1980 means

zonal_temp <- read.csv("zonalAnnualTempData.csv", stringsAsFactors = FALSE)
str(zonal_temp)
ggplot(zonal_temp, aes(Year)) + geom_line(aes(y=Temp, colour=as.factor(Zone)))  + labs(
  x = "Year",
  y = "Deviation in Celcius",
  title = "Average yearly temperature deviation",
  subtitle = "Deviation from 1951-1980 mean"
)

# Note: LOTI provides a more realistic representation of the global mean trends 
# than dTs below; it slightly underestimates warming or cooling trends, since 
# the much larger heat capacity of water compared to air causes a slower and 
# diminished reaction to changes; dTs on the other hand overestimates trends, 
# since it disregards most of the dampening effects of the oceans that cover about 
# two thirds of the Earth's surface.
zonal_temp <- data.frame(zonal_temp)
african_zones <- zonal_temp[zonal_temp$Zone %in% c("1","2","3","4"),]
global_zone <- zonal_temp[zonal_temp$Zone %in% c("G"),]

names(african_zones) <- c("YEAR", "ZONE", "TEMP")
model_data <- merge(yearly_zonal_violence, african_zones, by=c("YEAR","ZONE"))

# First fit, all of the data, doesn't make much sense though
fit1 <- lm(count ~ ., data = model_data)
summary(fit1)

# Break it down into zones and do lm on each zone.
zone1_temp <- zonal_temp[zonal_temp$Zone %in% c("1"),]
names(zone1_temp) <- c("YEAR", "ZONE", "TEMP")
zone1_viol <- yearly_zonal_violence[yearly_zonal_violence$ZONE %in% c("1"),]
zone1_merged <- merge(zone1_temp, zone1_viol, by=c("YEAR","ZONE"))
zone1_merged$YEAR <- as.character(zone1_merged$YEAR)

fit_z1 <- lm(count ~ TEMP, data = zone1_merged)
summary(fit_z1)
## ***** plot(fit_z1)

zone2_temp <- zonal_temp[zonal_temp$Zone %in% c("2"),]
names(zone2_temp) <- c("YEAR", "ZONE", "TEMP")
zone2_viol <- yearly_zonal_violence[yearly_zonal_violence$ZONE %in% c("2"),]
zone2_merged <- merge(zone2_temp, zone2_viol, by=c("YEAR","ZONE"))
zone2_merged$YEAR <- as.character(zone2_merged$YEAR)

fit_z2 <- lm(count ~ TEMP, data = zone2_merged)
summary(fit_z2)

zone3_temp <- zonal_temp[zonal_temp$Zone %in% c("3"),]
names(zone3_temp) <- c("YEAR", "ZONE", "TEMP")
zone3_viol <- yearly_zonal_violence[yearly_zonal_violence$ZONE %in% c("3"),]
zone3_merged <- merge(zone3_temp, zone3_viol, by=c("YEAR","ZONE"))
zone3_merged$YEAR <- as.character(zone3_merged$YEAR)

fit_z3 <- lm(count ~ TEMP, data = zone3_merged)
summary(fit_z3)

zone4_temp <- zonal_temp[zonal_temp$Zone %in% c("4"),]
names(zone4_temp) <- c("YEAR", "ZONE", "TEMP")
zone4_viol <- yearly_zonal_violence[yearly_zonal_violence$ZONE %in% c("4"),]
zone4_merged <- merge(zone4_temp, zone4_viol, by=c("YEAR","ZONE"))
zone4_merged$YEAR <- as.character(zone4_merged$YEAR)

fit_z4 <- lm(count ~ TEMP, data = zone4_merged)
summary(fit_z4)

# R-squareds are all over the place according to what zone we're looking at
# Let's look at the continent of Africa as a whole and global temps

tot_records_per_yr <- acled_data %>% group_by(YEAR)
tot_records_per_yr <- summarise(tot_records_per_yr, COUNT = n())

str(tot_records_per_yr)
names(global_zone) <- c("YEAR","ZONE","TEMP")

global_merged <- merge(tot_records_per_yr, global_zone, by="YEAR")

fit_glob <- lm(COUNT ~ TEMP, data = global_merged)
summary(fit_glob)

# still not a great R-squared
## ***** plot(fit_glob)
ggplot(global_merged, aes(TEMP,COUNT)) + geom_point() + labs(
  x = "Deviation in Celcius",
  y = "Incidents",
  title = "XY Plot of yearly temperature deviations vs number of incidents"
)

# I've collected data on internet data and population growth in africa
# we know that the arab spring was essentially organized completely through twitter
# that event corresponds with the huge spike in armed conflict occurences in the early 2010s

internet_data <- read.csv("africanInternetData.csv", stringsAsFactors = FALSE)
str(internet_data)
names(internet_data) <- c("Country", "2000", "2001", "2002",
                          "2003", "2004", "2005", "2006", "2007",
                          "2008", "2009", "2010", "2011", "2012",
                          "2013", "2014", "2015")

# Impute missing data
md.pattern(internet_data)
imputed_inet_data <- mice(internet_data, m=5, maxit = 50, method = 'pmm', seed = 500)
internet_data <- complete(imputed_inet_data, 2)

mlt_internet_data <- melt(internet_data)
str(mlt_internet_data)
names(mlt_internet_data) <- c("COUNTRY", "YEAR","INTERNET")

inet_countries <- acled_data %>% select(YEAR, FATALITIES, COUNTRY)
inet_countries <- inet_countries %>% group_by(YEAR, COUNTRY)
inet_countries <- summarise(inet_countries, count=n())

merged_inet_zones <- merge(inet_countries, mlt_internet_data, by=c("COUNTRY","YEAR"))
merged_inet_zones <- merge(merged_inet_zones, countryZones, by=c("COUNTRY"))

merged_inet_zones$ZONE <- as.character(merged_inet_zones$ZONE)

merged_inet_zones <- merge(merged_inet_zones, african_zones, by=c("YEAR","ZONE"))

inet_fit <- lm(count~., data = merged_inet_zones)
summary(inet_fit)
## ***** plot(inet_fit)

# TODO: multiply internet_data by population_data -> sum columns, divide by total for yearly
# inet pen. then group_by and summarise to get aggregate african inet pent. and rerun lm
# then add population data to the merged_inet_zones data and rerun lm
# then use aggregate population numbers to do a final lm at the aggregate level.

# then do write up...

pop_data <- read.csv("africanPopulationData.csv", stringsAsFactors = FALSE)
africa_agg_pop <- pop_data[1,]
pop_data <- pop_data[2:49,]
pop_data[10,1] <- "Democratic Republic of Congo"

inet_users <- internet_data[,2:17]*.01
inet_users[,2:17] <- pop_data[,5:20]*inet_users
inet_users[,1] <- internet_data[,1]
names(inet_users) <- c("COUNTRY", "2000", "2001", "2002",
                       "2003", "2004", "2005", "2006", "2007",
                       "2008", "2009", "2010", "2011", "2012",
                       "2013", "2014", "2015")

africa_total_inet_users <- c()
for(i in 2:17) {
  africa_total_inet_users[i-1] <- sum(inet_users[,i])
}
names(africa_total_inet_users) <- c("2000", "2001", "2002",
                       "2003", "2004", "2005", "2006", "2007",
                       "2008", "2009", "2010", "2011", "2012",
                       "2013", "2014", "2015")

africa_total_inet_users <- data.frame(YEAR=names(africa_total_inet_users), USERS=africa_total_inet_users, row.names=NULL)

global_merged <- merge(global_merged, africa_total_inet_users, by="YEAR")

glob_inet_fit <- lm(COUNT ~ TEMP + USERS, data = global_merged)
summary(glob_inet_fit)
## ***** plot(glob_inet_fit)

data.frame(africa_agg_pop)
names(africa_agg_pop) <- c("COUNTRY", "1997", "1998", "1999",
                           "2000", "2001", "2002", "2003", "2004", 
                           "2005", "2006", "2007", "2008", "2009", 
                           "2010", "2011", "2012", "2013", "2014", "2015")
africa_agg_pop <- melt(africa_agg_pop)
names(africa_agg_pop) <- c("COUNTRY", "YEAR", "POP")
africa_agg_pop <- africa_agg_pop[,2:3]

global_merged <- merge(global_merged, africa_agg_pop, by="YEAR")

all_factors_fit <- lm(COUNT ~ TEMP + USERS + POP, data = global_merged)
summary(all_factors_fit)

africa_inet_pen <- africa_total_inet_users[,2]/africa_agg_pop[4:19,2]
global_merged$USERS <- africa_inet_pen
final_fit <- lm(COUNT ~ TEMP + USERS + POP, data = global_merged)
summary(final_fit)
## ***** plot(final_fit)

# Most significant variable plot
users_fit <- lm(COUNT ~ USERS, data = global_merged)
plot(global_merged$USERS,global_merged$COUNT)
abline(users_fit)

par(mfrow=c(2,2))
plot(final_fit)
