### First presentation code

pacman::p_load(RMySQL, dplyr, lubridate, tidyverse, ggplot2, plotly, DBI,scales, 
               tidyverse, imputeTS, padr, chron, lattice, grid, forecast, stats, data.table,padr,naniar)

## Create a database connection 
con = dbConnect(MySQL(), user='deepAnalytics', password='Sqltask1234!', dbname='dataanalytics2018', host='data-analytics-2018.cbrosir2cswx.us-east-1.rds.amazonaws.com')

## List the tables contained in the database 
dbListTables(con)

# getting the data for the necessary attributes

yr_2006 <- dbGetQuery(con,"SELECT Date, Time, Global_active_power, Global_reactive_power, Global_intensity, Voltage, Sub_metering_1, Sub_metering_2, Sub_metering_3 FROM yr_2006")
yr_2007 <- dbGetQuery(con,"SELECT Date, Time, Global_active_power, Global_reactive_power, Global_intensity, Voltage, Sub_metering_1, Sub_metering_2, Sub_metering_3 FROM yr_2007")
yr_2008 <- dbGetQuery(con,"SELECT Date, Time, Global_active_power, Global_reactive_power, Global_intensity, Voltage, Sub_metering_1, Sub_metering_2, Sub_metering_3 FROM yr_2008")
yr_2009 <- dbGetQuery(con,"SELECT Date, Time, Global_active_power, Global_reactive_power, Global_intensity, Voltage, Sub_metering_1, Sub_metering_2, Sub_metering_3 FROM yr_2009")
yr_2010 <- dbGetQuery(con,"SELECT Date, Time, Global_active_power, Global_reactive_power, Global_intensity, Voltage, Sub_metering_1, Sub_metering_2, Sub_metering_3 FROM yr_2010")

# Investigating the dataframes## 2006 just 1 mo(DEC) and 2010 ONLY UNTIL 26 NOV

str(yr_2006)
summary(yr_2010)
head(yr_2010)
tail(yr_2010)


## Combine tables into one dataframe using dplyr## only full years
consumption <- bind_rows(yr_2007, yr_2008, yr_2009)

# Investigate primary dataframe

str(consumption)
summary(consumption)
head(consumption)
tail(consumption)

## Combine Date and Time attribute values in a new attribute column
consumption<-cbind(consumption,paste(consumption$Date,consumption$Time), stringsAsFactors=FALSE)

## Give the new attribute a header name 
colnames(consumption)[10] <-"DateTime"

## Move the DateTime attribute within the dataset
consumption <- consumption[,c(ncol(consumption), 1:(ncol(consumption)-1))]
head(consumption)

## Convert DateTime from POSIXlt to POSIXct 
consumption$DateTime <- as.POSIXct(consumption$DateTime, "%Y/%m/%d %H:%M:%S")

## Add the time zone
attr(consumption$DateTime, "tzone") <- "Europe/Paris"

## Inspect the data types
str(consumption)

##NAS 
sum(is.na(consumption))

#################NAS

consumption_NA <- pad(consumption, end_val = NULL, interval = NULL, break_above = 2) #adding blank rows
sum(is.na(consumption_NA)) # check blank rows presence
consumption_NA <- arrange(consumption_NA, DateTime)

str(consumption_NA)
consumption_NA$year <- year(consumption_NA$DateTime)
consumption_NA$month <- month(consumption_NA$DateTime)
consumption_NA$week <- week(consumption_NA$DateTime)
consumption_NA$day <- day(consumption_NA$DateTime)
consumption_NA$hour <- hour(consumption_NA$DateTime)
consumption_NA$minute <- minute(consumption_NA$DateTime)

#plot NA

myTableNA<- subset(consumption_NA, is.na(consumption_NA$Global_active_power))

#add small portions
myTableNA$year <- year(myTableNA$DateTime)
myTableNA$month <- month((myTableNA$DateTime))
myTableNA$day <- day(myTableNA$DateTime)
myTableNA$minute <- minute(myTableNA$DateTime)
myTableNA$hour <- hour(myTableNA$DateTime)

myTableNA2 <- 0
myTableNA2 <- as.data.frame(myTableNA2)
myTableNA2 <- myTableNA %>% group_by(year,month) %>% mutate(myTableNA2$numberofNA<-n())
myTableNA2$number_NA <- myTableNA2$`myTableNA2$numberofNA <- n()`
myTableNA2$`myTableNA2$numberofNA <- n()` <-NULL
ggplot(data=myTableNA2, aes(x=DateTime, y=number_NA))+
  geom_line()+
  scale_x_datetime(breaks = date_breaks("2 months"),labels = date_format("%d/%m"))+
  facet_grid(year~.)



### giving values to periods no value many missing

myTableNA_april_2007 <- myTableNA %>% filter(month == "4" & year == "2007")
myTable_april_2008R <- consumption_NA %>% filter(year == "2008" & month == "4")
myTable_april2007 <- left_join(myTableNA_april_2007,myTable_april_2008R, by = c("month" = "month", "day" = "day", "minute"="minute", "hour" = "hour"))
myTable_april2007 <- myTable_april2007[c(1,11:16,20:26)]


myTableNA_june_2009 <- myTableNA %>% filter(month =="6" & year =="2009")
myTableNA_june_2008R <- consumption_NA %>% filter(year == "2008" & month == "6" )
myTable_june2009 <- left_join(myTableNA_june_2009,myTableNA_june_2008R, by = c("month" = "month", "day" = "day", "minute"="minute", "hour" = "hour"))
myTable_june2009 <- myTable_june2009[c(1,11:16,20:26)]

myTable_august_2009 <- myTableNA %>% filter(month == "8" & year == "2009")
myTableNA_august_2008R<- consumption_NA %>% filter(year == "2008" & month =="8")
myTable_august2009 <- left_join(myTable_august_2009,myTableNA_august_2008R, by = c("month" = "month", "day" = "day", "minute"="minute", "hour" = "hour"))
myTable_august2009 <- myTable_august2009[c(1,11:16,20:26)] 

#adding all replaced until now with prev year values

myTable_noNA <- rbind(myTable_april2007,myTable_june2009,myTable_august2009)

#removing unncessary columns,renamin,reordering
myTableNA$Date <- NULL
myTableNA$Time <- NULL
myTable_noNA$DateTime <- myTable_noNA$DateTime.x
myTable_noNA$DateTime.x <-NULL

myTable_noNA[c(
  "year","week","Global_active_power","Global_reactive_power","Global_intensity","Voltage","Sub_metering_1","Sub_metering_2","Sub_metering_3")] <- 
  myTable_noNA[c(1,3,7:13)]
myTable_noNA[c(1,3,7:13)] <- NULL
myTable_noNA <- myTable_noNA[c(5,8:14,6,1,7,2,4,3)]
myTableNA <- myTableNA[c(1:12,14,13)]

still_NA <- myTableNA[ !(myTableNA$DateTime %in% myTable_noNA$DateTime), ]

consumption_NA <- consumption_NA[!(consumption_NA$DateTime %in% myTable_noNA$DateTime), ]
consumption_NA$Date <- NULL
consumption_NA$Time <- NULL
consumption_NA <- rbind(consumption_NA, myTable_noNA)


#### imputing rest of values values in missing values low number

consumption <- na_interpolation(consumption_NA, option = "linear")

sum(is.na(consumption)) #no more missing values

## Create "year" attribute with lubridate,  quarter, month, week, weekday, day, hour and minute

consumption$year <- year(consumption$DateTime)
consumption$quarter <- quarter(consumption$DateTime)
consumption$month <- month(consumption$DateTime)
consumption$week <- week(consumption$DateTime)
consumption$weekdays <- weekdays(consumption$DateTime)
consumption$day <- day(consumption$DateTime)
consumption$hour <- hour(consumption$DateTime)
consumption$minute <- minute(consumption$DateTime)

# Assign new column with season
consumption <- consumption %>% mutate(season = ifelse(month %in% 10:12, "Fall",
                                                      ifelse(month %in% 1:3, "Winter",
                                                             ifelse(month %in% 4:6, "Spring",
                                                                    "Summer"))))
# Assign new column day or nigth

consumption$nigth_or_day <-ifelse(consumption$hour > 22 | consumption$hour < 6, "nigth", "day")


# Calculating the remaining energy
consumption$sub_metering_remainder = (consumption$Global_active_power * 1000 / 60) - (consumption$Sub_metering_1 + consumption$Sub_metering_2 + consumption$Sub_metering_3)
consumption$sub_metering_remainder <- round(consumption$sub_metering_remainder,digits = 0)


# PLOTS 

### Histograms

ggplot(consumption, aes(x = Sub_metering_3))+
  geom_histogram(fill = "Yellow", bins=10, col="Orange")+
  scale_y_continuous(labels = comma) +
  labs(x = "Power(in KW)", title = "Sub Meter 3")

#last week march 2009 submetres

myTable1=consumption[consumption$Date>="2009-05-05" & consumption$Date<="2009-05-08",]

ggplot(data=myTable1 ,aes(myTable1$DateTime))+
  geom_line(aes(y = myTable1$Sub_metering_1, color="Kitchen")) + #kitchen, containing mainly a dishwasher, an oven and a microwave
  geom_line(aes(y = myTable1$Sub_metering_2, color="Laundry Room")) + #laundry room, containing a washing-machine, a tumble-drier, a refrigerator and a light
  geom_line(aes(y = myTable1$Sub_metering_3, color="Heater/AC")) + #electric water-heater and an air-conditioner
  xlab("Day/Time")+
  ylab("Global Active Power (kilowatts)")+
  ggtitle("Global Active Power by Time First 3 days of May 2009 Submeters")+
  labs(color="Room Submeter")+
  scale_x_datetime(breaks = date_breaks("1 day"),labels = date_format("%a %d/%m %H:%M"))+
  theme(panel.background = element_rect(fill = rgb(248, 236, 212, maxColorValue = 255)))+
  theme_bw()
# last week dec 2008 submeter nigth or day

myTable2 <- consumption %>% 
  filter(consumption$Date>="2008-12-24" & consumption$Date<="2008-12-31")

ggplot(data=myTable2 ,aes(myTable2$DateTime))+
  geom_line(aes(y = myTable2$Sub_metering_1, color="Kitchen")) + #kitchen, containing mainly a dishwasher, an oven and a microwave
  geom_line(aes(y = myTable2$Sub_metering_2, color="Laundry Room")) + #laundry room, containing a washing-machine, a tumble-drier, a refrigerator and a light
  geom_line(aes(y = myTable2$Sub_metering_3, color="Electric Water Heater and Air-conditioner")) + #electric water-heater and an air-conditioner
  xlab("Date")+
  ylab("Global Active Power (kilowatts)")+
  labs(color="Submeter")+
  ggtitle("Global Active Power by Time Last week of Dec 2008 Submeters Nigth or Day")+
  scale_x_datetime(breaks = date_breaks("1 day"),labels = date_format("%d"))+
  theme(panel.background = element_rect(fill = rgb(248, 236, 212, maxColorValue = 255)))+
  facet_grid(nigth_or_day~.)+
  theme_bw()

# all power 

myTable3=consumption[consumption$Date>="2009-02-23" & consumption$Date<="2009-03-01",]

ggplot(data=myTable3, aes(x=myTable3$DateTime, y=myTable3$Global_active_power))+
  geom_line()+
  xlab("Day/Time")+
  ylab("Global Active Power (kilowatts)")+
  ggtitle("Global Active Power by Time Last week of Feb 2009")+
  scale_x_datetime(breaks = date_breaks("1 day"),labels = date_format("%a %d/%m %H:%M"))+
  theme_bw()

# all power by season 2009

myTable4 <- consumption %>%
  filter(year==2009)

ggplot(data=myTable4, aes(x=myTable4$DateTime, y=myTable4$Global_active_power,color=myTable4$season))+
  geom_line()+
  xlab("Season")+
  ylab("Global Active Power (kilowatts)")+
  labs(color="season")+
  ggtitle("Global Active Power by Seasons 2009")+
  scale_x_datetime(breaks = date_breaks("2 months"),labels = date_format("%m"))+
  theme_bw()



# by day of the week average

myTable5 <- consumption %>% 
  filter(year==2009, month>9 & month<12) %>%
  group_by(month,weekdays) %>% 
  mutate(avg_power= mean(Global_active_power)) 

ggplot(data=myTable5, aes(x=myTable5$weekdays,color=weekdays))+
  geom_bar()+
  xlab("Month")+
  ylab("Average Power")+
  ggtitle("Global Active Power by Weekday 2009")+
  facet_grid(. ~ weekdays)


# by day or nigth

myTable6 <- consumption %>% 
  filter(year==2008) %>% 
  group_by(month, day, hour, nigth_or_day) %>%
  mutate(avg_power= mean(Global_active_power)) 


ggplot(data=myTable6, aes(x=myTable6$DateTime, y=myTable6$avg_power,color=nigth_or_day))+
  geom_line()+
  xlab("Month")+
  ylab("Average Power")+
  labs(color="Time of Day")+
  ggtitle("Global Active Power by Time of day 2008")+
  scale_x_datetime(breaks = date_breaks("30 days"),labels = date_format("%m"))+
  # theme(panel.background = element_rect(fill = rgb(248, 236, 212, maxColorValue = 255)))+
  facet_grid(nigth_or_day ~ .)

# all power by day/nigth 2007

myTable7 <- consumption %>% 
  filter(year==2007, week==46:48) %>%
  group_by(month,nigth_or_day)

ggplot(data=myTable7, aes(x=myTable7$DateTime, y=myTable7$Global_active_power,color=myTable7$nigth_or_day))+
  geom_line()+
  xlab("Month")+
  ylab("Global Active Power (kilowatts)")+
  labs(color="Time of Day")+
  ggtitle("Global Active Power by Nigth or Day 2007")+
  scale_x_datetime(breaks = date_breaks("3 months"),labels = date_format("%m"))+
  facet_grid(nigth_or_day~.)+
  theme_bw()

ggplot(data=myTable7, aes(x=myTable7$DateTime, y=myTable7$Global_active_power, color=myTable7$weekdays))+
  geom_line()+
  xlab("Day/Time")+
  ylab("Global Active Power (kilowatts)")+
  ggtitle("Global Active Power by Time Last week of Feb 2009")+
  scale_x_datetime(breaks = date_breaks("1 day"),labels = date_format("%a %d/%m %H:%M"))+
  theme_bw()+
  facet_wrap(weekdays ~.)

### Granularityb
