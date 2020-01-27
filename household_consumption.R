######-------------------------------------------- TIME SERIES ANALYSIS AND FORECASTING
#
#USING THE HOUSEHOLD CONSUMPTION DATA  http://archive.ics.uci.edu/ml/datasets/Individual+household+electric+power+consumption
# Data cleaning, solving the missing values
# Data analyze, visualizations and plots 
#  Building the time series, different granularities
# Building the models : Linear Regression, Hold Winters, ARIMA, SARIMA
# Model optimization, analysis and FORECASTING
##########################################################################################################################

pacman::p_load(RMySQL, dplyr, lubridate, tidyverse, ggplot2, plotly, DBI,scales, 
              imputeTS, padr, chron, lattice, grid, forecast, stats, data.table,naniar,tseries,astsa,TSstudio)

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

########-----------------------------------------------------------SOLVING THE MISSING VALUES----------------------------------------

consumption_NA <- pad(consumption, end_val = NULL, interval = NULL, break_above = 2) #adding blank rows
sum(is.na(consumption_NA)) # check blank rows presence
consumption_NA <- arrange(consumption_NA, DateTime) #reorder

# add fractions of time 
consumption_NA$year <- year(consumption_NA$DateTime)
consumption_NA$month <- month(consumption_NA$DateTime)
consumption_NA$week <- week(consumption_NA$DateTime)
consumption_NA$day <- day(consumption_NA$DateTime)
consumption_NA$hour <- hour(consumption_NA$DateTime)
consumption_NA$minute <- minute(consumption_NA$DateTime)

# Buit data frame that contains only the missing values

myTableNA<- subset(consumption_NA, is.na(consumption_NA$Global_active_power))

#add small portions

myTableNA$year <- year(myTableNA$DateTime)
myTableNA$month <- month((myTableNA$DateTime))
myTableNA$day <- day(myTableNA$DateTime)
myTableNA$minute <- minute(myTableNA$DateTime)
myTableNA$hour <- hour(myTableNA$DateTime)

#Building the missing value graph

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

# adding all replaced until now with previous year values

myTable_noNA <- rbind(myTable_april2007,myTable_june2009,myTable_august2009)

# removing unncessary columns,renaming,reordering

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

# Values that are still missing (low number of missing values/time)

still_NA <- myTableNA[ !(myTableNA$DateTime %in% myTable_noNA$DateTime), ]

# Replacing the previously replaced mising values from previous year to the dataframe 
consumption_NA <- consumption_NA[!(consumption_NA$DateTime %in% myTable_noNA$DateTime), ]
consumption_NA$Date <- NULL
consumption_NA$Time <- NULL
consumption_NA <- rbind(consumption_NA, myTable_noNA)


#### imputing rest of values values in missing values low number/time

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

consumption <- consumption[which((consumption$year) != 2010), ]


#########################---------------------------------------------- SETTING GRANULARITY------------------------------


## Plots based on different ganularities of the data

## Subset the 9th day of January 2008 - 10 Minute frequency
houseDay10 <- filter(consumption, year == 2008 & month == 1 & day == 9 & (minute == 0 | minute == 10 | minute == 20 | minute == 30 | minute == 40 | minute == 50))

## Plot sub-meter 1, 2 and 3 with title, legend and labels - 10 Minute frequency
plot_ly(houseDay10, x = ~houseDay10$DateTime, y = ~houseDay10$Sub_metering_1, name = 'Kitchen', type = 'scatter', mode = 'lines') %>%
  add_trace(y = ~houseDay10$Sub_metering_2, name = 'Laundry Room', mode = 'lines') %>%
  add_trace(y = ~houseDay10$Sub_metering_3, name = 'Water Heater & AC', mode = 'lines') %>%
  layout(title = "Power Consumption January 9th, 2008",
         xaxis = list(title = "Time"),
         yaxis = list (title = "Power (watt-hours)"))


## Subset the first week of november 2009 with a 2 hour frequency
novemberFirstWeek <- consumption %>% 
                              filter( year == 2009 & month == 11 & day>=1 & day<=7 & (hour == 0 | hour ==2 | hour == 4 | hour == 6 | hour == 8 | hour == 10 | hour == 12 | hour == 14 | hour == 16 | hour == 18
                          | hour == 20 | hour == 22 | hour == 24 ))
                           
                              
                              

## Plot first week of nov consumption for every submeter 2 hour frequency

p1 <- plot_ly(novemberFirstWeek, x = ~novemberFirstWeek$DateTime, y = ~novemberFirstWeek$Sub_metering_1, name = 'Kitchen', type = 'scatter', mode = 'lines') %>%
  layout(title = "Power Consumption First Week of  November 2009",
         yaxis = list (title = "Power (watt-hours)"),
         xaxis =  list(title = "", tickformat = "%a %H"))
         

 
p2<- plot_ly(novemberFirstWeek, x = ~novemberFirstWeek$DateTime, y = ~novemberFirstWeek$Sub_metering_2, name = 'Laundry Room', type = 'scatter', mode = 'lines') %>%
  layout(title = "Power Consumption First Week of  November 2009",
         yaxis = list (title = "Power (watt-hours)"),
         xaxis =  list(title = "",tickformat = "%a"))
     
  
p3 <-  plot_ly(novemberFirstWeek, x = ~novemberFirstWeek$DateTime, y = ~novemberFirstWeek$Sub_metering_3, name = 'Water Heater and AC', type = 'scatter', mode = 'lines') %>%
  layout(title = "Power Consumption First Week of  November 2009",
         xaxis = list(title = "Time",type = 'date',
                      tickformat = "%a %HH"),
         yaxis = list (title = "Power (watt-hours)"))
  
p <- subplot(p1,p2,p3,nrows = 3, titleX = T, titleY = T)
p

## Subset 2008 every month with a 10 day frequency
sub2008 <- consumption %>% filter(year == 2008 & (day == 1 | day == 10 | day == 20 | day == 28)) 
  
  
## Plot month may 2008 all power

p1y <- plot_ly(sub2008, x = ~sub2008$DateTime, y = ~sub2008$Global_active_power, type = 'scatter', mode = 'lines', showlegend = F) %>%
  layout(title = "Power Consumption 2008",
         xaxis = list(title = ""),
         yaxis = list (title = "Power (watt-hours)"))

## Subset 2009 every month with a 10 day frequency
sub2009 <- consumption %>% filter(year == 2009 & (day == 1 | day == 10 | day == 20 | day == 28)) 


## Plot month may 2008 all power
p2y <- plot_ly(sub2009, x = ~sub2009$DateTime, y = ~sub2009$Global_active_power, type = 'scatter', mode = 'lines') %>%
  layout(title = "Power Consumption 2008, 2009 10 Minute Frequency",
         xaxis = list(title = "Time"),
         yaxis = list (title = "Power (watt-hours)"))


# Comparison betwenn 2009 and 2008 plot month may with a 10 day frequency
pt <- subplot(p1y,p2y,nrows = 2, titleX = T, titleY = T)
pt

## Granularity start


consumptionGRAN <- list()
granularity <- c("year", "season", "month", "week", "day", "hour")

for(g in granularity){
 consumptionGRAN[[g]] <- consumption %>%
    group_by(DateTime=floor_date(DateTime, g)) %>%
    dplyr::summarize_at(vars(
      Global_active_power),
      funs(sum))
    consumptionGRAN[[g]]$DateTime <- NULL
}

#################################################################### Time Series Beggining

# See if the data is stationary Function

station <- function(ts) {
  if (adf.test(ts)$p.value > 0.05) {
    print("The time series is not stationary, the ADF test has not been passed")
    sprintf("The p-value is: %5.3f",adf.test(ts)$p.value)
    ggAcf(ts)
  } else {
    print("This time series is stationary, the ADF test has been passed")
    ggAcf(ts)
  }
}


##### Build The TS

# Yearly measurements
ts.year <- ts(consumptionGRAN[["year"]], start = c(2007,1), frequency = 1)
plot.ts(ts.year)

# Monthly measurements

ts.month <- ts(consumptionGRAN[["month"]],start = c(2007,1), frequency = 12)
plot.ts(ts.month)

# Seasonly Measurements
ts.seasonly <- ts(consumptionGRAN[["season"]],start=c(2007,1), frequency = 4)
plot.ts(ts.seasonly)


# Weekly measurements

ts.weekly <- ts(consumptionGRAN[["week"]],start=c(2007,1), frequency = 52)
autoplot(ts.weekly, ts.colour = 'red', xlab = "Time", ylab = "Watt Hours", main = "Global Active Energy/Week")


################################################################## Forecasting the weekly consumption

# Decomposind the weekly

decomposed.weekly <- decompose(ts.weekly)
plot(decomposed.weekly)
summary(decomposed.weekly)

split_weeklyWS <-ts_split(ts.weekly)
ts_trainS <- split_weeklyWS$train 
ts_testS  <- split_weeklyWS$test

# LINEAR REGRESSION MODEL

# Building the Model
fit_lmWeekly <- tslm(ts_trainS ~ trend + season) 

#Applying the model
forecast_lmWeekly <- forecast(fit_lmWeekly, h=47, level=(90)) #90 % confidence

# Testing Performance
test_forecast(actual = ts.weekly, forecast.obj = forecast_lmWeekly, train = ts_trainS, test = ts_testS)
summary(fit_lmWeekly)
accuracy(forecast_lmWeekly,ts_testS)

# HOLT-WINTERS MODEL

# Building the model

fit_HWweekly <- HoltWinters(ts_trainS)

#Applying the model

forecast_HWweekly <- forecast(fit_HWweekly,h=47)

# Testing Performance
test_forecast(actual = ts.weekly, forecast.obj = forecast_HWweekly, train = ts_trainS, test = ts_testS)
summary(forecast_HWweekly)
accuracy(forecast_HWweekly,ts_testS)

################################################################ ARIMA 
station(ts.weekly)
# Removing the seasonality
ts.weekly_noSeason <- ts.weekly - decomposed.weekly$seasonal
autoplot(ts.weekly_noSeason)
station(ts.weekly_noSeason) #Stationary

# Split the train and test

split_weekly <-ts_split(ts.weekly_noSeason) #30/70 split
ts_train <- split_weekly$train 
ts_test  <- split_weekly$test

# PACF and ACF test

ts_cor(ts.weekly_noSeason) #q=1,p=1

# Buiding the model

arimaModel1 = arima(ts_train,order = c(1,0,1))
arimaModelAuto = auto.arima(ts_train,ic="aicc",trace = T) #chose 1,0,0

# Residuals

checkresiduals(arimaModel)
checkresiduals(arimaModelAuto)


# Plotting the residuals

k1 <- cbind(residuals_arima1,residuals_arimaAuto)
autoplot(k1,xlab="Year",ylab="Residuals") +
  ggtitle("Residuals comparison") # They look very similar


# Forecasting arima

forecast_arima1= forecast(ts_train,model=arimaModel1,h=47)
forecast_arimaAuto = forecast(ts_train,model=arimaModelAuto,h=47)


autoplot(ts_train,xlab ="Year",ylab="Global Active Power") +
  autolayer(ts_test, series = "Testing Set") +
  autolayer(forecast_arima1,series="Arima 1 ",PI =FALSE,)+
  autolayer(forecast_arimaAuto,series="Auto Arima",PI =FALSE)+
ggtitle("Forecast obtained from ARIMA models")

### Model AUTO was better

test_forecast(actual = ts.weekly_noSeason, forecast.obj = forecast_arimaAuto, train = ts_train, test = ts_test) #actual vs forecasted

# Accuracy
accuracy(forecast_arimaAuto, ts_test)
summary(arimaModelAuto)


# Table with error metrics for weekly

errors_weekly_forecast <-  as.data.frame(rbind((accuracy(forecast_HWweekly,ts_testS)),(accuracy(forecast_lmWeekly,ts_testS)),(accuracy(forecast_arimaAuto, ts_test))))
errors_weekly_forecast$model <- c("Holt Winters Weekly","Holt Winters Weekly","Linear Model Weekly","Linear Model Weekly","Arima Weekly","Arima Weekly")


################################################################## Forecasting the monthly

# Decomposind the monthly

decomposed.monthly <- decompose(ts.month)
plot(decomposed.monthly)
station(ts.month) # Not Stationary

#Splitting for Hold and LM

split_monthlyS <-ts_split(ts.month)
ts_trainSM <- split_monthlyS$train 
ts_testSM  <- split_monthlyS$test

# Linear Model

fit_lmMonthly <- tslm(ts_trainSM ~ trend + season) 
forecast_lmMonthly <- forecast(fit_lmMonthly, h=11, level=(90)) #90 % confidence
test_forecast(actual = ts.month, forecast.obj = forecast_lmMonthly, train = ts_trainSM, test = ts_testSM)
accuracy(forecast_lmMonthly,ts_testSM)
summary(fit_lmMonthly)


## HOLT-WINTERS MODEL

# Building the model

fit_HWMonthly <- HoltWinters(ts_trainSM)

#Applying the model

forecast_HWMonthly <- forecast(fit_HWMonthly,h=11)
test_forecast(actual = ts.month, forecast.obj = forecast_HWMonthly, train = ts_trainSM, test = ts_testSM)
plot(forecast_HWMonthly)

# Assesing performance 
summary(forecast_HWMonthly)
accuracy(forecast_HWMonthly,ts_testSM)


################################################################ ARIMA MONTHLY 
station(ts.month)
# Removing the seasonality
ts.monthly_noSeason <- ts.month - decomposed.monthly$seasonal
autoplot(ts.monthly_noSeason)
station(ts.monthly_noSeason) #Stationary

# Split the train and test

split_monthly <-ts_split(ts.monthly_noSeason)
ts_trainM <- split_monthly$train 
ts_testM  <- split_monthly$test

# PACF and ACF tests

ts_cor(ts.monthly_noSeason)

# Buiding the model

arimaModelAutoM = auto.arima(ts_trainM,trace = T) #chose 0,0,0 

# Residuals
checkresiduals(arimaModelAutoM)



# Forecasting arima

forecast_arimaAutoM = forecast(ts_trainM,model=arimaModelAutoM,h=11)
test_forecast(actual = ts.monthly_noSeason, forecast.obj = forecast_arimaAutoM, train = ts_trainM, test = ts_testM)



###### Seasonal arima for Monthly Data

diff_12 <- diff(ts.month, 12) ## finding the pq and PQ
acf2(diff_12, 18)

# Building the model

seasonarima_month<-auto.arima(ts_trainSM,trace=T) 


# Forecasting

forecast_seasonArimaMonth <- forecast(ts_trainSM,model = seasonarima_month,h=11) 

#Check errors
test_forecast(actual = ts.month, forecast.obj = forecast_seasonArimaMonth, train = ts_trainSM, test = ts_testSM)
accuracy(forecats_seasonArimaMonth,ts_testSM)

# Table with error metrics for monthly

errors_monthly_forecast <-  as.data.frame(rbind((accuracy(forecast_HWMonthly,ts_testSM)),(accuracy(forecast_lmMonthly,ts_testSM)),(accuracy(forecats_seasonArimaMonth,ts_testSM))))
errors_monthly_forecast$model <- c("Holt Winters Monthly","Holt Winters Monthly","Linear Model Monthly","Linear Model Monthly","Arima Monthly","Arima Monthly")


######################################## Forecasting by Submeters ###  

submeters_monthly <- consumption %>%
  group_by(year, month) %>%
  summarise(Sub_metering_1=sum(Sub_metering_1),
            Sub_metering_2=sum(Sub_metering_2),
            Sub_metering_3=sum(Sub_metering_3),
            Sub_metering_remainder=sum(sub_metering_remainder))

submeters_monthly <- ts(submeters_monthly[,3:6],
                      frequency = 12,
                      start=c(2007,1),
                      end=c(2009,12))

#Plot monthly time series
ts_plot(submeters_monthly,slider = T,Xtitle="Date Time", Ytitle = "Global ACtive Power",title="Consumption by Submeters Monthly")


# Split the training and testing set

linearModelsSplitsSubmeters <- list()
submeters <- c("Sub_metering_1","Sub_metering_2","Sub_metering_3","Sub_metering_remainder")
j<-1
for (i in submeters) {
  linearModelsSplitsSubmeters[[i]] <- ts_split(submeters_monthly[,j])
  j <- j+1
  
}

# Building the train and test

train_and_test_Submeters <-  list()
for (i in submeters) {
  train_and_test_Submeters <- linearModelsSplitsSubmeters
 
}

# Build the linEar model for the 3 submeters + remainder

LM_SR <- tslm(train_and_test_Submeters$Sub_metering_remainder$train ~ trend + season) 
LM_S1 <- tslm(train_and_test_Submeters$Sub_metering_1$train ~ trend + season) 
LM_S2 <- tslm(train_and_test_Submeters$Sub_metering_2$train ~ trend + season) 
LM_S3 <- tslm(train_and_test_Submeters$Sub_metering_3$train ~ trend + season) 

# Forecasts LM Submeters

# S1 Kitchen 
forecast_S1 <- forecast(LM_S1, h=11, level=(90)) #90 % confidence
test_forecast(actual = submeters_monthly[,1], forecast.obj = forecast_S1, test = linearModelsSplitsSubmeters$Sub_metering_1$test)
accuracy(forecast_S1, linearModelsSplitsSubmeters$Sub_metering_1$test)

# S2 Laundry Room 
forecast_S2 <- forecast(LM_S2, h=11, level=(90)) #90 % confidence
test_forecast(actual = submeters_monthly[,2], forecast.obj = forecast_S2, test = linearModelsSplitsSubmeters$Sub_metering_2$test)
accuracy(forecast_S2, linearModelsSplitsSubmeters$Sub_metering_2$test)

# S3 Water Heater AC
forecast_S3 <- forecast(LM_S3, h=11, level=(90)) #90 % confidence
test_forecast(actual = submeters_monthly[,3], forecast.obj = forecast_S3, test = linearModelsSplitsSubmeters$Sub_metering_3$test)
accuracy(forecast_S3, linearModelsSplitsSubmeters$Sub_metering_3$test)

# Submeter remaining
forecast_SR <- forecast(LM_SR, h=11, level=(90)) #90 % confidence
test_forecast(actual = submeters_monthly[,4], forecast.obj = forecast_SR, test = linearModelsSplitsSubmeters$Sub_metering_remainder$test)
accuracy(forecast_SR, linearModelsSplitsSubmeters$Sub_metering_remainder$test)

# Build the Holt Winters model for the 3 submeters + remainder

HW_SR <- HoltWinters(train_and_test_Submeters$Sub_metering_remainder$train)
HW_S1 <- HoltWinters(train_and_test_Submeters$Sub_metering_1$train)
HW_S2 <- HoltWinters(train_and_test_Submeters$Sub_metering_2$train)
HW_S3 <- HoltWinters(train_and_test_Submeters$Sub_metering_3$train) 

# Forecasts LM Submeters

# S1 Kitchen 
forecast_S1HW <- forecast(HW_S1, h=11, level=(90)) #90 % confidence
test_forecast(actual = submeters_monthly[,1], forecast.obj = forecast_S1HW, test = linearModelsSplitsSubmeters$Sub_metering_1$test)
accuracy(forecast_S1HW, linearModelsSplitsSubmeters$Sub_metering_1$test)

# S2 Laundry Room 
forecast_S2HW <- forecast(HW_S2, h=11, level=(90)) #90 % confidence
test_forecast(actual = submeters_monthly[,2], forecast.obj = forecast_S2HW, test = linearModelsSplitsSubmeters$Sub_metering_2$test)
accuracy(forecast_S2HW, linearModelsSplitsSubmeters$Sub_metering_2$test)

# S3 Water Heater AC
forecast_S3HW <- forecast(HW_S3, h=11, level=(90)) #90 % confidence
test_forecast(actual = submeters_monthly[,3], forecast.obj = forecast_S3HW, test = linearModelsSplitsSubmeters$Sub_metering_3$test)
accuracy(forecast_S3HW, linearModelsSplitsSubmeters$Sub_metering_3$test)

# Submeter remaining
forecast_SRHW <- forecast(HW_SR, h=11, level=(90)) #90 % confidence
test_forecast(actual = submeters_monthly[,4], forecast.obj = forecast_SRHW, test = linearModelsSplitsSubmeters$Sub_metering_remainder$test)
accuracy(forecast_SRHW, linearModelsSplitsSubmeters$Sub_metering_remainder$test)

# Build the ARIMA model for the 3 submeters + remainder

# Take out the seasonal

decomposed.submeters_monthly <- decompose(submeters_monthly)
submeters_monthly_no_season <- submeters_monthly - decomposed.submeters_monthly$seasonal

# Buil the test and train

arimaSubmeters <- list()
submeters <- c("Sub_metering_1","Sub_metering_2","Sub_metering_3","Sub_metering_remainder")
j<-1
for (i in submeters) {
  arimaSubmeters[[i]] <- ts_split(submeters_monthly_no_season[,j])
  j <- j+1
  
}

# Building the train and test

train_and_test_Submeters_no_season <-  list()
for (i in submeters) {
  train_and_test_Submeters_no_season <- arimaSubmeters
  
}

#  Building the arima models

arima_S1 <- auto.arima(train_and_test_Submeters_no_season$Sub_metering_1$train,trace=T) 
arima_S2 <- auto.arima(train_and_test_Submeters_no_season$Sub_metering_2$train,trace=T)
arima_S3 <- auto.arima(train_and_test_Submeters_no_season$Sub_metering_3$train,trace = T)
arima_SR <- auto.arima(train_and_test_Submeters_no_season$Sub_metering_remainder$train,trace=T)

# Forecasts ARIMA Submeters

# S1 Kitchen 
forecast_S1arima <- forecast(arima_S1, h=11, level=(90)) #90 % confidence
test_forecast(actual = submeters_monthly_no_season[,1], forecast.obj = forecast_S1arima, test = arimaSubmeters$Sub_metering_1$test)
accuracy(forecast_S1arima, arimaSubmeters$Sub_metering_1$test)

# S2 Laundry Room 
forecast_S2arima <- forecast(arima_S2, h=11, level=(90)) #90 % confidence
test_forecast(actual = submeters_monthly_no_season[,2], forecast.obj = forecast_S2arima, test = arimaSubmeters$Sub_metering_2$test)
accuracy(forecast_S2arima, arimaSubmeters$Sub_metering_2$test)

# S3 Water Heater AC
forecast_S3arima <- forecast(arima_S3, h=11, level=(90)) #90 % confidence
test_forecast(actual = submeters_monthly_no_season[,3], forecast.obj = forecast_S3arima, test = arimaSubmeters$Sub_metering_3$test)
accuracy(forecast_S3arima, arimaSubmeters$Sub_metering_3$test)

# Submeter remaining
forecast_SRarima <- forecast(arima_SR, h=11, level=(90)) #90 % confidence
test_forecast(actual = submeters_monthly_no_season[,4], forecast.obj = forecast_SRarima, test = arimaSubmeters$Sub_metering_remainder$test)
accuracy(forecast_SRarima, arimaSubmeters$Sub_metering_remainder$test)

######################################################### Chosing the best models 1 for global power used 
################# 1 for every submeter

# For global power we chose the weekly Holt Winters
# We will apply it to the next 6 months in order to forecats the consumption the add the price and time period

fit_6months_weekly <- HoltWinters(ts.weekly)
forecast_6months_weekly <- forecast(fit_6months_weekly,h=24,level=(90))
forecast_6months_weekly_x <- ts(forecast_6months_weekly$x,start=(2010),end=c(2010,25),frequency = 52)
plot(forecast_6months_weekly,main = "Forecast Cosumption 6 months December 2010 to June 2010",
     xlab = "Date", ylab="Energy Consumption in Kilowatts")
forecast_values_weekly <- as.data.frame(forecast_6months_weekly) %>%
                          mutate(week =c(1:24),month= ifelse(week %in% 1:4,"January",(ifelse(week %in% 4:8 ,"February",
                                                                          ifelse( week %in% 8:12,"March",(ifelse(week %in% 12:16 ,"April",
                                                                                                                 ifelse( week %in% 16:20,"May","June")))))))) %>%
                          mutate(price = paste(round(((`Point Forecast`/168) * 0.14),0),"???"))
write.csv(forecast_values_weekly,file="Values Forecast Global Active Power weekly first 6 months 2010")

# Submeters

# Submeter 1 LM

ts.monthS1 <- ts(submeters_monthly[,1],
                 frequency = 12,
                 start=c(2007,1),
                 end=c(2009,12))
forecast_6months_S1Fit <- tslm(ts.monthS1 ~ trend + season)
forecast_6months_S1 <- forecast(forecast_6months_S1Fit, h=6, level=(90)) #90 % confidence
plot(forecast_6months_S1 ,main = "Forecast Cosumption 6 months December 2010 to June 2010 Submeter 1 Kitchen",
     xlab = "Date", ylab="Energy Consumption in Kilowatts")

forecast_values_monthly_S1 <- as.data.frame(forecast_6months_S1) %>%
  mutate(month =c(1:6),month.name= ifelse(month == 1,"January",(ifelse(month ==2,"February",
                                                                       ifelse( month == 3,"March",(ifelse(month ==  4 ,"April",
                                                                                                          ifelse( month == 5,"May","June")))))))) %>%
  mutate(price = paste(round(((`Point Forecast`*0.001) * 0.1390),0),"???"))
write.csv(forecast_values_monthly_S1,file="Values Forecast Submeter 1 monthly first 6 months 2010")


# submeter 2 HW

ts.monthS2 <- ts(submeters_monthly[,2],
                 frequency = 12,
                 start=c(2007,1),
                 end=c(2009,12))
forecast_6months_S2Fit <- HoltWinters(ts.monthS2)
forecast_6months_S2 <- forecast(forecast_6months_S2Fit, h=6, level=(90)) #90 % confidence
plot(forecast_6months_S2 ,main = "Forecast Cosumption 6 months December 2010 to June 2010 Submeter 2 Laundry Room",
     xlab = "Date", ylab="Energy Consumption in Kilowatts")

forecast_values_monthly_S2 <- as.data.frame(forecast_6months_S2) %>%
  mutate(month =c(1:6),month.name= ifelse(month == 1,"January",(ifelse(month ==2,"February",
                                                                       ifelse( month == 3,"March",(ifelse(month ==  4 ,"April",
                                                                                                          ifelse( month == 5,"May","June")))))))) %>%
  mutate(price = paste(round(((`Point Forecast`*0.001) * 0.1390),0),"???"))
write.csv(forecast_values_monthly_S2,file="Values Forecast Submeter 2 monthly first 6 months 2010")


# Submeter 3 ARIMA

ts.monthS3 <- ts(submeters_monthly[,3],
                 frequency = 12,
                 start=c(2007,1),
                 end=c(2009,12))
forecast_6months_S3Fit <- auto.arima(ts.monthS3)
forecast_6months_S3 <- forecast(forecast_6months_S3Fit,h=6)
plot(forecast_6months_S3 ,main = "Forecast Cosumption 6 months December 2010 to June 2010 Submeter 3 Heater and AC",
     xlab = "Date", ylab="Energy Consumption in Kilowatts")

forecast_values_monthly_S3 <- as.data.frame(forecast_6months_S3) %>%
  mutate(month =c(1:6),month.name= ifelse(month == 1,"January",(ifelse(month ==2,"February",
                                                                     ifelse( month == 3,"March",(ifelse(month ==  4 ,"April",
                                                                                                            ifelse( month == 5,"May","June")))))))) %>%
  mutate(price = paste(round(((`Point Forecast`*0.001) * 0.1390),0),"???"))
write.csv(forecast_values_monthly_S3,file="Values Forecast Submeter 3 monthly first 6 months 2010")


