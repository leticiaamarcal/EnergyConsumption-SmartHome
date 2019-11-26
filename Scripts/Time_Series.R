#------------------------------------------------------------------------------------ 
#Goal: Time Series
#Description: Deevenvolver análise de time series
#Developer: Letícia Marçal
#----------------------------------------------------------------------

#linkar com o preprocess
source("Scripts/Preprocess.R")

#library
library(ggfortify)
library(forecast)

#############################
temp <- dados_energia5 %>% 
  filter(year(DateTime) != 2006) %>% 
  group_by(year = year(DateTime), month = month(DateTime)) %>% 
  summarise(sum = sum(global_power))

ts <- ts(data = temp$sum, start = c(2007,1), frequency = 12)

autoplot(decompose(ts))
##############################

##Subset to one observation per week on Mondays at 8:00pm for 2007, 2008 and 2009
house070809weekly <- filter(dados_energia5, 
                            !year == 2006 & 
                              !year == 2010, 
                            weekday == "Monday" &
                              hours == 20 & minute == 1)


##Create TS object with SubMeter3
tsSM3_070809weekly <- ts(house070809weekly$Sub_metering_3, frequency=52, start=c(2007,1))

#plotar
autoplot(tsSM3_070809weekly, ts.colour = 'red', xlab = "Time", ylab = "Watt Hours", main = "Kitchen")

#plotar com plot.ts
plot.ts(tsSM3_070809weekly)

#Apply time series linear regression to the sub-meter 3 
#ts object and use summary to obtain R2 and RMSE 
fitSM3 <- tslm(tsSM3_070809weekly ~ trend + season) 
summary(fitSM3)

#Create the forecast for sub-meter 3. Forecast ahead 20 time periods 
forecastfitSM3 <- forecast(fitSM3, h=20)

#Plot the forecast for sub-meter 3. 
plot(forecastfitSM3)

#Create sub-meter 3 forecast with confidence levels 80 and 90
forecastfitSM3c <- forecast(fitSM3, h=20, level=c(80,90))

#Plot sub-meter 3 forecast, limit y and add labels
plot(forecastfitSM3c, ylim = c(0, 20), ylab= "Watt-Hours", xlab="Time")

#####Decompose

#Decompose Sub-meter 3 into trend, seasonal and remainder
components070809SM3weekly <- decompose(tsSM3_070809weekly)

#Plot decomposed sub-meter 3 
plot(components070809SM3weekly)

#Check summary statistics for decomposed sub-meter 3 
summary(components070809SM3weekly)

#######Tirar a sasionalidade

#Seasonal adjusting sub-meter 3 by subtracting the seasonal component & plot
tsSM3_070809Adjusted <- tsSM3_070809weekly - components070809SM3weekly$seasonal

#plot
autoplot(tsSM3_070809Adjusted)

#Test Seasonal Adjustment by running Decompose again. Note the very, very small scale for Seasonal
plot(decompose(tsSM3_070809Adjusted))

#Holt Winters Exponential Smoothing & Plot
tsSM3_HW070809 <- HoltWinters(tsSM3_070809Adjusted)
tsSM3_HW070809 <- HoltWinters(tsSM3_070809Adjusted, beta=FALSE, gamma=TRUE)
#dá pra mudar o beta e gamma colocando false, true, null ou números
#se o beta e gama forem falso, dará uma reta como resultado (é o
#máximo de smooth). Quando vai turbinando, ele te dá curvas mais detalhadas

#plot
plot(tsSM3_HW070809, ylim = c(0, 25))

#HoltWinters forecast & plot
tsSM3_HW070809for <- forecast(tsSM3_HW070809, h=25)

#plot
plot(tsSM3_HW070809for, ylim = c(0, 20), ylab= "Watt-Hours", xlab="Time - Sub-meter 3")

#Lastly, let's change the the confidence levels and then plot only the 
#forecasted area. Think of this just as you would when a 
#weatherperson forecasts the weather: They don't include 
#the preceding years, weeks and days. 
#Forecast HoltWinters with diminished confidence levels
tsSM3_HW070809forC <- forecast(tsSM3_HW070809, h=25, level=c(10,25))

#Plot only the forecasted area
plot(tsSM3_HW070809forC, ylim = c(0, 20), ylab= "Watt-Hours", xlab="Time - Sub-meter 3", start(2010))


  