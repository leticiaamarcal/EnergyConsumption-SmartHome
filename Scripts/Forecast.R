#------------------------------------------------------------------------------------ 
#Goal: Time Series/ Forecast
#Description: Charts for dashboard
#Developer: Letícia Marçal
#----------------------------------------------------------------------

#linkar com o preprocess
source("Scripts/Preprocess.R")

library(shiny)
library(shinydashboard)
library(ggplot2)
library(readr)
library(lubridate)
library(dplyr)
library(ggfortify)
library(forecast)
library(prophet)
library(tidyr)


##### Forecast per month

DFenergia %>% group_by(year, month) %>%
               summarise(ds = first(ds),
                 Total_House = sum(Total_House)) -> energia_month_house

energia_month_house %>% filter(!year == 2006,
                         !(year == 2010 & month %in% c(9, 10, 11))) -> energia_MH_train

energia_month_house %>% filter(year == 2010,
                               (year == 2010 & month %in% c(9, 10))) -> energia_MH_test

#criar objeto ts
house_month <- ts(energia_MH_train$Total_House, frequency=12, start=c(2007,1))

#plotar
plot(house_month)
plot.ts(house_month)
autoplot(house_month)

#modelo
model_month_house <- tslm(house_month ~ trend + season)

#set seed
set.seed(123)

#métrica
summary(model_month_house)
#Multiple R-squared:  0.8876,	Adjusted R-squared:  0.8441
#F-statistic:  20.4 on 12 and 31 DF,  p-value: 1.979e-11

#previsão do mês seguinte
forecast_month <- forecast(model_month_house, h=2, level=c(80,90))

#Comparar métrica
postResample(energia_MH_test$Total_House, forecast_month$mean)

#criar dataframe para plotar
Time <- c("September", "October", "September", "October", "September", "October", "September", "October", "September", "October", "September", "October")
Level <- c("Forecast", "Forecast", "Lo_80", "Lo_80", "Hi_80", "Hi_80", "Lo_90", "Lo_90", "Hi_90", "Hi_90", "Real", "Real")
Energy <- c(5318576,  6414180,  4233792,  5329396, 6403359,  7498964, 3913977,  5009581, 6723175,  7818779, 4675361, 6786407)
df <- data.frame(Time, Level, Energy)

df %>%
   ggplot(aes(x = Time, y= Energy, color= Level)) +
        geom_jitter()

####
#criar objeto prophet

energia_month_house %>% filter(!year == 2006,
                                !(year == 2010 & month %in% 11)) %>%
                           select("ds" = ds, "y" = Total_House)-> energia_MH

#excluir ano
energia_MH <- energia_MH[,-1]

#criar objeto
prophet_MH <- prophet(energia_MH)

#dataframe
future <- make_future_dataframe(prophet_MH, freq = 31*24*60*60, periods = 5)
tail(future)

#forecast
forecast <- predict(prophet_MH, future)
tail(forecast[c('ds', 'yhat', 'yhat_lower', 'yhat_upper')])
 
 
#plotar
plot(prophet_MH, forecast)
prophet_plot_components(prophet_MH, forecast)
dyplot.prophet(prophet_MH, forecast)


##############Começar o meu dashboard AQUI

#Fazer o modelo por mês para sub1, sub2, sub3, resto da casa e casa toda

#criar objeto ts para KITCHEN
kitchenTS <- ts(energy_month$Kitchen, frequency = 12, start=c(2007,1))
#autoplot (kitchenTS)
 
#decompor o objeto
dec_kitchen <- decompose(kitchenTS)
#plot(dec_kitchen)

#deixar tirar sazonalidade e random
kitchen_trend <- kitchenTS - dec_kitchen$random - dec_kitchen$seasonal
#plot(kitchen_trend)

#criar modelo
kitchenModel_month <- tslm(kitchen_trend ~ trend)
#summary(kitchenModel_month)

#forecast
forecast_kitchen_month <- forecast(kitchenModel_month, h=10, level=c(80,90))
plot(forecast_kitchen_month)

##agora para o Laundry_Room

laundryTS <- ts(energy_month$Laundry_Room, frequency = 12, start=c(2007,1))

#autoplot (laundryTS)

dec_laundry <- decompose(laundryTS)

#plot(dec_laundry)

laundry_trend <- laundryTS - dec_laundry$random - dec_laundry$seasonal

#plot(laundry_trend)

laundryModel_month <- tslm(laundry_trend ~ trend)

#summary(laundryModel_month)

forecast_laundry_month <- forecast(laundryModel_month, h=10, level=c(80,90))

#plot(forecast_laundry_month)

##temperature_Room

temperatureTS <- ts(energy_month$Temperature_Room, frequency = 12, start=c(2007,1))

#autoplot (temperatureTS)

dec_temperature <- decompose(temperatureTS)

#plot(dec_temperature)

temperature_trend <- temperatureTS - dec_temperature$random - dec_laundry$seasonal

#plot(temperature_trend)

temperatureModel_month <- tslm(temperature_trend ~ trend)

#summary(temperatureModel_month)

forecast_temperature_month <- forecast(temperatureModel_month, h=10, level=c(80,90))

#plot(forecast_temperature_month)

###other rooms

otherTS <- ts(energy_month$Other_Rooms, frequency = 12, start=c(2007,1))

#autoplot (otherTS)

dec_other <- decompose(otherTS)

#plot(dec_other)

other_trend <- otherTS - dec_other$random - dec_other$seasonal

#plot(other_trend)

otherModel_month <- tslm(other_trend ~ trend)

#summary(otherModel_month)

forecast_other_month <- forecast(otherModel_month, h=10, level=c(80,90))

#plot(forecast_other_month)

###total house

houseTS <- ts(energy_month$Total_House, frequency = 12, start=c(2007,1))

#autoplot(houseTS)

dec_house <- decompose(houseTS)

#plot(dec_house)

house_trend <- houseTS - dec_house$random - dec_house$seasonal

#plot(house_trend)

houseModel_month <- tslm(house_trend ~ trend)

#summary(houseModel_month)

forecast_house_month <- forecast(houseModel_month, h=10, level=c(80,90))

#plot(forecast_house_month)

#####ANO

#eu vou forecast os resultados para as 5 áreas e somar os meses
#de novembro e dezembro ao resultado de 2010

#kitchen
set.seed(123)
kitchenModel_month2 <- tslm(kitchenTS ~ trend + season) 
forecast_kitchen_month2 <- forecast(kitchenModel_month2, h=10, level=c(80,90))
#forecast_kitchen_month2 : 
#Nov 2010:  2255.519
#Dec 2010:  2345.824
 
#laundry
set.seed(123)
laundryModel_month2 <- tslm(laundryTS ~ trend + season) 
forecast_laundry_month2 <- forecast(laundryModel_month2, h=10, level=c(80,90))
#forecast_laundry_month2
#Nov 2010: 2810.726
#Dec 2010: 2733.703
 
#temperature
set.seed(123)
temperatureModel_month2 <- tslm(temperatureTS ~ trend + season) 
forecast_temperature_month2 <- forecast(temperatureModel_month2, h=10, level=c(80,90))
#forecast_temperature_month2
#Nov 2010: 5033.248
#Dec 2010: 5517.265
 
#other rooms
set.seed(123)
otherModel_month2 <- tslm(otherTS ~ trend + season)
forecast_other_month2 <- forecast(otherModel_month2, h=10, level=c(80,90))
#forecast_other_month2
#Nov 2010: 7325539
#Dec 2010: 8216732

#house
set.seed(123)
houseModel_month2 <- tslm(houseTS ~ trend + season)
forecast_house_month2 <- forecast(houseModel_month2, h=10, level=c(80,90))
#forecast_house_month2
#Nov 2010: 7335638
#Dec 2010: 8227329

#resultado
#Year     kitchen   laundry   temperat  other     total house
#2010	    21085.92	28775.70	46317.42	57057458	57153637
#2010     25687     34320     56868     72599729  72716604

#Kitchen: 21085.92 + 2255.519 + 2345.824  = 25,687.263,
#Laundry: 28775.70 + 2810.726 + 2733.703 = 34,320.129
#Temperature: 46317.42 + 5033.248 + 5517.265 = 56,867.933
#Other: 57057458 + 7325539 + 8216732 = 72599729
#Total: 57153637 + 7335638 + 8227329 = 72716604

#criar coluna com novos valores
newRow <- c(2010, 25687, 34320, 56868, 72599729, 72716604)

#juntar o dataframe com a nova coluna
energy_year_final <- rbind(energy_year, newRow)

#excluir a linha 4 (valores antigos)
energy_year_final <- energy_year_final[-4,]

##processo para ano

#Kitchen

kitchenTSy <- ts(energy_year_final$Kitchen, frequency = 1, start=c(2007,1))
#autoplot (kitchenTSy)

#nao decompoe, proque e anual. anual nao tem sazionalidade

kitchenModel_year <- tslm(kitchenTSy ~ trend)
#summary(kitchenModel_year)


forecast_kitchen_year <- forecast(kitchenModel_year, h=10, level=c(80,90))
plot(forecast_kitchen_year)

##laundry room

laundryTSy <- ts(energy_year_final$Laundry_Room, frequency = 1, start=c(2007,1))
#autoplot (laundryTSy)

#criar modelo
laundryModel_year <- tslm(laundryTSy ~ trend)
#summary(laundryModel_year)

#forecast
forecast_laundry_year <- forecast(laudnryModel_year, h=10, level=c(80,90))
plot(forecast_laundry_year)

##temperature room
 
temperatureTSy <- ts(energy_year_final$Temperature_Room, frequency = 1, start=c(2007,1))
#autoplot (temperatureTSy)

temperatureModel_year <- tslm(temperatureTSy ~ trend)
#summary(temperatureModel_year)

forecast_temperature_year <- forecast(temperatureModel_year, h=10, level=c(80,90))
#plot(forecast_temperature_year)

###other rooms

otherTSy <- ts(energy_year_final$Other_Rooms, frequency = 1, start=c(2007,1))
#autoplot (otherTSy)

otherModel_year <- tslm(otherTSy ~ trend)
#summary(otherModel_year)

forecast_other_year <- forecast(otherModel_year, h=10, level=c(80,90))
#plot(forecast_other_year)

###total house

houseTSy <- ts(energy_year_final$Total_House, frequency = 1, start=c(2007,1))
#autoplot (houseTSy)


houseModel_year <- tslm(houseTSy ~ trend)
#summary(houseModel_year)

forecast_house_year <- forecast(houseModel_year, h=10, level=c(80,90))
#plot(forecast_house_year)
 
 
########Dia


#forecast para Kitchen
 
kitchenTSd <- ts(energy_day$Kitchen, frequency = 365.25, start=c(2007,1))
#autoplot (kitchenTSd)

#decompor o objeto
dec_kitchen_d <- decompose(kitchenTSd)
#plot(dec_kitchen_d)

#deixar tirar sazonalidade e random
kitchen_trend_d <- kitchenTSd - dec_kitchen_d$random - dec_kitchen_d$seasonal
#plot(kitchen_trend_d)

#criar modelo
kitchenModel_day <- tslm(kitchen_trend_d ~ trend)
#summary(kitchenModel_day)

#forecast
forecast_kitchen_day <- forecast(kitchenModel_day, h=100, level=c(80,90))
#plot(forecast_kitchen_day)


######laundry room

laundryTSd <- ts(energy_day$Laundry_Room, frequency = 365.25, start=c(2007,1))
#autoplot (laundryTSd)

#decompor o objeto
dec_laundry_d <- decompose(laundryTSd)
#plot(dec_laundry_d)

#deixar tirar sazonalidade e random
laundry_trend_d <- laundryTSd - dec_laundry_d$random - dec_laundry_d$seasonal
#plot(laundry_trend_d)

#criar modelo
laundryModel_day <- tslm(laundry_trend_d ~ trend)
#summary(laundryModel_day)

#forecast
forecast_laundry_day <- forecast(laundryModel_day, h=100, level=c(80,90))
#plot(forecast_kitchen_day)
 
####Temperature Room
 
temperatureTSd <- ts(energy_day$Temperature_Room, frequency = 365.25, start=c(2007,1))
#autoplot (temperatureTSd)

#decompor o objeto
dec_temperature_d <- decompose(temperatureTSd)
#plot(dec_temperature_d)

#deixar tirar sazonalidade e random
temperature_trend_d <- temperatureTSd - dec_temperature_d$random - dec_temperature_d$seasonal
#plot(temperature_trend_d)

#criar modelo
temperatureModel_day <- tslm(temperature_trend_d ~ trend)
#summary(temperatureModel_day)

#forecast
forecast_temperature_day <- forecast(temperatureModel_day, h=100, level=c(80,90))
#plot(forecast_temperature_day)

###other rooms

otherTSd <- ts(energy_day$Other_Rooms, frequency = 365.25, start=c(2007,1))
#autoplot (otherTSd)

#decompor o objeto
dec_other_d <- decompose(otherTSd)
#plot(dec_other_d)

#deixar tirar sazonalidade e random
other_trend_d <- otherTSd - dec_other_d$random - dec_other_d$seasonal
#plot(other_trend_d)

#criar modelo
otherModel_day <- tslm(other_trend_d ~ trend)
#summary(otherModel_day)

#forecast
forecast_other_day <- forecast(otherModel_day, h=100, level=c(80,90))
#plot(forecast_other_day)

###total house

houseTSd <- ts(energy_day$Total_House, frequency = 365.25, start=c(2007,1))
#autoplot (houseTSd)

#decompor o objeto
dec_house_d <- decompose(houseTSd)
#plot(dec_house_d)

#deixar tirar sazonalidade e random
house_trend_d <- houseTSd - dec_house_d$random - dec_house_d$seasonal
#plot(house_trend_d)

#criar modelo
houseModel_day <- tslm(house_trend_d ~ trend) 
#summary(houseModel_day)

#forecast
forecast_house_day <- forecast(houseModel_day, h=100, level=c(80,90))
#plot(forecast_house_day)



