#------------------------------------------------------------------------------------ 
#Goal: Preprocess
#Description: Entender e limpar os dados
#Developer: Letícia Marçal
#-----------------------------------------------------------------------------------

#libraries
library(readr)
library(tidyverse)
library(ggplot2)
library(dplyr)
library(lubridate)
library(devtools)
library(scales)
library(grid)
library(plotly)
library(grid)
library(caret)

dados_energia <- read.csv2(file = "C:/Users/letic/Documents/UbiqumR/IoT Project Data and Code/Data/household_power_consumption.txt", 
                           sep = ";", na.strings = c("?"),stringsAsFactors = F)

#eliminar NAs
dados_energia2 <- na.omit(dados_energia)

#Combine Date and Time attribute values in a new attribute column
dados_energia3 <- cbind(dados_energia2, paste(dados_energia2$Date, dados_energia2$Time), stringsAsFactors=FALSE)

#Change the name 
colnames(dados_energia3)[10] <-"DateTime"

#Mover a coluna para o começo do dataset
dados_energia3 <- dados_energia3[,c(ncol(dados_energia3), 1:(ncol(dados_energia3)-1))]

#Convert DateTime from POSIXlt to POSIXct/ avisar o computador que é data
dados_energia3$DateTime <- as.POSIXct(dados_energia3$DateTime,
                                      "%d/%m/%Y %H:%M:%S",
                                      tz = "GMT")

#transformar atributo
dados_energia3$Sub_metering_1 <-  as.numeric(dados_energia3$Sub_metering_1)
dados_energia3$Sub_metering_2 <-  as.numeric(dados_energia3$Sub_metering_2)
dados_energia3$Sub_metering_3 <-  as.numeric(dados_energia3$Sub_metering_3)
dados_energia3$Global_active_power <- as.numeric(dados_energia3$Global_active_power)
dados_energia3$Global_reactive_power <- as.numeric(dados_energia3$Global_reactive_power)
dados_energia3$Voltage <- as.numeric(dados_energia3$Voltage)
dados_energia3$Global_intensity <- as.numeric(dados_energia3$Global_intensity)

#criar dia, mês, ano
dados_energia3 %>%
  dplyr::mutate(minute = lubridate::minute(DateTime),
                hours = lubridate::hour(DateTime),
                day = lubridate::day(DateTime),
                weekday = weekdays(DateTime),
                week = lubridate::week(DateTime),
                month = lubridate::month(DateTime), 
                year = lubridate::year(DateTime)) -> dados_energia4

#mudar colunas de posição
dados_energia4 <- dados_energia4[, c(1, 2, 3, 11, 12, 13, 14, 15, 16, 17, 4:10)]

#criar nova coluna para transformar global
dados_energia4 %>% 
  mutate(global_power = Global_active_power * 1000/60,
         global_minus_sub = Global_active_power * 1000/60 - Sub_metering_1 - Sub_metering_2 - Sub_metering_3,
         all_sub = Sub_metering_1 + Sub_metering_2 + Sub_metering_3) -> dados_energia5

---
  
  #criar um dataset que tenha a média dos gastos de energia por hora
  dados_energia5 %>% 
  group_by(year, month, day, hours) %>% 
  summarise(ds = first(DateTime),
            weekday = first(weekday), 
            week = first(week),
            Total_House = mean(global_power),
            Kitchen = mean(Sub_metering_1),
            Laundry_Room = mean(Sub_metering_2),
            Temperature_Room = mean(Sub_metering_3),
            Other_Rooms = mean(global_minus_sub),
            All_submeters = mean(all_sub)) -> DFenergia

#mudar as colunas de lugar
DFenergia <- DFenergia[,c(5,4,3,6,7,2,1,9,10,11,13,12,8)]

----
  
#Fazer o modelo por mês para sub1, sub2, sub3, resto da casa e casa toda
  
#excluir coluna ds e all_sub
DFenergia2 <- DFenergia[,-c(1, 11)]

#criar dataframe com as variáveis que eu quero - mês
DFenergia2 %>% group_by(year, month) %>% 
  summarise(Kitchen = sum(Kitchen),
            Laundry_Room = sum(Laundry_Room),
            Temperature_Room = sum(Temperature_Room),
            Other_Rooms = sum(Other_Rooms),
            Total_House = sum(Total_House)) -> energy_month

#excluir 2006 e novembro de 2010
energy_month %>% filter(!year == 2006,
                        !(year == 2010 & month %in% 11)) -> energy_month

----
#criar data frame para o ano
energy_month %>% group_by(year) %>% 
  summarise(Kitchen = sum(Kitchen),
            Laundry_Room = sum(Laundry_Room),
            Temperature_Room = sum(Temperature_Room),
            Other_Rooms = sum(Other_Rooms),
            Total_House = sum(Total_House)) -> energy_year

-----
  
  #criar dataframe dia
  DFenergia2 %>% group_by(year, month, day) %>% 
  summarise(Weekday = first(weekday),
            Kitchen = sum(Kitchen),
            Laundry_Room = sum(Laundry_Room),
            Temperature_Room = sum(Temperature_Room),
            Other_Rooms = sum(Other_Rooms),
            Total_House = sum(Total_House)) -> energy_day

#excluir 2006 e novembro de 2010
energy_day %>% filter(!(year == 2006 & month == 12 & day %in% 16),
                      !(year == 2010 & month == 11 & day %in% 26)) -> energy_day

----

#mudar nomes colunas
colnames(energy_month)[4] <- 'Laundry'
colnames(energy_month)[5] <- 'Heater'
colnames(energy_month)[6] <- 'Others'
colnames(energy_month)[7] <- 'House'

energy_month %>% gather(Room, Energy_Wh, Kitchen:House) -> energy_month2  


##############tentativa de somar os minutos em vez de tirar a média#################
# dados_energia5 %>% 
#   group_by(year, month, day, hours) %>% 
#   summarise(ds = first(DateTime),
#             weekday = first(weekday), 
#             week = mean(week),
#             Total_House = sum(global_power),
#             Kitchen = sum(Sub_metering_1),
#             Laundry_Room = sum(Sub_metering_2),
#             Temperature_Room = sum(Sub_metering_3),
#             Other_Rooms = sum(global_minus_sub),
#             All_submeters = sum(all_sub)) -> DFenergia_temp
# 
# #mudar as colunas de lugar
# DFenergia_temp <- DFenergia_temp[,c(5,4,3,6,7,2,1,9,10,11,13,12,8)]
# 
# #fazer por mes
# DFenergia_temp %>% group_by(year, month) %>% 
#                   summarise(Total_House = sum(Total_House),
#                              Kitchen = sum(Kitchen),
#                              Laundry_Room = sum(Laundry_Room),
#                              Temperature_Room = sum(Temperature_Room),
#                              Other_Rooms = sum(Other_Rooms)) -> energy_month_temp
# 
# energy_month_temp %>% filter(!year == 2006,
#                                !(year == 2010 & month %in% 11)) -> energy_month_temp
# 
# energy_month_temp %>% gather(Room, Energy, Total_House:Other_Rooms) -> energy_month_temp2
# 
# energy_month_temp2 %>% 
#   filter(year == 2007, month == 8) %>% 
#   ggplot(aes(x = Room, y = Energy)) + 
#   geom_col() +
#   scale_y_continuous(labels = comma) 

