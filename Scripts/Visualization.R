#------------------------------------------------------------------------------------ 
#Goal: Visualization
#Description: Entender e limpar os dados
#Developer: Letícia Marçal
#----------------------------------------------------------------------

#linkar com o preprocess
source("Scripts/Preprocess.R")

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

#plots

dados_energia4 %>%
group_by(year, month = month(DateTime, label = T, abbr = T)) %>%
summarise(area1 = sum(Sub_metering_1)) %>%
mutate(date = paste(year, month)) %>%
ggplot() +
 geom_col(aes(x = date, y = area1))


#area1
dados_energia4 %>%
  group_by(year, month = month(DateTime, label = T, abbr = T)) %>%
  summarise(energy_area1 = sum(Sub_metering_1)) %>%
  ggplot() +
  geom_line(aes(x = month, y = energy_area1,
                group = factor(year), col = factor(year)), size =1) +
  theme_classic() +
  scale_y_continuous(labels = scales::comma) +
  scale_color_discrete(name = "Year")

#area2
dados_energia4 %>%
  group_by(year, month = month(DateTime, label = T, abbr = T)) %>%
  summarise(energy_area2 = sum(Sub_metering_2)) %>%
  ggplot() +
  geom_line(aes(x = month, y = energy_area2,
                group = factor(year), col = factor(year))) +
  theme_classic() +
  scale_y_continuous(labels = scales::comma)

#area3
dados_energia4 %>%
  group_by(year, month = month(DateTime, label = T, abbr = T)) %>%
  summarise(energy_area3 = sum(Sub_metering_3)) %>%
  ggplot() +
  geom_line(aes(x = month, y = energy_area3,
                group = factor(year), col = factor(year))) +
  theme_classic() +
  scale_y_continuous(labels = scales::comma)

#total energy
dados_energia5 %>%
  group_by(year, month = month(DateTime, label = T, abbr = T)) %>%
  summarise(total_energy = sum(global_power)) %>%
  ggplot() +
  geom_line(aes(x = month, y = total_energy,
                group = factor(year), col = factor(year))) +
  theme_classic() +
  scale_y_continuous(labels = scales::comma)

###plots : colunas

#area1
dados_energia4 %>%
 filter(!year == "2006") %>%
  group_by(year, month = month(DateTime, label = T, abbr = T)) %>%
   summarise(area1 = sum(Sub_metering_1)) %>%
   ggplot() +
   geom_col(aes(x = month, y = area1), color = "deepskyblue4", fill = "deepskyblue") +
   scale_y_continuous(labels = scales::comma) +
   facet_wrap(~year) +
   theme_minimal()

#area2
 dados_energia4 %>%
   filter(!year == "2006") %>%
   group_by(year, month = month(DateTime, label = T, abbr = T)) %>%
   summarise(area2 = sum(Sub_metering_2)) %>%
   ggplot() +
   geom_col(aes(x = month, y = area2), color = "chartreuse4", fill = "chartreuse3") +
   scale_y_continuous(labels = scales::comma) +
   facet_wrap(~year) +
   theme_minimal()

#area3
 dados_energia4 %>%
   filter(!year == "2006") %>%
   group_by(year, month = month(DateTime, label = T, abbr = T)) %>%
   summarise(area3 = sum(Sub_metering_3)) %>%
   ggplot() +
   geom_col(aes(x = month, y = area3), color = "darkorchid4", fill = "darkorchid") +
   scale_y_continuous(labels = scales::comma) +
   facet_wrap(~year) +
   theme_minimal()

#total
dados_energia5 %>%
   filter(!year == "2006") %>%
   group_by(year, month = month(DateTime, label = T, abbr = T)) %>%
   summarise(total_energy = sum(global_power)) %>%
   ggplot() +
   geom_col(aes(x = month, y = total_energy), color = "coral2", fill = "coral3") +
   scale_y_continuous(labels = scales::comma) +
   facet_wrap(~year) +
   theme_minimal()


#criando novo dataset para plotar
dados_energia5 %>% group_by(hours) %>% 
 summarise(area1 = sum(Sub_metering_1),
             area2 = sum(Sub_metering_2),
             area3 = sum(Sub_metering_3),
             total_house = sum(global_power),
             others = sum(global_minus_sub),
             all_areas = sum(all_sub)) -> dados_plot

#plot / legendas não funcionaram/ mudar no paint
dados_plot %>%
   ggplot() +
   geom_line(aes(x = hours, y = area1, color = "blue"), size=1) + 
   geom_line(aes(x = hours, y = area2, color = "red"), size =1) +
   geom_line(aes(x = hours, y = area3, color = "purple"), size = 1) +
   geom_line(aes(x = hours, y = others, color = "orange"), size = 1) + 
   geom_line(aes(x = hours, y = total_house, color = "green"), size = 2) + 
   theme_classic() +
   scale_y_continuous(labels = scales::comma) +
   scale_color_discrete(name = "Energy Consumption", labels = c("Area 1", "Area 2", "Area 3", "Other Areas", "Total House"))

------
  
#ploting granularity
plot(dados_energia$Sub_metering_1)

#filter a week (week 2, year 2008)
houseWeek <- filter(dados_energia5, year == 2008 & week == 2)

#plot
plot(houseWeek$Sub_metering_1)

----
  
#Subset the 9th day of January 2008 - All observations
houseDay <- filter(dados_energia5, year == 2008 & month == 1 & day == 9)

#Plot sub-meter 1
plot_ly(houseDay, x = ~houseDay$DateTime, y = ~houseDay$Sub_metering_1, type = 'scatter', mode = 'lines')

#Plot sub-meter 1, 2 and 3 with title, legend and labels - All observations
plot_ly(houseDay, x = ~houseDay$DateTime, y = ~houseDay$Sub_metering_1, name = 'Kitchen', type = 'scatter', mode = 'lines') %>%
  add_trace(y = ~houseDay$Sub_metering_2, name = 'Laundry Room', mode = 'lines') %>%
  add_trace(y = ~houseDay$Sub_metering_3, name = 'Water Heater & AC', mode = 'lines') %>%
  layout(title = "Power Consumption January 9th, 2008",
         xaxis = list(title = "Time"),
         yaxis = list (title = "Power (watt-hours)"))

#22 de agosto de 2008
houseDay2 <- filter(dados_energia5, year == 2008 & month == 8 & day == 22)

plot_ly(houseDay2, x = ~houseDay2$DateTime, y = ~houseDay2$Sub_metering_1, name = 'Kitchen', type = 'scatter', mode = 'lines') %>%
  add_trace(y = ~houseDay2$Sub_metering_2, name = 'Laundry Room', mode = 'lines') %>%
  add_trace(y = ~houseDay2$Sub_metering_3, name = 'Water Heater & AC', mode = 'lines') %>%
  layout(title = "Power Consumption August 22nd, 2008",
         xaxis = list(title = "Time"),
         yaxis = list (title = "Power (watt-hours)"))

#25 de dezembro de 2009
houseDay3 <- filter(dados_energia5, year == 2009 & month == 12 & day == 25)

plot_ly(houseDay3, x = ~houseDay3$DateTime, y = ~houseDay3$Sub_metering_1, name = 'Kitchen', type = 'scatter', mode = 'lines') %>%
  add_trace(y = ~houseDay3$Sub_metering_2, name = 'Laundry Room', mode = 'lines') %>%
  add_trace(y = ~houseDay3$Sub_metering_3, name = 'Water Heater & AC', mode = 'lines') %>%
  layout(title = "Power Consumption December 25th, 2009",
         xaxis = list(title = "Time"),
         yaxis = list (title = "Power (watt-hours)"))

#reduzir os pontos para ter uma visão melhor. Em vez de todo minutos,
#colocar a cada 10 minutos
houseDay10 <- filter(dados_energia5, year == 2008 & month == 1 & day == 9 & (minute == 0 | minute == 10 | minute == 20 | minute == 30 | minute == 40 | minute == 50))

#Plot sub-meter 1, 2 and 3 with title, legend and labels - 10 Minute frequency
plot_ly(houseDay10, x = ~houseDay10$DateTime, y = ~houseDay10$Sub_metering_1, name = 'Kitchen', type = 'scatter', mode = 'lines') %>%
  add_trace(y = ~houseDay10$Sub_metering_2, name = 'Laundry Room', mode = 'lines') %>%
  add_trace(y = ~houseDay10$Sub_metering_3, name = 'Water Heater & AC', mode = 'lines') %>%
  layout(title = "Power Consumption January 9th, 2008",
         xaxis = list(title = "Time"),
         yaxis = list (title = "Power (watt-hours)"))

------ ###não funcionou essa parte
  
#colocar os dias da semana em ordem (avisar o computador para 
#quando ele plotar)
#dados_energia5$weekday <- factor(dados_energia5$weekday, levels = c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday"))

# #week
# dados_energia5 %>% filter(year == 2008 & month == 1 & week == 2) %>% 
#   group_by(weekday) %>% 
#   summarise(Mean_Kitchen = mean(Sub_metering_1), Mean_Laundry = mean(Sub_metering_2), Mean_Temp = mean(Sub_metering_3))-> houseWeek
# 
# dados_energia5 %>% filter(year == 2008 & month == 1 & week == 2) -> houseWeek2
# 
# plot_ly(houseWeek, x = ~ houseWeek$weekday, y = ~ houseWeek$Mean_Kitchen, name = 'Kitchen', type = 'scatter', mode = 'lines') %>%
#   add_trace(y = ~ houseWeek$Mean_Laundry, name = 'Laundry Room', mode = 'lines') %>%
#   add_trace(y = ~ houseWeek$Mean_Temp, name = 'Water Heater & AC', mode = 'lines') %>%
#   layout(title = "Power Consumption Second Week January, 2008",
#          xaxis = list(title = "Time"),
#          yaxis = list (title = "Power (watt-hours)"))
# 
# plot_ly(houseWeek, x = ~ houseWeek$weekday, y = ~ houseWeek$Sub_metering_1, name = 'Kitchen', type = 'scatter', mode = 'lines') %>%
#   add_trace(y = ~ houseWeek$Sub_metering_2, name = 'Laundry Room', mode = 'lines') %>%
#   add_trace(y = ~ houseWeek$Sub_metering_3, name = 'Water Heater & AC', mode = 'lines') %>%
#   layout(title = "Power Consumption Second Week January, 2008",
#          xaxis = list(title = "Time"),
#          yaxis = list (title = "Power (watt-hours)"))
