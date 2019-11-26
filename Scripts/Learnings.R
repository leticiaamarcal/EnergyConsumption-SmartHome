Learnings

#procurar NA. O dataset é muito grande. Leva muito tempo para ordenar 
#as colunas para entender onde estão. Então tem que usar funções para entender
#onde estão. Por default, o computador lê missing values como NA. Se
#você não "avisar"que podem exister outras maneiras de NAs, ele não vai encontrar.
#Por isso especifiquei na leitura do datase (read_csv2) que os NAs podem 
#estar escritos de outra maneira. Abaixo, veja o caminho feito para 
#localizar os NAs (entender a posiçao deles). Antes de mudar o parametro
#em read_csv2, eu só consegui encontrar NAs em sub_metering_3. Quando
#localizei, entendi que tinham linhas como ? que não estavam sendo 
#lidas comos NA.
# sum(is.na(dados_energia))
# sum(is.na(dados_energia$Date))
# sum(is.na(dados_energia$Sub_metering_3))
# dados_energia[is.na(dados_energia$Sub_metering_3),]



#Convert DateTime from POSIXlt to POSIXct/ avisar o computador que é data
#essa funçao é boa quando vc precisa usar a time zone
#poderia colocar separado
#attr(dados_energia3$DateTime, "tzone") <- "Europe/Paris"
#Para que o computador entenda e padronize como "date", é preciso ter 
#a data e o horário na mesma coluna
#ver mais em https://www.stat.berkeley.edu/~s133/dates.html
dados_energia3$DateTime <- as.POSIXct(dados_energia3$DateTime,
                                      "%d/%m/%Y %H:%M:%S",
                                      tz = "GMT")

# outra maneira
# dados_energia3 <- as_datetime(dados_energia3$DateTime)
#ver funçao as.Date

#checar dados
# str(dados_energia3)
# summary(dados_energia3)
# head(dados_energia3)
# tail(dados_energia3)


#outro código para separar por ano, por exemplo:
# yourdataframe$year <- year(yourdataframe$DateTime)
#Like "year", Lubridate also has functions to create
#attributes for quarter, month, week, weekday, day, hour and minute


#month() é uma funçao. você usa o DateTime e diz que vc quer só o mês
#label é o parametro que escolhe se mostra o mês como numero ou nome
#se colocar true, vai mostrar como Janeiro. abbr é sobre abreviar
#True vai abreviar. dentro do ggplot, o group e col é para criar
#as diferentes linhas para cada ano. Factor é para transformar
#em factor (para não ser lido como numero continuo)

##gather - exemplos:
#DF %>% gather(nameColumn1, nameColumn2, atrributeX:attributeY) #attX TO attY
#DF %>% gather(nameColumn1, nameColumn2, -attributeX, -attributeY) #except for
#DF %>% gather(nameColumn1, nameColumn2, 3:6) #number of the column (3 TO 6)
#DF %>% gather(nameColumn1, nameColumn2, attX, attY, attZ, attW) #all att you want

#col1 é o nome da nova coluna que vai agreagar as diferentes variáveis
#col2 é o nome da coluna que vai levar os valores numericos
#a terceira parte é sobre selecionar os atributos que vão ser gathered

#como ceomeça o dashboard


ui <- dashboardPage(
  dashboardHeader(),
  dashboardSidebar(),
  dashboardBody()
)

server <- function(input, output) { }

shinyApp(ui, server)




