#------------------------------------------------------------------------------------ 
#Goal: Shiny package
#Description: Understand how to work the package 2
#Developer: Letícia Marçal
#----------------------------------------------------------------------

#linkar com forecast
# source("Scripts/Forecast.R")

#libraries
library(shiny)
library(shinydashboard)
library(ggplot2)
library(readr)
library(lubridate)
library(dplyr)
library(shinyjs)
library(shinyBS)
library(shinyWidgets)

#file
dados_energia <- read.csv2(file = "C:/Users/letic/Documents/UbiqumR/IoT Project Data and Code/Data/household_power_consumption.txt", sep = ";")


ui <- dashboardPage(
  skin = "purple",
  dashboardHeader(title = "Power Consumption",
                  titleWidth = 230
  ),
  dashboardSidebar(width = 230,
                    sidebarMenu(id = "tabs",
                      menuItem(tabName = "Saving",
                               text = "Saving or Spending?"),
                      menuItem(tabName = "Month by Month", text = "Month by Month")
                    ),
                    uiOutput("out1")
  ),
  dashboardBody(
    conditionalPanel(
      condition = "input.tabs == 'Saving'", 
        fluidPage(
          imageOutput('dashPic'),
        box(
          selectInput(
                    inputId = "select_input",
                    label = "Select time:",
                    choices = c("day","month","year") )
          , width = 2, height = 2)
        
    )),
    conditionalPanel(
      condition = "input.tabs == 'Month by Month'", 
          fluidRow(
            box(
              sliderInput(
                inputId = "Year",
                label = "Year",
                min = 2007, max = 2010, value = 2008
              ),
              selectInput(
                inputId = "Month",
                label = "Month",
                choices = unique(month(energy_month2$month))
              ),
              plotOutput(outputId = "chartMonth")
          )
        )
     )
    )
  )

 

server <- function(input, output) {
  
  output$dashPic <- renderImage({
    # load the images
    # dashPic <- normalizePath(path = file.path("Images/dash3.png"))
    
    if (input$select_input == "day") {
      dashPic = normalizePath(path = file.path("Images/dashD.png"))
    } else if (input$select_input == "month") {
      dashPic = normalizePath(path = file.path("Images/dashM.png"))
    } else if (input$select_input == "year") {
      dashPic = normalizePath(path = file.path("Images/dashY.png"))
    }
    # return a list
    list(src = dashPic)
    
  }, deleteFile = FALSE)
  
  output$chartMonth <- renderPlot({
    energy_month2 %>% 
      filter(year == input$Year, month == input$Month) %>% 
      ggplot(aes(x = factor(Room,levels = c("House", "Others",
                                            "Heater", "Kitchen",
                                            "Laundry")) , y = Energy_Wh)) + 
      geom_col(color = "chartreuse3", fill = "chartreuse3") +
      scale_y_continuous(labels = comma) +
      xlab("Rooms") +
      theme_minimal()
  })
}


shinyApp(ui, server)


#####

ui <- dashboardPage(
  skin = "purple",
  dashboardHeader(title = "Power Consumption",
                  titleWidth = 230
  ),
  dashboardSidebar(width = 230,
                   sidebarMenu(id = "tabs",
                               menuItem(tabName = "Saving",
                                        text = "Saving or Spending?"),
                               menuItem(tabName = "Month by Month", text = "Month by Month")
                   ),
                   uiOutput("out1")
  ),
  dashboardBody(
    conditionalPanel(
      condition = "input.tabs == 'Saving'", 
      fluidRow(
        box(
          imageOutput('dashPic'), width = 8, height = 730),
        box(
          selectInput(
            inputId = "select_input",
            label = "Select time:",
            choices = c("day","month","year") )
          , width = 2, height = 2)
        
      )),
    conditionalPanel(
      condition = "input.tabs == 'Month by Month'", 
      fluidRow(
        box(
          sliderInput(
            inputId = "Year",
            label = "Year",
            min = 2007, max = 2010, value = 2008
          ),
          selectInput(
            inputId = "Month",
            label = "Month",
            choices = unique(month(energy_month2$month))
          ),
          plotOutput(outputId = "chartMonth")
        )
      )
    )
  )
)



server <- function(input, output) {
  
  output$dashPic <- renderImage({
    # load the images
    # dashPic <- normalizePath(path = file.path("Images/dash3.png"))
    
    if (input$select_input == "day") {
      dashPic = normalizePath(path = file.path("Images/dashD2.png"))
    } else if (input$select_input == "month") {
      dashPic = normalizePath(path = file.path("Images/dashM2.png"))
    } else if (input$select_input == "year") {
      dashPic = normalizePath(path = file.path("Images/dashY2.png"))
    }
    # return a list
    list(src = dashPic)
    
  }, deleteFile = FALSE)
  
  output$chartMonth <- renderPlot({
    energy_month2 %>% 
      filter(year == input$Year, month == input$Month) %>% 
      ggplot(aes(x = factor(Room,levels = c("House", "Others",
                                            "Heater", "Kitchen",
                                            "Laundry")) , y = Energy_Wh)) + 
      geom_col(color = "chartreuse3", fill = "chartreuse3") +
      scale_y_continuous(labels = comma) +
      xlab("Rooms") +
      theme_minimal()
  })
}


shinyApp(ui, server)
