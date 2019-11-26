#------------------------------------------------------------------------------------ 
#Goal: Create a dashboard
#Description: Dashboard for IoT Project
#Developer: Letícia Marçal
#---------------------------------------------------------------------------------

#linkar com forecast
source("Scripts/Preprocess.R")

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



ui <- dashboardPage(
  skin = "purple",
  dashboardHeader(title = "Power Consumption",
                  titleWidth = 230
  ),
  dashboardSidebar(width = 230,
                   sidebarMenu(id = "tabs",
                               menuItem(tabName = "Saving",
                                        text = "Saving or Spending?", icon = icon("charging-station")),
                               menuItem(tabName = "Month by Month", 
                                        text = "Month by Month",
                                        icon = icon("chart-bar"))
                   ),
                   uiOutput("out1")
  ),
  dashboardBody(
    conditionalPanel(
      condition = "input.tabs == 'Saving'", 
      fluidRow(
        column(width = 7,
               box(
                 imageOutput('dashPic'), width = 15, height = 730
               )),
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
        valueBox(65, "This bill was", icon = icon("euro-sign"), color = "yellow"), 
        valueBox(67, "Last bill was", icon = icon("credit-card"), color = "maroon"),
        valueBox(71, "Next bill may be ", icon = icon("chart-line"), color = "blue")
      ),
      fluidRow(
        box(
          width = 6,
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
        ),
        box(
          title = "Forecasting (Wh)", background = "black", solidHeader = TRUE,
          width = 6,
          plotOutput(outputId = "chartPrev")
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
      ylab("Power") +
      theme_minimal() +
      ggtitle("Power Consumption (Wh) - Previous Months")
    
  }) 
  
  output$chartPrev <- renderPlot({  
    energy_month2 %>% 
      filter(year == 2008 & month == 9) %>% 
      ggplot(aes(x = factor(Room,levels = c("House", "Others",
                                            "Heater", "Kitchen",
                                            "Laundry")) , y = Energy_Wh)) + 
      geom_col(color = "cadetblue3", fill = "cadetblue1") +
      scale_y_continuous(labels = comma) +
      xlab("Rooms") +
      ylab("Power") +
      theme_minimal()
  })
}


shinyApp(ui, server)
