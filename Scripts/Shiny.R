#------------------------------------------------------------------------------------ 
#Goal: Shiny package
#Description: Understand how to work the package
#Developer: Letícia Marçal
#----------------------------------------------------------------------

#libraries
library(shiny)
library(shinydashboard)

#criar template
ui <- fluidPage(
  
)

server <- function(input, output) {
  
}

shinyApp(ui, server)

###First step
dashboardPage(
  dashboardHeader(),
  dashboardSidebar(),
  dashboardBody()
)

###Second step
ui <- dashboardPage(
  dashboardHeader(),
  dashboardSidebar(),
  dashboardBody()
)

server <- function(input, output) { }

shinyApp(ui, server)

###Third step

#o template está me branco. temos que personalizar
#We are going to change our fluid page inside our ui (user interface)
#to a dashboard page, with a header, a sidebar and a body: 
#To add them, modify the fluidPage() to dashboardPage(), 
#and add the following functions inside:
  
ui <- dashboardPage(
  dashboardHeader(title = "Power Panel"),
  dashboardSidebar(),
  dashboardBody(fluidRow(
    box(plotOutput("plot1", height = 250)),
    box(title = "Controls",
      sliderInput("slider", "Number of observations:", 1, 100, 50))
  )
  )
)


server <- function(input, output) {

set.seed(122)
  
histdata <- rnorm(500)
  
output$plot1 <- renderPlot({
    data <- histdata[seq_len(input$slider)]
    hist(data)
  })
}

shinyApp(ui, server)

###Fourth step

#Next, we can add content to the sidebar. For this example we’ll 
#add menu items that behave like tabs. These function similarly 
#to Shiny’s tabPanels: when you click on one menu item, 
#it shows a different set of content in the main body.
#There are two parts that need to be done. First, you need to add 
#menuItems to the sidebar, with appropriate tabNames.

#Sidebar content
dashboardSidebar(
  sidebarMenu(
    menuItem("Dashboard", tabName = "dashboard", icon = icon("dashboard")),
    menuItem("Widgets", tabName = "widgets", icon = icon("th"))
  )
)

#Body content
dashboardBody(
  tabItems(
    # First tab content
    tabItem(tabName = "dashboard",
            fluidRow(
              box(plotOutput("plot1", height = 250)),
              box(
                title = "Controls",
                sliderInput("slider", "Number of observations:", 1, 100, 50)
              )
            )
    ),
    
    # Second tab content
    tabItem(tabName = "widgets",
            h2("Widgets tab content")
    )
  )
)

#vou colocar dentro da fórmula toda

ui <- dashboardPage(
  dashboardHeader(title = "Power Panel"),
  dashboardSidebar(sidebarMenu(menuItem("Dashboard", tabName = "dashboard", icon = icon("dashboard")),
                               menuItem("Widgets", tabName = "widgets", icon = icon("th")))))
   dashboardBody(fluidRow(box(plotOutput("plot1", height = 250)),
    box(title = "Controls", sliderInput("slider", "Number of observations:", 1, 100, 50))))
  tabItems(tabItem(tabName = "dashboard",
                   fluidRow(box(plotOutput("plot1", height = 250)),
                            box(title = "Controls", sliderInput("slider", "Number of observations:", 1, 100, 50)))),
           tabItem(tabName = "widgets", h2("Widgets tab content")))
  
server <- function(input, output) {
  
  set.seed(122)
  
  histdata <- rnorm(500)
  
  output$plot1 <- renderPlot({
    data <- histdata[seq_len(input$slider)]
    hist(data)
  })
}

shinyApp(ui, server)




#####################

#outro pipeline

ui <- dashboardPage(
  dashboardHeader(),
  dashboardSidebar(
    sliderInput(
      inputId = "animation",
      label = "whatever and ever",
      min = 10, max = 50, value = 25
    ),
    selectInput(
      inputId = "year",
      label = "Year",
      choices = c(2007,2008,2009,2010) #unique(year(data$date))
    )
  ),
  dashboardBody(
    fluidRow(
      box(
        plotOutput(outputId = "Active_Energy_line")
      ),
      box(
        plotOutput(outputId = "Active_Energy_hist")
      )
    )
  )
)


server <- function(input, output) {
  
  output$Active_Energy_line <- renderPlot({
    data %>% 
      filter(year(date) == input$year) %>% 
      ggplot() + 
      geom_line(aes(x = date, y = ActiveEnergy_avg))
  })
  
  output$Active_Energy_hist <- renderPlot({
    hist(data$ActiveEnergy_avg, breaks = input$animation)
  })
}

shinyApp(ui, server)
