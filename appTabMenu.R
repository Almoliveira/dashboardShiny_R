library(shinydashboard)

ui <- dashboardPage(
  dashboardHeader(title = "Sampling"), # DashBoardHeader
  
  
  
  dashboardSidebar(
    sidebarMenu(
      menuItem( "Uniform Distribution", tabName = "uniform", icon =
                  icon("square")),
      menuItem("Normal Distribution", tabName = "normal",
               icon = icon("bell-o"))
    ) # sidebarMenu
  ), # dashboardSidebar
  
  dashboardBody(
    
    tabItems(
      tabItem(
        tabName = "uniform",
        fluidRow(
          box(
            title = "Select a Number",
            solidHeader = TRUE,
            background = "yellow",
            status="warning",
            height = 312,
            sliderInput(inputId = "number",
                        label = "",
                        value = 500, min = 25,
                        max = 1000)),
          box(title = "Histogram",
              solidHeader=TRUE,
              background = "light-blue",
              status="primary",
              plotOutput("hist", height = 250)),
          valueBoxOutput("meanBox"),
          valueBoxOutput("medianBox"),
          valueBoxOutput("sdBox")
        )
      ),
      tabItem(tabName = "normal",
              fluidRow(
                box(title = "Select a Number",
                    solidHeader = TRUE,
                    collapsible = TRUE,
                    status="warning",
                    sliderInput(inputId = "normnumber",
                                label = "",
                                value = 500, min = 25,
                                max = 1000)),
                box(title = "Density Plot",
                    solidHeader=TRUE,
                    background = "light-blue",
                    status="primary",
                    plotOutput("density", height = 250)),
                infoBoxOutput("meanInfoBox"),
                infoBoxOutput("medianInfoBox"),
                infoBoxOutput("sdInfoBox")
              )
      )
      
    ) # tabItems
  ) # dashboardBody
) # dashboardPage



server <- function(input, output) {
  
  histdata <- reactive({runif(input$number,min=0,max=1)})
  densitydata <- reactive({rnorm(input$normnumber)})
  
  output$hist <- renderPlot({
    hist(histdata(),xlab="Value",
         main=paste(input$number,
                    "random values between 0 and 1"))
  })
  
  output$density <- renderPlot({
    hist(densitydata(),xlab="Value",
         main=paste("standard normal distribution \n",
                    input$normnumber,"random values"),
         probability=TRUE)
    lines(density(densitydata()))})
  
  output$meanBox <- renderValueBox({
    valueBox(
      round(mean(histdata()),3),"Mean",
      color = "navy"
    )
  })
  output$medianBox <- renderValueBox({
    valueBox(
      round(median(histdata()),3),"Median",
      color = "aqua"
    )
  })
  output$sdBox <- renderValueBox({
    valueBox(
      round(sd(histdata()),3), "Standard Deviation",
      color = "blue"
    )
  })
  
  output$meanInfoBox <- renderInfoBox({
    infoBox("Mean",
            round( mean(densitydata()),3),
            icon=icon("align-center"),
            color = "navy")
  })
  output$medianInfoBox <- renderInfoBox({
    infoBox(icon=icon("area-chart"), "Median",
            round(median(densitydata()),3),
            color = "aqua")
  })
  output$sdInfoBox <- renderInfoBox({
    infoBox("Standard Deviation",
            round(sd(densitydata()),3),icon=icon("scribd"),
            fill = TRUE,
            color = "blue")
  })
  
}

shinyApp(ui, server)
