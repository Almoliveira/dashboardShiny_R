library(shinydashboard)

ui <- dashboardPage(
    dashboardHeader(title = "Uniform Distribution"),
    dashboardSidebar(),
    
    dashboardBody(fluidRow(
        column(
            width = 6,
            box(
                title = "Select a Number",
                solidHeader = TRUE,
                background = "yellow",
                width = NULL,
                status = "warning",
                height = 312,
                sliderInput(
                    inputId = "number",
                    label = "",
                    value = 500,
                    min = 25,
                    max = 1000
                )
            ),
            box(
                title = "Histogram",
                solidHeader = TRUE,
                background = "light-blue",
                status = "primary",
                width = NULL,
                plotOutput("hist", height = 250)
            )
            
        ),
        column(
            width = 6,
            
            
            tabBox(
                title = "Central Tendency",
                id = "tabs2", height = 150, width = NULL,
                tabPanel("Mean",
        h2(textOutput("meantext")),width = NULL),
                tabPanel("Median", 
        h2(textOutput("mediantext")), width = NULL)
                
            ),
            
            tabBox(
                title  = "Variability",
                id = "tabs2", height = 150, width = NULL,
                side = "right",
                tabPanel("Variance",
        h2(textOutput("vartext")),width = NULL),
                tabPanel("Standard Deviaton",
        h2(textOutput("sdtext")), width = NULL))
            )
        )
        
        
    )
  )


server <- function(input, output) {
    histdata <- reactive({
        runif(input$number, min = 0, max = 1)
    })
    
    output$hist <- renderPlot({
        hist(
            histdata(),
            xlab = "Value",
            main = paste
            (input$number, "Random values between 0 and 1")
        )
    })
    
    output$meantext <- renderText({
        paste("Mean =", round(mean(histdata()),3))})
    
    output$mediantext <- renderText({
        paste("Median =", round(median(histdata()),3))})
    
    output$vartext <- renderText({
        paste("Variance =", round(var(histdata()),3))})
    
    output$sdtext <- renderText({
        paste("Standard Deviation =",
              round(sd(histdata()),3))})
 
}

shinyApp(ui, server)