library(shinydashboard)

ui <- dashboardPage(
    dashboardHeader(
        title = "Uniform Distribution"
    ),
    dashboardSidebar(),
    
    dashboardBody(
        
        fluidRow(
            box(
                title = "Select a Number",
                background = "yellow",
                status = "warning",
                height = 312,
            sliderInput(inputId = "number",
                        label = "",
                        value = 500, min = 25, max = 1000)),
            box(title= "Histogram",
                background = "light-blue",
                status = "primary",
                plotOutput("hist",height = 250))
        ),
        valueBoxOutput("meanBox"),
        valueBoxOutput("medianBox"),
        valueBoxOutput("sdBox"),
    )
)

server <- function(input, output) {
    histdata <- reactive({runif(input$number, min=0, max=1)})
    
    output$hist <- renderPlot({
        hist(histdata(), xlab="Value", main=paste
             (input$number, "Random values between 0 and 1"))
    })
    
    output$meanBox <- renderValueBox({
        valueBox(
            round(mean(histdata()), 3),"Mean",
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
            round(sd(histdata()),3),"Standard Deviation",
            color = "blue"
        )
    })
}

shinyApp(ui, server)