#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#
library(shiny)
library (dplyr)
library (ggplot2)
#library(gapminder)


# Define UI for application that draws a histogram
ui <- shinyUI(fluidPage(
    titlePanel("Covid19-Simulation"),
    sidebarLayout(
        position = "left",
        sidebarPanel(
            "sidebar panel",
            sliderInput(
                "n",
                "Number of People in the Office:",
                min = 1,
                #Min Value of the Slider
                max = 10,
                #Max Value
                value = 4
            ),
            #Startwert
            
            
            selectInput(
                "masks",
                "What masks do you wear in the office?",
                c(
                    "No Mask" = "No",
                    "OP Mask" = "OP",
                    "FFP2 Mask" = "FFP"
                )
            ),
            
            selectInput(
                "air",
                "Air Ventilation in the Room (Funktioniert noch nicht, muss berechnet werden --> Platzhalter aktuell",
                c(
                    "None" = "no",
                    "Window cracked open" = "wo",
                    "Brief active ventilation for min 10min./hr." = "br",
                    "Ventilation system" = "vs"
                )
            )
        ),
        mainPanel(plotOutput("plot1"),
                  textOutput("text1"),
                  plotOutput("plot2"))
    )
))


server <- function(input, output) {
    #ran
    
    output$text1 <- renderText({
        if (input$masks == "No") {
            ar <- "40%"
        } else if (input$masks == "OP") {
            ar <- "20%"
        } else if (input$masks == "FFP") {
            ar <- "5%"
        }
        
        
        paste(c("Dein Ansteckungsrisiko: ", ar), collapse = "")
    })
    
    output$plot1 <- renderPlot({
        
        y<-runif(50,2,8)
        plot(y,exp(y))
        
        
        if (FALSE) {
        x <- input$n
        y <- (input$n ^ 2)
        
        ggplot(data = NULL , aes(x , y)) + geom_line()
        }
        
    })
}

# Run the application
shinyApp(ui = ui, server = server)
