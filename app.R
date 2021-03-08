#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)

# Define UI for application that draws a histogram
ui <- fluidPage(
    
    verticalLayout(
        sidebarPanel(
            sliderInput("n",
                        "Number of People in the Office:",
                        min = 1, #Min Value of the Slider
                        max = 10, #Max Value 
                        value = 4) #Startwert
        ),
        sidebarPanel(
            selectInput("masks",
                        "Masks:",
                        c("No Mask" = "No",
                          "OP Mask" = "OP",
                          "FFP2 Mask" = "FFP"
                          )),
            
                        
                        
        ),
        sidebarPanel(
            selectInput("air",
                        "Air Ventilation in the Room",
                        c(
                            "None" = "no",
                            "Window cracked open" = "wo",
                            "Brief active ventilation for min 10min./hr." = "br",
                            "Ventilation system" = "vs"
                        ))
        )
    ),
    mainPanel(
        textOutput("masks"),
        plotOutput("n")
    )
    
    
    
)


server <- function(input, output) {
    
    output$masks <- renderText(
        {
            paste("You have chosen:", input$masks)
        }
    )
    output$n <- renderPlot({
        # generate n based on input$n from ui.R
        x    <- faithful[, 2]
        n <- seq(min(x), max(x), length.out = input$n + 1)
        
        # draw the histogram with the specified number of n
        hist(x, breaks = n, col = 'darkgray', border = 'white')
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
