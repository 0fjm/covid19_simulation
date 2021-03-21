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
        mainPanel(
            plotOutput("plot1"),
            textOutput("text1"),
            plotOutput("plot2")
        )
    )
))


server <- function(input, output) {
    #LIST OF VARIABLES NECESSARY FOR CALCULATIONS BASED ON LELIEVELD (2020)
    
    ##RNA for 50% infection probability (D50)
    RNA_num <- 316
    
    #DEPOSITION PROPABILITY
    deposition_prob <- 0.5
    
    #emission breathing [/cm³]
    emission_breathing <- 0.06
    
    #emission speaking [/cm³]
    emission_speaking <- 0.6
    
    #speaking / breathing ratio
    speaking_breathing_rat <- 0.1
    
    #respiratory rate [l/min]
    resp_rate <- 10
    
    #respiratory fluid RNA conc [/cm³]
    resp_fluid_RNA_conc <- 500000000
    
    #mean wet aerosol diameter [um]
    mean_aer_dia <- 5
    
    #infectious episode [d] *
    inf_epis <- 2
    
    #virus lifetime in aerosol [h]
    vir_life <- 1.7
    
    #Room Area ** = 10qm times #People --> Goes into user input
    num_people <- 6 #Temp Var (comes from the user normally)
    room_area <- function(num_people) {
        return (num_people * 10)
    }
    
    #room height [m]** ** Very large room sizes violate the assumption of instantaneaous mixing of the air
    room_height <- 3
    
    #infection probability [/RNA] calculated with RNA_num
    infect_prob <- function(RNA_num) {
        return (1 - 10 ^ (log10(0.5) / RNA_num))
    }
    
    #RNA content in aerosol
    RNA_content <- function(resp_fluid_RNA_conc, mean_aer_dia) {
        return (resp_fluid_RNA_conc * pi / 6 * (mean_aer_dia / 10000) ^ 3)
    }
    
    #aerosol emission [/h]
    aero_emission <-
        function(emission_breathing,
                 emission_speaking,
                 speaking_breathing_rat,
                 resp_rate) {
            return ((
                emission_breathing * (1 - speaking_breathing_rat) + emission_speaking *
                    speaking_breathing_rat
            ) * 1000 * resp_rate * 60
            )
        }
    
    
    
    #aerosol conc [/l]
    var_aer_conc <-
        aero_emission(emission_breathing,
                      emission_speaking,
                      speaking_breathing_rat,
                      resp_rate)
    var_room_area <- room_area(num_people)
    aero_conc <-
        function(var_aer_conc,
                 var_room_area,
                 room_height) {
            return (var_aer_conc / (var_room_area * room_height * 1000))
        }
    
    
    #RNA cont. aerosol conc [/l]
    var_aero_conc <- aero_conc(var_aer_conc,
                               var_room_area,
                               room_height)
    
    RNA_cont_aero_conc <- var_aero_conc * var_RNA_content
    
    
    var_RNA_cont_aero_conc <- var_aero_conc * var_RNA_content
    
    
    #RNA dosis [/h]
    RNA_dosis <-
        function(var_RNA_cont_aero_conc,
                 resp_rate,
                 deposition_prob) {
            return(var_RNA_cont_aero_conc * resp_rate * deposition_prob * 60)
        }
    var_RNA_dosis <- RNA_dosis(var_RNA_cont_aero_conc,
                               resp_rate,
                               deposition_prob)
    var_RNA_dosis
    
    
    #Rechte Seite der Excel ab hier
    
    
    
    
    
    
    
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
        y <- runif(50, 2, 8)
        plot(y, exp(y))
        
        
        if (FALSE) {
            x <- input$n
            y <- (input$n ^ 2)
            
            ggplot(data = NULL , aes(x , y)) + geom_line()
        }
        
    })
}

# Run the application
shinyApp(ui = ui, server = server)
