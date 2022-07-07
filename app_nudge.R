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
library(shinyWidgets)
library(bslib)
#thematic::thematic_shiny(font = "auto")
#library(gapminder)
library(rsconnect)
library(gridExtra)
library(png)
library(grid)

#library(shinyjs)
library(shinydashboard)


#Before running, setwd() to current source file directory

if (FALSE) {
  rsconnect::setAccountInfo(name='fau-erl-n',
                            token='E165BAE7FA06AB3DAC9A78987E8697BB',
                            secret='udZ2bilFkSd5DRzlLW3HUW1ZNarvIjZy0zjPfzz4')
  
  rsconnect::deployApp(getwd())
  
  rsconnect::showLogs()
}


# Define UI for application that draws a histogram
# Define UI for application that draws a histogram
ui <- shinyUI(
  fluidPage(
    #tags$table(
    # tags$link(rel = "stylesheet", type = "text/css", href = "table_style.css")
    #),
    
    theme = bs_theme(version = 4, bootswatch = "journal"),
    # theme = bs_theme(),
    chooseSliderSkin(skin = "Modern", color = "DodgerBlue"),
    
    titlePanel(div(
      HTML("<b style=\"color:DodgerBlue;\">COVID-19 Infektionsrechner</b>")
    )),
    #  fluidRow(#column(12,"Mit diesem Dashboard können Sie die Wahrscheinlichkeit berechnen, sich in einem Raum durch Aerosole an Covid-19 anzustecken."),
    #   column(12, div(
    #    HTML("<h3><b> Ihr Raumsetting:</b></h3>")
    # ))),
    
    
    fluidRow(
      column(4, sliderInput("people",
                            "Personen im Raum:",
                            2, 20, 4)) ,
      
      column(4, sliderInput("raum",
                            "Raumgröße (in m²):",
                            10, 40, 20)),
      column(4, sliderInput(
        "zeit",
        "Aufenthaltsdauer (in Std.):",
        1, 16, 6
      )),
      
    ),
    
    
    
    (div(HTML(
      "<h3><b>Maßnahmen</b></h3>"
    ))),
    sidebarLayout(
      position = "left",
      #Hier noch Siizing!!!!
      sidebarPanel(
        selectInput(
          "distance",
          "Abstand:",
          c("Weniger als 1,5m" = "k1m",
            "Mehr als 1,5m" = "m1m")
        ),
        
        selectInput(
          "masks",
          "Masken:",
          c(
            "Keine" = "No",
            #  "Alltagsmasken" = "AT",
            "OP Masken" = "OP",
            "FFP2 Masken" = "FFP2"
          )
        ),
        
        
        
        selectInput(
          "air",
          "Lüftung:",
          c(
            #Werte müssen noch angepasst werden
            "Nie" = "NoAir",
            "Fenster dauerhaft gekippt" = "FensterGekippt",
            "Regelmäßiges Stoßlüften (10min/h)" = "Stoßlueften"
            #"Lüftungssystem" = "Lueftungssystem"
          )
        ),
        #htmlOutput("researcher"),
        htmlOutput("attention"),
        imageOutput("icons")
        
      ),
      
      mainPanel(fluidRow(
        column(6,
               plotOutput("text2")),
        column(
          6,
          #fluidRow(tableOutput("table")),
          fluidRow(htmlOutput("header")),
          fluidRow(htmlOutput("text3")),
          fluidRow(htmlOutput("text6")),
          fluidRow(htmlOutput("text1")),
          fluidRow(imageOutput("image"))
        )
      ))
      
      
    )
  )
)
server <- function(input, output, session) {
  #bs_themer()
  
  output$researcher <- renderText({
    paste(    "<strong style=\"font-size:20px;\">",
              
              "Das halten Wissenschaftler von Ihrer Wahl:",
              
              "</strong>")
  })
  
  output$icons <- renderImage({
    if(((input$distance == "k1m" || input$distance == "m1m")  && input$masks == "No" && input$air == "NoAir") || 
       (input$distance == "k1m"  && input$masks == "No" && input$air == "FensterGekippt")) {
      
      input3 <- "smiley_1"
  
      #Rotes Bild
    } else if ((input$distance == "m1m" && input$masks == "OP" && (input$air == "NoAir" || input$air == "FensterGekippt" )) ||
               (input$distance == "m1m"  && input$masks == "No" && (input$air == "FensterGekippt" || input$air == "Stoßlueften" )) ||
               (input$distance == "k1m" && input$masks == "OP" && input$air == "NoAir") ||
               (input$distance == "k1m" && input$masks == "OP" &&  input$air == "FensterGekippt" ) ||
               (input$distance == "k1m" && input$masks == "No" && input$air == "Stoßlueften" )
    ) {
      input3 <- "smiley_2"
      #Gelbes Bild
    } else {
      input3 <- "smiley_3"
      #Grünes Bild
    }
    
    
    filename <- normalizePath(file.path('./images',
                                        paste(input3, '.png', sep = '')))
    
    list(src = filename,
         width = "100px",
         alt = paste(input3, "0"))
    
    
  }, deleteFile = FALSE)
  
  output$attention <-renderText({
    
    if(((input$distance == "k1m" || input$distance == "m1m")  && input$masks == "No" && input$air == "NoAir") || 
       (input$distance == "k1m"  && input$masks == "No" && input$air == "FensterGekippt")) {
      paste(
        "<strong style=\"font-size:25px;\">",
        "<font style=\"color:Red;\">",
        "Vorsicht, erhöhte Infektionswahrscheinlichkeit!",
        "</font>",
        "</strong>"
      )
      
      
    } else if ((input$distance == "m1m" && input$masks == "OP" && (input$air == "NoAir" || input$air == "FensterGekippt" )) ||
               (input$distance == "m1m"  && input$masks == "No" && (input$air == "FensterGekippt" || input$air == "Stoßlueften" )) ||
               (input$distance == "k1m" && input$masks == "OP" && input$air == "NoAir") ||
               (input$distance == "k1m" && input$masks == "OP" &&  input$air == "FensterGekippt" ) ||
               (input$distance == "k1m" && input$masks == "No" && input$air == "Stoßlueften" )
               ) {
      paste (
        "<strong style=\"font-size:25px;\">",
        "<font style=\"color:Orange;\">",
        "Weiter so, Sie können Ihre Infektionswahrscheinlichkeit immer noch reduzieren!",
        "</font>",
        "</strong>"
      )
    } else {
      paste (
        "<strong style=\"font-size:25px;\">",
        "<font style=\"color:Green;\">",
        "Sehr gut, minimierte Infektionswahrscheinlichkeit!",
        "</font>",
        "</strong>"
      )
    }
    
    

    
  })
  
  output$header <- renderText({
    paste (
      
      
      "<strong>",
      "Infektionswahrscheinlichkeiten",
      "</strong>")
  })
  
  output$table <- renderText({
    #border=\"3\">
    #LIST OF VARIABLES NECESSARY FOR CALCULATIONS BASED ON LELIEVELD (2020)
    
    ##RNA for 50% infection probability (D50)
    RNA_num <- 233
    
    #DEPOSITION PROPABILITY
    deposition_prob <- 0.5
    
    #emission breathing [/cm³]
    emission_breathing <- 0.06
    
    #emission speaking [/cm³]
    emission_speaking <- 0.8
    
    #speaking / breathing ratio
    speaking_breathing_rat <- 0.1
    
    #respiratory rate [l/min]
    resp_rate <- 10
    
    #respiratory fluid RNA conc [/cm³]
    resp_fluid_RNA_conc <- 500000000
    
    #mean wet aerosol diameter [um]
    mean_aer_dia <- 5
    
    #infectious episode [d] *
    
    
    zeit <- input$zeit
    
    
    
    inf_epis <- 0.16666666667 * zeit
    
    #virus lifetime in aerosol [h]
    vir_life <- 1.7
    #browser()
    
    #Das hier ändern in
    #https://www.sciencedirect.com/science/article/abs/pii/S2210670720306119
    #Wenn Distance 1.5m, dann finaler Wert * 0.8, z1u15 dann * 0.9, wenn k1m dann nix
    
    
    #observe(print(numpeople()))
    
    #Room Area in m square based on num people
    
    
    
    room_area <- input$raum
    
    
    
    #room height [m]** ** Very large room sizes violate the assumption of instantaneaous mixing of the air
    room_height <- 2.4
    
    #infection probability [/RNA] calculated with RNA_num
    infect_prob <- function(RNA_num) {
      return (1 - 10 ^ (log10(0.5) / RNA_num))
    }
    var_infect_prob <- 1 - 10 ^ (log10(0.5) / RNA_num)
    
    #RNA content in aerosol
    RNA_content <- function(resp_fluid_RNA_conc,
                            mean_aer_dia) {
      return (resp_fluid_RNA_conc * pi / 6 * (mean_aer_dia / 10000) ^ 3)
    }
    
    var_RNA_content <-
      (resp_fluid_RNA_conc * pi / 6 * (mean_aer_dia / 10000) ^ 3)
    
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
    var_aero_emission <-
      aero_emission(emission_breathing,
                    emission_speaking,
                    speaking_breathing_rat,
                    resp_rate)
    
    
    #aerosol conc [/l]
    
    aero_conc <-
      function(var_aero_emission,
               room_area,
               room_height) {
        return (var_aero_emission / (room_area * room_height * 1000))
      }
    var_aero_conc <- aero_conc(var_aero_emission,
                               room_area,
                               room_height)
    
    #RNA cont. aerosol conc [/l]
    
    
    
    
    var_RNA_cont_aero_conc <-
      var_aero_conc * var_RNA_content
    
    
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
    
    #room ventilation rate [/h]
    #Normally generated through user input
    if (input$air == "NoAir") {
      room_vent_rate <- 0
    } else if (input$air == "FensterGekippt") {
      room_vent_rate <- 0.35
    } else if (input$air == "Stoßlueften") {
      room_vent_rate <- 2
    } #else if (input$air == "Lueftungssystem") {
    #room_vent_rate <- 9
    #}
    
    
    #total mask efficiency (exhaling + inhaling)
    total_mask_effic <-
      0 #Normally generated through user input
    
    if (input$masks == "No") {
      total_mask_effic <- 0
      
    } else if (input$masks == "OP") {
      total_mask_effic <- 0.7
    } else if (input$masks == "FFP2") {
      total_mask_effic <- 0.95
    }
    
    
    
    # susceptible # persons / room
    
    #Normally generated through user input
    #dosis 6 hours (per  day)*
    dosis_6_hours <-
      var_RNA_dosis / (room_vent_rate + 1 / vir_life) * (1 - total_mask_effic) *
      6
    
    #dosis infectious episode
    dosis_infectious_episode <- dosis_6_hours * inf_epis
    
    #infection risk of individual person [% / episode]
    infect_risk_individual <-
      (1 - (1 - var_infect_prob) ^ dosis_infectious_episode) * 100
    
    #risk of 1  person in room being infected [% / episode]
    
    
    
    num_people <- input$people
    
    
    
    
    
    foo <- dosis_infectious_episode * num_people
    bar <- 1 - (1 - var_infect_prob) ^ foo
    
    risk_of_1_person_in_room_being_infected <- bar * 100
    
    
    
    if (input$distance == "k1m") {
      infect_risk_individual <- infect_risk_individual
    } else if (input$distance == "m1m") {
      infect_risk_individual <- infect_risk_individual * 0.85
      risk_of_1_person_in_room_being_infected <-
        risk_of_1_person_in_room_being_infected * 0.85
    }
    
    
    people_infected <-
      num_people * (infect_risk_individual / 100)
    
    
    
    if (people_infected < 0.5) {
      people_infected <- "Keine"
    } else if (people_infected >= 0.5 && people_infected < 1.5) {
      (people_infected <- 1)
    }
    
    
    
    
    #risk of 1  person in room being infected [% / episode]
    
    #foo <- dosis_infectious_episode * num_people
    #bar <- 1 - (1 - var_infect_prob) ^ foo
    
    #risk_of_1_person_in_room_being_infected <- bar * 100
    
    
    
    
    
    paste(
      "<table border=\"2\">",
      "<tr>",
      "<th width=\"150px\">",
      "Individuell",
      "</th>",
      "<th width=\"150px\">",
      "Allgemein",
      "</th>",
      "</tr>",
      "<tr>",
      "<td>",
      "<strong style=\"font-size:30px;\">",
      "<font style=\"color:DodgerBlue;\">",
      format(infect_risk_individual, digits = 3),
      "%" ,
      "</font>",
      "</strong>",
      "</td>",
      "<td>",
      "<strong style=\"font-size:30px;\">",
      "<font style=\"color:DodgerBlue;\">",
      format(risk_of_1_person_in_room_being_infected, digits = 3),
      "%" ,
      "</font>",
      "</strong>",
      "</td>",
      "</tr>",
      "</table>"
    )
    
    
  })
  
  
  output$image <- renderImage({
    #LIST OF VARIABLES NECESSARY FOR CALCULATIONS BASED ON LELIEVELD (2020)
    
    ##RNA for 50% infection probability (D50)
    RNA_num <- 233
    
    #DEPOSITION PROPABILITY
    deposition_prob <- 0.5
    
    #emission breathing [/cm³]
    emission_breathing <- 0.06
    
    #emission speaking [/cm³]
    emission_speaking <- 0.8
    
    #speaking / breathing ratio
    speaking_breathing_rat <- 0.1
    
    #respiratory rate [l/min]
    resp_rate <- 10
    
    #respiratory fluid RNA conc [/cm³]
    resp_fluid_RNA_conc <- 500000000
    
    #mean wet aerosol diameter [um]
    mean_aer_dia <- 5
    
    #infectious episode [d] *
    
    zeit <- input$zeit
    
    
    
    inf_epis <- 0.16666666667 * zeit
    
    #virus lifetime in aerosol [h]
    vir_life <- 1.7
    #browser()
    
    #Das hier ändern in
    #https://www.sciencedirect.com/science/article/abs/pii/S2210670720306119
    #Wenn Distance 1.5m, dann finaler Wert * 0.8, z1u15 dann * 0.9, wenn k1m dann nix
    
    
    #observe(print(numpeople()))
    
    #Room Area in m square based on num people
    room_area <- input$raum
    
    
    room_area <- input$raum
    
    
    
    #room height [m]** ** Very large room sizes violate the assumption of instantaneaous mixing of the air
    room_height <- 2.4
    
    #infection probability [/RNA] calculated with RNA_num
    infect_prob <- function(RNA_num) {
      return (1 - 10 ^ (log10(0.5) / RNA_num))
    }
    var_infect_prob <- 1 - 10 ^ (log10(0.5) / RNA_num)
    
    #RNA content in aerosol
    RNA_content <- function(resp_fluid_RNA_conc,
                            mean_aer_dia) {
      return (resp_fluid_RNA_conc * pi / 6 * (mean_aer_dia / 10000) ^ 3)
    }
    
    var_RNA_content <-
      (resp_fluid_RNA_conc * pi / 6 * (mean_aer_dia / 10000) ^ 3)
    
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
    var_aero_emission <-
      aero_emission(emission_breathing,
                    emission_speaking,
                    speaking_breathing_rat,
                    resp_rate)
    
    
    #aerosol conc [/l]
    
    aero_conc <-
      function(var_aero_emission,
               room_area,
               room_height) {
        return (var_aero_emission / (room_area * room_height * 1000))
      }
    var_aero_conc <- aero_conc(var_aero_emission,
                               room_area,
                               room_height)
    
    #RNA cont. aerosol conc [/l]
    
    
    
    
    var_RNA_cont_aero_conc <-
      var_aero_conc * var_RNA_content
    
    
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
    
    #room ventilation rate [/h]
    #Normally generated through user input
    if (input$air == "NoAir") {
      room_vent_rate <- 0
    } else if (input$air == "FensterGekippt") {
      room_vent_rate <- 0.35
    } else if (input$air == "Stoßlueften") {
      room_vent_rate <- 2
    } #else if (input$air == "Lueftungssystem") {
    #room_vent_rate <- 9
    #}
    
    
    #total mask efficiency (exhaling + inhaling)
    total_mask_effic <-
      0 #Normally generated through user input
    
    if (input$masks == "No") {
      total_mask_effic <- 0
      
    } else if (input$masks == "OP") {
      total_mask_effic <- 0.7
    } else if (input$masks == "FFP2") {
      total_mask_effic <- 0.95
    }
    
    
    
    # susceptible # persons / room
    
    #Normally generated through user input
    #dosis 6 hours (per  day)*
    dosis_6_hours <-
      var_RNA_dosis / (room_vent_rate + 1 / vir_life) * (1 - total_mask_effic) *
      6
    
    #dosis infectious episode
    dosis_infectious_episode <- dosis_6_hours * inf_epis
    
    #infection risk of individual person [% / episode]
    infect_risk_individual <-
      (1 - (1 - var_infect_prob) ^ dosis_infectious_episode) * 100
    
    #risk of 1  person in room being infected [% / episode]
    
    
    
    num_people <- input$people
    
    
    
    
    
    foo <- dosis_infectious_episode * num_people
    bar <- 1 - (1 - var_infect_prob) ^ foo
    
    risk_of_1_person_in_room_being_infected <- bar * 100
    
    
    
    if (input$distance == "k1m") {
      infect_risk_individual <- infect_risk_individual
    } else if (input$distance == "m1m") {
      infect_risk_individual <- infect_risk_individual * 0.85
    }
    
    
    people_infected <-
      num_people * (infect_risk_individual / 100)
    
    
    
    if (people_infected < 0.5) {
      people_infected <- 0
    } else if (people_infected >= 0.5 && people_infected < 1.5) {
      people_infected <- 1
    }
    
    #browser()
    format(people_infected, digits = 1)
    
    input3 <- format(people_infected, digits = 1)
    
    filename <- normalizePath(file.path('./images',
                                        paste('Folie', input3, '.png', sep = '')))
    
    list(src = filename,
         width = "220px",
         alt = paste(input3, "0"))
    
    
    
  }, deleteFile = FALSE)
  
  
  
  
  #eventReactive()
  output$text1 <- renderText({
    #LIST OF VARIABLES NECESSARY FOR CALCULATIONS BASED ON LELIEVELD (2020)
    
    ##RNA for 50% infection probability (D50)
    RNA_num <- 233
    
    #DEPOSITION PROPABILITY
    deposition_prob <- 0.5
    
    #emission breathing [/cm³]
    emission_breathing <- 0.06
    
    #emission speaking [/cm³]
    emission_speaking <- 0.8
    
    #speaking / breathing ratio
    speaking_breathing_rat <- 0.1
    
    #respiratory rate [l/min]
    resp_rate <- 10
    
    #respiratory fluid RNA conc [/cm³]
    resp_fluid_RNA_conc <- 500000000
    
    #mean wet aerosol diameter [um]
    mean_aer_dia <- 5
    
    #infectious episode [d] *
    
    
    zeit <- input$zeit
    
    
    
    inf_epis <- 0.16666666667 * zeit
    
    #virus lifetime in aerosol [h]
    vir_life <- 1.7
    #browser()
    
    #Das hier ändern in
    #https://www.sciencedirect.com/science/article/abs/pii/S2210670720306119
    #Wenn Distance 1.5m, dann finaler Wert * 0.8, z1u15 dann * 0.9, wenn k1m dann nix
    
    
    #observe(print(numpeople()))
    
    #Room Area in m square based on num people
    room_area <- input$raum
    
    
    
    #room height [m]** ** Very large room sizes violate the assumption of instantaneaous mixing of the air
    room_height <- 2.4
    
    #infection probability [/RNA] calculated with RNA_num
    infect_prob <- function(RNA_num) {
      return (1 - 10 ^ (log10(0.5) / RNA_num))
    }
    var_infect_prob <- 1 - 10 ^ (log10(0.5) / RNA_num)
    
    #RNA content in aerosol
    RNA_content <- function(resp_fluid_RNA_conc,
                            mean_aer_dia) {
      return (resp_fluid_RNA_conc * pi / 6 * (mean_aer_dia / 10000) ^ 3)
    }
    
    var_RNA_content <-
      (resp_fluid_RNA_conc * pi / 6 * (mean_aer_dia / 10000) ^ 3)
    
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
    var_aero_emission <-
      aero_emission(emission_breathing,
                    emission_speaking,
                    speaking_breathing_rat,
                    resp_rate)
    
    
    #aerosol conc [/l]
    
    aero_conc <-
      function(var_aero_emission,
               room_area,
               room_height) {
        return (var_aero_emission / (room_area * room_height * 1000))
      }
    var_aero_conc <- aero_conc(var_aero_emission,
                               room_area,
                               room_height)
    
    #RNA cont. aerosol conc [/l]
    
    
    
    
    var_RNA_cont_aero_conc <-
      var_aero_conc * var_RNA_content
    
    
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
    
    #room ventilation rate [/h]
    #Normally generated through user input
    if (input$air == "NoAir") {
      room_vent_rate <- 0
    } else if (input$air == "FensterGekippt") {
      room_vent_rate <- 0.35
    } else if (input$air == "Stoßlueften") {
      room_vent_rate <- 2
    } #else if (input$air == "Lueftungssystem") {
    #room_vent_rate <- 9
    #}
    
    
    #total mask efficiency (exhaling + inhaling)
    total_mask_effic <-
      0 #Normally generated through user input
    
    if (input$masks == "No") {
      total_mask_effic <- 0
      
    } else if (input$masks == "OP") {
      total_mask_effic <- 0.7
    } else if (input$masks == "FFP2") {
      total_mask_effic <- 0.95
    }
    
    
    
    # susceptible # persons / room
    
    #Normally generated through user input
    #dosis 6 hours (per  day)*
    dosis_6_hours <-
      var_RNA_dosis / (room_vent_rate + 1 / vir_life) * (1 - total_mask_effic) *
      6
    
    #dosis infectious episode
    dosis_infectious_episode <- dosis_6_hours * inf_epis
    
    #infection risk of individual person [% / episode]
    infect_risk_individual <-
      (1 - (1 - var_infect_prob) ^ dosis_infectious_episode) * 100
    
    #risk of 1  person in room being infected [% / episode]
    
    
    num_people <- input$people
    
    
    
    
    
    foo <- dosis_infectious_episode * num_people
    bar <- 1 - (1 - var_infect_prob) ^ foo
    
    risk_of_1_person_in_room_being_infected <- bar * 100
    
    
    
    if (input$distance == "k1m") {
      infect_risk_individual <- infect_risk_individual
    } else if (input$distance == "m1m") {
      infect_risk_individual <- infect_risk_individual * 0.85
      risk_of_1_person_in_room_being_infected <-
        risk_of_1_person_in_room_being_infected * 0.85
    }
    
    
    people_infected <-
      num_people * (infect_risk_individual / 100)
    
    
    
    if (people_infected < 0.5) {
      people_infected <- "0"
    } else if (people_infected >= 0.5 && people_infected < 1.5) {
      (people_infected <- 1)
    }
    
    
    
    
    #risk of 1  person in room being infected [% / episode]
    
    #foo <- dosis_infectious_episode * num_people
    #bar <- 1 - (1 - var_infect_prob) ^ foo
    
    #risk_of_1_person_in_room_being_infected <- bar * 100
    
    #Copy von erster Funktion noch einfügen
    paste(
      "<strong style=\"font-size:22px;\">",
      "<font style=\"color:DodgerBlue;\">",
      #Hier war vorher individuelle Risk aus risk_of_1_person_in_room_being_infected mit digits = 3
      #format(people_infected, digits = 1),
      
      "</font>",
      "Infizierte Person(en) von ", input$people, ":", sep = "",
      #Hier war vorher eine Zeit
      
      "</strong>"
      
      
    )
    
    
  })
  
  
  output$text3 <- renderText({
    #LIST OF VARIABLES NECESSARY FOR CALCULATIONS BASED ON LELIEVELD (2020)
    
    ##RNA for 50% infection probability (D50)
    RNA_num <- 233
    
    #DEPOSITION PROPABILITY
    deposition_prob <- 0.5
    
    #emission breathing [/cm³]
    emission_breathing <- 0.06
    
    #emission speaking [/cm³]
    emission_speaking <- 0.8
    
    #speaking / breathing ratio
    speaking_breathing_rat <- 0.1
    
    #respiratory rate [l/min]
    resp_rate <- 10
    
    #respiratory fluid RNA conc [/cm³]
    resp_fluid_RNA_conc <- 500000000
    
    #mean wet aerosol diameter [um]
    mean_aer_dia <- 5
    
    #infectious episode [d] *
    
    zeit <- input$zeit
    
    
    
    inf_epis <- 0.16666666667 * zeit
    
    #virus lifetime in aerosol [h]
    vir_life <- 1.7
    #browser()
    
    #Das hier ändern in
    #https://www.sciencedirect.com/science/article/abs/pii/S2210670720306119
    #Wenn Distance 1.5m, dann finaler Wert * 0.8, z1u15 dann * 0.9, wenn k1m dann nix
    
    
    #observe(print(numpeople()))
    
    #Room Area in m square based on num people
    
    
    
    room_area <- input$raum
    
    
    
    #room height [m]** ** Very large room sizes violate the assumption of instantaneaous mixing of the air
    room_height <- 2.4
    
    #infection probability [/RNA] calculated with RNA_num
    infect_prob <- function(RNA_num) {
      return (1 - 10 ^ (log10(0.5) / RNA_num))
    }
    var_infect_prob <- 1 - 10 ^ (log10(0.5) / RNA_num)
    
    #RNA content in aerosol
    RNA_content <- function(resp_fluid_RNA_conc,
                            mean_aer_dia) {
      return (resp_fluid_RNA_conc * pi / 6 * (mean_aer_dia / 10000) ^ 3)
    }
    
    var_RNA_content <-
      (resp_fluid_RNA_conc * pi / 6 * (mean_aer_dia / 10000) ^ 3)
    
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
    var_aero_emission <-
      aero_emission(emission_breathing,
                    emission_speaking,
                    speaking_breathing_rat,
                    resp_rate)
    
    
    #aerosol conc [/l]
    
    aero_conc <-
      function(var_aero_emission,
               room_area,
               room_height) {
        return (var_aero_emission / (room_area * room_height * 1000))
      }
    var_aero_conc <- aero_conc(var_aero_emission,
                               room_area,
                               room_height)
    
    #RNA cont. aerosol conc [/l]
    
    
    
    
    var_RNA_cont_aero_conc <-
      var_aero_conc * var_RNA_content
    
    
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
    
    #room ventilation rate [/h]
    #Normally generated through user input
    if (input$air == "NoAir") {
      room_vent_rate <- 0
    } else if (input$air == "FensterGekippt") {
      room_vent_rate <- 0.35
    } else if (input$air == "Stoßlueften") {
      room_vent_rate <- 2
    } #else if (input$air == "Lueftungssystem") {
    #room_vent_rate <- 9
    #}
    
    
    #total mask efficiency (exhaling + inhaling)
    total_mask_effic <-
      0 #Normally generated through user input
    
    if (input$masks == "No") {
      total_mask_effic <- 0
      
    } else if (input$masks == "OP") {
      total_mask_effic <- 0.7
    } else if (input$masks == "FFP2") {
      total_mask_effic <- 0.95
    }
    
    
    
    # susceptible # persons / room
    
    #Normally generated through user input
    #dosis 6 hours (per  day)*
    dosis_6_hours <-
      var_RNA_dosis / (room_vent_rate + 1 / vir_life) * (1 - total_mask_effic) *
      6
    
    #dosis infectious episode
    dosis_infectious_episode <- dosis_6_hours * inf_epis
    
    #infection risk of individual person [% / episode]
    infect_risk_individual <-
      (1 - (1 - var_infect_prob) ^ dosis_infectious_episode) * 100
    
    #risk of 1  person in room being infected [% / episode]
    
    
    
    num_people <- input$people
    
    
    
    
    
    foo <- dosis_infectious_episode * num_people
    bar <- 1 - (1 - var_infect_prob) ^ foo
    
    risk_of_1_person_in_room_being_infected <- bar * 100
    
    
    
    if (input$distance == "k1m") {
      infect_risk_individual <- infect_risk_individual
    } else if (input$distance == "m1m") {
      infect_risk_individual <- infect_risk_individual * 0.85
      risk_of_1_person_in_room_being_infected <-
        risk_of_1_person_in_room_being_infected * 0.85
    }
    
    
    people_infected <-
      num_people * (infect_risk_individual / 100)
    
    
    
    if (people_infected < 0.5) {
      people_infected <- "Keine"
    } else if (people_infected >= 0.5 && people_infected < 1.5) {
      (people_infected <- 1)
    }
    
    
    
    
    #risk of 1  person in room being infected [% / episode]
    
    #foo <- dosis_infectious_episode * num_people
    #bar <- 1 - (1 - var_infect_prob) ^ foo
    
    #risk_of_1_person_in_room_being_infected <- bar * 100
    
    
    if(((input$distance == "k1m" || input$distance == "m1m")  && input$masks == "No" && input$air == "NoAir") || 
       (input$distance == "k1m"  && input$masks == "No" && input$air == "FensterGekippt")) {
      paste(
        "<strong style=\"font-size:50px;\">",
        "<font style=\"color:Red;\">",
        format(infect_risk_individual, digits = 3), "%",
        "</font>",
        "</strong>"
      )
      
      
    } else if ((input$distance == "m1m" && input$masks == "OP" && (input$air == "NoAir" || input$air == "FensterGekippt" )) ||
               (input$distance == "m1m"  && input$masks == "No" && (input$air == "FensterGekippt" || input$air == "Stoßlueften" )) ||
               (input$distance == "k1m" && input$masks == "OP" && input$air == "NoAir") ||
               (input$distance == "k1m" && input$masks == "OP" &&  input$air == "FensterGekippt" ) ||
               (input$distance == "k1m" && input$masks == "No" && input$air == "Stoßlueften" )
    ) {
      paste (
        "<strong style=\"font-size:50px;\">",
        "<font style=\"color:Orange;\">",
        format(infect_risk_individual, digits = 3), "%",

        "</font>",
        "</strong>"
      )
    } else {
      paste (
        "<strong style=\"font-size:50px;\">",
        "<font style=\"color:Green;\">",
        format(infect_risk_individual, digits = 3), "%",
        "</font>",
        "</strong>"
      )
    }
    
  
    
    
  })
  output$text6 <- renderText({
    #LIST OF VARIABLES NECESSARY FOR CALCULATIONS BASED ON LELIEVELD (2020)
    
    ##RNA for 50% infection probability (D50)
    RNA_num <- 233
    
    #DEPOSITION PROPABILITY
    deposition_prob <- 0.5
    
    #emission breathing [/cm³]
    emission_breathing <- 0.06
    
    #emission speaking [/cm³]
    emission_speaking <- 0.8
    
    #speaking / breathing ratio
    speaking_breathing_rat <- 0.1
    
    #respiratory rate [l/min]
    resp_rate <- 10
    
    #respiratory fluid RNA conc [/cm³]
    resp_fluid_RNA_conc <- 500000000
    
    #mean wet aerosol diameter [um]
    mean_aer_dia <- 5
    
    #infectious episode [d] *
    
    
    zeit <- input$zeit
    
    
    
    inf_epis <- 0.16666666667 * zeit
    
    #virus lifetime in aerosol [h]
    vir_life <- 1.7
    #browser()
    
    #Das hier ändern in
    #https://www.sciencedirect.com/science/article/abs/pii/S2210670720306119
    #Wenn Distance 1.5m, dann finaler Wert * 0.8, z1u15 dann * 0.9, wenn k1m dann nix
    
    
    #observe(print(numpeople()))
    
    #Room Area in m square based on num people
    room_area <- input$raum
    
    
    
    
    
    
    #room height [m]** ** Very large room sizes violate the assumption of instantaneaous mixing of the air
    room_height <- 2.4
    
    #infection probability [/RNA] calculated with RNA_num
    infect_prob <- function(RNA_num) {
      return (1 - 10 ^ (log10(0.5) / RNA_num))
    }
    var_infect_prob <- 1 - 10 ^ (log10(0.5) / RNA_num)
    
    #RNA content in aerosol
    RNA_content <- function(resp_fluid_RNA_conc,
                            mean_aer_dia) {
      return (resp_fluid_RNA_conc * pi / 6 * (mean_aer_dia / 10000) ^ 3)
    }
    
    var_RNA_content <-
      (resp_fluid_RNA_conc * pi / 6 * (mean_aer_dia / 10000) ^ 3)
    
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
    var_aero_emission <-
      aero_emission(emission_breathing,
                    emission_speaking,
                    speaking_breathing_rat,
                    resp_rate)
    
    
    #aerosol conc [/l]
    
    aero_conc <-
      function(var_aero_emission,
               room_area,
               room_height) {
        return (var_aero_emission / (room_area * room_height * 1000))
      }
    var_aero_conc <- aero_conc(var_aero_emission,
                               room_area,
                               room_height)
    
    #RNA cont. aerosol conc [/l]
    
    
    
    
    var_RNA_cont_aero_conc <-
      var_aero_conc * var_RNA_content
    
    
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
    
    #room ventilation rate [/h]
    #Normally generated through user input
    if (input$air == "NoAir") {
      room_vent_rate <- 0
    } else if (input$air == "FensterGekippt") {
      room_vent_rate <- 0.35
    } else if (input$air == "Stoßlueften") {
      room_vent_rate <- 2
    } #else if (input$air == "Lueftungssystem") {
    #room_vent_rate <- 9
    #}
    
    
    #total mask efficiency (exhaling + inhaling)
    total_mask_effic <-
      0 #Normally generated through user input
    
    if (input$masks == "No") {
      total_mask_effic <- 0
      
    } else if (input$masks == "OP") {
      total_mask_effic <- 0.7
    } else if (input$masks == "FFP2") {
      total_mask_effic <- 0.95
    }
    
    
    
    # susceptible # persons / room
    
    #Normally generated through user input
    #dosis 6 hours (per  day)*
    dosis_6_hours <-
      var_RNA_dosis / (room_vent_rate + 1 / vir_life) * (1 - total_mask_effic) *
      6
    
    #dosis infectious episode
    dosis_infectious_episode <- dosis_6_hours * inf_epis
    
    #infection risk of individual person [% / episode]
    infect_risk_individual <-
      (1 - (1 - var_infect_prob) ^ dosis_infectious_episode) * 100
    
    #risk of 1  person in room being infected [% / episode]
    
    
    
    num_people <- input$people
    
    
    
    
    
    foo <- dosis_infectious_episode * num_people
    bar <- 1 - (1 - var_infect_prob) ^ foo
    
    risk_of_1_person_in_room_being_infected <- bar * 100
    
    
    
    if (input$distance == "k1m") {
      infect_risk_individual <- infect_risk_individual
    } else if (input$distance == "m1m") {
      infect_risk_individual <- infect_risk_individual * 0.85
      risk_of_1_person_in_room_being_infected <-
        risk_of_1_person_in_room_being_infected * 0.85
    }
    
    
    people_infected <-
      num_people * (infect_risk_individual / 100)
    
    
    
    if (people_infected < 0.5) {
      people_infected <- "Keine"
    } else if (people_infected >= 0.5 && people_infected < 1.5) {
      (people_infected <- 1)
    }
    
    
    
    
    #risk of 1  person in room being infected [% / episode]
    
    #foo <- dosis_infectious_episode * num_people
    #bar <- 1 - (1 - var_infect_prob) ^ foo
    
    #risk_of_1_person_in_room_being_infected <- bar * 100
    
    
    if(((input$distance == "k1m" || input$distance == "m1m")  && input$masks == "No" && input$air == "NoAir") || 
       (input$distance == "k1m"  && input$masks == "No" && input$air == "FensterGekippt")) {
      paste(
        "<strong style=\"font-size:50px;\">",
        "<font style=\"color:Red;\">",
        format(risk_of_1_person_in_room_being_infected, digits = 3), "%",
        "</font>",
        "</strong>"
      )
      
      
    } else if ((input$distance == "m1m" && input$masks == "OP" && (input$air == "NoAir" || input$air == "FensterGekippt" )) ||
               (input$distance == "m1m"  && input$masks == "No" && (input$air == "FensterGekippt" || input$air == "Stoßlueften" )) ||
               (input$distance == "k1m" && input$masks == "OP" && input$air == "NoAir") ||
               (input$distance == "k1m" && input$masks == "OP" &&  input$air == "FensterGekippt" ) ||
               (input$distance == "k1m" && input$masks == "No" && input$air == "Stoßlueften" )
    ) {
      paste (
        "<strong style=\"font-size:50px;\">",
        "<font style=\"color:Orange;\">",
        format(risk_of_1_person_in_room_being_infected, digits = 3), "%",
  
        "</font>",
        "</strong>"
      )
    } else {
      paste (
        "<strong style=\"font-size:50px;\">",
        "<font style=\"color:Green;\">",
        format(risk_of_1_person_in_room_being_infected, digits = 3), "%",
        "</font>",
        "</strong>"
      )
    }
    
    
    

    
    
  })
  
  
  
  output$text2 <- renderPlot({
    #LIST OF VARIABLES NECESSARY FOR CALCULATIONS BASED ON LELIEVELD (2020)
    ##RNA for 50% infection probability (D50)
    RNA_num <- 233
    
    #DEPOSITION PROPABILITY
    deposition_prob <- 0.5
    
    #emission breathing [/cm³]
    emission_breathing <- 0.06
    
    #emission speaking [/cm³]
    emission_speaking <- 0.8
    
    #speaking / breathing ratio
    speaking_breathing_rat <- 0.1
    
    #respiratory rate [l/min]
    resp_rate <- 10
    
    #respiratory fluid RNA conc [/cm³]
    resp_fluid_RNA_conc <- 500000000
    
    #mean wet aerosol diameter [um]
    mean_aer_dia <- 5
    
    #infectious episode [d] *
    
    
    zeit <- input$zeit
    
    
    
    inf_epis <- 0.16666666667 * zeit
    
    #virus lifetime in aerosol [h]
    vir_life <- 1.7
    #browser()
    
    #Das hier ändern in
    #https://www.sciencedirect.com/science/article/abs/pii/S2210670720306119
    #Wenn Distance 1.5m, dann finaler Wert * 0.8, z1u15 dann * 0.9, wenn k1m dann nix
    
    
    #observe(print(numpeople()))
    
    #Room Area in m square based on num people
    room_area <- input$raum
    
    
    room_area <- input$raum
    
    
    
    #room height [m]** ** Very large room sizes violate the assumption of instantaneaous mixing of the air
    room_height <- 2.4
    
    #infection probability [/RNA] calculated with RNA_num
    infect_prob <- function(RNA_num) {
      return (1 - 10 ^ (log10(0.5) / RNA_num))
    }
    var_infect_prob <- 1 - 10 ^ (log10(0.5) / RNA_num)
    
    #RNA content in aerosol
    RNA_content <- function(resp_fluid_RNA_conc,
                            mean_aer_dia) {
      return (resp_fluid_RNA_conc * pi / 6 * (mean_aer_dia / 10000) ^ 3)
    }
    
    var_RNA_content <-
      (resp_fluid_RNA_conc * pi / 6 * (mean_aer_dia / 10000) ^ 3)
    
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
    var_aero_emission <-
      aero_emission(emission_breathing,
                    emission_speaking,
                    speaking_breathing_rat,
                    resp_rate)
    
    
    #aerosol conc [/l]
    
    aero_conc <-
      function(var_aero_emission,
               room_area,
               room_height) {
        return (var_aero_emission / (room_area * room_height * 1000))
      }
    var_aero_conc <- aero_conc(var_aero_emission,
                               room_area,
                               room_height)
    
    #RNA cont. aerosol conc [/l]
    
    
    
    
    var_RNA_cont_aero_conc <-
      var_aero_conc * var_RNA_content
    
    
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
    
    
    
    #room ventilation rate [/h]
    #Normally generated through user input
    if (input$air == "NoAir") {
      room_vent_rate <- 0
    } else if (input$air == "FensterGekippt") {
      room_vent_rate <- 0.35
    } else if (input$air == "Stoßlueften") {
      room_vent_rate <- 2
    } #else if (input$air == "Lueftungssystem") {
    #room_vent_rate <- 9
    #}
    
    
    #total mask efficiency (exhaling + inhaling)
    total_mask_effic <-
      0 #Normally generated through user input
    
    if (input$masks == "No") {
      total_mask_effic <- 0
      
    } else if (input$masks == "OP") {
      total_mask_effic <- 0.7
    } else if (input$masks == "FFP2") {
      total_mask_effic <- 0.95
    }
    
    
    
    # susceptible # persons / room
    
    #Normally generated through user input
    #dosis 6 hours (per  day)*
    dosis_6_hours <-
      var_RNA_dosis / (room_vent_rate + 1 / vir_life) * (1 - total_mask_effic) *
      6
    
    #dosis infectious episode
    dosis_infectious_episode <- dosis_6_hours * inf_epis
    
    #infection risk of individual person [% / episode]
    infect_risk_individual <-
      (1 - (1 - var_infect_prob) ^ dosis_infectious_episode) * 100
    
    #risk of 1  person in room being infected [% / episode]
    
    
    
    num_people <- input$people
    
    
    
    
    
    foo <- dosis_infectious_episode * num_people
    bar <- 1 - (1 - var_infect_prob) ^ foo
    
    risk_of_1_person_in_room_being_infected <- bar * 100
    
    #if variable  Blödsinn, dann ändere sie, wenn variable nicht mehr blödsinn, dann ändere sie nicht
    
    
    if (input$distance == "k1m") {
      infect_risk_individual <- infect_risk_individual
    } else if (input$distance == "m1m") {
      infect_risk_individual <- infect_risk_individual * 0.85
    }
    
    
    people_infected <-
      num_people * (infect_risk_individual / 100)
    
    
    
    if (people_infected < 0.5) {
      people_infected <- "Keine"
    } else if (people_infected >= 0.5 && people_infected < 1.5) {
      (people_infected <- 1)
    }
    #browser()
    inf <- infect_risk_individual / 100
    
    worst_case_f <- function() {
      #room ventilation rate [/h]
      #Normally generated through user input
      
      room_vent_rate <- 0
      
      
      
      #total mask efficiency (exhaling + inhaling)
      total_mask_effic <-
        0 #Normally generated through user input
      
      
      
      
      
      # susceptible # persons / room
      
      #Normally generated through user input
      #dosis 6 hours (per  day)*
      dosis_6_hours <-
        var_RNA_dosis / (room_vent_rate + 1 / vir_life) * (1 - total_mask_effic) *
        6
      
      #dosis infectious episode
      dosis_infectious_episode <- dosis_6_hours * inf_epis
      
      #infection risk of individual person [% / episode]
      infect_risk_individual <-
        (1 - (1 - var_infect_prob) ^ dosis_infectious_episode) * 100
      
      #risk of 1  person in room being infected [% / episode]
      
      
      
      num_people <- input$people
      
      
      
      
      
      foo <- dosis_infectious_episode * num_people
      bar <- 1 - (1 - var_infect_prob) ^ foo
      
      risk_of_1_person_in_room_being_infected <- bar * 100
      
      #if variable  Blödsinn, dann ändere sie, wenn variable nicht mehr blödsinn, dann ändere sie nicht
      
      
      
      
      
      
      if (people_infected < 0.5) {
        people_infected <- "Keine"
      } else if (people_infected >= 0.5 &&
                 people_infected < 1.5) {
        (people_infected <- 1)
      }
      
      inf <- infect_risk_individual / 100
      
      return (inf)
      
    }
    best_case_f <- function() {
      #room ventilation rate [/h]
      
      room_vent_rate <- 2
      #else if (input$air == "Lueftungssystem") {
      #room_vent_rate <- 9
      #}
      
      
      #total mask efficiency (exhaling + inhaling)
      #Normally generated through user input
      
      total_mask_effic <- 0.95
      
      
      
      
      # susceptible # persons / room
      
      #Normally generated through user input
      #dosis 6 hours (per  day)*
      dosis_6_hours <-
        var_RNA_dosis / (room_vent_rate + 1 / vir_life) * (1 - total_mask_effic) *
        6
      
      #dosis infectious episode
      dosis_infectious_episode <- dosis_6_hours * inf_epis
      
      #infection risk of individual person [% / episode]
      infect_risk_individual <-
        (1 - (1 - var_infect_prob) ^ dosis_infectious_episode) * 100
      
      #risk of 1  person in room being infected [% / episode]
      
      
      
      num_people <- input$people
      
      
      
      
      
      foo <- dosis_infectious_episode * num_people
      bar <- 1 - (1 - var_infect_prob) ^ foo
      
      risk_of_1_person_in_room_being_infected <- bar * 100
      
      #if variable  Blödsinn, dann ändere sie, wenn variable nicht mehr blödsinn, dann ändere sie nicht
      
      
      infect_risk_individual <- infect_risk_individual * 0.85
      
      
      
      people_infected <-
        num_people * (infect_risk_individual / 100)
      
      
      
      if (people_infected < 0.5) {
        people_infected <- "Keine"
      } else if (people_infected >= 0.5 &&
                 people_infected < 1.5) {
        (people_infected <- 1)
      }
      
      inf <- infect_risk_individual / 100
      inf_zu_beginn <- -2
      
      return (inf)
      
    }
    
    worst_case <- worst_case_f()
    worst_case <- worst_case + 0.00001
    best_case <- best_case_f()
    #browser()
    ################### Alternative mit GGPLOT
    df <-
      data.frame(
        choices = c("Keine", "Ihre Auswahl",  "Alle"),
        Ansteckungswahrscheinlichkeit = c(worst_case, inf , best_case)
      )
    
    
    p <-
      ggplot(data = df,
             aes(
               x = reorder(choices,-Ansteckungswahrscheinlichkeit),
               y = Ansteckungswahrscheinlichkeit,
               fill = choices
             )) + geom_bar(stat = "identity",
                           show.legend = FALSE,
                           width = 0.7) +
      theme_minimal() + ylim(0, 1.0)
    p <- p + theme(legend.position = "bottom",)
    p <- p + scale_fill_brewer(palette = "Blues")
    p <-
      p + labs(x = "Eingehaltene Maßnahmen", y = "Ihre persönliche Infektionswahrscheinlichkeit") + scale_y_continuous(
        labels = function(x)
          paste0(x * 100, "%"),
        limits = c(0, 1)
      ) + geom_text(aes(label = paste(round((Ansteckungswahrscheinlichkeit * 100), digits = 1
      ), "%"), vjust = -0.6)) + theme(
        axis.title.x = element_text(
          color = "black",
          size = 16,
          face = "bold",
          margin = margin(
            t = 7,
            r = 0,
            b = 0,
            l = 0
          )
        ),
        axis.title.y = element_text(
          color = "black",
          size = 16,
          face = "bold",
          margin = margin(
            t = 0,
            r = 5,
            b = 0,
            l = 0
          )
        ),
        axis.text.x = element_text(size = 14),
        axis.text.y = element_text(size = 14),
        
        
      )
    #p+scale_fill_manual(values=c("#999999", "#E69F00", "#56B4E9"))
    p
    
    #scale_fill_brewer(palette="Paired")+
    
  })
  
  
}

# Run the application
shinyApp(ui = ui, server = server)
