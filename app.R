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

if (FALSE) {
rsconnect::setAccountInfo(name='fau-erl-nue', token='C4C6D39B6531E5AE28F944B0A52708F6', secret='SECRET')

rsconnect::deployApp(getwd())

rsconnect::showLogs()
}

# Define UI for application that draws a histogram
ui <- shinyUI(fluidPage(
  theme = bs_theme(version = 4, bootswatch = "superhero"),
 # theme = bs_theme(),
  chooseSliderSkin(skin = "Modern", color = "DodgerBlue"),
  
  titlePanel(div(
    HTML("<b style=\"color:DodgerBlue;\">Covid-19 Simulation</b>")
  )),
  fluidRow(
    column(12,"Mit diesem Dashboard können Sie die Wahrscheinlichkeit berechnen, sich in einem Raum durch Aerosole an Covid-19 anzustecken."),
    column(12, div(HTML("<h3><b> Ihr Raumsetting:</b></h3>")))
  ),
  
  fluidRow(
  column(4, sliderInput("people",
                        "Personen im Raum:",
                        2, 15, 8)) ,
  
  column(4, sliderInput("raum",
              "Raumgröße (in m²):",
              10, 35, 25)),
  column(4,sliderInput("zeit",
              "Aufenthaltsdauer (in Std.):",
              1, 16, 8)),
  
  ),
  (div(
    HTML("<h3><b>Maßnahmen gegen Covid-19:</b></h3>")
  )),
  sidebarLayout(
    position = "left",
    sidebarPanel(
     
      
      selectInput(
        "distance",
        "Wie viel Meter Abstand halten die Personen zueinander?",
        c(
          "Weniger als 1,5m" = "k1m",
          "Mehr als 1,5m" = "m1m"
        )
      ),
      
      selectInput(
        "masks",
        "Welche Masken tragen die Personen im Raum?",
        c(
          "Keine" = "No",
        #  "Alltagsmasken" = "AT",
          "OP Masken" = "OP",
          "FFP2 Masken" = "FFP2"
        )
      ),
      
     
      
      selectInput(
        "air",
        "Wie oft wird der Raum gelüftet?",
        c(
          #Werte müssen noch angepasst werden
          "Nie" = "NoAir",
          "Fenster dauerhaft gekippt" = "FensterGekippt",
          "Regelmäßiges Stoßlüften (10min/h)" = "Stoßlueften"
          #"Lüftungssystem" = "Lueftungssystem"
        )
      )
      
    ),
    
    mainPanel(
      
      htmlOutput("text3"),
      htmlOutput("text1"),
      
      #htmlOutput("text5"),
      #htmlOutput("text4"),
      #plotOutput("text2"),
      imageOutput("image")
    ),
    
    
  )
  
))


server <- function(input, output, session) {
  #bs_themer()
  
  
  
  
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
    speaking_breathing_rat <- 0.2
    
    #respiratory rate [l/min]
    resp_rate <- 10
    
    #respiratory fluid RNA conc [/cm³]
    resp_fluid_RNA_conc <- 500000000
    
    #mean wet aerosol diameter [um]
    mean_aer_dia <- 5
    
    #infectious episode [d] *
    inf_epis <- 0.16666666667 * input$zeit
    
    #virus lifetime in aerosol [h]
    vir_life <- 1.7
    
    
    #Das hier ändern in
    #https://www.sciencedirect.com/science/article/abs/pii/S2210670720306119
    #Wenn Distance 1.5m, dann finaler Wert * 0.8, z1u15 dann * 0.9, wenn k1m dann nix
    
    num_people <- input$people
    
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
    
    foo <- dosis_infectious_episode * num_people
    bar <- 1 - (1 - var_infect_prob) ^ foo
    
    risk_of_1_person_in_room_being_infected <- bar * 100
    
    
    if (FALSE) { #'Alter Stand nur für Referenz'
    if (input$distance == "k1m") {
      infect_risk_individual <- infect_risk_individual
    } else if (input$distance == "z1u15") {
      infect_risk_individual <- infect_risk_individual * 0.95
    } else if (input$distance == "m1m") {
      infect_risk_individual <- infect_risk_individual * 0.85
    }
    }
    
    if (input$distance == "k1m") {
      infect_risk_individual <- infect_risk_individual
     
    } else if (input$distance == "m1m") {
      infect_risk_individual <- infect_risk_individual * 0.85
     
    }
    
    
    
    people_infected <-
      input$people * (infect_risk_individual / 100)
    
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
      "<strong style=\"font-size:22px;\">",
      "<font style=\"color:DodgerBlue;\">",
      format(infect_risk_individual, digits = 3),
      "%",
      "</font>",
      "Wahrscheinlichkeit, dass Sie sich selbst mit Covid-19 infizieren und ",
      "<font style=\"color:DodgerBlue;\">",
      format(risk_of_1_person_in_room_being_infected, digits = 3),
      "%",
      "</font>",
      "Wahrscheinlichkeit, dass sich eine beliebige Person im Raum mit Covid-19 infiziert.",
      "</strong>"
      
      
    )
    
    
  })
  
  
  
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
    inf_epis <- 0.16666666667 * input$zeit
    
    #virus lifetime in aerosol [h]
    vir_life <- 1.7
    
    
    #Das hier ändern in
    #https://www.sciencedirect.com/science/article/abs/pii/S2210670720306119
    #Wenn Distance 1.5m, dann finaler Wert * 0.8, z1u15 dann * 0.9, wenn k1m dann nix
    
    num_people <- input$people
    
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
    
    foo <- dosis_infectious_episode * num_people
    bar <- 1 - (1 - var_infect_prob) ^ foo
    
    risk_of_1_person_in_room_being_infected <- bar * 100
    
    
    
    if (input$distance == "k1m") {
      infect_risk_individual <- infect_risk_individual
    } else if (input$distance == "m1m") {
      infect_risk_individual <- infect_risk_individual * 0.85
    }
    
    
    
    
    people_infected <-
      input$people * (infect_risk_individual / 100)
    
    if (people_infected < 0.5) {
      people_infected <- "Keine"
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
      "Die Anzahl der Personen, die sich im Durchschnitt in Ihrem Raum mit Covid-19 infizieren, beträgt:",
      "<font style=\"color:DodgerBlue;\">",
      #Hier war vorher individuelle Risk aus risk_of_1_person_in_room_being_infected mit digits = 3
      format(people_infected, digits = 1),
      
      "</font>",
      #Hier war vorher eine Zeit
      
      "</strong>"
      
      
    )
    
    
  })
  

  
  
  output$image <- renderImage({
    
    #browser()
    input3 <- 'man_col_w'
    
    input2 <- 'man_col_r'
    #outfile <- tempfile(fileext = '.png')
    
    
    
    filename <- normalizePath(file.path('./images',
                                        paste(input3,'.png', sep = '')))
    filename2 <- normalizePath(file.path('./images',
                                        paste(input2,'.png', sep = '')))
    
    inputs <- ""
    
    for (i in 1:5) {
      
      paste(inputs,input3, sep = '')
    }
    
    
    list(src = filename,
         alt = paste(input3, "Hier User Input"))
    
   
    
  },deleteFile = FALSE)
  
  
  
  
  output$text2 <- renderPlot({
    #Option for RENDERPLOT: https://shiny.rstudio.com/gallery/image-output.html
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
    inf_epis <- 0.16666666667 * input$zeit
    
    #virus lifetime in aerosol [h]
    vir_life <- 1.7
    
    
    #Das hier ändern in
    #https://www.sciencedirect.com/science/article/abs/pii/S2210670720306119
    #Wenn Distance 1.5m, dann finaler Wert * 0.8, z1u15 dann * 0.9, wenn k1m dann nix
    
    num_people <- input$people
    
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
    
    foo <- dosis_infectious_episode * num_people
    bar <- 1 - (1 - var_infect_prob) ^ foo
    
    risk_of_1_person_in_room_being_infected <- bar * 100
    
    
    
    if (input$distance == "k1m") {
      infect_risk_individual <- infect_risk_individual
    } else if (input$distance == "m1m") {
      infect_risk_individual <- infect_risk_individual * 0.85
    }
    
    
    
    
    people_infected <-
      input$people * (infect_risk_individual / 100)
    
    if (people_infected < 0.5) {
      people_infected <- "Keine"
    } else if (people_infected >= 0.5 && people_infected < 1.5) {
      (people_infected <- 1)
    }
    
    
    
    
    #risk of 1  person in room being infected [% / episode]
    
    #foo <- dosis_infectious_episode * num_people
    #bar <- 1 - (1 - var_infect_prob) ^ foo
    
    #risk_of_1_person_in_room_being_infected <- bar * 100
    
    ####Copy von erster Funktion noch einfügen
    
    
    
    #browser()
    last_value <- 2
    last_value_worst <- 2
    last_value_best <- 2
    
    R_value <- 1.6
    R_value_worst <- 1.6
    R_value_best <- 1.6
    
    #Vector mit Zahlen
    #### WORST AND BEST CASE SCENARIOS
    worst_case <- 1
    best_case <- 0.01
    
    risk_of_infectionl <-
      risk_of_1_person_in_room_being_infected / 100
    
    append_vector_worst <- integer()
    append_vector_best <- integer()
    append_vector_yours <- integer()
    
    for (i in 1:7) {
      R_value <- (R_value * risk_of_infectionl)
      append_vector_yours[i] <- last_value ^ R_value
      last_value <- append_vector_yours[i]
      
    }
    for (i in 1:7) {
      R_value_best <- (R_value_best * best_case)
      append_vector_best[i] <- last_value_best ^ R_value_best
      last_value_best <- append_vector_best[i]
      
    }
    for (i in 1:7) {
      R_value_worst <- (R_value_worst * worst_case)
      append_vector_worst[i] <- last_value_worst ^ R_value_worst
      last_value_worst <- append_vector_worst[i]
      
    }
    #browser()
    #y <- runif(length(append_vector_yours), 1, 50)
    #https://ggplot2.tidyverse.org/reference/geom_path.html
    ################### Alternative mit GGPLOT
    data <- as.data.frame(append_vector_worst)
    main <- "Worst Case Data"
    ggplot(data, aes(x = 1:7, append_vector_worst)) + geom_line(color = "DodgerBlue")
    
    
    
    #Graph der zahlen in Blau mit punkten
    plot(
      append_vector_worst,
      ylim = c(0, 120),
      type = "l",
      col = "Red",
      axes = TRUE,
      ann = FALSE
    )
    
    
    
    box()
    
    #Blödsinn aktuell noch
    lines(
      append_vector_yours,
      type = "l",
      pch = 22,
      lty = ,
      col = "DodgerBlue"
    )
    lines(
      append_vector_best,
      type = "l",
      pch = 22,
      lty = ,
      col = "ForestGreen"
    )
    
    ##Kopiert
    
    #Titel mit roter fetter schrift
    title(main = "Pandemieentwicklung in Deutschland",
          col.main = "DodgerBlue",
          font.main = 4)
    
    title(ylab = "Kumulierte Fallzahlen in Deutschland in Zehntausend", col.lab = rgb(0, 0.5, 0))
    title(xlab = "Tägliche Infizierte in den nächsten 7 Tagen", col.lab = rgb(0, 0.5, 0))
    
    
    
    
    
    
    
  })
  

  
  
  
  
  
  output$text4 <- renderText({
    "Der folgende Graph zeigt die Entwicklung der täglich neu infizierten an Covid-19, wenn sich alle anderen wie Sie verhalten würden in Bezug auf die Maßnahmen."
  })
  

  
}

# Run the application
shinyApp(ui = ui, server = server)
