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
  titlePanel(div(
    HTML("<b style=\"color:DodgerBlue;\">Covid19-Simulation</b>")
  )),
  sidebarLayout(
    position = "left",
    sidebarPanel(
      (div(
        HTML("<h3><b>Maßnahmen gegen Covid-19</b></h3>")
      )),
      
      sliderInput("people", 
                  "Wie viele Personen befinden sich im Raum?",
                  2, 15, 5),
      
      
      selectInput(
        "distance",
        "Wie viel Meter Abstand halten die Personen zueinander?",
        c(
          "Weniger als 1m" = "k1m",
          "Zwischen 1m und 1,5m" = "z1u15",
          "Mehr als 1,5m" = "m1m"
        )
      ),
      
      selectInput(
        "masks",
        "Welche Masken tragen die Personen im Raum?",
        c(
          "Keine" = "No",
          "Alltagsmasken" = "AT",
          "OP Masken" = "OP",
          "FFP2 Masken" = "FFP2"
        )
      ),
      
      selectInput(
        "masks_dur",
        "Wie lange tragen die Personen die Masken?",
        c(
          "Nie" = "NieMask",
          "Gelegentlich" = "GelMask",
          "Die Hälfte der Zeit" = "HalfMask",
          "Dauerhaft" = "ImmerMask"
        )
      ),
      
      selectInput(
        "air",
        "Wie oft wird der Raum gelüftet?",
        c(
          #Werte müssen noch angepasst werden
          "Nie" = "NoAir",
          "Fenster dauerhaft gekippt" = "FensterGekippt",
          "Regelmäßiges Stoßlüften (10min/h)" = "Stoßlueften",
          "Lüftungssystem" = "Lueftungssystem"
        )
      )
      
    ),
    
    mainPanel(
      htmlOutput("text3"),
      htmlOutput("text1"),
      
      plotOutput("text2")
    ),
    
    
  )
  
))


server <- function(input, output, session) {
  output$text1 <- renderText({
    #LIST OF VARIABLES NECESSARY FOR CALCULATIONS BASED ON LELIEVELD (2020)
    
    ##RNA for 50% infection probability (D50)
    RNA_num <- 233
    
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
    
    
    #Das hier ändern in 
    #https://www.sciencedirect.com/science/article/abs/pii/S2210670720306119
    #Wenn Distance 1.5m, dann finaler Wert * 0.8, z1u15 dann * 0.9, wenn k1m dann nix
    
    
    
    if (input$distance == "k1m") {
      num_people <- 30
    } else if (input$distance == "z1u15") {
      num_people <- 12
    } else if (input$distance == "m1m") {
      num_people <- 9
    }
    num_people <- 12
    
    #Room Area in m square based on num people
    room_area <- 30
    
    #room height [m]** ** Very large room sizes violate the assumption of instantaneaous mixing of the air
    room_height <- 3
    
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
    
    
    RNA_cont_aero_conc <- var_aero_conc * var_RNA_content
    
    
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
    } else if (input$air == "Lueftungssystem") {
      room_vent_rate <- 9
    }
    
    
    #total mask efficiency (exhaling + inhaling)
    total_mask_effic <-
      0 #Normally generated through user input
    
    if (input$masks == "No") {
      total_mask_effic <- 0
    } else if (input$masks == "AT") {
      total_mask_effic <- 0.35 #ACHTUNG Wert erfunden! Hier ggf. nachsehen
    } else if (input$masks == "OP") {
      total_mask_effic <- 0.7
    } else if (input$masks == "FFP2") {
      total_mask_effic <- 0.95
    }
    
    #Das macht aus meiner Sicht keinen Sinn aber funktioniert lustiger weise (quasi falsch herum?)
    if (input$masks_dur == "NieMask") {
      total_mask_effic = total_mask_effic * 0
    } else if (input$masks_dur == "GelMask") {
      total_mask_effic = total_mask_effic * 0.3
    } else if (input$masks_dur == "HalfMask") {
      total_mask_effic = total_mask_effic * 0.6
    } else if (input$masks_dur == "ImmerMask") {
      total_mask_effic = total_mask_effic * 1
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
    
    people_infected = input$people * (infect_risk_individual/100)
    
    if (people_infected<0.5) {
      people_infected <- "Keine"
    } else if (people_infected >= 0.5 && people_infected < 1.5) {
      people_infected <- 1
    }
    
    paste(
      "<strong style=\"font-size:22px;\">",
      "<font style=\"color:DarkRed;\">",
      #Hier war vorher individuelle Risk aus risk_of_1_person_in_room_being_infected mit digits = 3
      format(people_infected, digits = 1),
      
      "</font>",
      #Hier war vorher eine Zeit
      "Personen infizieren sich im Modell mit Covid-19",
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
    
    
    if (input$distance == "k1m") {
      num_people <- 30
    } else if (input$distance == "z1u15") {
      num_people <- 12
    } else if (input$distance == "m1m") {
      num_people <- 9
    }
    num_people <- 12
    
    #Room Area in m square based on num people
    room_area <- 30
    
    #room height [m]** ** Very large room sizes violate the assumption of instantaneaous mixing of the air
    room_height <- 3
    
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
    
    
    RNA_cont_aero_conc <- var_aero_conc * var_RNA_content
    
    
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
    } else if (input$air == "Lueftungssystem") {
      room_vent_rate <- 9
    }
    
    
    #total mask efficiency (exhaling + inhaling)
    total_mask_effic <-
      0 #Normally generated through user input
    
    if (input$masks == "No") {
      total_mask_effic <- 0
    } else if (input$masks == "AT") {
      total_mask_effic <- 0.35 #ACHTUNG Wert erfunden! Hier ggf. nachsehen
    } else if (input$masks == "OP") {
      total_mask_effic <- 0.7
    } else if (input$masks == "FFP2") {
      total_mask_effic <- 0.95
    }
    
    #Das macht aus meiner Sicht keinen Sinn aber funktioniert lustiger weise (quasi falsch herum?)
    if (input$masks_dur == "NieMask") {
      total_mask_effic = total_mask_effic * 0
    } else if (input$masks_dur == "GelMask") {
      total_mask_effic = total_mask_effic * 0.3
    } else if (input$masks_dur == "HalfMask") {
      total_mask_effic = total_mask_effic * 0.6
    } else if (input$masks_dur == "ImmerMask") {
      total_mask_effic = total_mask_effic * 1
    }
    

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
    
    
    paste(
      "<strong style=\"font-size:22px;\">",
      "<font style=\"color:DarkRed;\">",
      format(infect_risk_individual, digits = 3),
      "%",
      "</font>",
      "Wahrscheinlichkeit, dass Sie sich selbst mit Covid-19 infizieren und ", "<font style=\"color:DarkRed;\">",  format(risk_of_1_person_in_room_being_infected, digits = 3), "%",
      "</font>", "Wahrscheinlichkeit, dass sich eine beliebige Person im Raum mit Covid-19 infiziert.",
      "</strong>"
      
      
    )
    
    
  })
  
  output$text2 <- renderPlot({
    #Option for RENDERPLOT: https://shiny.rstudio.com/gallery/image-output.html
    
    
    
    ############################################################################
    #COPIED FROM OUTPUT1
    ############################################################################
    
    
    #LIST OF VARIABLES NECESSARY FOR CALCULATIONS BASED ON LELIEVELD (2020)
    
    ##RNA for 50% infection probability (D50)
    RNA_num <- 233
    
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
    
    ############################################################### This may be needs reconsideration
    
    if (input$distance == "k1m") {
      num_people <- 33
    } else if (input$distance == "z1u15") {
      num_people <- 20
    } else if (input$distance == "m1m") {
      num_people <- 12
    }
    
    #Room Area in m square based on num people
    room_area <- 30
    
    #room height [m]** ** Very large room sizes violate the assumption of instantaneaous mixing of the air
    room_height <- 3
    
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
    
    
    RNA_cont_aero_conc <- var_aero_conc * var_RNA_content
    
    
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
    } else if (input$air == "Lueftungssystem") {
      room_vent_rate <- 9
    }
    
    
    #total mask efficiency (exhaling + inhaling)
    total_mask_effic <-
      0 #Normally generated through user input
    
    if (input$masks == "No") {
      total_mask_effic <- 0
    } else if (input$masks == "AT") {
      total_mask_effic <- 0.35 #ACHTUNG Wert erfunden! Hier ggf. nachsehen
    } else if (input$masks == "OP") {
      total_mask_effic <- 0.7
    } else if (input$masks == "FFP2") {
      total_mask_effic <- 0.95
    }
    
    #Das macht aus meiner Sicht keinen Sinn aber funktioniert lustiger weise (quasi falsch herum?)
    if (input$masks_dur == "NieMask") {
      total_mask_effic = total_mask_effic * 0
    } else if (input$masks_dur == "GelMask") {
      total_mask_effic = total_mask_effic * 0.3
    } else if (input$masks_dur == "HalfMask") {
      total_mask_effic = total_mask_effic * 0.6
    } else if (input$masks_dur == "ImmerMask") {
      total_mask_effic = total_mask_effic * 1
    }
    
    # susceptible # persons / room
    susceptible_num_pers_per_room <- num_people
    #Normally generated through user input
    #dosis 6 hours (per  day)*
    dosis_6_hours <-
      var_RNA_dosis / (room_vent_rate + 1 / vir_life) * (1 - total_mask_effic) *
      6
    
    #dosis infectious episode
    dosis_infectious_episode <- dosis_6_hours * inf_epis
    #browser()
    #infection risk of individual person [% / episode]
    infect_risk_individual <-
      (1 - (1 - var_infect_prob) ^ dosis_infectious_episode) * 100
    
    
    
    #risk of 1  person in room being infected [% / episode]
    
    foo <- dosis_infectious_episode * num_people
    bar <- 1 - (1 - var_infect_prob) ^ foo
    
    risk_of_1_person_in_room_being_infected <- bar * 100
    
    
    
    
    ############################################################################
    #END OF COPIED FROM OUTPUT1
    ############################################################################
    
    
    
    
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
    ggplot(data, aes(x=1:7,append_vector_worst)) + geom_line(color = "DarkRed") 
    
    
    
    #Graph der zahlen in Blau mit punkten
    plot(
      append_vector_worst,
      ylim = c(0, 30),
      type = "l",
      col = "DarkRed",
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
      col = "DarkBlue"
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
          col.main = "DarkRed",
          font.main = 4)
    
    title(ylab = "Kumulierte Fallzahlen in Deutschland in Tausend", col.lab = rgb(0, 0.5, 0))
    title(xlab = "Tägliche Infizierte in den nächsten 7 Tagen", col.lab = rgb(0, 0.5, 0))
    
    
    
    
    
    
    
  })
  
}

# Run the application
shinyApp(ui = ui, server = server)
