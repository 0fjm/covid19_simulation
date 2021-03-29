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


  titlePanel(div(HTML("<b>Covid19-Simulation</b>"))),
  sidebarLayout(
    position = "left",
    sidebarPanel(
      (div(HTML("<h3><b>Maßnahmen gegen Covid-19</b></h3>"))),
      
      
      
      
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
    
    mainPanel(htmlOutput("text1"), plotOutput("text2")),
    
    
  )
  
)




)


server <- function(input, output, session) {
  
  output$text0 <- renderText({
    paste("Hallo")
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
    
    #infection risk of individual person [% / episode]
    infect_risk_individual <-
      (1 - (1 - var_infect_prob) ^ dosis_infectious_episode) * 100
    
    #risk of 1  person in room being infected [% / episode]
    
    foo <- dosis_infectious_episode * num_people
    bar <- 1 - (1 - var_infect_prob) ^ foo
    
    risk_of_1_person_in_room_being_infected <- bar * 100
    
    
    paste("<h4>","Die Wahrscheinlichkeit, dass sich eine Person im Raum mit Covid-19 infiziert ist: ","<font color=\"#0000FF\"<b>", risk_of_1_person_in_room_being_infected, "</b>", "%","</h4>")
    
    
  })
  
  output$text2 <- renderPlot({
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
    last_value <- 2772401
    R_value <- risk_of_1_person_in_room_being_infected / 100
    foo <- (3.5 * R_value)
    append_vector <- integer()
    
    for (i in 1:50) {
      append_vector[i] <- last_value + foo * last_value
      last_value <- append_vector[i]
      foo <-(foo * R_value)
    }
    
    #Vector mit Zahlen
    #### WORST AND BEST CASE SCENARIOS
    worst_case <- 0.95
    best_case <- 0.01
    
    append_vector_worst <- integer()
    append_vector_best <- integer()
    
    if (FALSE ) {
    for (i in 1:50) {
      append_vector_worst[i] <- last_value + foo * last_value
      last_value <- append_vector[i]
      foo <-(foo * worst_case)
    }
    
    
    append_vector_best <- integer()
    
    for (i in 1:50) {
      append_vector_worst[i] <- last_value + foo * last_value
      last_value <- append_vector[i]
      foo <-(foo * best_case)
    }
    }
    
    ####### Absolute CASE NUMBERS FROM RKI
    case_nrs <-
      c(
        170,
        210,
        295,
        451,
        637,
        823,
        965,
        1065,
        1411,
        2010,
        2767,
        3757,
        5208,
        6513,
        7491,
        9535,
        12576,
        16190,
        20255,
        24307,
        27664,
        29931,
        33669,
        38551,
        44233,
        50140,
        56126,
        60853,
        63935,
        68059,
        74120,
        80381,
        86935,
        93124,
        97467,
        100018,
        103695,
        108894,
        114175,
        119088,
        122447,
        125335,
        127201,
        128800,
        131275,
        134604,
        138025,
        141079,
        143190,
        144569,
        146294,
        148480,
        150973,
        153060,
        154965,
        156240,
        156944,
        158062,
        159532,
        160966,
        162424,
        163343,
        163964,
        164389,
        165116,
        166209,
        167431,
        168629,
        169609,
        170288,
        170629,
        171317,
        172170,
        173081,
        173887,
        174605,
        175067,
        175384,
        175935,
        176665,
        177498,
        177917,
        178445,
        178783,
        179008,
        179421,
        180038,
        180686,
        181213,
        181662,
        182036,
        182215,
        182352,
        182656,
        183152,
        183680,
        184118,
        184403,
        184567,
        184864,
        185292,
        185809,
        186087,
        186437,
        186736,
        186908,
        187168,
        187710,
        188692,
        189326,
        190258,
        190783,
        191039,
        191524,
        192075,
        192579,
        193150,
        193725,
        194053,
        194252,
        194670,
        195132,
        195616,
        196086,
        196512,
        196804,
        196946,
        197266,
        197657,
        198110,
        198538,
        198965,
        199243,
        199374,
        199698,
        200159,
        200696,
        201253,
        201776,
        202204,
        202390,
        202846,
        203367,
        204065,
        204762,
        205568,
        206111,
        206322,
        206849,
        207602,
        208507,
        209361,
        210281,
        210841,
        211142,
        211940,
        212886,
        213999,
        215165,
        216170,
        216880,
        217199,
        218304,
        219497,
        221038,
        222527,
        223903,
        224657,
        225149,
        226699,
        228235,
        229987,
        231608,
        233262,
        234176,
        234740,
        236171,
        237649,
        239259,
        240802,
        242191,
        243023,
        243561,
        244745,
        246065,
        247482,
        249002,
        250495,
        251602,
        252180,
        253592,
        254974,
        256727,
        258318,
        259970,
        261195,
        261953,
        263435,
        265440,
        267637,
        269990,
        272164,
        273558,
        274249,
        275906,
        277721,
        279926,
        282322,
        284651,
        286436,
        287314,
        289134,
        291304,
        294037,
        296963,
        299882,
        302091,
        303244,
        305649,
        309237,
        313725,
        318405,
        323458,
        327093,
        329371,
        333558,
        338931,
        346183,
        354105,
        362079,
        367775,
        371459,
        377932,
        387376,
        399779,
        413675,
        427718,
        438827,
        446294,
        458319,
        474289,
        493548,
        513408,
        532836,
        546854,
        557392,
        572650,
        592001,
        613229,
        635515,
        657106,
        673035,
        683201,
        699181,
        718803,
        741505,
        765017,
        786404,
        802212,
        811076,
        825793,
        846142,
        869841,
        893605,
        915911,
        930705,
        939559,
        954655,
        974094,
        996184,
        1018965,
        1039114,
        1054043,
        1062847,
        1076177,
        1095426,
        1118627,
        1143083,
        1165370,
        1181089,
        1191342,
        1207821,
        1230655,
        1258332,
        1287425,
        1314722,
        1335375,
        1347880,
        1366967,
        1393899,
        1425275,
        1457324,
        1487107,
        1508862,
        1522817,
        1543195,
        1572207,
        1606230,
        1627418,
        1640637,
        1652227,
        1662065,
        1677804,
        1705363,
        1736477,
        1756135,
        1766399,
        1776415,
        1785249,
        1799474,
        1826869,
        1854873,
        1881031,
        1905581,
        1922235,
        1930734,
        1944255,
        1966838,
        1990941,
        2011398,
        2029708,
        2043106,
        2049723,
        2058885,
        2077334,
        2096849,
        2113666,
        2128704,
        2140292,
        2145313,
        2152074,
        2167569,
        2183401,
        2196789,
        2209361,
        2219360,
        2223572,
        2229447,
        2241781,
        2255207,
        2266718,
        2276740,
        2285136,
        2288224,
        2292430,
        2301527,
        2311809,
        2321174,
        2329710,
        2335335,
        2339092,
        2343494,
        2352756,
        2362763,
        2372019,
        2380759,
        2388552,
        2391599,
        2395999,
        2406209,
        2417535,
        2427482,
        2436941,
        2444612,
        2448079,
        2452783,
        2463754,
        2475078,
        2485204,
        2494882,
        2503197,
        2506574,
        2511421,
        2522940,
        2536943,
        2550082,
        2562621,
        2573106,
        2577977,
        2584787,
        2600574,
        2618920,
        2635628,
        2651288,
        2664511,
        2670473,
        2679319,
        2698615,
        2721029,
        2742499,
        2761420,
        2772401
      )
    
    #Blödsinn aktuell noch
    case_nrs2 <- c(case_nrs, append_vector)
    append_vector_worst <- c(case_nrs,append_vector_worst)
    append_vector_best <- c(case_nrs, append_vector_best)
    
    #Graph der zahlen in Blau mit punkten
    plot(case_nrs2, type = "l", col = "blue", axes=TRUE, ann=FALSE)

    box()
    
    #Blödsinn aktuell noch
    #lines(append_vector_worst, type = "l", pch=22, lty=, col = "red")
    #lines(append_vector_best, type = "l", pch=22, lty=, col = "green")
    
    
    
    #Titel mit roter fetter schrift
    title(main = "Pandemieentwicklung in Deutschland",
          col.main = "red",
          font.main = 4)
    
    title(ylab="Kumulierte Fallzahlen in Deutschland", col.lab=rgb(0,0.5,0))
    title(xlab="Tage seit Ausbruch der Covid-19 Pandemie", col.lab=rgb(0,0.5,0))
    
    
    
  })
  
}

# Run the application
shinyApp(ui = ui, server = server)
