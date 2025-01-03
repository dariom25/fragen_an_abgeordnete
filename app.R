# load packages
library(shiny)
library(bslib)
library(dplyr)
library(stringr)
source("plots.R")

# load data
data <- read.csv(
  file.path("data", "abgeordnetenwatch_data_long_preprocessed_utf8.csv")
)

data$party <- str_replace_all(data$party, "ü", "ue")
data$party <- str_replace_all(data$party, "Ä", "AE")

selected_parties <- checkboxGroupInput(
  inputId = "selected_parties", 
  label = "Parteien:",
  choices = list(
    "SPD" = "SPD",
    "FDP" = "FDP",
    "BSW" = "BSW",
    "DIE LINKE" = "DIE LINKE",
    "CSU" = "CSU",
    "parteilos" = "parteilos",
    "FREIE WAEHLER" = "FREIE WAEHLER",
    "CDU" = "CDU",
    "Buendnis 90/Die Gruenen" = "Buendnis 90/Die Gruenen",
    "AfD" = "AfD",
    "SSW" = "SSW"
  ),
  selected = c(
    "SPD",
    "FDP",
    "BSW",
    "DIE LINKE",
    "CSU",
    "parteilos",
    "FREIE WAEHLER",
    "CDU",
    "Buendnis 90/Die Gruenen",
    "AfD",
    "SSW"
  ))

selected_topics <- checkboxGroupInput(
  inputId = "selected_topics",
  label = "Topics:",
  choices = list(
    "Arbeit und Beschäftigung" = "Arbeit und Beschäftigung",
    "Außenpolitische Themen" = "Außenpolitische Themen",
    "Bildung und Forschung" = "Bildung und Forschung",
    "Digitales" = "Digitales",
    "Energie und Umwelt" = "Energie und Umwelt",
    "Finanzen und Wirtschaft" = "Finanzen und Wirtschaft",
    "Frauen, Jugend, Familie" = "Frauen, Jugend, Familie",
    "Gesundheit und Ernährung" = "Gesundheit und Ernährung",
    "Inneres und Sicherheit" = "Inneres und Sicherheit",
    "Migration und Aufenthaltsrecht" = "Migration und Aufenthaltsrecht",
    "Politik und Parteien" = "Politik und Parteien",
    "Sport, Kultur und Tourismus" = "Sport, Kultur und Tourismus",
    "Wahlen" = "Wahlen"
  ),
  selected = c(
    "Arbeit und Beschäftigung",
    "Außenpolitische Themen",
    "Bildung und Forschung",
    "Digitales",
    "Energie und Umwelt",
    "Finanzen und Wirtschaft",
    "Frauen, Jugend, Familie",
    "Gesundheit und Ernährung",
    "Inneres und Sicherheit",
    "Migration und Aufenthaltsrecht",
    "Politik und Parteien",
    "Sport, Kultur und Tourismus",
    "Wahlen"
  ))

ui <- page_sidebar(
  navset_bar(
    nav_panel("Parteien gesamt", plotOutput("plot_party")),
    nav_panel("Fragenzahl im Zeitverlauf", plotlyOutput("plot_timeseries")),
    nav_panel("Topics gesamt", plotOutput("plot_topic")),
    nav_panel("Topics im Zeitverlauf", plotlyOutput("plot_timeseries_topics")),
    nav_panel("Parteien X Topics", plotOutput("plot_party_topic")),
  ),
  title = "Fragen auf Abgeordnetenwatch.de von 2005 bis 2024",
  sidebar = sidebar("Wähle deine Anzeigeeinstellungen aus!",
    width = 300,
    dateRangeInput(
      inputId = "date_range",
      label = "Zeitspanne:",
      start = "2004-12-29",
      end = "2024-08-03",
      min = "2004-12-29",
      max = "2024-08-03",
      separator = " bis ",
    ),
    
    sliderInput(
      inputId = "granularity",
      label = "Zeitintervall (in Tagen):",
      min = 1,
      max = 365,
      value = 365,
      ticks = FALSE
    ),
      
    accordion(
      open = FALSE,
      accordion_panel(
        "Parteien",
        selected_parties
    )),
    
    accordion(
      open = FALSE,
      accordion_panel(
        "Topics",
        selected_topics
    )),
    
  )
)

server <- function(input, output) {
  validated_date_range <- reactive({
    validate(
      need(!is.na(input$date_range[1]), "Erstes Datum ist ungültig!"),
      need(!is.na(input$date_range[2]), "Zweites Datum ist ungültig!"),
      need(input$date_range[1] <= input$date_range[2], "Erstes Datum muss vor zweitem Datum liegen!"),
      need(input$date_range[1] != input$date_range[2], "Erstes Datum darf nicht gleich zweites Datum sein!")
    )
    return(input$date_range)
  })
  
  filtered_data_parties <- reactive({
    data |>
      filter(question_date >= validated_date_range()[1] & question_date <= validated_date_range()[2]) |>
      filter(party %in% input$selected_parties)
  })
  
  filtered_data_topics <- reactive({
    data |>
      filter(question_date >= validated_date_range()[1] & question_date <= validated_date_range()[2]) |>
      filter(topics_mapped %in% input$selected_topics)
  })
  
  output$plot_party <- renderPlot({
    tryCatch({
      display_parties(
      filtered_data_parties()
      )},
      error = function(e) {
        showNotification(
          paste("Ups, etwas hat nicht funktioniert. Bitte ändere die Einstellungen.", e$message),
          type = "error",
          duration = 15
          )
    })
  })
  output$plot_topic <- renderPlot({
    tryCatch({
      display_topics(
        filtered_data_topics()
      )},
      error = function(e) {
        showNotification(
          paste("Ups, etwas hat nicht funktioniert. Bitte ändere die Einstellungen.", e$message),
          type = "error",
          duration = 15
        )
      })
  })
  output$plot_timeseries <- renderPlotly({
    tryCatch({
      display_period(
        as.Date(validated_date_range()[1]),
        as.Date(validated_date_range()[2]),
        filtered_data_parties(),
        input$granularity
      )},
      error = function(e) {
        showNotification(
          paste("Ups, etwas hat nicht funktioniert. Bitte ändere die Einstellungen.", e$message),
          type = "error",
          duration = 15
        )
    })
  })
  output$plot_party_topic <- renderPlot({
    tryCatch({
      display_parties_and_topics(
        filtered_data_parties()
      )},
      error = function(e) {
        showNotification(
          paste("Ups, etwas hat nicht funktioniert. Bitte ändere die Einstellungen.", e$message),
          type = "error",
          duration = 15
        )
    })
  })
  output$plot_timeseries_topics <- renderPlotly({
      tryCatch({
        display_period_topics(
          as.Date(validated_date_range()[1]),
          as.Date(validated_date_range()[2]),
          filtered_data_topics(),
          input$granularity
        )},
        error = function(e) {
          showNotification(
            paste("Ups, etwas hat nicht funktioniert. Bitte ändere die Einstellungen.", e$message),
            type = "error",
            duration = 15
          )
        })
    })
}

shinyApp(ui = ui, server = server)