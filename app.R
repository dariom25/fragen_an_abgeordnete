# load packages
library(shiny)
library(bslib)
library(dplyr)
source("plots.R")

# load data
data <- read.csv(
  file.path("data", "abgeordnetenwatch_data_long_preprocessed.csv"), 
  fileEncoding = "ISO-8859-1"
)

ui <- page_sidebar(
  title = "Informationen über die Anzahl der Fragen an Abgeordnete auf Abgeordnetenwatch.de von 2005 bis 2024",
  sidebar = sidebar("Wähle deine Anzeigeeinstellungen aus!",
    dateRangeInput(
      inputId = "date_range",
      label = "Zeitspanne:",
      start = "2004-12-29",
      end = "2024-08-03",
      min = "2004-12-29",
      max = "2024-08-03",
      separator = " bis "
    ),
    
    sliderInput(
      inputId = "granularity",
      label = "Granularität:",
      min = 1,
      max = 365,
      value = 21,
      ticks = FALSE
    ),
    
    checkboxGroupInput(
      inputId = "selected_parties", 
      label = "Parteien:",
      choices = list(
        "SPD" = "SPD",
        "FDP" = "FDP",
        "BSW" = "BSW",
        "DIE LINKE" = "DIE LINKE",
        "CSU" = "CSU",
        "parteilos" = "parteilos",
        "FREIE WÄHLER" = "FREIE WÄHLER",
        "CDU" = "CDU",
        "Bündnis 90/Die Grünen" = "Bündnis 90/Die Grünen",
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
        "FREIE WÄHLER",
        "CDU",
        "Bündnis 90/Die Grünen",
        "AfD",
        "SSW"
    )),
                    
   
  ),
  card(
    plotOutput("plot_timeseries")
  ),
  card(
    plotOutput("plot_party")
  ),
  card(
    plotOutput("plot_party_topic")
  )

)

server <- function(input, output) {
  validated_date_range <- reactive({
    validate(
      need(!is.na(input$date_range[1]), "First date is invalid!"),
      need(!is.na(input$date_range[2]), "Second date is invalid!"),
      need(input$date_range[1] <= input$date_range[2], "First date must be earlier than second date!")
    )
    return(input$date_range)
  })
  
  filtered_data <- reactive({
    data |>
      filter(question_date > validated_date_range()[1] & question_date < validated_date_range()[2]) |>
      filter(party %in% input$selected_parties)
  })
  
  output$plot_party <- renderPlot({
    tryCatch({
      display_parties(
      filtered_data()
      )},
      error = function(e) {
        showNotification(
          paste("Ups, etwas hat nicht funktioniert. Bitte ändere die Einstellungen.", e$message),
          type = "error",
          duration = 5
          )
    })
  })
  output$plot_timeseries <- renderPlot({
    tryCatch({
      display_period(
        data,
        as.Date(validated_date_range()[1]),
        as.Date(validated_date_range()[2]),
        filtered_data(),
        input$granularity
      )},
      error = function(e) {
        showNotification(
          paste("Ups, etwas hat nicht funktioniert. Bitte ändere die Einstellungen.", e$message),
          type = "error",
          duration = 5
        )
    })
  })
  output$plot_party_topic <- renderPlot({
    tryCatch({
      display_parties_and_topics(
        filtered_data()
      )},
      error = function(e) {
        showNotification(
          paste("Ups, etwas hat nicht funktioniert. Bitte ändere die Einstellungen.", e$message),
          type = "error",
          duration = 5
        )
    })
  })
}


shinyApp(ui = ui, server = server)