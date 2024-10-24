# load packages
library(shiny)
library(bslib)
library(tidyverse)
source("plots.R")

# load data
data <- read.csv(".//data//abgeordnetenwatch_data_long_preprocessed.csv", fileEncoding = "ISO-8859-1")

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
  ),

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
  
  output$plot_party <- renderPlot({
    display_parties(
      data,
      validated_date_range()[1],
      validated_date_range()[2],
      input$selected_parties
    )
  })
  output$plot_timeseries <- renderPlot({
    display_period(
      data,
      as.Date(validated_date_range()[1]),
      as.Date(validated_date_range()[2]),
      input$selected_parties,
      input$granularity
    )
  })
  output$plot_party_topic <- renderPlot({
    display_parties_and_topics(
      data,
      as.Date(validated_date_range()[1]),
      as.Date(validated_date_range()[2]),
      input$selected_parties
    )
  })
}


shinyApp(ui = ui, server = server)