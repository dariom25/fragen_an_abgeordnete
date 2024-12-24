library(dplyr)
library(ggplot2)
library(forcats)
library(tibble)

# load data
data <- read.csv(
  file.path("data", "abgeordnetenwatch_data_long_preprocessed_utf8_2.csv")
)

display_period <- function(data, start, end, interval_lengths, parties) {
  
  # create intervals for data aggregation
  no_of_days <- as.integer(floor((end - start)))
  no_of_intervals = ceiling(no_of_days/interval_lengths)

  # get starting date for each interval
  interval_dates <- data |>
    filter(question_date >= start & question_date <= end) |>
    filter(party %in% parties) |>
    mutate(interval = findInterval(
      as.integer(as.Date(question_date) - start), 
      c(0, interval_lengths*1:no_of_intervals),
    )) |>     
    group_by(interval) |>
    arrange(question_date) |>
    slice(1) |>
    ungroup() |> 
    select(question_date)
  
  # select and transform relevant data
  data |>
    filter(question_date >= start & question_date <= end) |>
    filter(party %in% parties) |>
    mutate(interval = findInterval(
      as.integer(as.Date(question_date) - start), 
      c(0, interval_lengths*1:no_of_intervals))) |>
    group_by(interval, party) |>
    summarise(
      no_of_questions = n()
    ) |>
    ungroup() |>
    
    # create plot
    ggplot(
      aes(
        x = interval,
        y = no_of_questions,
        colour = party,
        group = party
      )
    ) +
    geom_line(position = position_dodge(0.2)) +
    geom_point(position = position_dodge(0.2))+
    labs(
      y = "Anzahl der Fragen",
      x = paste0("Intervalllänge (~", interval_lengths, " Tag(e))"),
      color = NULL,
      title = "Anzahl der Fragen in Zeitintervallen nach Partei"
    ) +
    scale_x_continuous(
      breaks = c(1:length(interval_dates$question_date)), 
      labels = interval_dates$question_date,
      guide = guide_axis(angle = 45)
    ) +
    #scale_color_manual(values = party_colors) +
    theme_bw() + 
    theme(
      panel.grid.minor = element_blank(),
      axis.title = element_text(size = 18),
      axis.text = element_text(size = 13),
      plot.title = element_text(face = "bold", size = 20),
      legend.text = element_text(size = 13)
    )
}

display_period(data, as.Date("2019-02-21"), as.Date("2024-08-03"), 60, c("SPD", "SSW", "FDP"))

