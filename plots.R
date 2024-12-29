library(dplyr)
library(ggplot2)
library(forcats)
library(RColorBrewer)
library(plotly)

party_colors <- c(
  "AfD" = "#009ee0",
  "BSW" = "#7d254f",
  "Buendnis 90/Die Gruenen" = "#409A3C",
  "CDU" = "#151518",
  "CSU" = "#008AC5",
  "DIE LINKE" = "#BE3075",
  "FDP" = "#FFED00",
  "FREIE WAEHLER" = "#F7A800",
  "parteilos" = "grey",
  "SPD" = "#E3000F",
  "SSW" = "#003c91"
)

# create colorpalette for topics
colors <- brewer.pal(12, "Paired")
colors <- c(colors, "#E41A1C")
topic_colors <- c(
  "Arbeit und Beschäftigung" = colors[1],
  "Außenpolitische Themen" = colors[2],
  "Bildung und Forschung" = colors[3],
  "Digitales" = colors[4],
  "Energie und Umwelt" = colors[5],
  "Finanzen und Wirtschaft" = colors[6],
  "Frauen, Jugend, Familie" = colors[7],
  "Gesundheit und Ernährung" = colors[8],
  "Inneres und Sicherheit" = colors[9],
  "Migration und Aufenthaltsrecht" = colors[10],
  "Politik und Parteien" = colors[11],
  "Sport, Kultur und Tourismus" = colors[12],
  "Wahlen" = colors[13]
)

# bar plot for parties
display_parties <- function(data) {
  data |>
    group_by(party) |>
    summarise(
      no_of_questions = n()
    ) |>
    ungroup() |>
    arrange(no_of_questions) |>
    mutate(party = factor(party, levels = party)) |>
    ggplot(
      aes(
        x = no_of_questions,
        y = fct_rev(fct_infreq(party)),
        fill = party
      )
    ) +
    geom_bar(
      stat = "identity"
    ) +
    geom_text(
      aes(
        label = no_of_questions
      ),
      vjust = 0.5,
      hjust = -0.2
    ) +
    labs(
      x = "Anzahl der Fragen",
      y = NULL,
      title = "Anzahl der insgesamt gestellten Fragen nach Partei",
      fill = "Partei"
    ) +
    scale_fill_manual(values = party_colors, guide = FALSE) +
    theme_bw() + 
    theme(
      panel.grid.minor = element_blank(),
      axis.title = element_text(size = 18),
      axis.text = element_text(size = 13),
      plot.title = element_text(face = "bold", size = 20)
    )
}

# lineplot for parties
display_period <- function(start, end, filtered_data, interval_lengths) {

  # create intervals for data aggregation
  no_of_days <- as.integer(floor((end - start)))
  no_of_intervals = ceiling(no_of_days/interval_lengths)
  
  # get starting date for each interval
  starting_interval_date <- seq(start, end, by = interval_lengths)

  # select and transform relevant data
  plot <- filtered_data |>
    mutate(interval = findInterval(
      as.integer(as.Date(question_date) - start), 
      c(0, interval_lengths*1:no_of_intervals))) |>
    group_by(interval, party) |>
    summarise(no_of_questions = n(), .groups = "drop") |>
    
    # create plot
    ggplot(
      aes(
        x = interval,
        y = no_of_questions,
        colour = party,
        group = party,
        text = paste(
          "Partei:", party,
          "<br>Fragen:", no_of_questions,
          "<br>Intervallstart:", as.character(starting_interval_date[interval])
        )
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
      breaks = c(1:length(starting_interval_date)), 
      labels = starting_interval_date
    ) +
    scale_color_manual(values = party_colors) +
    theme_bw() + 
    theme(
      panel.grid.minor = element_blank(),
      axis.title = element_text(size = 18),
      axis.text = element_text(size = 13),
      plot.title = element_text(face = "bold", size = 20),
      legend.text = element_text(size = 13)
    )
  ggplotly(plot, tooltip = "text") |>
    layout(
      xaxis = list(
        tickangle = 90
      )
    )
}

# heatmap for parties-topic frequency
display_parties_and_topics <- function(data) {
  
  data |>
    group_by(topics_mapped, party) |>
    summarise(
      no_of_questions = n()
    ) |>
    ungroup() |>
    ggplot(aes(x = topics_mapped, y = party, fill = no_of_questions)) +
    geom_raster() +
    geom_text(aes(label = no_of_questions)) +
    scale_x_discrete(guide = guide_axis(angle = 45)) +
    labs(
      y = NULL,
      title = "Anzahl der gestellten Fragen zu Themen nach Partei",
      x = NULL,
      fill = "Anzahl der Fragen"
    ) +
    scale_fill_gradient(low = "white", high = "steelblue") +
    theme_bw() + 
    theme(
      panel.grid.minor = element_blank(),
      axis.title = element_text(size = 18),
      axis.text = element_text(size = 13),
      plot.title = element_text(face = "bold", size = 20),
      legend.title = element_text(size = 13),
      legend.text = element_text(size = 13)
    )
}

# bar plot for topics
display_topics <- function(data) {
  data |>
    group_by(topics_mapped) |>
    summarise(
      no_of_questions = n()
    ) |>
    ungroup() |>
    arrange(no_of_questions) |>
    mutate(topics_mapped = factor(topics_mapped, levels = topics_mapped)) |>
    ggplot(
      aes(
        x = no_of_questions,
        y = fct_rev(fct_infreq(topics_mapped)),
        fill = topics_mapped
      )
    ) +
    geom_bar(
      stat = "identity"
    ) +
    geom_text(
      aes(
        label = no_of_questions
      ),
      vjust = 0.5,
      hjust = -0.2
    ) +
    labs(
      x = "Anzahl der Fragen",
      y = NULL,
      title = "Anzahl der insgesamt gestellten Fragen nach Topic",
      fill = "Partei"
    ) +
    scale_fill_manual(values = topic_colors, guide = FALSE) +
    theme_bw() + 
    theme(
      panel.grid.minor = element_blank(),
      axis.title = element_text(size = 18),
      axis.text = element_text(size = 13),
      plot.title = element_text(face = "bold", size = 20)
    )
}

# lineplot for topics
display_period_topics <- function(start, end, filtered_data, interval_lengths) {
  

  
  # create intervals for data aggregation
  no_of_days <- as.integer(floor((end - start)))
  no_of_intervals = ceiling(no_of_days/interval_lengths)
  
  # get starting date for each interval
  starting_interval_date <- seq(start, end, by = interval_lengths)
  
  # select and transform relevant data
  filtered_data |>
    mutate(interval = findInterval(
      as.integer(as.Date(question_date) - start), 
      c(0, interval_lengths*1:no_of_intervals))) |>
    group_by(interval, topics_mapped) |>
    summarise(no_of_questions = n(), .groups = "drop") |>
  
    # create plot
    ggplot(
      aes(
        x = interval,
        y = no_of_questions,
        colour = topics_mapped,
        group = topics_mapped
      )
    ) +
    geom_line(position = position_dodge(0.2)) +
    geom_point(position = position_dodge(0.2))+
    labs(
      y = "Anzahl der Fragen",
      x = paste0("Intervalllänge (~", interval_lengths, " Tag(e))"),
      color = NULL,
      title = "Anzahl der Fragen in Zeitintervallen nach Topic"
    ) +
    scale_color_manual(values = topic_colors) +
    scale_x_continuous(
      breaks = c(1:length(starting_interval_date)), 
      labels = starting_interval_date,
      guide = guide_axis(angle = 90)
    ) +
    theme_bw() + 
    theme(
      panel.grid.minor = element_blank(),
      axis.title = element_text(size = 18),
      axis.text = element_text(size = 13),
      plot.title = element_text(face = "bold", size = 20),
      legend.text = element_text(size = 13)
    )
}
