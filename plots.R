library(tidyverse)

party_colors <- c(
  "AfD" = "#009ee0",
  "BSW" = "#7d254f",
  "Bündnis 90/Die Grünen" = "#409A3C",
  "CDU" = "#151518",
  "CSU" = "#008AC5",
  "DIE LINKE" = "#BE3075",
  "FDP" = "#FFED00",
  "FREIE WÄHLER" = "#F7A800",
  "parteilos" = "grey",
  "SPD" = "#E3000F",
  "SSW" = "#003c91",
  "Unbekannt" = "black"
)

display_parties <- function(data, start, end, selected) {
  data |>
    filter(question_date > start & question_date < end) |>
    filter(party %in% selected) |>
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
      hjust = 0
    ) +
    labs(
      x = "Anzahl der Fragen",
      y = NULL,
      title = "Anzahl der insgesamt gestellten Fragen nach Partei",
      fill = "Partei"
    ) +
    scale_fill_manual(values = party_colors, guide = FALSE) 
    
}

# TODO: X-Achsenbeschriftung anpassen - Datumsangaben oder so hinzufügen
# TODO: Gucken, warum immer Intervall + 1 angezeigt wird
display_period <- function(data, start, end, selected, granularity) {
  
  # create intervals for data aggregation
  no_of_days <- as.integer(floor((end - start)))
  interval_floored <- no_of_days%/%granularity
  interval_remainder <- no_of_days%%granularity
  
  interval_length <- c(
    rep(interval_floored + 1, interval_remainder),
    rep(interval_floored, granularity - interval_remainder)
  )
  
  # select and transform relevant data
  data |>
    filter(party %in% selected) |>
    filter(question_date >= start & question_date <= end) |>
    mutate(interval = findInterval(
      as.integer(as.Date(question_date) - start), 
      cumsum(interval_length)
    )) |>
    group_by(interval, party) |>
    summarise(
      no_of_questions = n(),
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
      x = paste0("Intervall (~", interval_floored, " Tage pro Intervall)"),
      color = NULL,
      title = "Anzahl der Fragen in Zeitintervallen nach Partei"
    ) +
    scale_x_continuous(breaks = seq(0, granularity, by = 1)) +
    scale_color_manual(values = party_colors) 
}

# heatmap for parties-topic frequency
display_parties_and_topics <- function(data, start, end, selected) {
  
  data |>
    filter(party %in% selected) |>
    filter(question_date >= start & question_date <= end) |>
    group_by(topics_mapped, party) |>
    summarise(
      no_of_questions = n()
    ) |>
    ungroup() |>
    ggplot(aes(x = topics_mapped, y = party, fill = no_of_questions)) +
    geom_raster() +
    scale_x_discrete(guide = guide_axis(angle = 45)) +
    labs(
      y = NULL,
      title = "Anzahl der gestellten Fragen zu Themen nach Partei",
      x = NULL,
      fill = "Anzahl der Fragen"
    ) +
    scale_fill_gradient(low = "lightblue", high = "darkred")
}
