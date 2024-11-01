# Fragen an Abgeordnete von 2005 bis 2024
Hier einsehbar: https://dariom25.github.io/fragen_an_abgeordnete (Das Laden dauert leider kurz.)

## Beschreibung
Eine kleine, interaktive Shiny-App in R, die mit Daten von Abgeordnetenwatch.de eine Übersicht über die Zahl der gestellten Fragen an gewählte Abgeordnete auf Landes-, Bundes- und EU-Ebene auf Abgeordnetenwatch.de gibt. Die App umfasst drei Grafiken. 
#Bild
Bei der ersten Grafik handelt es sich um ein Liniendiagramm, dass die Zahl der gestellten Fragen an die ausgewählten Parteien in einem gewählten Zeitraum darstellt. Dabei kann selbst bestimmt werden, wie groß die Intervalle der einzelnen Datenpunkte sind, welche Parteien angezeigt werden und in welchem Zeitraum die Anzahl der Fragen dargestellt werden soll.
#Bild
Die zweite Grafik ist ein Balkendiagramm, dass die Zahl der insgesamt gestellten Fragen pro Partei im gewählten Zeitraum anzeigt. Hier können Partei und Zeitraum selber gewählt werden.
#Bild
In der dritten Grafik wird eine Heatmap dargestellt, die angibt wie viele Frage an welche Partei zu einem bestimmten Thema gestellt werden.

## Datengrundlage
Als Datengrundlage dient ein selbst zusammengestellter Datensatz, der alle Fragen und Antworten der gewählten Abgeordneten auf Abgeordnetenwatch.de zum 03.08.2024 enthält. Da bei der Datenerhebung nur zum Zeitpunkt der Erhebung gewählte Abgeordnete berücksichtigt werden, sind die Daten bis ca. 2019 nicht vollständig, weil nicht alle Abgeordneten, die 2024 gewählt sind, schon vorher gewählt waren.

## Bekannte Probleme
- Grüne und Freie Wähler werden wegen Umlaut und Formatierung aktuell nicht angezeigt
- Ein Intervalltick zu viel in Grafik 1
