#Wiktor Kostera
library(rvest)
library(dplyr)
library(ggplot2)

#Pobieranie danych z Wikipedii
url <- "https://en.wikipedia.org/wiki/List_of_tallest_buildings_and_structures"
page <- read_html(url)

#Ekstrakcja tabeli z HTML
tab <- page %>%
  html_nodes(css = "table.wikitable")
tabela <- tab[[1]] %>% html_table(fill=T)

str(tabela)

#Zmiana nazw kolumn i wartości na numeryczne
colnames(tabela) <- c("Kategoria", "Nazwa", "Kraj", "Miasto", "Wysokosc(m)", "Wysokosc(ft)", "Rok", "Koordynaty")

tabela$Rok <- as.numeric(tabela$Rok, na.rm = T)
tabela$`Wysokosc(m)` <- as.numeric(tabela$`Wysokosc(m)`, na.rm = T)

#Wykres 1 - Top 20 najwyższych budynków na świecie
najwyzsze <- tabela %>%
  select(Nazwa, `Wysokosc(m)`) %>%
  slice(1:10)

najwyzsze
najwyzsze %>%
  ggplot() +
  geom_bar(aes(x = reorder(Nazwa, desc(`Wysokosc(m)`)), y = `Wysokosc(m)`),
           stat = "identity",
           color = "peachpuff4",
           fill = "peachpuff") + 
  theme_light() +
  labs(title = "Top 20 najwyższych budynków na świecie",
       x = "Nazwa budynku",
       y = "Wysokość budynku") + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
 
#Wykres 2 - Kraje z największą ilością najwyższych budynków
kraje <- tabela %>%
  select(Kraj) %>%
  group_by(Kraj) %>%
  summarize(Ilosc = n()) %>%
  arrange(desc(Ilosc)) %>%
  slice(1:10)
kraje

kraje %>%
  ggplot() +
  geom_bar(aes(x = reorder(Kraj, desc(Ilosc)), y = Ilosc),
           stat = "identity",
           color = "darkblue",
           fill = "lightblue") + 
  theme_light() +
  labs(title = "Kraje z największą ilością najwyższych budynków",
       x = "Kraj",
       y = "Ilość budynków") + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

#Wykres 3 - Wysokość budynków w czasie
tabela %>%
  ggplot() +
  geom_point(aes(x = Rok, y = `Wysokosc(m)`),
             color = "blue",
             alpha = 0.5,
             na.rm = T) +
  theme_light() +
  labs(title = "Wysokość budynków w czasie",
       x = "Rok",
       y = "Wysokość w metrach")

#Wykres 4 - Zmiana średniej wysokości budynków w latach
srednia_czas <- tabela %>%
  filter(Rok >= 2000) %>%
  group_by(Rok) %>%
  summarize(Średnia = mean(`Wysokosc(m)`))

srednia_czas$Średnia[is.na(srednia_czas$Średnia)] <- mean(srednia_czas$Średnia, na.rm = TRUE)
srednia_czas

srednia_czas %>%
  ggplot() +
  geom_point(aes(x = Rok, y = Średnia),
             color = "darkblue",
             size = 5) +
  geom_line(aes(x = Rok, y = Średnia),
            color = "red",
            linewidth = 0.2) +
  theme_light() +
  labs(title = "Zmiana średniej wysokości budynków w latach",
       x = "Rok",
       y = "Średnia wysokość w metrach")
