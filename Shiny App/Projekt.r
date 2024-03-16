students <- read.csv("/Users/wiktorkostera/Desktop/Programowanie w R/StudentsPerformance.csv")

library(ggplot2)
library(dplyr)
library(plotly)
library(corrplot)

colnames(students) <- c("Płeć", "Grupa etniczna", "Poziom wykształcenia rodziców",
                        "Rodzaj spożywanych posiłków", "Kurs przygotowawczy", "Wynik-matematyka",
                        "Wynik-czytanie", "Wynik-pisanie")
students$Płeć <- ifelse(students$Płeć == "female", "Kobieta", "Mężczyzna")

students$`Grupa etniczna` <- ifelse(students$`Grupa etniczna` == "group A", "Hawajczycy i inni", 
                                    ifelse(students$`Grupa etniczna` == "group B", "Azjaci",
                                           ifelse(students$`Grupa etniczna` == "group C", "Biali",
                                                  ifelse(students$`Grupa etniczna` == "group D", "Afroamerykanie", "Rdzenni Amerykanie"))))

students$`Poziom wykształcenia rodziców` <- ifelse(students$`Poziom wykształcenia rodziców` == "associate's degree", "Studia policealne",
                                                   ifelse(students$`Poziom wykształcenia rodziców` == "bachelor's degree", "Licencjat",
                                                          ifelse(students$`Poziom wykształcenia rodziców` == "high school", "Średnie",
                                                                 ifelse(students$`Poziom wykształcenia rodziców` == "master's degree", "Magister",
                                                                        ifelse(students$`Poziom wykształcenia rodziców` == "some college", "Studia nieukończone", "Podstawowe")))))

students$`Rodzaj spożywanych posiłków` <- ifelse(students$`Rodzaj spożywanych posiłków` == "free/reduced", "Podstawowy", "Pełny")

students$`Kurs przygotowawczy` <- ifelse(students$`Kurs przygotowawczy` == "completed", "Ukończony", "Brak")

students$Płeć <- as.factor(students$Płeć)

students$`Grupa etniczna` <- as.factor(students$`Grupa etniczna`)

students$`Poziom wykształcenia rodziców` <- as.factor(students$`Poziom wykształcenia rodziców`)
students$`Poziom wykształcenia rodziców` <- factor(students$`Poziom wykształcenia rodziców`, levels = c("Magister", "Licencjat", "Studia policealne",
                  "Studia nieukończone", "Średnie", "Podstawowe"))

students$`Rodzaj spożywanych posiłków` <- as.factor(students$`Rodzaj spożywanych posiłków`)

students$`Kurs przygotowawczy` <- as.factor(students$`Kurs przygotowawczy`)

str(students)

#Rozklad zmiennych kategorycznych
#Wykres - plec
plot_ly(students,
        labels = ~Płeć,
        type = 'pie') %>%
  layout(title = "Płeć studentów",
         colorway = c("hotpink","mediumblue"))

#Wykres - Grupa etniczna
students %>%
  ggplot() +
  geom_bar(aes(x = `Grupa etniczna`),
           color = "darkblue",
           fill = "lightskyblue") +
  theme_light() +
  theme(plot.title = element_text(hjust = 0.5)) +
  labs(title = "Liczba studentów w poszczególnych grupach etnicznych",
       y = "Liczba studentów")

#Wykres - Poziom wyksztalcenia rodzicow
students %>%
  ggplot() +
  geom_bar(aes(x = `Poziom wykształcenia rodziców`),
           color = "indianred4",
           fill = "indianred") +
  theme_light() +
  theme(plot.title = element_text(hjust = 0.5),
        axis.text.x = element_text(angle = 45, hjust = 1)) + 
  labs(title = "Liczba rodziców studentów\nz poszczególnym poziomem wykształcenia",
       y = "Liczba rodziców")

#Wykres - Rodzaj spozywanych posilkow
plot_ly(students,
        labels = ~`Rodzaj spożywanych posiłków`,
        type = 'pie') %>%
  layout(title = "Rodzaj spożywanych posiłków",
         colorway = c("darkgreen","lightgreen"))

#Wykres - Kurs przygotowawczy
plot_ly(students,
        labels = ~`Kurs przygotowawczy`,
        type = 'pie') %>%
  layout(title = "Odsetek studentów, którzy\nukończyli kurs przygotowawczy",
         colorway = c("darkred","forestgreen"))

#Zmienne ilościowe
#Wykres - Wynik matematyka
students %>%
  ggplot() +
  geom_density(aes(x = `Wynik-matematyka`),
               color = "magenta4",
               fill = "magenta",
               alpha = 0.3) +
  theme_light() +
  theme(plot.title = element_text(hjust = 0.5)) +
  labs(title = "Wynik studentów z matematyki",
       x = "Wynik",
       y = "Gęstość")

#Wykres - Wynik czytanie
students %>%
  ggplot() +
  geom_density(aes(x = `Wynik-czytanie`),
               color = "darkblue",
               fill = "lightskyblue") +
  theme_light() +
  theme(plot.title = element_text(hjust = 0.5)) +
  labs(title = "Wynik studentów z czytania",
       x = "Wynik",
       y = "Gęstość")

#Wykres - Wynik czytanie
students %>%
  ggplot() +
  geom_density(aes(x = `Wynik-pisanie`),
               color = "darkgreen",
               fill = "lightgreen") +
  theme_light() +
  theme(plot.title = element_text(hjust = 0.5)) +
  labs(title = "Wynik studentów z pisania",
       x = "Wynik",
       y = "Gęstość")

#Wykres - macierz korelacji
korelacje <- cor(students[, c("Wynik-matematyka", "Wynik-czytanie", "Wynik-pisanie")],
                 method = "spearman")
round(korelacje, 2)
corrplot(korelacje, method = "square", type = "upper") 

#Wykres - CZY KOBIETY MAJA LEPSZE WYNIKI Z CZESCI HUMANISTYCZNEJ?
students %>%
  ggplot() +
  geom_point(aes(x = `Wynik-czytanie`, y = `Wynik-pisanie`, color = Płeć),
             alpha = 0.2) +
  facet_wrap(~ Płeć) +
  scale_color_manual(values = c("Kobieta" = "hotpink", "Mężczyzna" = "mediumblue")) +
  theme_light() +
  theme(plot.title = element_text(hjust = 0.5)) +
  labs(title = "Wyniki egzaminu z pisania i czytania\nw zależności od płci studentów")

students %>%
  select(Płeć, `Wynik-czytanie`, `Wynik-pisanie`) %>%
  group_by(Płeć) %>%
  summarize(ŚREDNIA_PISANIE = mean(`Wynik-pisanie`),
            ŚREDNIA_CZYTANIE = mean(`Wynik-czytanie`),
            ODCH_PISANIE = sd(`Wynik-pisanie`),
            ODCH_CZYTANIE = sd(`Wynik-czytanie`),
            MIN_PISANIE = min(`Wynik-pisanie`),
            MIN_CZYTANIE = min(`Wynik-czytanie`),
            MAX_PISANIE = max(`Wynik-pisanie`),
            MAX_CZYTANIE = max(`Wynik-czytanie`))

#Wykres - WYKSZTALCENIE RODZICOW A WYNIK
students %>%
  ggplot() +
  geom_density(aes(x = `Wynik-matematyka`, fill = `Poziom wykształcenia rodziców`),
             alpha = 0.2) +
  facet_wrap(~ `Poziom wykształcenia rodziców`, ncol = 2) +
  theme_light() +
  theme(plot.title = element_text(hjust = 0.5)) +
  labs(title = "Wyniki egzaminu z matematyki\nw zależności od poziomu wykształcenia rodziców",
       y = "Gęstość")

students %>%
  select(`Poziom wykształcenia rodziców`, `Wynik-matematyka`) %>%
  group_by(`Poziom wykształcenia rodziców`) %>%
  summarize(ŚREDNIA = mean(`Wynik-matematyka`),
            ODCHYLENIE = sd(`Wynik-matematyka`),
            MINIMUM = min(`Wynik-matematyka`),
            MAKSIMUM = max(`Wynik-matematyka`))

students %>%
  ggplot() +
  geom_density(aes(x = `Wynik-pisanie`, fill = `Poziom wykształcenia rodziców`),
               alpha = 0.2) +
  facet_wrap(~ `Poziom wykształcenia rodziców`, ncol = 2) +
  theme_light() +
  theme(plot.title = element_text(hjust = 0.5)) +
  labs(title = "Wyniki egzaminu z pisania\nw zależności od poziomu wykształcenia rodziców",
       y = "Gęstość")

students %>%
  select(`Poziom wykształcenia rodziców`, `Wynik-pisanie`) %>%
  group_by(`Poziom wykształcenia rodziców`) %>%
  summarize(ŚREDNIA = mean(`Wynik-pisanie`),
            ODCHYLENIE = sd(`Wynik-pisanie`),
            MINIMUM = min(`Wynik-pisanie`),
            MAKSIMUM = max(`Wynik-pisanie`))

students %>%
  ggplot() +
  geom_density(aes(x = `Wynik-czytanie`, fill = `Poziom wykształcenia rodziców`),
               alpha = 0.2) +
  facet_wrap(~ `Poziom wykształcenia rodziców`, ncol = 2) +
  theme_light() +
  theme(plot.title = element_text(hjust = 0.5)) +
  labs(title = "Wyniki egzaminu z czytania\nw zależności od poziomu wykształcenia rodziców",
       y = "Gęstość")

students %>%
  select(`Poziom wykształcenia rodziców`, `Wynik-czytanie`) %>%
  group_by(`Poziom wykształcenia rodziców`) %>%
  summarize(ŚREDNIA = mean(`Wynik-czytanie`),
            ODCHYLENIE = sd(`Wynik-czytanie`),
            MINIMUM = min(`Wynik-czytanie`),
            MAKSIMUM = max(`Wynik-czytanie`))

#Wykres - POSILEK I KURS A WYNIK
students %>%
  ggplot() +
  geom_density(aes(x = `Wynik-matematyka`, fill = `Kurs przygotowawczy`),
               alpha = 0.2) +
  facet_grid(`Kurs przygotowawczy` ~ `Rodzaj spożywanych posiłków`) +
  theme_light() +
  theme(plot.title = element_text(hjust = 0.5)) +
  labs(title = "Wyniki egzaminu z czytania\nw zależności od poziomu wykształcenia rodziców",
       y = "Gęstość")

#ZALOZENIA KONIECZNE DO ANOVY
#Homogenicznosc wariancji
bartlett.test(students$`Wynik-matematyka` ~ students$`Grupa etniczna`)
bartlett.test(students$`Wynik-matematyka` ~ students$`Poziom wykształcenia rodziców`)
bartlett.test(students$`Wynik-matematyka` ~ students$Płeć)
bartlett.test(students$`Wynik-matematyka` ~ students$`Rodzaj spożywanych posiłków`)
bartlett.test(students$`Wynik-matematyka` ~ students$`Kurs przygotowawczy`)

bartlett.test(students$`Wynik-czytanie` ~ students$`Grupa etniczna`)
bartlett.test(students$`Wynik-czytanie` ~ students$`Poziom wykształcenia rodziców`)
bartlett.test(students$`Wynik-czytanie` ~ students$Płeć)
bartlett.test(students$`Wynik-czytanie` ~ students$`Rodzaj spożywanych posiłków`)
bartlett.test(students$`Wynik-czytanie` ~ students$`Kurs przygotowawczy`)

bartlett.test(students$`Wynik-pisanie` ~ students$`Grupa etniczna`)
bartlett.test(students$`Wynik-pisanie` ~ students$`Poziom wykształcenia rodziców`)
bartlett.test(students$`Wynik-pisanie` ~ students$Płeć)
bartlett.test(students$`Wynik-pisanie` ~ students$`Rodzaj spożywanych posiłków`)
bartlett.test(students$`Wynik-pisanie` ~ students$`Kurs przygotowawczy`)
#Brak homogenicznosci dla: 
#Wynik-matematyka - rodzaj spozywanych posilkow
#Wynik-pisanie - kurs przygotowawczy

#ANOVA
aov1 <- aov(students$`Wynik-matematyka` ~ students$`Grupa etniczna`)
summary(aov1)
aov2 <- aov(students$`Wynik-matematyka` ~ students$`Poziom wykształcenia rodziców`)
summary(aov2)
aov3 <- aov(students$`Wynik-matematyka` ~ students$Płeć)
summary(aov3)
aov4 <- aov(students$`Wynik-matematyka` ~ students$`Kurs przygotowawczy`)
summary(aov4)

aov5 <- aov(students$`Wynik-czytanie` ~ students$`Grupa etniczna`)
summary(aov5)
aov6 <- aov(students$`Wynik-czytanie` ~ students$`Poziom wykształcenia rodziców`)
summary(aov6)
aov7 <- aov(students$`Wynik-czytanie` ~ students$Płeć)
summary(aov7)
aov8 <- aov(students$`Wynik-czytanie` ~ students$`Rodzaj spożywanych posiłków`)
summary(aov8)
aov9 <- aov(students$`Wynik-czytanie` ~ students$`Kurs przygotowawczy`)
summary(aov9)

aov10 <- aov(students$`Wynik-pisanie` ~ students$`Grupa etniczna`)
summary(aov10)
aov11 <- aov(students$`Wynik-pisanie` ~ students$`Poziom wykształcenia rodziców`)
summary(aov11)
aov12 <- aov(students$`Wynik-pisanie` ~ students$Płeć)
summary(aov12)
aov13 <- aov(students$`Wynik-pisanie` ~ students$`Rodzaj spożywanych posiłków`)
summary(aov13)

#KTORE SREDNIE ROZNIA SIE ISTOTNIE
TukeyHSD(aov1)
TukeyHSD(aov2)

TukeyHSD(aov5)
TukeyHSD(aov6)

TukeyHSD(aov10)
TukeyHSD(aov11)


#Testy normalnosci dla zmiennych ilosciowych ze wzgledu na wszystkie kategorie
tapply(students$`Wynik-matematyka`, students$`Grupa etniczna`, shapiro.test)
tapply(students$`Wynik-matematyka`, students$`Poziom wykształcenia rodziców`, shapiro.test)

tapply(students$`Wynik-czytanie`, students$`Grupa etniczna`, shapiro.test)
tapply(students$`Wynik-czytanie`, students$`Poziom wykształcenia rodziców`, shapiro.test)

tapply(students$`Wynik-pisanie`, students$`Grupa etniczna`, shapiro.test)
tapply(students$`Wynik-pisanie`, students$`Poziom wykształcenia rodziców`, shapiro.test)


#Nieparametryczna analiza wariancji
kruskal.test(students$`Wynik-matematyka` ~ students$`Rodzaj spożywanych posiłków`)
kruskal.test(students$`Wynik-pisanie` ~ students$`Kurs przygotowawczy`)
#tez odrzucamy H0
