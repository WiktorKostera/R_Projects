library(shiny)
library(ggplot2)
library(dplyr)
library(plotly)
library(corrplot)
library(tidyr)
library(shinythemes)
library(psych)

students <- read.csv("/Users/wiktorkostera/Desktop/Programowanie w R/Projekt/StudentsPerformance.csv")

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

ui <- fluidPage(
  theme = shinytheme("lumen"),
  titlePanel("Wyniki egzaminów studentów"),
  navbarPage(title = "",
             tabPanel("Rozkład danych kategorycznych",
                      sidebarLayout(
                        sidebarPanel(
                          selectInput(inputId = "kat", 
                                      label = "Zmienne kategoryczne",
                                      choices = c("Płeć",
                                                  "Grupa etniczna",
                                                  "Poziom wykształcenia rodziców",
                                                  "Rodzaj spożywanych posiłków",
                                                  "Kurs przygotowawczy"), 
                                      selected = "Płeć")
                        ),
                        mainPanel(
                          plotOutput("Plot1"),
                          tableOutput("Stat1")
                        )
                      )
             ),
             tabPanel("Rozkład danych ilościowych",
                      sidebarLayout(
                        sidebarPanel(
                          selectInput(inputId = "ilo", 
                                      label = "Zmienne ilościowe",
                                      choices = c("Wynik-matematyka",
                                                  "Wynik-czytanie",
                                                  "Wynik-pisanie"), 
                                      selected = "Wynik-matematyka")
                        ),
                        mainPanel(
                          plotOutput("Plot2"),
                          tableOutput("Stat2")
                        )
                      )
             ),             
             tabPanel("Analiza poszczególnych zależności",
                      sidebarLayout(
                        sidebarPanel(
                          selectInput(inputId = "zm",
                                      label = "Pytania badawcze",
                                      choices = c("Czy kobiety mają lepsze wyniki z części humanistycznej?",
                                                  "Czy wykształcenie rodziców ma wpływ na wynik egzaminu z matematyki?",
                                                  "Czy wykształcenie rodziców ma wpływ na wynik egzaminu z czytania?",
                                                  "Czy wykształcenie rodziców ma wpływ na wynik egzaminu z pisania?",
                                                  "Czy rodzaj spożywanego posiłku oraz odbycie kursu wpływa na wynik z matematyki?"),
                                      selected = "Czy kobiety mają lepsze wyniki z części humanistycznej?")
                        ),
                        mainPanel(
                          plotOutput("Plot3"),
                          tableOutput("tab1")
                        )
                      )
             ),
             tabPanel("Analiza wariancji - spełnione założenia",
                      sidebarLayout(
                        sidebarPanel(
                          selectInput(inputId = "zm1",
                                      label = "Zmienne",
                                      choices = c("Wynik-matematyka - Grupa etniczna",
                                                  "Wynik-matematyka - Poziom wykształcenia rodziców",
                                                  "Wynik-czytanie - Grupa etniczna",
                                                  "Wynik-czytanie - Poziom wykształcenia rodziców",
                                                  "Wynik-pisanie - Grupa etniczna",
                                                  "Wynik-pisanie - Poziom wykształcenia rodziców"),
                                      selected = "Wynik-matematyka - Grupa etniczna"),
                          checkboxInput("b1",
                                        "Bartlett"),
                          checkboxInput("a1",
                                        "ANOVA"),
                          checkboxInput("t",
                                        "TukeyHSD"),
                          checkboxInput("n",
                                        "Normalny")
                        ),
                        mainPanel(
                          textOutput("resultText1"),
                          textOutput("resultText2"),
                          verbatimTextOutput("resultText3"),
                          tableOutput("resultText4")
                        )
                      )                          
              ),
             tabPanel("Analiza wariancji - odrzucenie ANOVY",
                      sidebarLayout(
                        sidebarPanel(
                          selectInput(inputId = "zm2",
                                      label = "Zmienne",
                                      choices = c("Wynik-matematyka - Płeć",
                                                  "Wynik-matematyka - Kurs przygotowawczy",
                                                  "Wynik-czytanie - Płeć",
                                                  "Wynik-czytanie - Rodzaj spożywanych posiłków",
                                                  "Wynik-czytanie - Kurs przygotowawczy",
                                                  "Wynik-pisanie - Płeć",
                                                  "Wynik-pisanie - Rodzaj spożywanych posiłków"),
                                      selected = "Wynik-matematyka - Płeć"),
                          checkboxInput("b2",
                                        "Bartlett"),
                          checkboxInput("a2",
                                        "ANOVA")
                        ),
                        mainPanel(
                          textOutput("resultText5"),
                          textOutput("resultText6")                          
                        )
                      )
              ),
             tabPanel("Nieparametryczna analiza wariancji",
                      sidebarLayout(
                        sidebarPanel(
                          selectInput(inputId = "zm3",
                                      label = "Zmienne",
                                      choices = c("Wynik-matematyka - Rodzaj spożywanych posiłków",
                                                  "Wynik-pisanie - Kurs przygotowawczy"),
                                      selected = "Wynik-matematyka - Rodzaj spożywanych posiłków"),
                          checkboxInput("b3",
                                        "Bartlett"),
                          checkboxInput("k",
                                       "Kruskal")
                        ),
                        mainPanel(
                          textOutput("resultText7"),
                          textOutput("resultText8")
                        )
                      )
              )
  )
)
server <- function(input, output) {
  options(encoding = 'UTF-8')
  output$Plot1 <- renderPlot({
    if (input$kat == "Płeć") {
      students %>%
        ggplot() +
        geom_bar(aes(x = "", fill = Płeć)) +
        theme_light() +
        coord_polar("y") +
        theme(plot.title = element_text(hjust = 0.5)) +
        labs(title = "Płeć studentów",
             y = "",
             x = "") +
        scale_fill_manual(values = c("Mężczyzna" = "mediumblue", "Kobieta" = "hotpink"),
                          name = "Płeć") +
        theme(legend.position = "bottom")
    } else if (input$kat == "Grupa etniczna") {
      students %>%
        ggplot() +
        geom_bar(aes(x = `Grupa etniczna`),
                 color = "darkblue",
                 fill = "lightskyblue") +
        theme_light() +
        theme(plot.title = element_text(hjust = 0.5)) +
        labs(title = "Liczba studentów w poszczególnych grupach etnicznych",
             y = "Liczba studentów")
    } else if (input$kat == "Poziom wykształcenia rodziców") {
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
    } else if (input$kat == "Rodzaj spożywanych posiłków") {
      students %>%
        ggplot() +
        geom_bar(aes(x = "", fill = `Rodzaj spożywanych posiłków`)) +
        theme_light() +
        coord_polar("y") +
        theme(plot.title = element_text(hjust = 0.5)) +
        labs(title = "Rodzaj spożywanych posiłków",
             y = "",
             x = "") +
        scale_fill_manual(values = c("Podstawowy" = "lightgreen", "Pełny" = "darkgreen"),
                          name = "Rodzaj posiłku") +
        theme(legend.position = "bottom")     
    } else {
      students %>%
        ggplot() +
        geom_bar(aes(x = "", fill = `Kurs przygotowawczy`)) +
        theme_light() +
        coord_polar("y") +
        theme(plot.title = element_text(hjust = 0.5)) +
        labs(title = "Kurs przygotowawczy",
             y = "",
             x = "") +
        scale_fill_manual(values = c("Ukończony" = "forestgreen", "Brak" = "darkred"),
                          name = "Kurs") +
        theme(legend.position = "bottom") 
    }
  })
  output$Stat1 <- renderTable({
    if(input$kat == "Płeć") {
      stat <- as.data.frame(table(students$Płeć))
      colnames(stat) <- c("Płeć", "Liczba wystąpień")
      return(stat)
    }
    if(input$kat == "Grupa etniczna") {
      stat <- as.data.frame(table(students$`Grupa etniczna`))
      colnames(stat) <- c("Grupa etniczna", "Liczba wystąpień")
      return(stat)      
    }
    if(input$kat == "Poziom wykształcenia rodziców") {
      stat <- as.data.frame(table(students$`Poziom wykształcenia rodziców`))
      colnames(stat) <- c("Poziom wykształcenia rodziców", "Liczba wystąpień")
      return(stat)      
    }
    if(input$kat == "Rodzaj spożywanych posiłków") {
      stat <- as.data.frame(table(students$`Rodzaj spożywanych posiłków`))
      colnames(stat) <- c("Rodzaj spożywanych posiłków", "Liczba wystąpień")
      return(stat)      
    }
    if(input$kat == "Kurs przygotowawczy") {
      stat <- as.data.frame(table(students$`Kurs przygotowawczy`))
      colnames(stat) <- c("Kurs przygotowawczy", "Liczba wystąpień")
      return(stat)      
    }
  })
  output$Plot2 <- renderPlot({
    if (input$ilo == "Wynik-matematyka") {
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
    } else if (input$ilo == "Wynik-czytanie") {
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
    } else {
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
    }
  })
  output$Stat2 <- renderTable({
    if(input$ilo == "Wynik-matematyka") {
    data.frame("Kwartyl_1" = quantile(students$`Wynik-matematyka`, 0.25),
               "Mediana" = median(students$`Wynik-matematyka`),
               "Kwartyl_3" = quantile(students$`Wynik-matematyka`, 0.75),
               "Średnia" = mean(students$`Wynik-matematyka`),
               "Odchylenie" = sd(students$`Wynik-matematyka`),
               "Skośność" = skew(students$`Wynik-matematyka`),
               "Kurtoza" = kurtosi(students$`Wynik-matematyka`),
               "Minimum" = min(students$`Wynik-matematyka`),
               "Maksimum" = max(students$`Wynik-matematyka`))
    } else if(input$ilo == "Wynik-czytanie") {
      data.frame("Kwartyl_1" = quantile(students$`Wynik-czytanie`, 0.25),
                 "Mediana" = median(students$`Wynik-czytanie`),
                 "Kwartyl_3" = quantile(students$`Wynik-czytanie`, 0.75),
                 "Średnia" = mean(students$`Wynik-czytanie`),
                 "Odchylenie" = sd(students$`Wynik-czytanie`),
                 "Skośność" = skew(students$`Wynik-czytanie`),
                 "Kurtoza" = kurtosi(students$`Wynik-czytanie`),
                 "Minimum" = min(students$`Wynik-czytanie`),
                 "Maksimum" = max(students$`Wynik-czytanie`))
    } else {
      data.frame("Kwartyl_1" = quantile(students$`Wynik-pisanie`, 0.25),
                 "Mediana" = median(students$`Wynik-pisanie`),
                 "Kwartyl_3" = quantile(students$`Wynik-pisanie`, 0.75),
                 "Średnia" = mean(students$`Wynik-pisanie`),
                 "Odchylenie" = sd(students$`Wynik-pisanie`),
                 "Skośność" = skew(students$`Wynik-pisanie`),
                 "Kurtoza" = kurtosi(students$`Wynik-pisanie`),
                 "Minimum" = min(students$`Wynik-pisanie`),
                 "Maksimum" = max(students$`Wynik-pisanie`))
    }
  })
  output$Plot3 <- renderPlot({
    if (input$zm == "Czy kobiety mają lepsze wyniki z części humanistycznej?") {
      students %>%
        ggplot() +
        geom_point(aes(x = `Wynik-czytanie`, y = `Wynik-pisanie`, color = Płeć),
                   alpha = 0.2) +
        facet_wrap(~ Płeć) +
        scale_color_manual(values = c("Kobieta" = "hotpink", "Mężczyzna" = "mediumblue")) +
        theme_light() +
        theme(plot.title = element_text(hjust = 0.5)) +
        labs(title = "Wyniki egzaminu z pisania i czytania\nw zależności od płci studentów")
    } else if (input$zm == "Czy wykształcenie rodziców ma wpływ na wynik egzaminu z matematyki?") {
      students %>%
        ggplot() +
        geom_density(aes(x = `Wynik-matematyka`, fill = `Poziom wykształcenia rodziców`),
                     alpha = 0.2) +
        facet_wrap(~ `Poziom wykształcenia rodziców`, ncol = 2) +
        theme_light() +
        theme(plot.title = element_text(hjust = 0.5)) +
        labs(title = "Wyniki egzaminu z matematyki\nw zależności od poziomu wykształcenia rodziców",
             y = "Gęstość")
    } else if (input$zm == "Czy wykształcenie rodziców ma wpływ na wynik egzaminu z czytania?") {
      students %>%
        ggplot() +
        geom_density(aes(x = `Wynik-czytanie`, fill = `Poziom wykształcenia rodziców`),
                     alpha = 0.2) +
        facet_wrap(~ `Poziom wykształcenia rodziców`, ncol = 2) +
        theme_light() +
        theme(plot.title = element_text(hjust = 0.5)) +
        labs(title = "Wyniki egzaminu z czytania\nw zależności od poziomu wykształcenia rodziców",
             y = "Gęstość")
    } else if (input$zm == "Czy wykształcenie rodziców ma wpływ na wynik egzaminu z pisania?") {
      students %>%
        ggplot() +
        geom_density(aes(x = `Wynik-pisanie`, fill = `Poziom wykształcenia rodziców`),
                     alpha = 0.2) +
        facet_wrap(~ `Poziom wykształcenia rodziców`, ncol = 2) +
        theme_light() +
        theme(plot.title = element_text(hjust = 0.5)) +
        labs(title = "Wyniki egzaminu z pisania\nw zależności od poziomu wykształcenia rodziców",
             y = "Gęstość")
    } else {
      students %>%
        ggplot() +
        geom_density(aes(x = `Wynik-matematyka`, fill = `Kurs przygotowawczy`),
                     alpha = 0.2) +
        facet_grid(`Kurs przygotowawczy` ~ `Rodzaj spożywanych posiłków`) +
        theme_light() +
        theme(plot.title = element_text(hjust = 0.5)) +
        labs(title = "Wyniki egzaminu z czytania\nw zależności od poziomu wykształcenia rodziców",
             y = "Gęstość")
    }
  })
  output$tab1 <- renderTable({
    if (input$zm == "Czy kobiety mają lepsze wyniki z części humanistycznej?") {
      students %>%
        select(Płeć, `Wynik-czytanie`, `Wynik-pisanie`) %>%
        group_by(Płeć) %>%
        summarize(MEAN_P = mean(`Wynik-pisanie`),
                  MEAN_CZ = mean(`Wynik-czytanie`),
                  SD_P = sd(`Wynik-pisanie`),
                  SD_CZ = sd(`Wynik-czytanie`),
                  MIN_P= min(`Wynik-pisanie`),
                  MIN_CZ = min(`Wynik-czytanie`),
                  MAX_P = max(`Wynik-pisanie`),
                  MAX_CZ = max(`Wynik-czytanie`))
    } else if (input$zm == "Czy wykształcenie rodziców ma wpływ na wynik egzaminu z matematyki?") {
      students %>%
        select(`Poziom wykształcenia rodziców`, `Wynik-matematyka`) %>%
        group_by(`Poziom wykształcenia rodziców`) %>%
        summarize(ŚREDNIA = mean(`Wynik-matematyka`),
                  ODCHYLENIE = sd(`Wynik-matematyka`),
                  MINIMUM = min(`Wynik-matematyka`),
                  MAKSIMUM = max(`Wynik-matematyka`))
    } else if (input$zm == "Czy wykształcenie rodziców ma wpływ na wynik egzaminu z czytania?") {
      students %>%
        select(`Poziom wykształcenia rodziców`, `Wynik-czytanie`) %>%
        group_by(`Poziom wykształcenia rodziców`) %>%
        summarize(ŚREDNIA = mean(`Wynik-czytanie`),
                  ODCHYLENIE = sd(`Wynik-czytanie`),
                  MINIMUM = min(`Wynik-czytanie`),
                  MAKSIMUM = max(`Wynik-czytanie`))
    } else if (input$zm == "Czy wykształcenie rodziców ma wpływ na wynik egzaminu z pisania?") {
      students %>%
        select(`Poziom wykształcenia rodziców`, `Wynik-pisanie`) %>%
        group_by(`Poziom wykształcenia rodziców`) %>%
        summarize(ŚREDNIA = mean(`Wynik-pisanie`),
                  ODCHYLENIE = sd(`Wynik-pisanie`),
                  MINIMUM = min(`Wynik-pisanie`),
                  MAKSIMUM = max(`Wynik-pisanie`))
    } else {
      students %>%
        select(`Wynik-matematyka`, `Rodzaj spożywanych posiłków`, `Kurs przygotowawczy`) %>%
        group_by(`Rodzaj spożywanych posiłków`, `Kurs przygotowawczy`) %>%
        summarize(ŚREDNIA = mean(`Wynik-matematyka`),
                  ODCHYLENIE = sd(`Wynik-matematyka`),
                  MINIMUM = min(`Wynik-matematyka`),
                  MAKSIMUM = max(`Wynik-matematyka`))
    }
  })
  reactiveW1 <- reactive({
    if(input$zm1 == "Wynik-matematyka - Grupa etniczna") {
      if (input$b1) {
        wynik <- paste("P-value w teście Bartletta wynosi: ", round(bartlett.test(students$`Wynik-matematyka` ~ students$`Grupa etniczna`)$p.value, 4))
        return(wynik)
      }
    }
    if(input$zm1 == "Wynik-matematyka - Poziom wykształcenia rodziców") {
      if(input$b1) {
        wynik <- paste("P-value w teście Bartletta wynosi: ", round(bartlett.test(students$`Wynik-matematyka` ~ students$`Poziom wykształcenia rodziców`)$p.value, 4))
        return(wynik)              
      }
    }
    if(input$zm1 == "Wynik-czytanie - Grupa etniczna") {
      if (input$b1) {
        wynik <- paste("P-value w teście Bartletta wynosi: ", round(bartlett.test(students$`Wynik-czytanie` ~ students$`Grupa etniczna`)$p.value, 4))
        return(wynik)
      }
    }
    if(input$zm1 == "Wynik-czytanie - Poziom wykształcenia rodziców") {
      if(input$b1) {
        wynik <- paste("P-value w teście Bartletta wynosi: ", round(bartlett.test(students$`Wynik-czytanie` ~ students$`Poziom wykształcenia rodziców`)$p.value, 4))
        return(wynik)              
      }
    }
    if(input$zm1 == "Wynik-pisanie - Grupa etniczna") {
      if (input$b1) {
        wynik <- paste("P-value w teście Bartletta wynosi: ", round(bartlett.test(students$`Wynik-pisanie` ~ students$`Grupa etniczna`)$p.value, 4))
        return(wynik)
      }
    }
    if(input$zm1 == "Wynik-pisanie - Poziom wykształcenia rodziców") {
      if(input$b1) {
        wynik <- paste("P-value w teście Bartletta wynosi: ", round(bartlett.test(students$`Wynik-pisanie` ~ students$`Poziom wykształcenia rodziców`)$p.value, 4))
        return(wynik)              
      }
    }
  })
  reactiveW2 <- reactive({
    if(input$zm1 == "Wynik-matematyka - Grupa etniczna") {
      if (input$a1) {
        wynik <- paste("P-value w teście ANOVA wynosi: ", summary(aov(students$`Wynik-matematyka` ~ students$`Grupa etniczna`))[[1]]$`Pr(>F)`[1])
        return(wynik)
      }      
    }
    if(input$zm1 == "Wynik-matematyka - Poziom wykształcenia rodziców") {
      if (input$a1) {
        wynik <- paste("P-value w teście ANOVA wynosi: ", summary(aov(students$`Wynik-matematyka` ~ students$`Poziom wykształcenia rodziców`))[[1]]$`Pr(>F)`[1])
        return(wynik)
      }      
    }
    if(input$zm1 == "Wynik-czytanie - Grupa etniczna") {
      if (input$a1) {
        wynik <- paste("P-value w teście ANOVA wynosi: ", summary(aov(students$`Wynik-czytanie` ~ students$`Grupa etniczna`))[[1]]$`Pr(>F)`[1])
        return(wynik)
      }      
    }
    if(input$zm1 == "Wynik-czytanie - Poziom wykształcenia rodziców") {
      if (input$a1) {
        wynik <- paste("P-value w teście ANOVA wynosi: ", summary(aov(students$`Wynik-czytanie` ~ students$`Poziom wykształcenia rodziców`))[[1]]$`Pr(>F)`[1])
        return(wynik)
      }      
    }
    if(input$zm1 == "Wynik-pisanie - Grupa etniczna") {
      if (input$a1) {
        wynik <- paste("P-value w teście ANOVA wynosi: ", summary(aov(students$`Wynik-pisanie` ~ students$`Grupa etniczna`))[[1]]$`Pr(>F)`[1])
        return(wynik)
      }      
    }
    if(input$zm1 == "Wynik-pisanie - Poziom wykształcenia rodziców") {
      if (input$a1) {
        wynik <- paste("P-value w teście ANOVA wynosi: ", summary(aov(students$`Wynik-pisanie` ~ students$`Poziom wykształcenia rodziców`))[[1]]$`Pr(>F)`[1])
        return(wynik)
      }      
    }
  })
  reactiveW3 <- reactive({
    if(input$zm1 == "Wynik-matematyka - Grupa etniczna") {
      if(input$t) {
        wynik <- capture.output(TukeyHSD(aov(students$`Wynik-matematyka` ~ students$`Grupa etniczna`)))
        return(wynik)
      }
    }
    if(input$zm1 == "Wynik-matematyka - Poziom wykształcenia rodziców") {
      if(input$t) {
        wynik <- capture.output(TukeyHSD(aov(students$`Wynik-matematyka` ~ students$`Poziom wykształcenia rodziców`)))
        return(wynik)
      }
    }
    if(input$zm1 == "Wynik-czytanie - Grupa etniczna") {
      if(input$t) {
        wynik <- capture.output(TukeyHSD(aov(students$`Wynik-czytanie` ~ students$`Grupa etniczna`)))
        return(wynik)
      }
    }
    if(input$zm1 == "Wynik-czytanie - Poziom wykształcenia rodziców") {
      if(input$t) {
        wynik <- capture.output(TukeyHSD(aov(students$`Wynik-czytanie` ~ students$`Poziom wykształcenia rodziców`)))
        return(wynik)
      }
    }
    if(input$zm1 == "Wynik-pisanie - Grupa etniczna") {
      if(input$t) {
        wynik <- capture.output(TukeyHSD(aov(students$`Wynik-pisanie` ~ students$`Grupa etniczna`)))
        return(wynik)
      }
    }
    if(input$zm1 == "Wynik-pisanie - Poziom wykształcenia rodziców") {
      if(input$t) {
        wynik <- capture.output(TukeyHSD(aov(students$`Wynik-pisanie` ~ students$`Poziom wykształcenia rodziców`)))
        return(wynik)
      }
    }
  })
  reactiveW4 <- reactive({
    if(input$zm1 == "Wynik-matematyka - Grupa etniczna") {
      if(input$n) {
        shapiro_wyniki <- tapply(students$`Wynik-matematyka`, students$`Grupa etniczna`, shapiro.test)
        wynik <- data.frame(
          Grupa = names(shapiro_wyniki),
          P_value = sapply(shapiro_wyniki, function(x) x$p.value)
        )
        return(wynik)
      }  
    }
    if(input$zm1 == "Wynik-matematyka - Poziom wykształcenia rodziców") {
      if(input$n) {
        shapiro_wyniki <- tapply(students$`Wynik-matematyka`, students$`Poziom wykształcenia rodziców`, shapiro.test)
        wynik <- data.frame(
          Grupa = names(shapiro_wyniki),
          P_value = sapply(shapiro_wyniki, function(x) x$p.value)
        )
        return(wynik)
      }  
    }
    if(input$zm1 == "Wynik-czytanie - Grupa etniczna") {
      if(input$n) {
        shapiro_wyniki <- tapply(students$`Wynik-czytanie`, students$`Grupa etniczna`, shapiro.test)
        wynik <- data.frame(
          Grupa = names(shapiro_wyniki),
          P_value = sapply(shapiro_wyniki, function(x) x$p.value)
        )
        return(wynik)
      }  
    }
    if(input$zm1 == "Wynik-czytanie - Poziom wykształcenia rodziców") {
      if(input$n) {
        shapiro_wyniki <- tapply(students$`Wynik-czytanie`, students$`Poziom wykształcenia rodziców`, shapiro.test)
        wynik <- data.frame(
          Grupa = names(shapiro_wyniki),
          P_value = sapply(shapiro_wyniki, function(x) x$p.value)
        )
        return(wynik)
      }   
    }
    if(input$zm1 == "Wynik-pisanie - Grupa etniczna") {
      if(input$n) {
        shapiro_wyniki <- tapply(students$`Wynik-pisanie`, students$`Grupa etniczna`, shapiro.test)
        wynik <- data.frame(
          Grupa = names(shapiro_wyniki),
          P_value = sapply(shapiro_wyniki, function(x) x$p.value)
        )
        return(wynik)
      }  
    }
    if(input$zm1 == "Wynik-pisanie - Poziom wykształcenia rodziców") {
      if(input$n) {
        shapiro_wyniki <- tapply(students$`Wynik-pisanie`, students$`Poziom wykształcenia rodziców`, shapiro.test)
        wynik <- data.frame(
          Grupa = names(shapiro_wyniki),
          P_value = sapply(shapiro_wyniki, function(x) x$p.value)
        )
        return(wynik)
      }  
    }
  })
  output$resultText1 <- renderText({
    reactiveW1()
  })
  output$resultText2 <- renderText({
    reactiveW2()
  })
  output$resultText3 <- renderPrint({
    reactiveW3()
  })
  output$resultText4 <- renderTable({
    reactiveW4()
  })
  
  reactiveW5 <- reactive({
    if(input$zm2 == "Wynik-matematyka - Płeć") {
      if (input$b2) {
        wynik <- paste("P-value w teście Bartletta wynosi: ", round(bartlett.test(students$`Wynik-matematyka` ~ students$Płeć)$p.value, 4))
        return(wynik)
      }
    }
    if(input$zm2 == "Wynik-matematyka - Kurs przygotowawczy") {
      if(input$b2) {
        wynik <- paste("P-value w teście Bartletta wynosi: ", round(bartlett.test(students$`Wynik-matematyka` ~ students$`Kurs przygotowawczy`)$p.value, 4))
        return(wynik)              
      }
    }
    if(input$zm2 == "Wynik-czytanie - Płeć") {
      if (input$b2) {
        wynik <- paste("P-value w teście Bartletta wynosi: ", round(bartlett.test(students$`Wynik-czytanie` ~ students$`Płeć`)$p.value, 4))
        return(wynik)
      }
    }
    if(input$zm2 == "Wynik-czytanie - Kurs przygotowawczy") {
      if (input$b2) {
        wynik <- paste("P-value w teście Bartletta wynosi: ", round(bartlett.test(students$`Wynik-czytanie` ~ students$`Kurs przygotowawczy`)$p.value, 4))
        return(wynik)
      }
    }
    if(input$zm2 == "Wynik-czytanie - Rodzaj spożywanych posiłków") {
      if (input$b2) {
        wynik <- paste("P-value w teście Bartletta wynosi: ", round(bartlett.test(students$`Wynik-czytanie` ~ students$`Rodzaj spożywanych posiłków`)$p.value, 4))
        return(wynik)
      }
    }
    if(input$zm2 == "Wynik-pisanie - Płeć") {
      if (input$b2) {
        wynik <- paste("P-value w teście Bartletta wynosi: ", round(bartlett.test(students$`Wynik-pisanie` ~ students$`Płeć`)$p.value, 4))
        return(wynik)
      }
    }
    if(input$zm2 == "Wynik-pisanie - Rodzaj spożywanych posiłków") {
      if (input$b2) {
        wynik <- paste("P-value w teście Bartletta wynosi: ", round(bartlett.test(students$`Wynik-pisanie` ~ students$`Rodzaj spożywanych posiłków`)$p.value, 4))
        return(wynik)
      }
    }
  })
  reactiveW6 <- reactive({
    if(input$zm2 == "Wynik-matematyka - Płeć") {
      if (input$a2) {
        wynik <- paste("P-value w teście ANOVA wynosi: ", summary(aov(students$`Wynik-matematyka` ~ students$Płeć))[[1]]$`Pr(>F)`[1])
        return(wynik)
      }      
    }
    if(input$zm2 == "Wynik-matematyka - Kurs przygotowawczy") {
      if (input$a2) {
        wynik <- paste("P-value w teście ANOVA wynosi: ", summary(aov(students$`Wynik-matematyka` ~ students$`Kurs przygotowawczy`))[[1]]$`Pr(>F)`[1])
        return(wynik)
      }      
    }
    if(input$zm2 == "Wynik-czytanie - Płeć") {
      if (input$a2) {
        wynik <- paste("P-value w teście ANOVA wynosi: ", summary(aov(students$`Wynik-czytanie` ~ students$Płeć))[[1]]$`Pr(>F)`[1])
        return(wynik)
      }      
    }
    if(input$zm2 == "Wynik-czytanie - Kurs przygotowawczy") {
      if (input$a2) {
        wynik <- paste("P-value w teście ANOVA wynosi: ", summary(aov(students$`Wynik-czytanie` ~ students$`Kurs przygotowawczy`))[[1]]$`Pr(>F)`[1])
        return(wynik)
      }      
    }
    if(input$zm2 == "Wynik-czytanie - Rodzaj spożywanych posiłków") {
      if (input$a2) {
        wynik <- paste("P-value w teście ANOVA wynosi: ", summary(aov(students$`Wynik-czytanie` ~ students$`Rodzaj spożywanych posiłków`))[[1]]$`Pr(>F)`[1])
        return(wynik)
      }      
    }
    if(input$zm2 == "Wynik-pisanie - Płeć") {
      if (input$a2) {
        wynik <- paste("P-value w teście ANOVA wynosi: ", summary(aov(students$`Wynik-pisanie` ~ students$Płeć))[[1]]$`Pr(>F)`[1])
        return(wynik)
      }      
    }
    if(input$zm2 == "Wynik-pisanie - Rodzaj spożywanych posiłków") {
      if (input$a2) {
        wynik <- paste("P-value w teście ANOVA wynosi: ", summary(aov(students$`Wynik-pisanie` ~ students$`Rodzaj spożywanych posiłków`))[[1]]$`Pr(>F)`[1])
        return(wynik)
      }      
    } 
  })
  output$resultText5 <- renderText({
    reactiveW5()
  })
  output$resultText6 <- renderText({
    reactiveW6()
  })
  reactiveW7 <- reactive({
    if(input$zm3 == "Wynik-matematyka - Rodzaj spożywanych posiłków") {
      if (input$b3) {
        wynik <- paste("P-value w teście Bartletta wynosi: ", round(bartlett.test(students$`Wynik-matematyka` ~ students$`Rodzaj spożywanych posiłków`)$p.value, 4))
        return(wynik)
      }
    }
    if(input$zm3 == "Wynik-pisanie - Kurs przygotowawczy") {
      if (input$b3) {
        wynik <- paste("P-value w teście Bartletta wynosi: ", round(bartlett.test(students$`Wynik-pisanie` ~ students$`Kurs przygotowawczy`)$p.value, 4))
        return(wynik)
      }
    }   
  })
  reactiveW8 <- reactive({
    if(input$zm3 == "Wynik-matematyka - Rodzaj spożywanych posiłków") {
      if (input$k) {
        wynik <- paste("P-value w teście Kruskala-Wallisa wynosi: ", kruskal.test(students$`Wynik-matematyka` ~ students$`Rodzaj spożywanych posiłków`)$p.value)
        return(wynik)
      }
    }
    if(input$zm3 == "Wynik-pisanie - Kurs przygotowawczy") {
      if (input$k) {
        wynik <- paste("P-value w teście Kruskala-Wallisa wynosi: ", kruskal.test(students$`Wynik-pisanie` ~ students$`Kurs przygotowawczy`)$p.value)
        return(wynik)
      }
    }     
  })
  output$resultText7 <- renderText({
    reactiveW7()
  })
  output$resultText8 <- renderText({
    reactiveW8()
  })
}

# Run the application
shinyApp(ui = ui, server = server)
