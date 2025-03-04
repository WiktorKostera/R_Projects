library(readxl)
library(plotly)
library(MASS)
library(smacof)
library(corrplot)
library(tidyr)
library(vegan)

dane <- read_excel("/Users/wiktorkostera/Desktop/STUDIA/SEMESTR 5/Statystyczna analiza danych/Dane_skalowanie.xlsx", 
                   sheet = "Dane do R")
dane <- as.data.frame(dane)  
rownames(dane) <- dane[, 1]
dane <- dane[, -1]
colnames(dane) <- c("X1", "X2", "X3", "X4", "X5", "X6", "X7", "X8")

# EDA
# statystyki opisowe
statystyki <- round(data.frame(Średnia = sapply(dane, mean),
                         Odchylenie = sapply(dane, sd),
                         Wariancja = sapply(dane, var),
                         Skośność = sapply(dane, skewness),
                         Kurtoza = sapply(dane, kurtosis),
                         Minimum = sapply(dane, min),
                         Maksimum = sapply(dane, max)), 2)
statystyki

# korelacje
korelacje <- cor(dane, method="kendall")
round(korelacje, 2)
corrplot(korelacje, method = "square", type = "upper") 

# wartosci odstajace
dane <- scale(dane)
dane <- as.data.frame(dane)

stand_long <- dane %>%
  pivot_longer(cols = everything(), 
               names_to = "Variable", 
               values_to = "Value")
stand_long$Variable <- factor(stand_long$Variable, 
                              levels = c("X1", "X2", "X3", "X4", "X5", 
                                         "X6", "X7", "X8"))

stand_long %>%
  ggplot() +
  geom_boxplot(aes(x = Variable, y = Value),
               color = "darkgreen") +
  theme_light() +
  theme(plot.title = element_text(hjust = 0.5, 
                                  face = "bold"),
        axis.text.x = element_text(angle = 45,
                                   hjust = 1)) + 
  labs(title = "Wykresy pudełkowe",
       x = "Zmienna",
       y = "Zestandardyzowana wartość zmiennej") 

# obliczenie odleglosci
odleglosci <- dist(dane)
odleglosci

# SKALOWANIE - SPOSOB 1 - cmdscale() - klasyczne skalowanie wielowymiarowe
# jeden wymiar
mds11 <- cmdscale(odleglosci, 1)
mds11
cmdscale
stress <- sqrt(sum((dist(mds11) - odleglosci)^2) / sum(odleglosci^2))
stress

# do posortowania wynikow, ale uzyc dopiero po policzeniu STRESS!!!
# mds11 <- mds11[order(-mds11[, 1]), , drop = F]
# mds11

# dwa wymiary
mds12 <- cmdscale(odleglosci, 2)
mds12
eurodist

plot(mds12[,1], mds12[,2], type = "n", xlab = "", ylab = "",
     main = "Klasyczne skalowanie wielowymiarowe")
text(mds12[,1], mds12[,2], labels(eurodist), cex = 0.6, xpd = T)

stress <- sqrt(sum((dist(mds12) - odleglosci)^2) / sum(odleglosci^2))
stress

# trzy wymiary
mds13 <- cmdscale(odleglosci, 3)
mds13

plot_ly(x = mds13[, 1], 
        y = mds13[, 2], 
        z = mds13[, 3], 
        type = "scatter3d",
        mode = "markers+text",
        text = rownames(mds13), 
        marker = list(size = 3, color = 'blue'),
        textfont = list(size = 8))

stress <- sqrt(sum((dist(mds13) - odleglosci)^2) / sum(odleglosci^2))
stress

# SKALOWANIE - SPOSOB 2 - sammon() - skalowanie sammona
# UWAGA!!! stress w tej funkcji to "blad Sammona", a nie wartosc STRESS ze skalowania Kruskala!!
# jeden wymiar
mds21 <- sammon(odleglosci, k = 1)
mds21$points

mds21$stress
(1/sum(odleglosci))*sum((dist(mds13) - odleglosci)^2) / odleglosci
mds21$points[order(-mds21$points[, 1]), , drop = F]

# STRESS
sqrt(sum((odleglosci - dist(mds21$points))^2)/sum(odleglosci^2))

(1/sum(odleglosci))*sum(((odleglosci - dist(mds21$points))^2)/odleglosci)
# dwa wymiary
mds22 <- sammon(odleglosci, k = 2)
mds22$points

plot(mds22$points[,1], mds22$points[,2], type = "n", xlab = "", ylab = "",
     main = "Skalowanie Sammona")
text(mds22$points[,1], mds22$points[,2], labels(odleglosci), cex = 0.6, xpd = T)

mds22$stress

# trzy wymiary
mds23 <- sammon(odleglosci, k = 3)
mds23$points

plot_ly(x = mds23$points[, 1], 
        y = mds23$points[, 2], 
        z = mds23$points[, 3], 
        type = "scatter3d",
        mode = "markers+text",
        text = rownames(mds23$points), 
        marker = list(size = 3, color = 'blue'),
        textfont = list(size = 8))

mds23$stress

# SKALOWANIE - SPOSOB 3 - smacof()
# jeden wymiar
mds31 <- mds(odleglosci, type="ratio", ndim = 1)
mds31
mds31$conf
mds31$conf[order(-mds31$conf[, 1]), , drop = F]
mds31$stress

# dwa wymiary
mds32 <- mds(odleglosci, type="ratio", ndim = 2)
mds32$conf


plot(mds32$conf[,1], mds32$conf[,2], type = "n", xlab = "", ylab = "",
     main = "Skalowanie SMACOF")
text(mds32$conf[,1], mds32$conf[,2], labels(odleglosci), cex = 0.6, xpd = T)

mds32$stress

# trzy wymiary
mds33 <- mds(odleglosci, type="ratio", ndim = 3)
mds33
mds33$conf
?mds
plot_ly(x = mds33$conf[, 1], 
        y = mds33$conf[, 2], 
        z = mds33$conf[, 3], 
        type = "scatter3d",
        mode = "markers+text",
        text = rownames(mds33$conf), 
        marker = list(size = 3, color = 'blue'),
        textfont = list(size = 8))

mds33$stress

mds32 <- mds(odleglosci, type="ratio", ndim = 2)
stress32 <- sqrt(sum((dist(mds32$conf) - odleglosci)^2) / sum(odleglosci^2))
mds32$conf

plot(mds32$conf[,1], mds32$conf[,2], type = "n", xlab = "", ylab = "",
     main = "Skalowanie SMACOF")
text(mds32$conf[,1], mds32$conf[,2], labels(odleglosci), cex = 0.6, xpd = T)


stress32
cat("Wartość funkcji STRESS wynosi:", mds32$stress)

# SKALOWANIE - SPOSOB 4 - isomds() - skalowanie niemetryczne
# jeden wymiar
mds41 <- isoMDS(odleglosci, k = 1)
mds41$points

mds41$stress/100
mds21$stress
sqrt(sum((dist(mds21$points) - odleglosci)^2) / sum(odleglosci^2))
sqrt(sum((dist(mds41$points) - odleglosci)^2) / sum(odleglosci^2))

# dwa wymiary
mds42 <- isoMDS(odleglosci, k = 2)
mds42$points

plot(mds42$points[,1], mds42$points[,2], type = "n", xlab = "", ylab = "",
     main = "Skalowanie niemetryczne")
text(mds42$points[,1], mds42$points[,2], labels(odleglosci), cex = 0.6, xpd = T)

mds42$stress/100

# trzy wymiary
mds43 <- isoMDS(odleglosci, k = 3)
mds43$points

plot_ly(x = mds43$points[, 1], 
        y = mds43$points[, 2], 
        z = mds43$points[, 3], 
        type = "scatter3d",
        mode = "markers+text",
        text = rownames(mds23$points), 
        marker = list(size = 3, color = 'blue'),
        textfont = list(size = 8))

mds43$stress/100

# SKALOWANIE - SPOSOB 5 - rStressMin()

mds51 <- metaMDS(odleglosci, k = 1)
mds51$stress
sqrt(sum((dist(mds51$points) - odleglosci)^2) / sum(odleglosci^2))

mds52 <- metaMDS(odleglosci, k = 2)
mds52$stress
sqrt(sum((dist(mds52$points) - odleglosci)^2) / sum(odleglosci^2))

mds53 <- metaMDS(odleglosci, k = 3)
mds53$stress
sqrt(sum((dist(mds53$points) - odleglosci)^2) / sum(odleglosci^2))

plot_ly(x = mds42$points[,1],
        y = mds42$points[,2],
        mode = "text",
        text = rownames(mds42$points))




