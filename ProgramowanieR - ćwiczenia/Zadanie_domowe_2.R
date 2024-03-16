#Wiktor Kostera

#Zadanie 1

set.seed(11)
n <- sample(seq(5, 29, 2), 1)
B <- matrix(1:n, nrow = n)
print(B)

#Zadanie 2

set.seed(41)
C <- matrix(runif(5 * 10, 5, 10), nrow = 5)
D <- matrix(runif(5 * 10, 5, 10), nrow = 5)
CTD <- t(C) %*% D
CDT <- crossprod(t(C), t(D))
print(CTD)
print(CDT)

#Zadanie 3

x <- as.vector(c(C, D))
set.seed(40)
x[sample(length(x), 20)] <- NA
srednia <- mean(x, na.rm = T)
odchylenie <- sd(x, na.rm = T)
print(srednia)
print(odchylenie)

#Zadanie 4
n1 <- 10
n2 <- 9

macierzA <- matrix(outer(0:(n1-1), 0:(n1-1), "+") %% n1, nrow = n1)
print(macierzA)

macierzB <- matrix(outer(0:(n2-1), 0:(n2-1), "-") %% n2, nrow = n2)
print(macierzB)

#Zadanie 5

set.seed(31)
G <- matrix(sample(-20:20, 20 * 10, replace = T), nrow = 20)
print(G)
najwieksze <- t(apply(G, 1, function(row) tail(sort(row), 2)))
print(najwieksze)

#Zadanie 6

set.seed(31)
E <- matrix(sample(1:10, 6 * 10, T), nrow = 6)

pary1 <- which(outer(colSums(E), colSums(E), "+") > 75, arr.ind = TRUE)
print(pary1)

pary2 <- pary1[duplicated(t(apply(pary1, 1, sort))) == F, ]
print(pary2)
#Zadanie 7

set.seed(27)

x <- sample(20:27, 200, T)
y <- ifelse(x >= 20 & x <= 22, 
            sample(c("lic", "inż."), length(x[x >= 20 & x <= 22]), T, prob = c(0.4, 0.6)),
            sample(c("mgr", "mgr inż."), length(x) - length(x[x >= 20 & x <= 22]), T, prob = c(0.3, 0.7)))
z <- sample(c("Kraków", "Warszawa", "Katowice", "Rzeszów", "Częstochowa"), 200, T)      
dane.stud <- data.frame(wiek = x,
                        wykształcenie = y,
                        adres = z)

print(x)
print(y)
print(z)
print(dane.stud)

#Zadanie 8

kompletne <- nrow(dane.stud)
print(kompletne)

unikatowe <- sum(duplicated(dane.stud) == F)
print(unikatowe)

ileWiek <- table(dane.stud$wiek)
print(ileWiek)

#Zadanie 9

dane9a <- subset(dane.stud, wiek > 20 & (adres == "Kraków" | adres == "Warszawa"), select = c("wiek", "adres"))
print(dane9a)

dane9b <- subset(dane.stud, wiek > 20 & (adres == "Kraków" | adres == "Warszawa")) 
print(dane9b)

#Zadanie 10

library(gridExtra)
library(dplyr)

dane <- dane.stud %>%
  group_by(wykształcenie, adres) %>%
  summarize(średnia = round(mean(wiek), 2))
dane

unikalne_wyksztalcenie <- unique(dane$wykształcenie)
unikalne_adresy <- unique(dane$adres)

#Tworzenie "tabeli" z wynikami w odpowiednim formacie zeby uzyc jej w tableGrob
tabela_wynikowa <- matrix(dane$średnia, ncol = length(unikalne_adresy), byrow = T,
                          dimnames = list(unikalne_wyksztalcenie, unikalne_adresy))
tabela_wynikowa

# Tworzenie tabelki koncowej
tabela_wykres <- tableGrob(tabela_wynikowa)

# Wyswietlanie tabelki
grid.arrange(tabela_wykres)

#Zadanie 11

set.seed(23)
lista1 <- lapply(1:6, function(x) runif(x, 2, 8))
print(lista1)

#Zadanie 12 

lista2 <- lapply(1:6, function(x) runif(x, 2, 8))
print(lista2)

lista3 <- lapply(1:6, function(x) lista1[[x]] + lista2[[x]])
print(lista3)
