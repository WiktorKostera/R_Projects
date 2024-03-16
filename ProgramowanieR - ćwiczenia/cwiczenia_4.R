#Wiktor Kostera

#Zadanie 1

minmaxK <- function(x, K=5) {
  x <- sort(x)
  minimum <- x[1:K]
  maksimum <- sort(x[(length(x) - K):length(x)],T)
  
  return(list(minimum, maksimum))
}

proba100 <- sample(1:4000, 100, T)

test1 <- minmaxK(proba100, 10)
test1

#Zadanie 2

lDsknl <- function(x) {
  if(x > 1) {
    sum = 0
    for(i in 1:(x %/% 2)) {
       if((x %% i) == 0) {
         sum = sum + i        
       }
    }
    if(sum == x) {
      cat("Liczba", x, "jest doskonała\n")
      return(T)
    }
    else
      return(F)
  }
  else
    return(F)
}

system.time(sum(sapply(1:10000, lDsknl)))

#Zadanie 3

myNorm <- function(x) {
  if(max(x) - min(x) != 0) {
    znormalizowany <- (x - min(x))/(max(x) - min(x))
    return(znormalizowany)    
  }
}

wektor3 <- c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10)
test3 <- myNorm(wektor3)
test3

#Zadanie 4

myCorr <- function(x, y) {
  if(length(x) != length(y))
    return("Długości wektorów muszą być takie same!!!")
  
  pearson <- cor(x, y, method = "pearson")
  kendall <- cor(x, y, method = "kendall")
  spearman <- cor(x, y, method = "spearman")
  
  return(list(Pearson = pearson, Kendall = kendall, Spearman = spearman))
}

x <- runif(100, 0, 5)
e <- rnorm(100, 0 , 1)
y <- x + e 

test4 <- myCorr(x, y)
test4

#Zadanie 5

myStats <- function(x, p) {
  if(p == 0) {
    srednia <- mean(x)
    odchylenie <- sd(x)
    cat("Srednia wynosi: ", srednia, "\nOdchylenie standardowe wynosi: ", odchylenie, "\n")
  }
  else if (p == 1) {
    mediana <- median(x)
    medOdchylenie <- mad(x)
    cat("Mediana wynosi: ", mediana, "\nMedianowe odchylenie standardowe wynosi: ", medOdchylenie, "\n")
  }
  else
    cat("Podano błędny parametr p!!!\n")
}

test5a1 <- myStats(proba100, 0)
test5a2 <- myStats(proba100, 1)
test5a3 <- myStats(proba100, 123)

test5b1 <- myStats(x, 0)
test5b2 <- myStats(x, 1)
test5b3 <- myStats(x, -3213)

#Zadanie 6

myFun <- function(x) {
  return(10*sin(1.5*x)* cos(0.5*x^3) + (1/2)*sqrt(abs(x)))
}

#6a

uniroot(myFun, c(6, 7))$root
uniroot(myFun, c(1, 2))$root
uniroot(myFun, c(-5, 5))$root

#6b

library(rootSolve)
zerowe <- uniroot.all(myFun, c(-3, 3))
zerowe

#6c

plot(myFun, xlim = c(-3, 3), main = "Wykres funkcji", ylab = "f(x)")
abline(h = 0)
points(zerowe, rep(0, length(zerowe)), col = "red", pch = 19)

#Zadanie 7

myLin <- function(x) {
  f1 = sum(c(2, 1, -2) * x) + 2
  f2 = sum(c(1, 2, -2) * x) - 1
  f3 = sum(c(2, 1, -1) * x) + 3
  c(f1 = f1, f2 = f2, f3 = f3)
}

myLin(c(1, 2, 3))

multiroot(f = myLin, c(0, 0, 0))$root

#Zadanie 8

myNonLin <- function(x) {
  f1 = 2*x[1] + x[2]^2 - 2*x[3] - 2
  f2 = x[1]^2 + 2*x[2] - 2*x[3] - 3
  f3 = 2*x[1] + x[2] - x[3] - 3
  c(f1 = f1, f2 = f2, f3 = f3)
}

myNonLin(c(1, 2, 3))
multiroot(f = myNonLin, c(0, 0, 0))

#Zadanie 9

library(rvest)

myDane <- function(x) {
  page <- read_html(x)
  tab <- page %>%
    html_nodes(css = "table.wikitable")
  tabela <- tab[[1]] %>% html_table(fill=T)
  return(tabela)
}

przedsiebiorstwa <- myDane("https://pl.wikipedia.org/wiki/Najwi%C4%99ksze_przedsi%C4%99biorstwa_%C5%9Bwiata")
przedsiebiorstwa
str(przedsiebiorstwa)

przedsiebiorstwa <- as.data.frame(przedsiebiorstwa)

#weryfikacja brakow danych
library(mice)
md.pattern(przedsiebiorstwa, rotate.names = T)
#dane są pełne więc nie trzeba szukać braków

#kolumny 4 i 5 moga byc numeryczne wiec je zamienie
przedsiebiorstwa$`Przychód(mln $)` <- as.numeric(gsub("\u00A0", "", przedsiebiorstwa$`Przychód(mln $)`))
przedsiebiorstwa$Zatrudnienie <- as.numeric(gsub("\u00A0", "", przedsiebiorstwa$Zatrudnienie))

#normalizacja wykorzystujaca funkcja myNorm
przedsiebiorstwa$`Przychód(mln $)` <- myNorm(przedsiebiorstwa$`Przychód(mln $)`)
przedsiebiorstwa$Zatrudnienie <- myNorm(przedsiebiorstwa$Zatrudnienie)

#sprawdzenie wartosci odstajacych
boxplot(przedsiebiorstwa$`Przychód(mln $)`)
boxplot(przedsiebiorstwa$Zatrudnienie)

#przygotowanie argumentow koniecznych do usuniecia
q1z <- quantile(przedsiebiorstwa$Zatrudnienie, 0.25)
q3z <- quantile(przedsiebiorstwa$Zatrudnienie, 0.75)
q1p <- quantile(przedsiebiorstwa$`Przychód(mln $)`, 0.25)
q3p <- quantile(przedsiebiorstwa$`Przychód(mln $)`, 0.75)
iqr1 <- q3z - q1z
iqr2 <- q3p - q1p

library(dplyr)

#usuwanie wartości odstających
przedsiebiorstwa <- przedsiebiorstwa %>% 
  filter(between(`Przychód(mln $)`, q1p - 1.5 * iqr1, q3p + 1.5 * iqr1) &
           between(Zatrudnienie, q1z - 1.5 * iqr2, q3z + 1.5 * iqr2))

#ponowne sprawdzenie efektow
boxplot(przedsiebiorstwa$`Przychód(mln $)`)
boxplot(przedsiebiorstwa$Zatrudnienie)
