#Wiktor Kostera

wektor1a <- c(55, 22, 1:200, 13)
print(wektor1a)

wektor1b <- rep(c(4, 6, 3), times = 10)
print(wektor1b)

wektor1c <- rep(c(3:9), each = 3)
print(wektor1c)

wektor1d <- seq(from = -5, to = 5, length = 100)
print(wektor1d)

wektor1e <- sample(c(1:99), size = 30, replace = F)
print(wektor1e)

wektor6 <- sample(c(0, 1), size = 99, replace = T, prob = c(0.3, 0.7))
print(wektor1f)

x <- sample(c(0:9), size = 150, replace = T)
y <- sample(c(0:9), size = 150, replace = T)

wektor2a <- (exp(1)^x)*cos(y)
print(wektor2a)

wektor2b <- y[2:150]-x[1:149]
print(wektor2b)

wektor2c <- sin(y[1:149])/cos(x[2:150])
print(wektor2c)

wektor3a <- sum(seq(10, 100, 1)^3 + 4 * seq(10, 100, 1)^2)
print(wektor3a)

wektor3b <- sum((5*(2^seq(1, 20, 1))/seq(1, 20, 1)) + sum((3^seq(1, 5, 1))/seq(1, 5, 1)))
print(wektor3b)

x <- sample(c(0:99), size = 60, T)
y <- sample(c(0:99), size = 60, T)

wektor4a <- y[y > 60]
print(wektor4a)

wektor4b <- which(y > 60)
print(wektor4b)

wektor4c <- x[which(y > 60)]
print(wektor4c)

wektor4d <- sqrt(abs(x - mean(x)))
print(wektor4d)

wektor4e <- x[order(y)]
print(wektor4e)

wektor4f <- y[seq(1, 58, 3)]
print(wektor4f)

probka10_100 <- sample(c(10:100), size = 60, T)
wektor5a <- any(probka10_100 > 95)
print(wektor5a)

wektor5b <- all(probka10_100 > 85)
print(wektor5b)

wektor6 <- 1 + sum(cumprod(seq(2, 38, 2)/seq(3, 39, 2)))
print(wektor7)

#ZADANIE 7
xu35 <- runif(500, 3, 5)
xu01 <- rnorm(500, 0, 1)

mean(xu35)
mean(xu01)
sd(xu35)
sd(xu01)
var(xu35)
var(xu01)

library(ggplot2)
library(dplyr)

ggplot() +
  geom_histogram(aes(x = xu35), 
               bins = 10,
               color = "darkgreen",
               fill = "lightgreen") +
  theme_light() + 
  labs(title = "Histogram dla xu35",
       y = "Ilość")

ggplot() +
  geom_histogram(aes(x = xu01), 
                 bins = 10,
                 color = "darkblue",
                 fill = "lightblue") +
  theme_light() + 
  labs(title = "Histogram dla xu01",
       y = "Ilość")

ggplot() +
  geom_boxplot(aes(y = xu35),
               color = "darkgreen",
               fill = "lightgreen") +
  theme_light() +
  labs(title = "Wykres pudełkowy z wąsem dla xu35")

ggplot() +
  geom_boxplot(aes(y = xu01),
               color = "darkblue",
               fill = "lightblue") +
  theme_light() +
  labs(title = "Wykres pudełkowy z wąsem dla xu01")

wektor8a <- paste("label", seq(1, 50, 1))
print(wektor8a)  

wektor8b <- paste0("fn", seq(1, 50, 1))  
print(wektor8b)

napis <- c("Katedra", "Informatyki", "Biznesowej", "i", "Inżynierii", "Zarządzania", "WZ",
              "AGH", 2022)

wektor9a <- length(napis)
print(wektor9a)

wektor9b <- class(napis)
print(wektor9b)

wektor9c <- nchar(napis)

a <- c(2, 3, 5)
b <- c("aa", "bb", "cc", "dd", "ee")
c <- c(TRUE, FALSE, TRUE, FALSE, FALSE)

xlist <- list(a, b, c)
#10a
xlist[c(2, 3)]
#10b
xlist[[2]]
xlist[[3]]
#10c
xlist[-2]
#10d
names(xlist)
#10e
names(xlist) <- c("V1", "V2", "V3")
#10f
xlist$V1
#10g
xlist[["V4"]] <- c(1:10)

#ZADANIE 11
l1 <- lapply(1:6, function(x) runif(x, 2, 8))
l2 <- lapply(1:6, function(x) runif(x, 2, 8))
#suma
lapply(1:6, function(x) l1[[x]] + l2[[x]])