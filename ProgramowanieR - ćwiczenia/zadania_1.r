# Wiktor Kostera

wektor1 <- c(1:20,19:1)
print(wektor1)

wektor2 <- rep(c(4, 6, 3), length = 50)
print(wektor2)

wektor3 <- c(rep(4, length = 10), rep(6, length = 20), rep(3, length = 30))
print(wektor3)

wektor4 <- seq(from = 100, to = 4, by = -8)
print(wektor4)

wektor5 <- c((0.1 ^ seq(3, 36, 3)) * 0.2 ^ seq(1, 34, 3))
print(wektor5)

wektor6 <- paste0(c("A", "X"), "_", seq(1, 30, 1), ".", c("B", "D", "F"))
print(wektor6)

set.seed(50)
wektor7 <- sample(5:15, 100, TRUE)
print(wektor7)

wektor8 <- sample(c(letters, LETTERS), 100, TRUE)
print(wektor8) 

set.seed(30)
x <- sample(0:999, 250, TRUE)
y <- sample(0:999, 250, TRUE)
wektor9 <- x[seq(1, 248, 1)] + 2 * x[seq(2, 249, 1)] - y[seq(3, 250, 1)]
print(wektor9)

wektor10 <- sum(((exp(1))^(-1 * x[seq(2, 250, 1)]))/(x[seq(1, 249, 1)] + 10))
print(wektor10)         

wektor11 <- sum(seq(1, 20, 1) ^ 4 * sum((1/(3 + seq(1, 5, 1)))))
print(wektor11)

set.seed(50)
x <- sample(0:999, 500, TRUE)
wektor12 <- sum(x %% 2 == 0)
print(wektor12)

wektor13 <- sum(x %% 2 == 0 & x %% 3 == 0)
print(wektor13)

wektor14 <- sum(x > 70 | x < 30)
print(wektor14)

napis <- c("Katedra", "Informatyki", "Biznesowej", "i", "Inżynierii", "Zarządzania",
           "WZ", "AGH", 2022)
wektor15 <- length(unique(unlist(strsplit(napis, ""))))
print(wektor15)

wektor16 <- names(which.max(table(unlist(strsplit(napis, "")))))
print(wektor16)
