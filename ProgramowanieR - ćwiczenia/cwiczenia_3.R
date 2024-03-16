#Wiktor Kostera

#Zadanie 1
market <- read.csv("/Users/wiktorkostera/Desktop/Programowanie w R/market.csv")
market

#a
sum(market$ilosc)

#b
sum(market$ilosc * market$cena)

#c
market %>%
  group_by(nazwa) %>%
  summarize(suma = sum(ilosc)) %>%
  arrange(desc(suma)) %>%
  slice(1:3)

#d
market %>%
  mutate(obrot = ilosc * cena) %>%
  group_by(nazwa) %>%
  summarize(suma_obrotow = sum(obrot)) %>%
  arrange(desc(suma_obrotow)) %>%
  slice(1:3)

#Zadanie 2

crimes <- read.table("/Users/wiktorkostera/Desktop/Programowanie w R/crim_gen.tsv", header = T, sep = "\t")
crimes

library(mice)
md.pattern(crimes, rotate.names = T)

#Zadanie 3

zloto <- read.csv("https://stooq.pl/q/d/?s=xaupln&c=0&d1=20210101&d2=20231125", header = T, sep = ";")
head(zloto)
