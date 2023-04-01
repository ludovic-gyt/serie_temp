library(haven)
library("zoo")
library("tseries")
setwd("C:/Users/louis/OneDrive/Documents/Cours/Git/serie_temp/input")
valeurs_serie <- read.csv("valeurs_serie.csv", sep=";")
df<-valeurs_serie
