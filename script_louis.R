library(haven)
library("zoo")
library("tseries")
library("ggplot2")
library(fUnitRoots)
setwd("C:/Users/louis/OneDrive/Documents/Cours/Git/serie_temp/input")
data <- read.csv("valeurs_serie.csv", sep=";")
T<-length(data$Date)
data<-data[-c((T-3), (T-2), (T-1), T),]
data <- data[nrow(data):1,]
data$Date <- seq(as.Date("1990-05-01"), as.Date("2023-01-01"), by = "month") # Convertir la colonne "date" en format date
xm.source <- zoo(data[[2]]) # convertit le premiers element de data en serie temporelle de type "zoo"
T <- length(xm.source)
xm<-xm.source
ggplot(data, aes(y = xm, x = Date)) +geom_line()
desaison <- xm-lag(xm,-36)
dev.off()
summary(lm(xm ~ data$Date))
#Cette régression linéaire nous permets de déterminer quel type de DIckey-Fuller test nous devons utiliser
#Ici, la constante est significative, et la time trend également, nous utiliserons donc  Test for a unit root with constant and deterministic time trend
adf <- adfTest(xm, lag=0, type="ct") 
adf
#On rejette donc l'hypothèse d'une racine unitaire avec une confiance d'au moins 95%
#On ajoute des différences de lag pour contrôler l'effet du passé sur la relation entre X_t_-_1 et X_t
#à faire

#Partie 2 identification
dev.off()
par(mfrow=c(1,2))
acf(xm)
axis(side=1,at=seq(0,25))
pacf(xm)
axis(side=1,at=seq(0,25))
