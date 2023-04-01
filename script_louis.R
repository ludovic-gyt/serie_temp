library(haven)
library("zoo")
library("tseries")
library("ggplot2")
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
