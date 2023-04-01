rm(list = ls())
library(zoo) #format de serie temporelle pratique et facile d'utilisation (mais plus volumineux)
library(tseries) #diverses fonctions sur les series temporelles
library(ggplot2)

####set-up####
path <- "/Users/ludovic/Desktop/ENSAE/S2/series/serie_temp"
setwd(path) #definit l'espace de travail (working directory ou "wd")
getwd() #affiche le wd
list.files() #liste les elements du wd

####import####

datafile <- "input/valeurs_serie.csv" #definit le fichier de donnees

data <- read.csv(datafile,sep = ";") #importe un fichier .csv dans un objet de classe data.frame
data <- data[nrow(data):1,]

####representation de la série####

# On enlève les 4 dernière valeurs en vue de la prévision

T<-length(data$Date)
data<-data[-c((T-3), (T-2), (T-1), T),]

# Convertir la colonne "date" en format date et création d'une série de type zoo

data$Date <- seq(as.Date("1990-05-01"), as.Date("2023-01-01"), by = "month") # Convertir la colonne "date" en format date
xm <- zoo(data[[2]]) # convertit le premiers element de data en serie temporelle de type "zoo"
T <- length(xm.source)


# représentation graphique

ggplot(data, aes(y = xm, x = Date)) +
  geom_line()


####autocorrélation####
par(mfrow=c(1,2))
acf(xm); pacf(xm)
dev.off()

####detrend#####

y <- xm - mean(xm)
par(mfrow=c(1,2)) 
plot(xm)
plot(y)
acf(y,30);pacf(y,30) 

