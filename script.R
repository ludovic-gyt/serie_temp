rm(list = ls())
library(zoo) #format de serie temporelle pratique et facile d'utilisation (mais plus volumineux)
library(tseries) #diverses fonctions sur les series temporelles

####set-up####
path <- "/Users/ludovic/Desktop/ENSAE/S2/series/serie_temp"
setwd(path) #definit l'espace de travail (working directory ou "wd")
getwd() #affiche le wd
list.files() #liste les elements du wd

####import####

datafile <- "input/valeurs_serie.csv" #definit le fichier de donnees

data <- read.csv(datafile,sep = ";") #importe un fichier .csv dans un objet de classe data.frame
data <- data[nrow(data):1,]

# Convertir la colonne "date" en format date



#Convertir les valeurs en zoo
xm.source <- zoo(data[[2]])# convertit le premiers element de data en serie temporelle de type "zoo"
T <- length(xm.source)
xm <- xm.source[1:(T-4)] #supprime les 4 dernieres valeurs



####Graphique####
plot(xm, xaxt="n")


####autocorrélation####
par(mfrow=c(1,2))
acf(xm); pacf(xm)
dev.off()
####detrend#####

y <- xm - mean(xm)
plot(xm)
plot(y)
par(mfrow=c(1,2)) 
acf(y,30);pacf(y,30) 


