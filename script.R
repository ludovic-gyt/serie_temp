library(zoo) #format de serie temporelle pratique et facile d'utilisation (mais plus volumineux)
library(tseries) #diverses fonctions sur les series temporelles

#set-up
path <- "/Users/ludovic/Desktop/ENSAE/S2/series temporelles/serie_temp"
setwd(path) #definit l'espace de travail (working directory ou "wd")
getwd() #affiche le wd
list.files() #liste les elements du wd

#import

datafile <- "input/valeurs_mensuelles.csv" #definit le fichier de donnees

data <- read.csv(datafile,sep=";") #importe un fichier .csv dans un objet de classe data.frame

xm.source <- zoo(data[[1]]) #convertit le premier element de data en serie temporelle de type "zoo"
T <- length(xm.source)
xm <- xm.source[1:(T-4)] #supprime les 4 dernieres valeurs


