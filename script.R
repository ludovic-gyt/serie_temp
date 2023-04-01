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
data$Date <- seq(as.Date("1990-01-01"), as.Date("2023-01-01"), by = "month")
# Convertir la colonne "date" en format date



#Convertir les valeurs en zoo
xm.source <- zoo(data[[2]]) # convertit le premiers element de data en serie temporelle de type "zoo"
T <- length(xm.source)
xm <- xm.source[1:(T-4)] #supprime les 4 dernieres valeurs



####Graphique####

ggplot(data, aes(y = data$Valeur, x = data$Date)) +
  geom_line()

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

####Modelisation####

arima(y,c(3,0,2))

arima302 <- arima(y,c(3,0,2))
####stationarisation####

Box.test(arima302$residuals, lag=6, type="Ljung-Box", fitdf=5) 

Qtests <- function(series, k, fitdf=0) {
  pvals <- apply(matrix(1:k), 1, FUN=function(l) {
    pval <- if (l<=fitdf) NA else Box.test(series, lag=l, type="Ljung-Box", fitdf=fitdf)$p.value
    return(c("lag"=l,"pval"=pval))
  })
  return(t(pvals))
}
Qtests(arima302$residuals, 24, 5) #tests de LB pour les ordres 1 a 24
round(Qtests(arima302$residuals,24,fitdf=5),3)


