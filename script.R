rm(list = ls())
library(zoo) #format de serie temporelle pratique et facile d'utilisation (mais plus volumineux)
library(tseries) #diverses fonctions sur les series temporelles
library(ggplot2)
library(fUnitRoots)


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

data$Date <- seq(as.Date("1990-05-01"), as.Date("2023-01-01"), by = "1 month") # Convertir la colonne "date" en format date
xm <- zoo(data$Valeur) # convertit le premiers element de data en serie temporelle de type "zoo"
T <- length(xm)

#data$Date <- seq(as.Date("2023-01-01"), as.Date("1990-05-01"), by = "-1 month")
#xm <- zoo(data$Valeur, order.by=data$Date)


# représentation graphique

dxm <- diff(xm,1)
plot(cbind(xm,dxm))

ggplot( data, aes(y = xm, x = Date)) +
  geom_line()

#ggplot( data, aes(y = xm, x = rev(data$Date))) +
#  geom_line()

####autocorrélation####

par(mfrow=c(1,2))
acf(xm); pacf(xm)
dev.off()

####Moyenne à 0#####

y <- xm - mean(xm)
par(mfrow=c(1,2)) 
plot(xm)
plot(y)
acf(y,30);pacf(y,30) 

#### Test d'integration ####

summary(lm(xm ~ data$Date))

#Cette regression lineaire nous permets de determiner quel type de Dickey-Fuller test nous devons utiliser
#Ici, la constante est significative, et la time trend également,
#nous utiliserons donc  Test for a unit root with constant and deterministic time trend

adf <- adfTest(xm, lag=0, type="ct") 
adf

#On rejette donc l'hypothese d'une racine unitaire avec une confiance d'au moins 95%
#On ajoute des differences de lag pour controler l'effet du passe sur la relation entre X_t_-_1 et X_t



Qtests <- function(series, k, fitdf=0) {
  pvals <- apply(matrix(1:k), 1, FUN=function(l) {
    pval <- if (l<=fitdf) NA else Box.test(series, lag=l, type="Ljung-Box", fitdf=fitdf)$p.value
    return(c("lag"=l,"pval"=pval))
  })
  return(t(pvals))
}

Qtests(adf@test$lm$residuals, 50, fitdf = length(adf@test$lm$coefficients))
#Les résidus sont tous autocorrélés

adfTest_valid <- function(series, kmax, adftype){
  k <- 0
  noautocorr <- 0
  while (noautocorr==0){
    cat(paste0("ADF with ",k," lags: residuals OK? "))
    adf <- adfTest(series, lags=k, type=adftype)
    pvals <- Qtests(adf@test$lm$residuals, kmax, fitdf = length(adf@test$lm$coefficients))[,2]
    if (sum(pvals<0.05,na.rm=T)==0) {
      noautocorr <- 1; cat("OK \n")
    } else cat("nope \n")
    k <- k+1
  }
  return(adf)
}
adf <- adfTest_valid(xm,393,adftype="ct")
adf <- adfTest(xm, lags=100, type="ct")
Qtests(adf@test$lm$residuals, 194, fitdf = length(adf@test$lm$coefficients))
#en fait tout nos résidus sont autocorrélés donc pas possible de choisir le bon
#nombre de lag ccar au bout d'un moment on a pluss de degré de liberté
#On va faire un test adf avec le plus de lag qu'on peut

adfTest(xm, lags=30, type="ct")

summary(lm(dxm ~ data$Date[-1]))
#
adf <- adfTest_valid(dxm,30,"nc")
adfTest(dxm, lags=8, type="nc")
#série stationnaire en différence première



