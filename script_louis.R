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
#en regardant l'acf je dirais que q= 18 et p=4, même si + flou pour le p comme dépasse le seuil également en p=11
# donc tester AR(4), MA(18), and mixed ARMA models. a noter que cette partie nous informe sur les ordres maximums vraisemblables
pmax=4
qmax=18


#je récup q_test pour tourner la fin du code
Qtests <- function(series, k, fitdf=0) {
  pvals <- apply(matrix(1:k), 1, FUN=function(l) {
    pval <- if (l<=fitdf) NA else Box.test(series, lag=l, type="Ljung-Box", fitdf=fitdf)$p.value
    return(c("lag"=l,"pval"=pval))
  })
  return(t(pvals))
}
signif <- function(estim){ #fonction de test des significations individuelles des coefficients
  coef <- estim$coef
  se <- sqrt(diag(estim$var.coef))
  t <- coef/se
  pval <- (1-pnorm(abs(t)))*2
  return(rbind(coef,se,pval))
}









## fonction pour estimer un arima et en verifier l'ajustement et la validite
modelchoice <- function(p,q,data=xm, k=24){
  estim <- try(arima(data, c(p,0,q),optim.control=list(maxit=20000)))
  if (class(estim)=="try-error") return(c("p"=p,"q"=q,"arsignif"=NA,"masignif"=NA,"resnocorr"=NA, "ok"=NA))
  arsignif <- if (p==0) NA else signif(estim)[3,p]<=0.05
  masignif <- if (q==0) NA else signif(estim)[3,p+q]<=0.05
  resnocorr <- sum(Qtests(estim$residuals,393,length(estim$coef)-1)[,2]<=0.05,na.rm=T)==0
  checks <- c(arsignif,masignif,resnocorr)
  ok <- as.numeric(sum(checks,na.rm=T)==(3-sum(is.na(checks))))
  return(c("p"=p,"q"=q,"arsignif"=arsignif,"masignif"=masignif,"resnocorr"=resnocorr,"ok"=ok))
}

## fonction pour estimer et verifier tous les arima(p,q) avec p<=pmax et q<=max
armamodelchoice <- function(pmax,qmax){
  pqs <- expand.grid(0:pmax,0:qmax)
  t(apply(matrix(1:dim(pqs)[1]),1,function(row) {
    p <- pqs[row,1]; q <- pqs[row,2]
    cat(paste0("Computing ARMA(",p,",",q,") \n"))
    modelchoice(p,q)
  }))
}

armamodels <- armamodelchoice(pmax,qmax) #estime tous les arima (patienter...)
#Maintenant, je conserve que les modèles bien ajustés et valides
selec <- armamodels[armamodels[,"ok"]==1&!is.na(armamodels[,"ok"]),] #modeles bien ajustes et valides
selec
### On a 5 modeles bien ajustes et valides
# ok veut dire que les 3 autres conditions sont valides
#resnocorr test pour voir si les résidus sont corrélés. résidus d'arima du coup. test du portemanteau. Because each e_t is a function of the observations, it is not an iid sequence
#à chaque étape, la statistique Q est croissante car on inclue le carré d'une autocorrelation function pour un nouveau lag
#le quantile khi deux auquel on le compare croit également car le ddl augmente
#afin d'être VALIDE, notre modèle ne doit jamais rejeter l'hypithèse nulle de coef iid 
#à chaque étape, à chaque fois que l'on ajoute une nouvelle autocorrélation, jusqu'au seuil fixé
#si les résidus étaient trop corrélés, le modèle ne serait pas valide, il n'expliquerait pas bien la dépendant temporelle de nos données
# test sur les paramètres :  le code vérifie si le test de Student pour chaque coefficient AR(i) est significatif à un niveau de 5%. Si c'est le cas, il attribue la valeur 1 à la variable arsignif correspondant à ce coefficient, sinon il attribue la valeur 0.
#Si p=0 (pas de coefficients AR), alors la variable arsignif prend la valeur NA. pareil pour chaque coef MA(i). Si on passe ce test, notre modèle est bien ajusté.
