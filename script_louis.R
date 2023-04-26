rm(list = ls())
library(zoo) #format de serie temporelle pratique et facile d'utilisation (mais plus volumineux)
library(tseries) #diverses fonctions sur les series temporelles
library(ggplot2)
library(fUnitRoots)
library(forecast)
library(plyr)

####set-up####
#path <- "/Users/ludovic/Desktop/ENSAE/S2/series/serie_temp"
path <- "C:/Users/louis/OneDrive/Documents/Cours/Git/serie_temp"
setwd(path) #definit l'espace de travail (working directory ou "wd")
getwd() #affiche le wd
list.files() #liste les elements du wd

####import####

# data link : https://www.insee.fr/fr/statistiques/serie/010537309#Telechargement
datafile <- "input/beer.csv" #definit le fichier de donnees

data <- read.csv(datafile, sep = ";") #importe un fichier .csv dans un objet de classe data.frame


data <- data[nrow(data):1,] #inverse les donnÃ©es
data <- data.frame(data, row.names = NULL) #rÃ©initialise l'index

####representation de la série####

# On enlÃ¨ve les 4 dernière valeurs en vue de la prÃ©vision
names(data)[names(data) == "ï..Date"] <- "Date"
data$Date <-seq(as.Date("1990-01-01"), as.Date("2023-02-01"), by = "1 month")
T <- length(data$Date)
data.source<-data
data <- data[-c((T - 3), (T - 2), (T - 1), T),]

xm <- zoo(data$value) # converti les premiers element de data en serie temporelle de type "zoo"

#T <- length(xm)

#data$Date <- seq(as.Date("2023-01-01"), as.Date("1990-05-01"), by = "-1 month")
#xm <- zoo(data$Valeur, order.by=data$Date)


# reprÃ©sentation graphique


ggplot(data, aes(y = xm, x = Date)) +
  geom_line()
dxm <- diff(xm, 1)
newDate <- data$Date[2:length(data$Date)]
newData <- data.frame(Date = newDate, dxm = dxm)
ggplot(newData, aes(y = dxm, x = Date)) +
  geom_line()


#### Test d'integration ####

summary(lm(xm ~ data$Date))

#Cette regression lineaire nous permet de determiner quel type d'équation parmis les 3 possibles utiliser dans le test Dickey-Fuller 
#Dans cette régression de la série sur le temps, la constante signif et le coefficient non significatifs
#Nous utiliserons donc le test ADF, Test de racine unitaire, avec une constante et sans tendance temporelle déterministe

adf <- adfTest(xm, lag = 0, type = "c")
adf

#On accepte donc l'hypothese d'une racine unitaire. La sÃ©rie semble donc inclure une tendance comme le prÃ©sageait les graphiques
#Cependant ce test est biaisé si
#les résidus de la spécification du test ADF sont autocorrélés.
#On ajoute donc des lag dans la specification du test ADF
#pour controler l'effet du passe sur la relation entre X_t et X_t_-_1 (et X_t (récrire la spÃ©cification complÃ¨te choisie dans le latex)
#Pour choisir le nombre de lag a incorporer dans la spécification testé dans le test ADF
#On utilise la fonction Qtests (repréciser son fonctionnement avecc chat gpt)

Qtests <- function(series, k, fitdf = 0) {
  pvals <- apply(
    matrix(1:k),
    1,
    FUN = function(l) {
      pval <- if (l <= fitdf)
        NA
      else
        Box.test(series,
                 lag = l,
                 type = "Ljung-Box",
                 fitdf = fitdf)$p.value
      return(c("lag" = l, "pval" = pval))
    }
  )
  return(t(pvals))
}

Qtests(adf@test$lm$residuals, 50, fitdf = length(adf@test$lm$coefficients))

#les rÃ©sidus sont tous autocorrÃ©lÃ©s

adfTest_valid <- function(series, kmax, adftype) {
  k <- 0
  noautocorr <- 0
  while (noautocorr == 0) {
    cat(paste0("ADF with ", k, " lags: residuals OK? "))
    adf <- adfTest(series, lags = k, type = adftype)
    pvals <-
      Qtests(adf@test$lm$residuals,
             kmax,
             fitdf = length(adf@test$lm$coefficients))[, 2]
    if (sum(pvals < 0.05, na.rm = T) == 0) {
      noautocorr <- 1
      cat("OK \n")
    } else
      cat("nope \n")
    k <- k + 1
  }
  return(adf)
}

adf <- adfTest_valid(xm, 100, adftype = "c")

#en fait tout nos résidus sont autocorrélés donc pas possible de choisir le bon
#nombre de lag car au bout d'un moment on a pluss de degré de liberté
#On va faire un test adf avec le plus de lag qu'on peut

summary(lm(dxm ~ data$Date[-1]))

#On choisit un modÃ¨le sans trend ni constante d'aprÃ¨s la rÃ©gression
adf <- adfTest_valid(dxm, 50, "nc")
adfTest(dxm, lags = 8, type = "nc")
#on rejette l'ypothÃ¨se de racine unitaire --> sÃ©rie stationnaire en diffÃ©rence premiÃ¨re
#il faut donc travailler avec dxm

#Partie 2 identification

par(mfrow = c(1, 2))
acf(dxm, 30)
pacf(dxm, 30)


#acf-> q, pacf -> p : q=3,  p=9,
# donc tester AR(6), MA(3),
#and mixed ARMA models.
#a noter que cette partie nous informe sur les ordres maximums vraissemblables
pmax = 9
qmax = 3

#fonction de test des significations individuelles des coefficients

signif <-
  function(estim) {
    coef <- estim$coef
    se <- sqrt(diag(estim$var.coef))
    t <- coef / se
    pval <- (1 - pnorm(abs(t))) * 2
    return(rbind(coef, se, pval))
  }

## fonction pour estimer un arima et en verifier l'ajustement et la validite

modelchoice <- function(p, q, data = dxm, k = 24) {
  estim <-
    try(arima(data, c(p, 0, q), optim.control = list(maxit = 20000)))
  if (class(estim) == "try-error")
    return(c(
      "p" = p,
      "q" = q,
      "arsignif" = NA,
      "masignif" = NA,
      "resnocorr" = NA,
      "ok" = NA
    ))
  arsignif <- if (p == 0)
    NA
  else
    signif(estim)[3, p] <= 0.05
  masignif <- if (q == 0)
    NA
  else
    signif(estim)[3, p + q] <= 0.05
  resnocorr <-
    sum(Qtests(estim$residuals, 393, length(estim$coef) - 1)[, 2] <= 0.05, na.rm =
          T) == 0
  checks <- c(arsignif, masignif, resnocorr)
  ok <-
    as.numeric(sum(checks, na.rm = T) == (3 - sum(is.na(checks))))
  return(
    c(
      "p" = p,
      "q" = q,
      "arsignif" = arsignif,
      "masignif" = masignif,
      "resnocorr" = resnocorr,
      "ok" = ok
    )
  )
}

## fonction pour estimer et verifier tous les arima(p,q) avec p<=pmax et q<=max
armamodelchoice <- function(pmax, qmax) {
  pqs <- expand.grid(0:pmax, 0:qmax)
  t(apply(matrix(1:dim(pqs)[1]), 1, function(row) {
    p <- pqs[row, 1]
    q <- pqs[row, 2]
    cat(paste0("Computing ARMA(", p, ",", q, ") \n"))
    modelchoice(p, q)
  }))
}

armamodels <- armamodelchoice(pmax, qmax) #estime tous les arima (patienter...)
#Maintenant, je conserve que les modeles bien ajustes et valides
selec <-
  armamodels[armamodels[, "ok"] == 1 &
               !is.na(armamodels[, "ok"]),] #modeles bien ajustes et valides
selec
### On a 4 modeles bien ajustes et valides
# ok veut dire que les 3 autres conditions sont valides
#resnocorr test pour voir si les residus sont correles. residus d'arima du coup. test du portemanteau. Because each e_t is a function of the observations, it is not an iid sequence
#? chaque ?tape, la statistique Q est croissante car on inclue le carr? d'une autocorrelation function pour un nouveau lag
#le quantile khi deux auquel on le compare croit ?galement car le ddl augmente
#afin d'?tre VALIDE, notre mod?le ne doit jamais rejeter l'hypith?se nulle de coef iid
#? chaque ?tape, ? chaque fois que l'on ajoute une nouvelle autocorr?lation, jusqu'au seuil fix?
#si les r?sidus ?taient trop corr?l?s, le mod?le ne serait pas valide, il n'expliquerait pas bien la d?pendant temporelle de nos donn?es
# test sur les param?tres :  le code v?rifie si le test de Student pour chaque coefficient AR(i) est significatif ? un niveau de 5%. Si c'est le cas, il attribue la valeur 1 ? la variable arsignif correspondant ? ce coefficient, sinon il attribue la valeur 0.
#Si p=0 (pas de coefficients AR), alors la variable arsignif prend la valeur NA. pareil pour chaque coef MA(i). Si on passe ce test, notre mod?le est bien ajust?.


pqs <-
  apply(selec, 1, function(row)
    list("p" = as.numeric(row[1]), "q" = as.numeric(row[2]))) #cree une liste des ordres p et q des modeles candidats
names(pqs) <-
  paste0("arma(", selec[, 1], ",", selec[, 2], ")") #renomme les elements de la liste
models <-
  lapply(pqs, function(pq)
    arima(dxm, c(pq[["p"]], 0, pq[["q"]]))) #cree une liste des modeles candidats estimes
vapply(models, FUN.VALUE = numeric(2), function(m)
  c("AIC" = AIC(m), "BIC" = BIC(m))) #calcule les AIC et BIC des modeles candidats
###
#distance entre le true et l'estimated model car prends en compte une somme des carr? des termes d'erreur+ terme de p?nalisation pour le nombre d'ordre
#BIC consistent estimators of p and q. AIC meilleur asymptotiquement AR(infini). AIC often leads to over parametrisation. AIC favorise les mod?les complexe, alors que BIC p?nalise +

#  L'ARMA(5,3) minimise l'AIC
# L'ARMA(2,1) minimise le BIC

#récupérer les modèles arima310 arma<- arima(dxm,c(3,1,0),include.mean=F) arima choisis
arma53<- arima(xm,c(5,1,3),include.mean=F) 
arma21<- arima(xm,c(2,1,1),include.mean=F)
adj_r2 <- function(model){ 
  p <- model$arma[1]
  q <- model$arma[2]
  ss_res <- sum(model$residuals^2)
  ss_tot <- sum(dxm[-c(1:max(p,q))]^2)
  n <- model$nobs-max(p,q)
  adj_r2 <- 1-(ss_res/(n-p-q-1))/(ss_tot/(n-1))
  return(adj_r2)
}
adj_r2(arma53)
adj_r2(arma21)
#je garde l'ARMA(5,3)
#a le R2 ajust´e le plus important, il donne donc la meilleure pr´evision dans l'´echantillon. On le garde comme meilleur mod`ele au final.

arima_pred <- as.zoo(predict(arma53, n.ahead = 4)$pred)
xm_pred <- as.zoo(merge(xm, arima_pred))
xm_true <- zoo(data.source$value)
ggplot(data.source, aes(y=c(xm_pred, xm_true), x = Date)) +
  geom_line()+
  geom_line()
