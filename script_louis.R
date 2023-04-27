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

data <-
  read.csv(datafile, sep = ";") #importe un fichier .csv dans un objet de classe data.frame


data <- data[nrow(data):1,] #inverse les données
data <- data.frame(data, row.names = NULL) #réinitialise l'index
#data$Date <-
#  seq(as.Date("1990-01-01"), as.Date("2023-02-01"), by = "1 month") #Convertir la colonne "date" en format date

data$Date <- as.yearmon(seq(from = 1990+0/12, to = 2023+1/12 , by=1/12))
data.source <- data # on stock les données sources avant d'enlever les 4 dernieres valeurs

####representation de la série####

# On enlève les 4 dernière valeurs en vue de la prévision

T <- length(data$Date)
data <- data[-c((T - 1), T),]

# Création d'une série de type zoo

xm.source<-
  zoo(data.source$value, order.by = data.source$Date)
xm <-
  zoo(data$value, order.by = data$Date[-c((T - 1), T)] ) # converti les premiers element de data en serie temporelle de type "zoo"


# représentation graphique

plot(xm)
dxm <- diff(xm, 1)
plot(dxm)


#### Test d'integration ####

index <- as.numeric(rownames(data))
summary(lm(xm ~ index))

#Cette regression lineaire nous permet de determiner quel type d'équation parmis les 3 possibless utiliser dans le test Dickey-Fuller 
#Dans cette régression de la série sur le temps, seulement la constante significative
#Nous utiliserons donc le test ADF, Test de racine unitaire, avec une constante 

adf <- adfTest(xm, lag = 0, type = "c")
adf

#On rejecte donc l'hypothese d'une racine unitaire. La série semble donc être stationnaire
#Cependant ce test est biaisé si
#les résidus de la spécification du test ADF sont autocorrélés.
#On ajoute donc des lag dans la specification du test ADF
#pour controler l'effet du passe sur la relation entre X_t et X_t_-_1 (et X_t (récrire la spécification complète choisie dans le latex)
#Pour choisir le nombre de lag a incorporer dans la sspéccification testé dans le test ADF
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

#en sa basant sur les résultats de la régressions dans le test ADF précédemment exécuté, nous
# concluons à partir de Qtests que tout les résidus sont autocorrélés. Il faut itérer ce test en 
#changeant le nombre de lag dans le test ADF. Pour trouver le nombre de lag optimal à inclure dans le
#test ADF, il suffit d'arrêter l'itération quand tout les p valeurs de Qtests sont supérieur à 0.05. Cela
#signifie en effet qu'aucun résidus n'est corrélé dans la régression car il y a assez de lag dans
#spécification pour prendre en compte l'effet du passé sur le présent. adfTest s'arrête donc quand
#le nombre de lag permet de ne plus avoir d'autoccorrélation des résidus.


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

#9 lag dans la spécification du test ADF permettent de ne plus avoir de problème d'autocorrélation des résidus.
#On peut donc analyser la p value du tesst ADF avec 9 lag et une constante pour conclure
#a la stationarité ou non des résidus
#adfTest_valid a en fait enregistré le dernier test, celui avec le nombre de lag optimal:

adf

#On accepte l'hypothèse de non stationarité, on doit donc différentier la série
#(on enlève la première date car en différenciant la série on a perdu une date) : 

summary(lm(dxm ~ index[-1]))

#On réitère les étapes précédentes afin d'étudier la stationarité de la série différenciée

#Ni la constante ou la tendance temporelle ne sont significatifs avec la série différenciée

#On choisit donc un modèle sans trend ni constante dans notre test ADF:
adf <- adfTest_valid(dxm, 50, "nc")
adf
#On rejette l'ypothèse de racine unitaire, la série est donc bien stationnaire en différence première

#il faut donc travailler avec dxm


#Partie 2 identification

par(mfrow = c(1, 2))
acf(dxm, 30)
pacf(dxm,30)
dev.off()

#acf-> q, pacf -> p : q=2,  p=9,
# donc tester AR(6), MA(3),
#and mixed ARMA models.
#a noter que cette partie nous informe sur les ordres maximums vraissemblables
pmax = 9
qmax = 2

#annexe : méthode TD5 AIC/BIC

mat <- matrix(NA,nrow=pmax+1,ncol=qmax+1) #matrice vide `a remplir
rownames(mat) <- paste0("p=",0:pmax) #renomme les lignes
colnames(mat) <- paste0("q=",0:qmax) #renomme les colonnes
AICs <- mat #matrice des AIC non remplie
BICs <- mat #matrice des BIC non remplie
pqs <- expand.grid(0:pmax,0:qmax) #toutes les combinaisons possibles de p et q
for (row in 1:dim(pqs)[1]){ #boucle pour chaque (p,q)
  p <- pqs[row,1] #r ́ecup`ere p
  q <- pqs[row,2] #r ́ecup`ere q
  estim <- try(arima(xm,c(p,1,q),include.mean = F)) #tente d’estimer l’ARIMA 
  AICs[p+1,q+1] <- if (class(estim)=="try-error") NA else estim$aic #assigne l’AIC 
  BICs[p+1,q+1] <- if (class(estim)=="try-error") NA else BIC(estim) #assigne le BIC
}
AICs
AICs==min(AICs)
BICs
BICs==min(BICs)

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
    sum(Qtests(estim$residuals, 30, length(estim$coef) - 1)[, 2] <= 0.05, na.rm =
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

#  L'ARMA(2,2) minimise l'AIC
# L'ARMA(2,1) minimise le BIC


#r?cup?rer les mod?les arima310 arma<- arima(dxm,c(3,1,0),include.mean=F) arima choisis
arma22<- arima(xm,c(2,1,2),include.mean=F) 
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
adj_r2(arma22)
adj_r2(arma21)

signif(arma22)

#je garde l'ARMA(2,2)
#a le R2 ajust?e le plus important, il donne donc la meilleure pr?evision dans l'?echantillon. On le garde comme meilleur mod`ele au final.

plot(arma22$residuals)
acf(arma22$residuals)
pacf(arma22$residuals)

hist(arma22$residuals,breaks = 50)
checkresiduals(arma22) #une valeur abbérante pourrait être prise en compte dans la régression avec une indicatrice

#causalité
roots <- polyroot(sort(arma22$coef[c('ar1', 'ar2')]))
modulus_roots <- Mod(roots)
modulus_roots #les coefficients sont bien plus grands que 1 donc le modèle est causal

#prévision
model_pred <- predict(arma22, n.ahead=2)
pred <- zoo(model_pred$pred , order.by = as.yearmon(c(2023+0/12,2023+1/12)))

#serie_pred <- zoo(c(xm, model_pred$pred))
link = rbind(xm[length(xm)],pred[1])

#graphiques
dxm.source <- diff(xm.source, 1)

plot_pred <- function(start){
  plot(xm.source, col = 'black', ylab = 'Série', main = 'Prévision des 2 prochaines valeurs de la série',xlim = c(start,2023+3/12))
  #lines(xm_all, col = 'black', type = 'p') # pour avoir des ronds à chaque valeur de la série temporelle
  U = model_pred$pred + 1.96*model_pred$se
  L = model_pred$pred - 1.96*model_pred$se
  xx = c(time (U), rev (time (U)))
  yy = c(L, rev(U))
  polygon(xx, yy, border = 8, col = gray (0.6, alpha=0.2))
  lines(pred, type = "p", col = "red")
  lines(pred, type = 'l', col = 'red') 
  lines(link, type = 'l', col = 'red')
  legend("topleft", legend=c("Donn�es r�elles", "Pr�diction"), col=c("red", "black"), lty=1:2, cex=0.4)
  }
plot_pred(2020)
signif(arma22)[1]

arima_22 <- function(xm_1, xm_2, xm_3){
  xm_arima<- xm_1+ (xm_1-xm_2)*arma22$coef[1]+ (xm_2-xm_3)* arma22$coef[2]
  return(xm_arima)
}

xm_arima<-c(NA, NA, NA)
for (i in 4:398) {
  xm_arima[i] <- arima_22(as.numeric(xm[i-1]), as.numeric(xm[i-2]), as.numeric(xm[i-3]))
}

xm_arima <- zoo(xm_arima, order.by = data.source$Date)http://127.0.0.1:11831/graphics/794537bf-40d8-40f9-ae10-f1f5725d60d1.png

plot(xm[index(xm) >= 2010+0/12], main = "Comparaison de la s�rie et des pr�dictions de l'ARIMA(2,1,2)", xlab = "Ann�es", ylab = "S�ries", col = "black")
lines(xm_arima[index(xm) >= 2010+0/12], col = "red")

legend("topright", legend = expression(X[t], "ARIMA(2,1,2)"), col = c("black", "red"), lty = 1)
