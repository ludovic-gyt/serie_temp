rm(list = ls()) #permet de réiniatialiser l'environnement

####LIBRAIRIES####

library(zoo) #Package permettant de formater les séries
#library(tseries) #Diverses fonctions sur les series temporelles
library(fUnitRoots) #Package pour calculer les racines d'un polynome
library(forecast) #Package pour la prévision
#library(dplyr)
#library(plyr)

####SET-UP de l'environnement####

path <- "/Users/ludovic/Desktop/ENSAE/S2/series/serie_temp" #définition de l'emplacement du projet en local
setwd(path) #définition de l'espace de travail (working directory ou "wd")


####IMPORT et RETRAITEMENT DATA####

# lien des données INSEE : https://www.insee.fr/fr/statistiques/serie/010537309#Telechargement
datafile <- "input/beer.csv" #definition du nom du fichier de données dans le wd

data <-
  read.csv(datafile, sep = ";") #importe un fichier .csv dans un objet de classe data.frame

data <- data[nrow(data):1,] #inverse l'ordre des données (car elles sont dans l'ordre chronologique inverse à l'import)
data <- data.frame(data, row.names = NULL) #réinitialise l'index (car l'index peut crée des problème à cause de la commande précédente)

data$Date <- as.yearmon(seq(from = 1990+0/12, to = 2023+1/12 , by=1/12)) #Création d'une liste de type date adapté à l'utilisation de zoo
data.source <- data # On stock les données sources avant d'enlever les 2 dernières valeurs 

T <- length(data$Date) # On stock la longeur de la série dans T
data <- data[-c(T-1, T),] # On enlève les 2 dernière valeurs en vue de la prévision out-of-sample

####CREATION des SERIES ZOO####

xm.source<-
  zoo(data.source$value, order.by = data.source$Date) # converti les valeurs de data.source en série temporelle de type "zoo" en les indiçant par la colonne Date
xm <-
  zoo(data$value, order.by = data$Date[-c(T - 1, T)] ) # converti les valeurs de data en série temporelle de type "zoo" en les indiçant par la colonne Date
# xm est donc la série sans les deux dernières valeurs

####REPRESENTATION GRAPHIQUE####

plot(xm) #graphique  de la série xm
dxm <- diff(xm, 1) #création de dxm, la série xm en différence première
plot(dxm) #graphique  de la série dxm


####TEST de STATIONARITÉ####

index <- as.numeric(rownames(data)) #conversion de l'index en vecteur numérique
summary(lm(xm ~ index)) #régréssion de xm sur l'index, c'est à dire sur le temps

# Nous choisissons le modèle avec constante sans tendance temporelle d'après la 
# significativité des coefficients:

adf <- adfTest(xm, lag = 0, type = "c") # Test ADF avec constante sans tendance temporelle avec 0 lag
adf #résultat du test : on rejette l'hypothèse de non stationarité

# La série semble donc être stationnaire.

# Cependant ce test est biaisé si les résidus de la spécification du test ADF
# sont autocorrélés et il faut alors ajouter des lag dans la specification du test ADF.
# On teste, avec la fonction Qtests, l'hypothèse de non autoccorélation des résidus
# avec un test Ljung-Box pour notre test ADF avec 0 lag :

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
                 fitdf = fitdf)$p.value #Calcul la p_value du test Ljung-Box pour chaque résidus avec un lag supérieur au nombre de degré de liberté et inférieur à k
      return(c("lag" = l, "pval" = pval))
    }
  )
  return(t(pvals))
}

Qtests(adf@test$lm$residuals, 20, fitdf = length(adf@test$lm$coefficients)) # On inclue le paramètre 20 pour voir 
# l'autocorrélation des résidus sur les 20 premiers lags. Les deux premiers ne sont pas calculés car il y a 2 
# degrés de liberté en moins à cause du nombre de coefficients de la spécification du test ADF.

# Tous les résidus du test ADF avec 0 lag sont autocorrélés. Il faut ajouter des lags :

# Pour trouver le nombre de lag optimal à inclure dans le test ADF, il faut itérer 
# ce test en changeant le nombre de lag du test ADF. Il suffit d'arrêter l'itération quand 
# toutes les p valeurs de Qtests sont supérieur à 0.05. Cela signifie en effet qu'aucun 
# résidus n'est corrélé dans la régression car il y a assez de lag dans spécification 
# pour prendre en compte l'effet du passé sur le présent. 


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
    if (sum(pvals < 0.05, na.rm = T) == 0) { #tant qu'au moins une p-value est inférieur à 0.05, le tesst continue a augmenter le nombre de lag dans le test ADF
      noautocorr <- 1
      cat("OK \n")
    } else
      cat("nope \n")
    k <- k + 1
  }
  return(adf)
}

adf <- adfTest_valid(xm, 20, adftype = "c") # On exécute adfTest_valid sur xm avec le paramètre 20 pour Q test et toujours un modèle avec constante sans tendance temporelle

# Le nombre de lag optimal a inclure dans la spécification du test ADF est 9
# En effet avec 9 lags il n'y a plus de problème d'autocorrélation des résidus.
# On peut donc analyser la p value du test ADF avec 9 lag et une constante pour conclure
# a la stationarité ou non de la série. 
# adfTest_valid a en fait enregistré le dernier test réalisé, celui avec le nombre
# de lag optimal:

adf

# On accepte l'hypothèse de non stationarité. On doit donc différencier la série.
# On réitère la procédure pour s'assurer que la série différenciée est statioinnaire (on 
# enlève le premier élément d'index car en différenciant la série on a perdu une date) : 

summary(lm(dxm ~ index[-1]))

# Ni la constante ou la tendance temporelle ne sont significatifs dans cette régression.
# On choisit donc un modèle sans trend ni constante dans notre test ADF :

adf <- adfTest_valid(dxm, 50, "nc")
adf

# On rejette l'hypothèse de racine unitaire avec 8 lags.
# La série est donc bien stationnaire en différence première.
# Il faut donc travailler avec la série dxm


####MODÉLISATION####

# Afin de déterminer les ordres maximums vraissemblables de p et q, nous construisons
# les graphiques d'ACF et PACF

par(mfrow = c(1, 2))
acf(dxm, 30)
pacf(dxm,30)

# La fonction d'autocorrélation partielle est significative jusqu'à l'ordre 9.
# La fonction d'autocorrélation est significative jusqu'à l'ordre 2.

pmax = 9
qmax = 2

# Il s'agit maintenant de sélectioner le meilleur modèle ARIMA(p,1,q) avec p
# et q entre 0 et respectivement pmax et qmax.

# Pour cela on construit une matrice affichant le critère AIC et BIC de chaque modèle : 

mat <- matrix(NA,nrow=pmax+1,ncol=qmax+1) #matrice vide à remplir
rownames(mat) <- paste0("p=",0:pmax) #renomme les lignes
colnames(mat) <- paste0("q=",0:qmax) #renomme les colonnes
AICs <- mat #matrice des AIC non remplie
BICs <- mat #matrice des BIC non remplie
pqs <- expand.grid(0:pmax,0:qmax) #toutes les combinaisons possibles de p et q
for (row in 1:dim(pqs)[1]){ #boucle pour chaque (p,q)
  p <- pqs[row,1] #récupère p
  q <- pqs[row,2] #récupère q
  estim <- try(arima(xm,c(p,1,q),include.mean = F)) #tente d’estimer l’ARIMA 
  AICs[p+1,q+1] <- if (class(estim)=="try-error") NA else estim$aic #assigne l’AIC 
  BICs[p+1,q+1] <- if (class(estim)=="try-error") NA else BIC(estim) #assigne le BIC
}
AICs==min(AICs) #cherche le modèle minimisant le critère AIC
BICs==min(BICs) #cherche le modèle minimisant le critère BIC

# Les modèles ARIMA(3,1,2) et ARIMA(0,1,2) minimisent respectement le AIC et le BIC.

# Cependant, nous allons réaliser une étude plus poussé pour déterminer le meilleur modèle. Nous
# allons notamment déterminer la significativité des coefficient des AR et des MA, et analyser
# l'autoccorélatioin des résidus de chaque modèle.

# Cette fonction permet de calculer la p_value des coefficients d'un modèle ARMA donné :

signif <-
  function(estim) {
    coef <- estim$coef #extrait chaque coefficient
    se <- sqrt(diag(estim$var.coef)) #calcul les écarts types de chaque coefficient
    t <- coef / se #calcul le t de student de chaque coefficient
    pval <- (1 - pnorm(abs(t))) * 2 #calcul la p_value de chaque coefficient
    return(rbind(coef, se, pval))
  }

# Cette fonction estimer un arima et verifie l'ajustement et la validité du moddèle grâce à signif :

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
  arsignif <- if (p == 0) #si p égal 0, il n'y a pas de coefficient AR donc la fonction renvoie NA
    NA
  else
    signif(estim)[3, p] <= 0.05 #si la p-value du coefficient AR le plus laggé est inférieur à 0.05, la fonction renvoie 1 dans la colonne "arsignif"
  masignif <- if (q == 0) #si q égal 0, il n'y a pas de coefficient MA donc la fonction renvoie NA
    NA
  else
    signif(estim)[3, p + q] <= 0.05 #si la p-value du coefficient MA le plus laggé est inférieur à 0.05, la fonction renvoie 1 dans la colonne "masignif"
  resnocorr <-
    sum(Qtests(estim$residuals, 30, length(estim$coef) - 1)[, 2] <= 0.05, na.rm =
          T) == 0 #si les résidus ne sont pas autocorrélés, la fonction renvoie 1
  checks <- c(arsignif, masignif, resnocorr) 
  ok <-
    as.numeric(sum(checks, na.rm = T) == (3 - sum(is.na(checks)))) #la fonction asssigne 1 à la colonne "Ok" uniquement si arsignif, massignif, renocorr sont simultanément égaux à 1 ou NA
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


## la fonction armamodelchoice estime et éxécute modelchoice sur tous les arima(p,q) avec p<=pmax et q<=max

armamodelchoice <- function(pmax, qmax) {
  pqs <- expand.grid(0:pmax, 0:qmax) #crréation de la grille de p et q
  t(apply(matrix(1:dim(pqs)[1]), 1, function(row) { #itération sur la grille
    p <- pqs[row, 1]
    q <- pqs[row, 2]
    cat(paste0("Computing ARMA(", p, ",", q, ") \n"))
    modelchoice(p, q) #éxécute la fonction modelchoice sur le modèle (p,q)
  }))
}

armamodels <- armamodelchoice(pmax, qmax) 
armamodels

# Il s'agit de sélectionner uniquement les modèles ou la colonne "ok" est égale à 1, c'est à dire
# où les coefficients de l'AR le plus laggé et du MA le plus laggé sont significatifs et où les 
# résidus ne sont pas autocorrélés : 

selec <-
  armamodels[armamodels[, "ok"] == 1 &
               !is.na(armamodels[, "ok"]),] 

selec 
# On retient seulement 3 modèles avec cette méthode, les modèles ARIMA(9,1,0), ARIMA(2,1,1) et ARIMA(2,1,2)

# Il faut maintenant choisir le meilleur de ces 3 modèles.

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


#r?cup?rer les mod?les arima310 arma<- arima(dxm,c(3,1,0),include.mean=F) arima choisis
arma22<- arima(xm,c(2,1,2),include.mean=F) #essayer avec T
arma21<- arima(xm,c(2,1,1),include.mean=F)
arma53<- arima(xm,c(5,1,3),include.mean=F)
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
#je garde l'ARMA(5,3)
#a le R2 ajust?e le plus important, il donne donc la meilleure pr?evision dans l'?echantillon. On le garde comme meilleur mod`ele au final.

plot(arma22$residuals)
acf(arma22$residuals)
pacf(arma22$residuals)

hist(arma22$residuals,breaks = 50)
checkresiduals(arma22) #une valeur abbérante pourrait être prise en compte dans la régression avec une indicatrice

#causalité
roots <- polyroot(sort(arma21$coef[c('ar1', 'ar2', 'ar3', 'ar4','ar5')]))
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
  legend("topleft", legend=c("Données réelles", "Prédiction"), col=c("red", "black"), lty=1:2, cex=0.4)
  }
plot_pred(2016)


#graph comparaison

arima_22 <- function(xm_1, xm_2, xm_3) {
  xm_arima <-
    xm_1 + (xm_1 - xm_2) * arma22$coef[1] + (xm_2 - xm_3) * arma22$coef[2]
  return(xm_arima)
}
length(xm)
xm_arima <- c(NA, NA, NA)
for (i in 4:length(xm)) {
  xm_arima[i] <-
    arima_22(as.numeric(xm[i - 1]), as.numeric(xm[i - 2]), as.numeric(xm[i - 3]))
}

xm_arima <- zoo(xm_arima, order.by = data.source$Date)

plot(
  xm[index(xm) >= 2010 + 0 / 12],
  main = "Comparaison de la s?rie et des pr?dictions de l'ARIMA(2,1,2)",
  xlab = "Ann?es",
  ylab = "S?ries",
  col = "black"
)
lines(xm_arima[index(xm) >= 2010 + 0 / 12], col = "red")

legend(
  "topright",
  legend = expression(X[t], "ARIMA(2,1,2)"),
  col = c("black", "red"),
  lty = 1,
  cex = 0.4
)

model <- Arima(prod,c(5,1,0))
plot(model)
