# TD4_Stats_etudiants.R

#' Les faits sont têtus. 
#' Il est plus facile de s'arranger avec les statistiques (Mark Twain).
#' 


rm(list=ls()) # start with an empty environment
graphics.off()
setwd("~")

setwd('./Documents/Enseignement/TD Stats Coissac/')
#setwd('F:/TD Stats Coissac/')
list.files()

# 1 Culturecompar?edesbact?riesduBCG  ############

 
# 1.1 Lesdonnees experimentales 

A <- c(10,12,8,10,6,13,9,10,8,9) 
B <- c(11,18,12,15,13,8,15,16,9,13)
C <- c(7,14,10,11,9,10,0,11,7,9) 
D <- c(12,9,11,10,7,8,13,14,10,11) 
E <- c(7,6,10,7,7,5,6,7,9,6)
BCG <- data.frame(A,B,C,D,E) 
BCG

# 1.2 Descriptiondes?chantillons 

boxplot(BCG)

?boxplot
  colMeans(BCG)

var(BCG)
?var
diag(var(BCG))
var(A)

summary(BCG)

# 1.3 Test des conditions prealables la ralisation de l'analyse  #####    

# 1.3.1. PREALABLE1 : test de normalite
qqnorm(A)
qqline(A)
?qqline(A)

shapiro.test(A)
??shapiro.test

par(mfrow=c(2,3))
apply(BCG,2,function(x) 
  {qqnorm(x)
  qqline(x)})

apply(BCG,2,function(x) shapiro.test(x)$p.value)

graphics.off()
par(mfrow=c(2,3))

# unhappy with apply() ? Use mapply instead
{mapply(function(j) {qqnorm(BCG[,j]); qqline(BCG[,j])}, j=1:ncol(BCG))

BCGview <-  function(j){
  qqnorm(BCG[,j])
  qqline(BCG[,j])
  }

BCGview(4)
mapply(BCGview, j=1:ncol(BCG))

# ecrire de meme la fonction BCGshap pour les tests Shapiro
# tester la fonction
# l'utiliser avec mapply

myShapiro <- function(j)
  shapiro.test(BCG[,j])$p.value

# verifier que notre fonction fonctionne
myShapiro(j=1)

mapply(myShapiro, j=1:ncol(BCG))

#version de luxe
myShapiro2 <- function(j){
  ans <- shapiro.test(BCG[,j])$p.value
  names(ans) <- colnames(BCG)[j]
  return(ans)
}

myShapiro2(3)  
mapply(myShapiro2, j=1:ncol(BCG))
}

# 1.3.2.PREALABLE2: test d'homogeneite des variances

?var.test # Test de Fisher pour n=2
?bartlett.test # pour n>2

uu <- bartlett.test(BCG)
# quelle est l'hypothese nulle ?
uu
# conclusion ?
names(uu)
uu$p.value
uu$statistic
uu$data.name
uu$method

# 1.4 Analyse de la variance  #############

# 1.4.1. variance totale #####
?rapply
X <- rapply(BCG, c)
X
BCG
mu <- mean(X)
mu
mean(BCG)

SCT <- sum((x- mean(x))^2)

SCT # somme des carres des ecarts a la moyenne

#sum(sum((x- mean(x))**2)

# degres de liberte, ddl
ddl <- length(x) -1
ddl

VT <- SCT/ddl
VT # variance totale estimee

var(x)

# 1.4.2 La variance intra-groupe de l'experience  ########
# variance intra groupe ##
colMeans(BCG)
BCG

?sweep

BCG2 <- sweep(BCG,2,colMeans(BCG),'-')
colMeans(BCG2)
BCG2^2
sum(BCG2^2)

uu <- sweep(BCG,2,colMeans(BCG),'-')
uu
colMeans(uu)

sweep(BCG,2,colMeans(BCG),'-')**2

sum(sweep(BCG,2,colMeans(BCG),'-')**2)
sum(uu^2)

SCE <-  sum(sweep(BCG,2,colMeans(BCG),'-')**2) 


# an alternative to sweep() : home made functions
{
  SCEijF <-  function(i,j)
    (BCG[i,j] - colMeans(BCG)[j])**2
  
 SCEijF(i=1) # attention j n'est pas defini

SCEijF(j=1) # attention i n'est pas defini
 
SCEijF(1,1) # OK
SCEijF(1,2)  # OK

mapply(SCEijF, i=1:nrow(BCG), j=1)
mapply(SCEijF, i=1:nrow(BCG), j=2)

sum(mapply(SCEijF, i=1:nrow(BCG), j=1))

j=2
 sum(mapply(SCEijF, i=1:nrow(BCG), j=j))

SCEjF <- function( j, i=1:nrow(BCG)) {
  hans <- sum(mapply(SCEijF, i=i, j=j)) # fonctions perso en poupee russe
  hans
}

SCEjF(j=1) 
SCEjF(2)   

mapply(SCEjF, j=1:ncol(BCG))

SCEb <- sum(mapply(SCEjF, j=1:ncol(BCG)))
SCEb
}

sum(sweep(BCG,2,colMeans(BCG),'-')**2) == SCEb

k=length(BCG)
k
length(BCG)==ncol(BCG) # True
length(as.matrix(BCG))==ncol(BCG) # FALSE !
k <-  ncol(BCG)

# degrees of freedom
(ddl <- length(x) -k)
ddl

VE <- SCE/ddl
VE


## 1.4.3 variance inter-groupe ####

apply(BCG,2,length) # nj

mean(x) # mu globale

colMeans(BCG) # mu d'indice j

(colMeans(BCG)- mean(x))**2

apply(BCG,2,length) * (colMeans(BCG)- mean(x))**2

SCI <- sum(apply(BCG,2,length) * (colMeans(BCG)- mean(x))**2)

SCI

# degrees of freedom
ddl <- ncol(BCG) -1
ddl

VI <- SCI /ddl
VI # variance inter groupe

SCT
SCE + SCI

SCT==SCE+SCI # eureka !
SCE # composante intra-groupe
SCI # composante inter-groupe

# demontrer l'egalite des moyennes revient a demontrer que les variances intra et inter sont egales
Fc = VI/VE
Fc
# Fc suit une loi de Fischer avec k-1 ddl au numerateur, 
#   soit  (length(BCG)-1) ddl, 
# et
# n-k ddl au denominateur,  soit 
# k=(length(x) -length(BCG)) ddl,


# visualiser la table de Fischer
#http://www.biostatistique.u-psud.fr/cours-F.php

# Plus F est grand, moins l'hypothese nulle est credible

length(BCG)-1# 4, ## k-1,  ddl au numerateur

length(x)-k # 46 ddl au denominateur

# retrouver la p. value associee a notre cas

?pf # court-circuite le recours a la table F

1-pf(Fc,k-1,length(x)-k)

1 -pf(Fc, 4, 45)
# c'etait quoi Ho, deja ?
      boxplot(BCG)
      graphics.off()
      boxplot(BCG)
# conclusion ?

# 1.6 Utilisation de la fonction d’ANOVA intégré à   R######

# 1.6.1 Reformatage des données
BCG
?stack
BCG_Group <- stack(BCG)
head(BCG_Group, n=20)
head(BCG)
colnames(BCG_Group) <- c("UFC", "milieu")
head(BCG_Group, n=15)

# une alternative a stack()
{
  #install.packages('reshape2')
library(reshape2)
uu <- melt(BCG)
head(uu)
colnames(uu) <- c('milieu', 'UFC')
}

# 1.6.2 Réalisation de l’ANOVA
?aov
a <- aov(BCG_Group$UFC ~ BCG_Group$milieu) 
a

b <- aov(UFC ~ milieu, data = BCG_Group)

summary(a)
summary(b)

# NB ici on a analyse l'influence d'une variable discrete sur une deuxieme variable discrete
#'  une ANOVA peut aussi analyser l'influence d'une variable discrete 
#'  sur une  variable continue
#'  
#'  Ici, statistiquement,  on rejette Ho: au moins une des moyennes differe des autres. 
#'  Biologiquement,on conclut que le milieu de culture a une influence sur la croissance du BCG (saturations ≠)

graphics.off()
boxplot(BCG)

?pairwise.t.test
pairwise.t.test(BCG_Group$UFC, BCG_Group$milieu,
                p.adjust.method="bonferroni",
                var.equal=T)
# notion de correction de p.value pour test multiples
# Bonferroni multiplie les p.valeurs (faibles) par le nombre de comparaisons

ptt <- pairwise.t.test(BCG_Group$UFC, BCG_Group$milieu,
                p.adjust.method="bonferroni",
                #var.equal=T
                )

ptt

#as.data.frame(ptt)
class(ptt)
names(ptt)

ptt$p.value
class(ptt$p.value)

? symnum # voir le dernier example dans le help

symnum(ptt$p.value,
       cutpoints = c(0,  .001,.01,.05, .1, 1),
       symbols = c("***","**","*","."," "))


 # 2 Analyse de variance à deux facteurs #  ####

#' il peut y avoir interaction entre dose et nature de l'alimentation
#'  sur la production laitiere
#'  

rendement <- data.frame(Faib_Pail = c(8,11,11,10,7), 
                        Faib_Foin = c(12,13,14,11,10), 
                        Faib_Herb = c(17,13,17,15,13),
                        Faib_Autr = c(8,9,8,10,9), 
                        Fort_Pail = c(8,9,8,10,9), 
                        Fort_Foin = c(10,7,10,12,11), 
                        Fort_Herb = c(11,9,11,11,12), 
                        Fort_Autr = c(17,19,17,16,21))
colMeans(rendement)
graphics.off()
boxplot(rendement)
boxplot(rendement, las=2)
boxplot(rendement, horizontal = T, las=2)
(parmar <-  par()$mar)
?par

par(mar=parmar+ c(0,3,0,0))
boxplot(rendement, horizontal = T, las=2)
boxplot(rendement, horizontal = F, las=2)
#' de visu, y a t'il une influence de la qualite ?  de la  quantité ?
#' de visu, y a t'il interaction entre qualite et quantité ?



#' 
colMeans(rendement)
diag(var(rendement))

#' H0 il n’y a pas d’effet du type d’alimentation et de la dose
#' H1 il y a un effet de l’alimentation
#' H1’ ilya un effetdeladose
#' H1” il y a un effet de l’interaction entre les deux facteurs
#' 
#' On verifie la normalite des echantillons par le test de Shapiro
#' On verifie l'egalite des variances
#' 
#' Quelles sont les hypotheses incompatibles entre elles ?

apply(rendement,2,function(x) shapiro.test(x)$p.value)
apply(rendement,2,function(x) shapiro.test(x)$p.value) > 0.05

# Vérifier également la normalité de manière graphique (cf TD précédent).
{
  graphics.off()
par(mfrow=c(2,4))

apply(rendement,2,function(x) {qqnorm(x); qqline(x)})
}

?bartlett.test(rendement)
bartlett.test(rendement)
bartlett.test(rendement)$p.value 
  
bartlett.test(rendement)$p.value > 0.05 # TRUE, on accepte l'egalite des variances

# avec ces deux prerequis, on peut faire l'ANOVA

rendement_aov <- stack(rendement)

head(rendement_aov)
head(rendement_aov,n=12)
colnames(rendement_aov) <- c('kgLait', 'Traitement')


rendement_aov$dose <- apply(rendement_aov[,2, drop = FALSE], 1, 
                            function(i){
  unlist(strsplit(x = i, split = "_"))[1]})


rendement_aov$alim <- apply(rendement_aov[,2, drop = FALSE], 1,
                            function(i){
  unlist(strsplit(x = i, split = "_"))[2]}) 


rendement_aov$dose <-  NULL
rendement_aov$alim <-  NULL

i=1
#i=8
rendement_aov[i, 'Traitement'] 
class(rendement_aov[i, 'Traitement'])  # attention, un facteur n'est pas une chaine de caracteres

strsplit(rendement_aov[i, 'Traitement'], split='_' ) # et donc, ERR


strsplit(as.character(rendement_aov[i, 'Traitement'] ),'_' ) # OK, a list is returned
unlist(strsplit(as.character(rendement_aov[i, 'Traitement'] ),'_' )) # OK, a list is returned

strsplit(as.character(rendement_aov[i, 'Traitement'] ),'_' )[[1]] # OK

cowF <- function(i){
  ans <- strsplit(as.character(rendement_aov[i, 'Traitement'] ),'_' )[[1]]
  return(ans)
}

cowF(1)
cowF(8)
mapply(cowF, i =1:8)
t(mapply(cowF, i =1:8))

hans <- mapply(cowF, i=1:nrow(rendement_aov))
t(hans)
rendement_aov <-  cbind(rendement_aov, t(hans))

head(rendement_aov)
colnames(rendement_aov)[3:4] <- c('DOSE', 'ALIM')
head(rendement_aov, n=15)

# rendement_aov$ind <- NULL 
# #names(rendement_aov)[1] <- "rdt" 
# names(rendement_aov)[1] <- "kgLait" 
# rendement_aov

# rendement_aov$DOSE <- NULL 
# rendement_aov$ALIM <- NULL 

#aov1<-aov(rdt ~ alim + dose + alim*dose, data=rendement_aov) 
aov1<-aov(kgLait ~ ALIM + DOSE + ALIM*DOSE, data=rendement_aov) 
summary(aov1)

# NB : la fonction aov() ne calcule pas les interactions entre facteurs d'elle meme
# Not run
aov0<-aov(kgLait ~ ALIM + DOSE , data=rendement_aov)
summary(aov0)

# Exo rendement du ble ######

# A vous de jouer !




