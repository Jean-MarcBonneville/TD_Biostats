# TD4_Stats_Etudiants.R


rm(list=ls()) # start with an empty environment
graphics.off()
setwd("~")

setwd('./Documents/Enseignement/TD Stats Coissac2021/')

#setwd('F:/TD Stats Coissac/')
list.files()

# 1 Culture compar?e des bact?ries du BCG  ############

 
# 1.1 Lesdonnees experimentales 

A <- c(10,12,8,10,6,13,9,10,8,9) 
B <- c(11,18,12,15,13,8,15,16,9,13)
C <- c(7,14,10,11,9,10,0,11,7,9) 
D <- c(12,9,11,10,7,8,13,14,10,11) 
E <- c(7,6,10,7,7,5,6,7,9,6)
BCG <- data.frame(A,B,C,D,E) 
BCG

# 1.2 Description des echantillons 

#' IL MANQUE UNE ETAPE PRELIMINAIRE IMPORTANTE DANS TD4.pdf ! Laquelle ?
{
  # ON VISUALISE TOUJOURS SES DONNEES POUR COMMENCER !
  boxplot(BCG)
  }

colMeans(BCG)

var(BCG)
diag(var(BCG))

summary(BCG)

# 1.3 Test des conditions prealables la ralisation de l'analyse  #####    

# 1.3.1. test de normalite
qqnorm(A)
qqline(A)
?qqline(A)

shapiro.test(A)
?shapiro.test

par(mfrow=c(2,3))
apply(BCG,2,function(x) 
  {qqnorm(x)
  qqline(x)})

## NULL

graphics.off()

# mapply
myplot <-  function(j){
  qqnorm(BCG[,j])
  qqline(BCG[,j]) 
  mtext(paste('milieu',j))
}

myplot(3)

par(mfrow=c(2,3))
mapply(j=1:ncol(BCG), myplot)

##
apply(BCG,2,function(x) shapiro.test(x)$p.value)


# ecrire de meme la fonction BCGshap pour les tests Shapiro
# tester la fonction
# l'utiliser avec mapply

myShapiro <- function(j)
  shapiro.test(BCG[,j])$p.value

mapply(myShapiro, j=1:ncol(BCG))
# 1.3.2. test d'homogeneite des variances

?var.test # Test de Fisher pour n=2
?bartlett.test # pour n>2

uu <- bartlett.test(BCG)
# quelle est l'hypothese nulle ?
# conclusion ?
uu
names(uu)
uu$p.value
uu$statistic
uu$data.name
uu$method

# 1.4 Analyse de la variance  #############

# 1.4.1. variance totale #####
?rapply
x <- rapply(BCG, c)
x

BCG
mean(x)
SCT <- sum((x- mean(x))^2)

SCT # somme des carres des ecarts a la moyenne

sum((x- mean(x))**2)

df= (length(x) -1) # ddl total
VT <- SCT/df
VT # variance totale estimee

var(x) # la fonction ad hoc dans R

# 1.4.2 La variance intra-groupe del'exp?rience  ########
# variance intra groupe ##
BCG
colMeans(BCG)

sweep(BCG,2,colMeans(BCG),'-')

sweep(BCG,2,colMeans(BCG),'-')**2

# an alternative to sweep()
{
  SCE_ij_F <-  function(i,j)
    (BCG[i,j] - colMeans(BCG)[j])**2
  
  SCE_ij_F(1,1) # OK
  SCE_ij_F(1,2)  # OK
  
  
  SCE_ij_F(i=1) # attention j n'est pas defini

  SCE_ij_F(j=1) # attention i n'est pas defini
 
 
mapply(SCE_ij_F, i=1:nrow(BCG), j=1)
mapply(SCE_ij_F, i=1:nrow(BCG), j=2)

sum(mapply(SCE_ij_F, i=1:nrow(BCG), j=1))

j=2
 sum(mapply(SCE_ij_F, i=1:nrow(BCG), j=j))

SCE_j_F <- function( j, i=1:nrow(BCG)) {
  hans <- sum(mapply(SCE_ij_F, i=i, j=j))
  hans
}

SCE_j_F(j=1) 
SCE_j_F(2)   

mapply(SCE_j_F, j=1:ncol(BCG))

SCE <- sum(mapply(SCE_j_F, j=1:ncol(BCG)))
SCE
}


?sweep # the ad hoc R function

sum(sweep(BCG,2,colMeans(BCG),'-')**2) 
SCE <-  sum(sweep(BCG,2,colMeans(BCG),'-')**2) 

sum(sweep(BCG,2,colMeans(BCG),'-')**2) == SCE

# df, variabilité intra
k=length(BCG)
k
length(BCG)==ncol(BCG) # True
length(as.matrix(BCG))==ncol(BCG) # FALSE ! # Matrix ≠ Data frame...

k
df1 <- (length(x) -k) 
VE <- SCE/df1
VE


## 1.4.3 variance inter-groupe ####

apply(BCG,2,length) # nj

mean(x) # mu globale

colMeans(BCG) # mu d'indice j

SCI <- sum(apply(BCG,2,length) * (colMeans(BCG)- mean(x))**2)

SCI

# df, variabilite inter
df2=ncol(BCG)-1
VI <- SCI /df2

VI <- SCI /(length(BCG)-1)
#(colMeans(BCG)- mean(x))**2
VI # variance inter groupe

SCT
SCE + SCI # theoreme general verifié sur l'exemple BCG

# demontrer l'egalite des moyennes revient a demontrer que les variances intra et inter sont egales
Fc = VI/VE
Fc
df1 # ddl 'intra' en numerateur
df2 # ddl 'inter' en denominateur

# Fc suit une loi de Fischer a (length(BCG)-1) et k=(length(x) -length(BCG)) ddl

# visualiser la table de Fischer
# http://www.biostatistique.u-psud.fr/cours-F.php
#' https://www.supagro.fr/cnam-lr/statnet/tables.htm#fisher0.05
#' https://www.researchgate.net/figure/Table-de-la-loi-de-Fisher-Snedecor5_fig16_289700323

# retrouver la p. value associee a notre cas

?pf # court-circuite le recours a la table F

1-pf(Fc,k-1,length(x)-k)
1- pf(Fc, df2, df1)

pf(Fc,k-1,length(x)-k)
qf(0.95, k-1,length(x)-k) # renvoie la valeur de F pour 1-alpha=0.95

1-pf(Fc,k-1,length(x)-k)
# c'etait quoi Ho, deja ?
boxplot(BCG)
graphics.off() 
boxplot(BCG)
#  conclusion ?

# 1.6 Utilisation de la fonction d’ANOVA intégré à R ##
# 1.6.1 Reformatage des données

BCG
BCG_Group <- stack(BCG)
head(BCG_Group, n=20)
head(BCG)
colnames(BCG_Group) <- c("UFC", "milieu")
head(BCG_Group, n=10)

# #install.packages('reshape2')
# library(reshape2)
# BCG.m <- melt(BCG)
# head(BCG.m)
# colnames(BCG.m) <- c('milieu', 'UFC')

library(ggplot2)
ggplot(BCG_Group, aes(x=milieu, y=UFC))+
  geom_boxplot()


# 1.6.2 Réalisation de l’ANOVA
?aov
a <- aov(BCG_Group$UFC ~ BCG_Group$milieu) 
a

b <- aov(UFC ~ milieu, data = BCG_Group)
b
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

as.data.frame(ptt)
class(ptt)
names(ptt)

ptt$p.value
class(ptt$p.value)

? symnum # voir le dernier example pour le choix des symboles

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


#' Les faits sont têtus. 
#' Il est plus facile de s'arranger avec les statistiques (Mark Twain).
#' 

#' 
colMeans(rendement)
diag(var(rendement))

#' H0 il n’y a pas d’effet ni du type d’alimentation, ni de la dose
#' H1 il y a un effet de l’alimentation
#' H1’ ilyauneffetdeladose
#' H1” il y a un effet de l’interaction entre les deux facteurs
#' 
#' On verifie la normalite des echantillons par le test de Shapiro
#' On verifie l'egalite des variances
#' 

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
head(rendement_aov)

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

graphics.off()
boxplot(rendement)
# # Not run
# aov0<-aov(kgLait ~ ALIM + DOSE , data=rendement_aov) 
# summary(aov0)
# NB : la fonction aov() ne calcule pas les interactions entre facteurs d'elle meme

# Exo rendement du ble ######





