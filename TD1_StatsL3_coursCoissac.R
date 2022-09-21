# TD1_StatsL3_coursCoissac.R

#' les etudiants sont censes avoir lu le cours Coissac et
#' le TD.
#' Tout le monde doit avoir installé R et Rstudio
#' Si possible, preferer les ordinateurs perso aux ordis UGA...
#' 
#' Pause 10 min a 15h (me le rappeler !)

rm(list=ls()) # start with an empty environment
graphics.off()

getwd()

# setwd('./Documents/Enseignement/TD Stats Coissac/')
setwd('~/Documents/Enseignement/TD Stats Coissac/')
list.files()

# 1 Les données peuplier 
# 1.1. Manipulation des données sous R  #######

# articuler un script R avec #######'

peuplier <-  read.table('./peuplier.txt', header = TRUE)

head(peuplier)
class(peuplier) # data.frame

dim(peuplier)
peuplier$Diametre

peuplier[1,]
peuplier[2:5,]

peuplier[c(3,7,9),]

peuplier[c(3,7,9),c('Annee','Hauteur')]

colnames(peuplier)
peuplier[c(3,7,9),c(2,5)]

class(c(1,4,7))

class(peuplier$Hauteur) # voir aussi dans l'environnement
class(peuplier$Site) # voir aussi dans l'environnement

peuplier$Hauteur
peuplier[peuplier$Diametre < 7 & peuplier$Hauteur > 10, c('Diametre','Age','Hauteur')]
# peu d'arbres freles

peuplier[300:310,] # sortie standard dans la console
uu <- peuplier[300:310,] # attribution d'un objet
uu # rappel de l'objet

peuplier[,12:15] # msg d'erreur, sortie Erreur

vv <- peuplier[,12:15]
vv # pas d'objet attribue

peuplier[peuplier$Hauteur > 10,]

peuplier[peuplier$Hauteur > 10,c('Diametre','Age','Hauteur')]

peuplier[peuplier$Diametre < 7 & peuplier$Hauteur > 10, c('Diametre','Age','Hauteur')]

dim(peuplier[peuplier$Diametre < 7 & peuplier$Hauteur > 10, c('Diametre','Age','Hauteur')])
 dim(peuplier[peuplier$Diametre < 7 | peuplier$Hauteur > 10, c('Diametre','Age','Hauteur')])

 #' combien de traitements differents ?
 {
   unique(peuplier$Traitement)
   table(peuplier$Traitement)
 }
 
 #' Combien d'arbres plantes l'annee 1 ont subi le traitement 4 ?
 {
   dim(peuplier[peuplier$Annee==1 & peuplier$Traitement==4,])[1] # 33
 }
 
 #' Combien d'arbres plantes l'annee 1 ont subi le traitement 3 ou 4 ?
 {dim(peuplier[peuplier$Annee==1 & 
                 (peuplier$Traitement==4 | peuplier$Traitement==3)
               ,])[1] # 68
   
   dim(peuplier[peuplier$Annee==1 & 
                  (peuplier$Traitement %in% 3:4)
                ,])[1] # 68
 }
 
 #' Combien d'arbres plantes l'annee 1 ont un diametre inferieur a 4 ? 
 #' inferieur ou egal a 4.05 ?
 {dim(peuplier[peuplier$Annee==1 & peuplier$Diametre<4,])[1] # 58
   dim(peuplier[peuplier$Annee==1 & peuplier$Diametre<=4.05,])[1] # 63
 }
 
 #' Combien d'arbres plantes l'annee 2 ont un diametre compris entre 6 et 8 ?
 {dim(peuplier[peuplier$Annee==2 & peuplier$Diametre> 6 &
                 peuplier$Diametre<8,])[1] # 56
 }
 
 #' combien de sites, en excluant le premier ?
 {
   length(unique(peuplier$Site))
   st <- unique(peuplier$Site)
   st[st!= 1]
   length(st[st!= 1])
 }
 
 # Y a t'il des arbres de deux ans ?
 {
   2 %in% unique(peuplier$Age)
   which(peuplier$Age==2)
   length(which(peuplier$Age==2))
 }
 
 
# 1.2 Test d'un parametre #####
#' 
#' Quel est l'intervalle de confiance de la moyenne du poids des arbres 
#' de 4 ans et plantes l'annee 1 ? 

# les dieses (#) annotent
# noter l'articulation du script par ########## en haut a D (a cote de Source) 

poids.4ans.annee1 <- peuplier$Poids[peuplier$Age==4 & peuplier$Annee==1]
poids.4ans.annee1 
class(poids.4ans.annee1 )
length(poids.4ans.annee1)

hist(poids.4ans.annee1)
uu <-  hist(poids.4ans.annee1)
uu
names(uu)
class(uu)
plot(uu)

#' RAPPEL DEFINITION : p.value = probabilité de se tromper en rejetant l'hypothese nulle
#' >> il faut donc formuler une hypothese nulle
#' On chosit et effectue ensuite un test statistique qui renvoie une p.valeur

#' Qu'est ce qu'un intervalle de confiance ?
#' Prerequis pour le calculer ?


moyenne.p.4.1 = mean(poids.4ans.annee1) 
abline(v=moyenne.p.4.1 , lty=2, col='red')
abline(v=median(poids.4ans.annee1) , lty=2, col='green')
# difference entre mediane et moyenne ?

var(poids.4ans.annee1) 
# grandeur de cette variance ? (unites ?)

# ecart-type
var(poids.4ans.annee1)^0.5 
var(poids.4ans.annee1)^(1/2) # essayer sans parentheses
var(poids.4ans.annee1)**0.5
sqrt(var(poids.4ans.annee1))

sd(poids.4ans.annee1) 
# grandeur de cet ecart-type ?

qqnorm(poids.4ans.annee1) 
qqline(poids.4ans.annee1) 

### Generation de nombres aleatoires
?rnorm
help(rnorm)
help('rnorm')
help('+') # All R function come with a help
help(dnorm)
help(qunif) # all stats functions are organized the same way in R
help(rt)

rnorm(length(poids.4ans.annee1), mean(poids.4ans.annee1),
      sd(poids.4ans.annee1))

rnorm(length(poids.4ans.annee1), mean(poids.4ans.annee1),
      sd(poids.4ans.annee1))
# Obtient on les memes chiffres ?

mean(rnorm(length(poids.4ans.annee1), mean(poids.4ans.annee1),
           sd(poids.4ans.annee1))
)
mean(rnorm(length(poids.4ans.annee1), mean(poids.4ans.annee1),
           sd(poids.4ans.annee1))
)
mean(rnorm(length(poids.4ans.annee1), mean(poids.4ans.annee1),
           sd(poids.4ans.annee1))
)

sd(rnorm(length(poids.4ans.annee1), mean(poids.4ans.annee1),
           sd(poids.4ans.annee1))
)
sd(rnorm(length(poids.4ans.annee1), mean(poids.4ans.annee1),
           sd(poids.4ans.annee1))
)

normal.alea = rnorm(length(poids.4ans.annee1), mean(poids.4ans.annee1),
                    sd(poids.4ans.annee1))
normal.alea2 <- rnorm(length(poids.4ans.annee1), mean(poids.4ans.annee1),
                    sd(poids.4ans.annee1))
normal.alea
normal.alea # Obtient on les memes chiffres ?

normal.alea2 # Obtient on les memes chiffres ?
normal.alea2 # Obtient on les memes chiffres ?

# notes sur = et <- : attribution
# https://delladata.fr/huit-raccourcis-clavier-a-utiliser-sous-r-studio/

# 1.2.1 Test de la normalité d’un echantillon ####### 
qqnorm(normal.alea) 
qqline(normal.alea) 

qqnorm(normal.alea2) 
qqline(normal.alea2) 

# Peut on considerer qu'on a verifie le pre-requis de normalite ?
shapiro.test(poids.4ans.annee1)

names(shapiro.test(poids.4ans.annee1))

shapiro.test(poids.4ans.annee1)$p.value
shapiro.test(poids.4ans.annee1)$statistic

help("shapiro.test")
# Peut on considerer qu'on a verifie le pre-requis de normalite ?

# Lorsque k est grand, la loi de Student peut ??tre approch??e par la loi normale centr??e r??duite. 

# 1.2.2 Calcul de l’intervalle de confiance ######

mm <- mean(poids.4ans.annee1)
sm <- sqrt(var(poids.4ans.annee1)/length(poids.4ans.annee1)) 
df <- length(poids.4ans.annee1)-1
alpha <- 0.05 # le fameux risque accept?? de 1/20  de se tromper

## montrer une table de Student: student.png
#https://sites.google.com/site/coursdestatistiques/_/rsrc/1468756185576/table-de-student/student.png

?qt # permet de calculer n'importe quel quantile pour la loi de student
t <- qt(alpha/2,df)
t # noter t ~= 2

erreur <- -t * sm 

barplot(mm)

borne.min <- mm - erreur 
borne.max <- mm + erreur
borne.min
borne.max
c(borne.min, borne.max)
round(c(borne.min, borne.max), 2) # l'intervalle de confiance a 95% de la moyenne

graphics.off()
quartz() # X11()
par(mfrow=c(2,1))

hist(poids.4ans.annee1)
abline(v=moyenne.p.4.1 ,  col='red')
abline(v=median(poids.4ans.annee1) , col='green')

abline(v=c(borne.min, borne.max) ,  col='red', lty=2)

#boxplot(poids.4ans.annee1)
boxplot(poids.4ans.annee1, horizontal=T)

plot(function(x)dt((x - mm) / sm ,df),2,3.2)
plot(function(x)dt((x - mm) / sm , df),xlim = c(2, 3.2)) 

abline(v=borne.min,  col='red', lty=2)
abline(v=borne.max,  col='red', lty=2) 
text(borne.min,0.35,expression(t[alpha/2]^(n-1)),adj = c(-0.2,0)) 
text(borne.max,0.35,expression(t[1-alpha/2]^(n-1)),adj = c(-0.2,0))

# Comparaison d'une moyenne observee a une moyenne theorique (p12) ##########
#'
#'  On voudrait savoir si le poids moyen des arbres ??g??s de 4 ans 
#' et plant??s l???ann??e 1 est ??gal ?? 2 kg.
#' Faire les calculs dans les deux sites
#' 
#' 
poids.4ans.annee1.site1 <- peuplier$Poids[peuplier$Age==4
                                          & peuplier$Annee==1 & peuplier$Site==1]
poids.4ans.annee1.site2 <- peuplier$Poids[peuplier$Age==4
                                          & peuplier$Annee==1 & peuplier$Site==2]

length(poids.4ans.annee1.site1) 
length(poids.4ans.annee1.site2) 
# visualiser la question
par(mfrow=c(1,2)) 
hist(poids.4ans.annee1.site1) 
hist(poids.4ans.annee1.site2)

boxplot(list(poids.4ans.annee1.site1, poids.4ans.annee1.site2))
uu <- boxplot(list(poids.4ans.annee1.site1, poids.4ans.annee1.site2))
uu
poids.4ans.annee1.site1
min(poids.4ans.annee1.site1) # lower wiskher
max(poids.4ans.annee1.site1) # an outlier
# qu'est ce qu'un quantile ?
?quantile
quantile(poids.4ans.annee1.site1, probs = c(0.1,0.25, 0.5,0.75, 0.9))
quantile(poids.4ans.annee1.site1, probs = c(0.05,0.25, 0.5,0.75, 0.95))

# On a deja verifi?? la normalit??.
#' on peut donc legitimement calculer un intervalle de confiance

# la valeur theorique moyenne 2 est elle dans l'intervalle de confiance ? #####

m1 = mean(poids.4ans.annee1.site1)
s1 = sqrt(var(poids.4ans.annee1.site1)/length(poids.4ans.annee1.site1)) 
df1 = length(poids.4ans.annee1.site1) -1
m1
s1
df1

shapiro.test(poids.4ans.annee1.site1)
shapiro.test(poids.4ans.annee1.site2)

T1 <- (m1 -2)/s1
T1 # variable aleatoire d'esperance nulle et de variance 1 qui suit une loi de Student a df1 ddl.
pt(q = T1, df = df1) # 


(1 - pt(T1,df1))*2 # p-valeur du test de student. On note que P> 0.05, donc on ne rejette pas l'hypothese nulle.
#' on peut donc considerer que la moyenne theorique, mu=2 est acceptable

# T1.3 <-( m1-3)/s1
# pt(q = T1.3, df = df1) # ?t.test
#(1 - pt(T1.3,df1))*2

t.test(poids.4ans.annee1.site1, mu=2)
t.test(poids.4ans.annee1.site1, mu=3)
t.test(poids.4ans.annee1.site1, mu=1)



## test Ho: mu=2 for second site
m2 <-  mean(poids.4ans.annee1.site2)
s2 <- sqrt(var(poids.4ans.annee1.site2)/length(poids.4ans.annee1.site2))
df2 <-  length(poids.4ans.annee1.site2) -1

m2
s2
df2
# on a verifi?? la normalite, et on connait donc une variable aleatoire qui suit une loi de student
T2 <- (m2-2)/s2
T2

pt(q=T2, df = df2)

# proba associee a Ho:
(1-pt(q=T2, df = df2))*2 #   0.004474529

t.test(poids.4ans.annee1.site2, mu=2)
t.test(poids.4ans.annee1.site2, mu=2)$p.value # 0.004474529

#'   la probabilite associee a l'hypothese nulle (Ho) est inferieure a 0.01. 
#' On peut donc la rejeter, en acceptant  une probabilite de se tromper (risque alpha)  de 1%
#' On conclut donc que la moyenne observee au site 2 est superieure a 2 kg
#' 
#' Peut on conclure que les moyennes aux sites 1 et 2 sont differentes ?

# student vs normal distribution ####
graphics.off()
quartz()
par(mfcol=c(2,2))

plot(function(x)dt((x - mm) / sm , df),xlim = c(2,3.2)) 
# lines(function(x)dt((x - mm) / sm , df=10),xlim = c(2,3.2), col='blue') 
# lines(function(x)dt((x - mm) / sm , df=10)) 
curve((function(x)dt((x - mm) / sm , df=10)), add = T) 
?line


# Data generation
# x  <- seq(-2, 2, 0.05)
# x
# y1 <- pnorm(x) # mean=0
# y1
# ?pnorm
# #y2 <- pnorm(x,1,1)
# y2 <- pnorm(x,mean=1,sd=1)
# y2
# df <- data.frame(x,y1,y2)
# 
# require(ggplot2)
# 
# ggplot(df, aes(x)) +                    # basic graphical object
#   geom_line(aes(y=y1), colour="red") +  # first layer
#   geom_line(aes(y=y2), colour="green")  # second layer

#
# plot(function(x)dt((x - mm) / sm , df),xlim = c(2,3.2)) 
# x  <- seq(-2, 2, 0.05)

# Data generation
x <-  seq(2, 3.2, 0.05)
df <- length(poids.4ans.annee1)-1

y1 <- dt(x=(x - mm) / sm , df = df)
y2 <- dt(x=(x - mm) / sm , df = 10)
y3 <-  dnorm(x=(x - mm) / sm,mean = 0 ,sd = 1)             

DF <- data.frame(x, y1,y2)

#install.packages('ggplot2')
library(ggplot2)

g <- ggplot(DF, aes(x)) +                    # basic graphical object
  geom_line(aes(y=y1), colour="red")   # first layer

g + geom_line(aes(y=y2), colour='blue') 


DF <- data.frame(x, y1,y2, y3)

g <- ggplot(DF, aes(x)) +                    # basic graphical object
  geom_line(aes(y=y1), colour="red") +  # first layer
  geom_line(aes(y=y2), colour='blue') +  # second layer
  geom_line(aes(y=y3), colour='black') + # third layer
  ylab('Density of probability')

g

head(DF)


ggplot(data.frame(x = c(-4, 4)), aes(x)) + 
  stat_function(fun = dnorm, args = list(mean = 0, sd = 1), aes(colour = "mean = 0 / sd =1")) +
  stat_function(fun = dnorm, args = list(mean = 0, sd = .5),aes(colour = "mean = 0 / sd =0.5"))+
  scale_colour_manual("Parameters",values=c("blue","red"))

# ggplot()+
#   geom_line(data=Summary,aes(y=Y1,x= X,colour="Y1"),size=1 )+
#   geom_line(data=Summary,aes(y=Y2,x= X,colour="Y2"),size=1) +
#   scale_color_manual(name = "Y series", values = c("Y1" = "darkblue", "Y2" = "red"))

g + scale_color_manual(name='loi de proba', values=c('Student, df=67'= 'red', 'Student, df=10'='blue',
                                                     'Normal'='black'))

ggplot()+
  #geom_line(data=Summary,aes(y=Y1,x= X,colour="Y1"),size=1 )+
  geom_line( data= DF, aes(x=x, y=y1, colour="Student, df=67"), size=1) +  # first layer
  geom_line( data= DF, aes(x=x, y=y2, colour="Student, df=10"), size=1) +  # second layer
  geom_line( data= DF, aes(x=x, y=y3, colour="'Normal"), size=1) +  # third layer
  scale_colour_manual(name='loi de proba', values=c('Student, df=67'= 'red', 'Student, df=10'='cyan',
                                                    'Normal'='blue'))

?t.test
