
# TD5_etudiants.R

# Objectif Regression lineaire

#' Il y a trois sortes de mensonges : 
#' les petits mensonges, 
#' les gros mensonges 
#' et les statistiques.
#' (Mark Twain)

rm(list=ls()) # start with an empty environment
graphics.off()

getwd()

# setwd('./Documents/Enseignement/TD Stats Coissac/')
setwd('~/Documents/Enseignement/TD Stats Coissac2021/')

peuplier <-  read.table('./peuplier.txt', header = TRUE)

names(peuplier)

peuplier1<-peuplier[peuplier$Age==3 &
                      peuplier$Annee==1 &
                      peuplier$Traitement==1,]

peuplier1

peuplier2 <-  peuplier1[, 4:6]

head(peuplier2)

plot(peuplier2)

# nous avons realise notre etude preliminaire

# variable reponse : poids (=variable dependante)
# variable predictive : ddh (=variable independante)
#' Quel serait l'interet du modele Poids ~ ddh ?
?lm
ml0 <- lm(Poids ~ Diametre**2 * Hauteur, data=peuplier2)

ml0

summary(ml0)
#' enoncer l'hypothese nulle
#' les hypotheses alternatives
#' 
ddh <- peuplier2$Diametre**2 * peuplier2$Hauteur
ddh

plot(x=ddh, y=peuplier2$Poids)

# reperer un outlier, methode 1:
which(ddh<80 & ddh>70) # 15


#plot(Poids~ ddh,  data=peuplier2)
plot(peuplier2)
#plot(Poids~ Diametre,  data=peuplier2) # note outlier
#plot(Poids~ Hauteur,  data=peuplier2) # note outlier
peuplier2[13:16,]

which(peuplier2$Hauteur>4.8 & peuplier2$Hauteur< 5 & peuplier2$Poids<0.2) # 15


peuplier2$ddh <- peuplier2$Diametre**2 * peuplier2$Hauteur # il faut rectifier ddh !

# methode 2:
head(peuplier2)
rownames(peuplier2)
?text
plot(x=ddh, y=peuplier2$Poids)
plot(Poids~ ddh,  data=peuplier2, xlim=c(0,150))
text(x = ddh+5, y = peuplier2$Poids, rownames(peuplier2), col='blue',
     cex=0.8)

peuplier2$Poids

head(peuplier2)
ddh

peuplier2[15,"Poids"] <- 0.7 # correction d'une erreur de saisie !
peuplier2$Poids[15] <- 0.7 #  equivalent

# Pourquoi a t'on decidé de d'augmenter le poids plutot que de diminuer
#' le diametre ou la hauteur ?
#' Quelle deontologie respecter ?
#' 

peuplier2$ddh <-  ddh

plot(peuplier2)
# Calcul des paramètres du modèle ########
ml0 <- lm(Poids ~ Diametre**2 * Hauteur, data=peuplier2)

ml <-  lm(Poids~ ddh, data = peuplier2)
ml

plot(Poids ~ ddh, data = peuplier2)
?abline()
abline(a=ml$coefficients[1], b=ml$coefficients[2], col='red')

slope <-  round(ml$coefficients[2],3)
intercept <- round(ml$coefficients[1], 3)

text(paste0('H1: y=', slope, '* x + ', intercept),x=40,y=0.9, cex=0.7,col='red' )
# A ce stade, a t'on rejeté H0 ?

summary(ml) 
#'depouiller l'information
#'interpreter
#'quelle est l'hypothese nulle ?
#'Que conclure ?

summary(ml0)

summary(ml) 
names(ml)
ml$coefficients


names(summary(ml))
summary(ml)[[4]]
summary(ml)$coefficients
summary(ml)$coefficients[2, 'Pr(>|t|)']



# validation du modele #######
# 3.1 Test sur la pente #####

names(summary(ml))
summary(ml)$coefficients
summary(ml)$coefficients['ddh', 'Pr(>|t|)'] #  6.145819e-18
#' Que vient on d'extraire de ml ?
#'  c'etait quoi Ho? Que decide t'on ?
summary(ml)$coefficients['ddh', 'Pr(>|t|)']<1e-6

#completer graphiquement pour Ho
{abline(h=mean(peuplier2$Poids), col='blue')
  text(x=100, y=0.3, paste0('H0 : slope=0, P< 1e-6' ), col='blue', cex=0.7)
}

#' Un peu de geometrie !
#' etonnant ? volume d'un cylindre V= pi * D**2/4 * H
#' etonnant ? volume d'un cone V=1/3* pi * D**2/4 * H
#' Conclusion ?

# 3.2 Etude des résidus #######
names(ml)
ml$residuals

#graphics.off()
quartz()
par(mfrow=c(2,2)) 
plot(ml)

residus <- ml$residuals

qqnorm(residus)
qqline(residus)
shapiro.test(residus) # 0.01016 # on rejette la normalite

hist(residus)
?rstudent

#quartz()

plot(peuplier2$ddh,y=rstudent(ml),ylim=c(-3,3))
# Il y a une erreur dans le TD ! ne pas se mettre d'oeilleres !!

plot(peuplier2$ddh,y=rstudent(ml),#ylim=c(-3,3)
)


{
  plot(peuplier2$ddh,y=rstudent(ml),ylim=c(-7,3))
  
  abline(0,0,lty=1)
  abline(-2,0,lty=2)
  abline(2,0,lty=2)
}

#' quel type de distribution sur le plot obtiendrait-on 
#' si la distribution etait Normale ?
#' si on avait 500 arbres ?
qnorm(0.025)
qnorm(1/6)

plot(x = 1:500, y=rnorm(500), cex=0.3, ylim=c(-4, 4))
abline(0,0,lty=1, col='blue')
abline(-2,0,lty=2, col='blue')
abline(2,0,lty=2, col='blue')


# quartz(w=10)
# par(mfrow=c(1,2))
# plot(Poids~ ddh,  data=peuplier2)
# text(x = peuplier2$ddh , y = peuplier2$Poids-0.03, labels = c(1:20))
# abline(a=ml$coefficients[1], b=ml$coefficients[2], col='red')
# 
# plot(peuplier2$ddh,y=rstudent(ml))
# abline(h=0, col='red')
# abline(h=2, col-'red')
# abline(h=-2, col-'red')

graphics.off()
quartz(w=10, h=6)
par(mfrow=c(1,2))
plot(Poids~ ddh,  data=peuplier2)
text(x = peuplier2$ddh , y = peuplier2$Poids-0.03, labels = c(1:20),
     col='blue', cex=0.5)
abline(a=ml$coefficients[1], b=ml$coefficients[2], col='red')

#plot(peuplier2$ddh,y=rstudent(ml))
plot(peuplier2$ddh,y=rstudent(ml), xlim=c(0, 150))
abline(h=0, col='red')
abline(h=2, col='red', lty=2)
abline(h=-2, col='red', lty=2)
text(x = peuplier2$ddh+5,y=rstudent(ml) , labels = c(1:20), 
     col='blue', cex=0.5)

dev.copy2pdf(file='./residus_peuplier_ddh.pdf')


# 4 Régression linéaire mupl    #####
# la chenille processionnaire du pin ##

pin<-read.table("pin.txt",h=T)

head(pin)
dim(pin)
colnames(pin)
colMeans(pin)
var(pin)
diag(var(pin))

cor(pin)
symnum(cor(pin)) # interpreter

cor.test(haut ~ diam, data = pin)
cor.test(pin$haut , pin$diam) # tres forte correlation

graphics.off()
quartz()
plot(pin)

#' variable dependante = 
#' variable reponse =
#' variable à expliquer =
#' variable de sortie = y : ici ?
#' 
#' 
#' variables independantes = 
#' variables d'entree = 
#' variables explicatives = x1, x2,... : ici ?
#' 
#' Combien de variables d'entree dans l'exo peuplier ?
#' Quelle etait la variable de sortie ?
#' 
#' PIN: enoncer l'hypothese nulle
#' ecrire le modele alternatif y= a1*x1 + a2*x2 + ... + epsilon
#' 
lm1 <- lm(proce ~ alt + pente + densi + haut +diam, data=pin)
lm1
lm2 <- lm(proce ~ alt + pente + haut +diam, data=pin)

lm1
lm2

summary(lm1)
summary(lm2)

#' noter l'analyse de la variance sous jacente

# 4.4 Recherche du meilleur modèle#######

#'Il a bossé pendant des jours 
#'Tâchant avec amour 
#'d'améliorer le modèle
#'(Boris Vian, la bombe atomique)


library(MASS)
lm0 <-  lm(pin$proce ~ 1)
lm0 <-  lm(proce ~ 1, data=pin)

lm0
?stepAIC
stepAIC(lm0,
        .~pin$alt+pin$pente+pin$densi+pin$haut+pin$diam,
        trace=F)
stepAIC(lm0,
        .~alt+pente+densi+haut+diam, data=pin,
        trace=F)
# en changeant l'ordre des variables explicatives
stepAIC(lm0,
        .~densi+alt+haut+diam+pente, data=pin,
        trace=F)

pin.lm1<-lm(pin$proce ~ pin$densi + pin$pente + pin$alt) 

summary(pin.lm1)
#' le modele a trois variables d'entree explique 50% de la variance,
#' vs 61 % avec les 5  variables d'entree
#' La contribution de la densité n'est pas significative
#' 

pin.lm1b<-lm(pin$proce ~ pin$pente + pin$alt + pin$densi ) 
summary(pin.lm1b) # validite independante de l'ordre d'entree des variables explicatives

pin.lm2<-lm(pin$proce ~ pin$pente + pin$alt)
pin.lm2<-lm(proce ~ pente + alt, data = pin)
pin.lm2

summary(pin.lm2)
#' le modele a deux variables d'entree explique encore 45% de la variance,
#' vs 50 % avec les  trois variables d'entree
#' 
pin.lm3<-lm(pin$proce ~ pin$pente )
summary(pin.lm3)
# R2: on tombe a 23 % de la variance expliquee avec 1 seul parametre

pin.lm4<-lm(pin$proce ~ pin$alt )
summary(pin.lm4)
# R2: on tombe a 28 % de la variance expliquee avec l'autre

# 4.5 Etude des residus #######
residus<-pin.lm2$residuals 

shapiro.test(residus) # 

plot(x=pin$proce, y=rstudent(pin.lm2),#ylim=c(-3,3)
)
plot(x=pin$proce, y=rstudent(pin.lm2),ylim=c(-3,3))
abline(+2,0,lty=2) 
abline(0,0,lty=1) 
abline(-2,0,lty=2)

##########

