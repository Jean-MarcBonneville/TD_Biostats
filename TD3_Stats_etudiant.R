# TD3_Stats_etudiant.R


rm(list=ls()) # start with an empty environment
graphics.off()

setwd('~/Documents/Enseignement/TD Stats Coissac/')
#setwd('F:/TD Stats Coissac/')
list.files()


peuplier <-  read.table('./peuplier.txt', header = TRUE)

head(peuplier)
class(peuplier) # data.frame

dim(peuplier)
summary(peuplier)

# L'objectif du TD est de comparer les moyennes des poids des arbres
# de 4 ans aux sites 1 et 2. 

# Calcul d' un intervalle de confiance pour les 2 echantillons

# 1.2 Prealable au calcul d'intervalle:  Normalit? des ?chantillons 
#' 
#1.2.1 Testerlanormalite de la variable poids dans chaque site.
#Constaterquecettehypoth?seestrespect?e.


peuplier1<- peuplier[peuplier$Site==1 & 
                       peuplier$Age==4 & 
                       peuplier$Annee==1,]


peuplier2<- peuplier[peuplier$Site==2 & 
                       peuplier$Age==4 & 
                       peuplier$Annee==1,]

#??? Ho: distribution normale

# visualisation
par(mfrow=c(1,2))
boxplot(peuplier1$Poids)
boxplot(peuplier2$Poids)
# attention a l'echelle y !
boxplot(list(peuplier1$Poids, peuplier2$Poids))

hist(peuplier1$Poids)
hist(peuplier2$Poids)

length(peuplier1$Poids)
length(peuplier2$Poids)
#' NB on peut faire un test de Student avec des 
#' echantillons de tailles (tres) differentes

# test normalite ( precondition 1)

shapiro.test(peuplier1$Poids)
shapiro.test(peuplier2$Poids)

shapiro.test(peuplier1$Poids)$p.value
shapiro.test(peuplier2$Poids)$p.value

shapiro.test(peuplier1$Poids)$p.value > 0.05 # TRUE ## on ne peut pas rejeter Ho, donc on accepte
# que la distribution est normale 
shapiro.test(peuplier2$Poids)$p.value  > 0.05 # idem

# le vrai test de Student demande des variances egales entre les echantillons testes
?var.test
uu <- var.test(peuplier1$Poids, peuplier2$Poids)
uu
names(uu)
uu$conf.int # contient la valeur 1,  donc on accepte H0
uu$p.value # >0.05, donc on accepte H0
# 

# on peut donc legitimement calculer les intervalles de confiance des moyennes

?t.test
uu <- t.test(peuplier1$Poids)
uu
names(uu)
uu$estimate
uu$conf.int

vv <- t.test(peuplier2$Poids)
vv
vv$conf.int
# A quoi sert un test de Student avec une seule variable ?

pp12 <- cbind(peuplier1$Poids, peuplier2$Poids)
colMeans(pp12)

ww <- rbind(
  site1=uu$conf.int, 
  site2=vv$conf.int 
)

colnames(ww) <- c('min','max')
ww
ww <- cbind(ww, poids=colMeans(pp12))
ww <-  cbind(name=paste('site',1:2), ww)
ww <- as.data.frame(ww)
ww

## Visualiser la question de la diff des moyennes

boxplot(ww$poids)
# Load ggplot2
#install.packages('ggplot2')
library(ggplot2)

g <- ggplot(ww) +
  geom_bar( aes(x=name, y=poids), stat="identity", 
            fill="blue", alpha=0.7) 
g

g + 
  geom_errorbar( aes(x=name, ymin=min, ymax=max), width=0.4, colour="orange", alpha=0.9, size=1.3)
 

#1.2.2 Tester l'egalite des variances ( precondition 2)

var.test(peuplier1$Poids,peuplier2$Poids)

var.test(peuplier1$Poids,peuplier2$Poids)$p.value
# P>0.05,on ne rejette donc pas H0 pour une valeursde ?? = 5%.Onconsid?redoncquelesdeux echantillons on la m?me variance. # 

# on peut utiliser le test de student avec var.equal = TRUE; plus precis

t.test(peuplier1$Poids,peuplier2$Poids,var.equal = TRUE)
#  vrai test de Student
# conclusion sur nos moyennes?


t.test(peuplier1$Poids,peuplier2$Poids,#var.equal = TRUE
       )
t.test(peuplier1$Poids,peuplier2$Poids) # Test de Welsh !
# NB difference ridicule avec effectifs raisonnables (~30)

### analyse pour les diametres #############


# visualisation des donnees
par(mfrow=c(1,2))
boxplot(peuplier1$Diametre)
boxplot(peuplier2$Diametre)
boxplot(list( peuplier1$Diametre, peuplier2$Diametre))

# 1.1.1 test normalite ( precondition 1)

shapiro.test(peuplier1$Diametre)
shapiro.test(peuplier2$Diametre)

shapiro.test(peuplier1$Diametre)$p.value
shapiro.test(peuplier2$Diametre)$p.value

shapiro.test(peuplier1$Diametre)$p.value > 0.05 # TRUE ## on ne peut pas rejeter Ho, donc on accepte
# que la distribution est normale 
shapiro.test(peuplier2$Diametre)$p.value  > 0.05 # idem

var.test(peuplier1$Diametre, peuplier2$Diametre)
var.test(peuplier1$Diametre, peuplier2$Diametre)$p.value
var.test(peuplier1$Diametre, peuplier2$Diametre)$conf.int


# on peut donc legitimement calculer les intervalles de confiance des moyennes, ou s'en passer
{
  uu <- t.test(peuplier1$Diametre)
  uu
  names(uu)
  uu$estimate
  uu$conf.int
  
  vv <- t.test(peuplier2$Diametre)
  vv$conf.int
  
  pp12 <- cbind(peuplier1$Diametre, peuplier2$Diametre)
  colMeans(pp12)
  
  ww <- rbind(
    site1=uu$conf.int, 
    site2=vv$conf.int 
  )
  
  colnames(ww) <- c('min','max')
  ww
  ww <- cbind(ww, Diametre=colMeans(pp12))
  ww <-  cbind(name=paste('site',1:2), ww)
  ww <- as.data.frame(ww)
  ww
  
  g <- ggplot(ww) +
    geom_bar( aes(x=name, y=Diametre), stat="identity", fill="blue", alpha=0.7) 
  
  g
  
  g + 
    geom_errorbar( aes(x=name, ymin=min, ymax=max), width=0.4, colour="orange", alpha=0.9, size=1.3)
  
  
}

#1.1.2 Tester l'egalite des variances ( precondition 2)

var.test(peuplier1$Diametre,peuplier2$Diametre)

#' conduite a tenir si les variances ne sont pas homogenes ?
#' 

# 1.2 test de Ho ( egalite des moyennes)

t.test(peuplier1$Diametre,peuplier2$Diametre,var.equal = TRUE)
t.test(peuplier1$Diametre,peuplier2$Diametre)

#' Conclusion sur Ho : on ne peut pas la rejeter; donc on l'accepte. 
#' On peut donc considerer que les moyennes des diametres sont les memes sur less deux sites
#' 
#' 
#' comparaison de moyennes, 2 echantillons, RECAP METHODE
#' verifier normalite des 2 echantillons 
#' si non: test non parametrique sur les rangs: wilcox.test()
#' si oui ( on peut utiliser un test parametrique, sur les valeurs-> test student possible)
#' verifier egalite des variance
#' si non: t.test() : Test de Welch
#' ( si oui -> t.test(, var.equal =T)) # test de Student
#' 
###  1.4 Comparaison de l'effet de deux drogues ############### 

Hyosciamine=c(0.7,-1.6,-0.2,-1.2,-0.1,3.4,3.7,0.8,0,2) 

Hyoscine=c(1.9,0.8,1.1,0.1,-0.1,4.4,5.5,1.6,4.6,3.4)


somnifere <- data.frame(Hyosciamine, Hyoscine)
somnifere


mean(somnifere$Hyosciamine)
mean(somnifere$Hyoscine)


boxplot(somnifere)
boxplot(somnifere, las=2)
boxplot(somnifere, las=2, ylab='Temps de sommeil additionnel (h)')

somnifere$Hyosciamine-somnifere$Hyoscine

plot(x = rep(1, nrow(somnifere)), 
     , y=somnifere$Hyosciamine-somnifere$Hyoscine, xaxt='n',
     xlab='')

?shapiro.test #  NB pas d'option pour valeurs appariees
shapiro.test(somnifere$Hyosciamine-somnifere$Hyoscine)
shapiro.test(somnifere$Hyosciamine-somnifere$Hyoscine)$p.value # legerement inferieur a 0.05...

?t.test
#' NB pour  un test t avec les valeurs appariees, il n'est pas necessaire de verifier
#' l'homogeneite des variances
#' http://www.sthda.com/french/wiki/test-de-student-est-il-toujours-correct-de-comparer-des-moyennes#google_vignette

t.test(somnifere$Hyosciamine, 
       somnifere$Hyoscine , 
       paired = TRUE, 
       alternative = 'greater')
# NB si on se trompe de sens, on obtient une p.value proche de 1,


t.test(somnifere$Hyosciamine, 
       somnifere$Hyoscine , 
       paired = TRUE)
# si on oublie qu'il y a un sens, on perd en puissance

t.test(somnifere$Hyosciamine,
       somnifere$Hyoscine , 
       paired = TRUE, alternative = 'less')

t.test(somnifere$Hyosciamine,
       somnifere$Hyoscine , 
       #paired = TRUE,
       alternative = 'less')
# si on oublie l'appariement, le test perd BEAUCOUP en puissance

# On garde en memoire alpha =0.03 pour l'interpretation de rejet de Ho
t.test(somnifere$Hyosciamine,
       somnifere$Hyoscine , 
       var.equal = T,
       #paired = TRUE,
       alternative = 'less')

# On garde en memoire alpha =0.03 pour l'interpretation de rejet de Ho

# 2 Statistique non parametrique ###########

x<-c(23,15,28,26,13,8,21,25,24,29)
y<-c(18,22,33,34,19,12,27,32,31,30) 
?wilcox.test(x,y)


boxplot(cbind(x, y))
# # Not run
# wilcox.test(x,y) # manque de puissance et ne correspond pas au setup

# OK
wilcox.test(x,y, paired = T)

# alternative OK
wilcox.test(x-y)

wilcox.test(somnifere$Hyosciamine,somnifere$Hyoscine , paired = T)
wilcox.test(somnifere$Hyosciamine-somnifere$Hyoscine )


plot(x=rep(1, length(x)), y=x, xlim=c(0,3), ylim = c(0, max(y)))
points(x=rep(2, length(y)), y=y)

shapiro.test(x)
shapiro.test(y)

t.test(x,y)

# Exercice ######

