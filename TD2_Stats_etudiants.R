# TD2_Stats_etudiants.R

# retour sur Ho #######
#'
#' Se mettre dans la peau de l'experimentateur, Ho sera typiquement 
#' l'hypothese que l'on veut refuter
#' 
#' Medecin avec un vaccin en phase d'essai:
#' 2 cohortes vaccines/non vaccines
#' % de personnes tombees malades pour les 2 categories
#' Ho: proportions identiques dans les 2 categories
#' Test Xhi2
#' on rejette Ho si la proba associee (p.value) est <0.05
#' on conclut alors a l'efficacite du vaccin
#' 
#' Agronome avec un traitement miracle augmentant le rendement
#' Ho: les rendements sont identiques avec ou sans traitement
#' test student
#'
#'
#'Exception, test type Shapiro !
#' On souhaite pouvoir accepter Ho, normalite de la distrib, car les tests en aval
#' avec une distribution normale sont plus puissants
#'

### A propos du CR ##########
#' Le compte rendu doit être concis : Normalement pas plus de 4 pages
#'  au total pour les 3 exo
#'  Vous pouvez rediger par binome ou en solo, pas par trinome...
#'  La copie d'un binome doit IMPERAtIVEMENT comporter les DEUX noms.
#' 
#' Pour chaque exo 6 points à verifier
#' 1 - Replacer le sujet de l’exo (la question bio) en une phrase
#' 2 - Indiquer quel genre d’analyse permettra de répondre à la question
#' 3 - Tester les pré-condition ou indiquer qu’il n’y en a pas
#'4 - Réaliser le test
#'5 - Conclure sur le test en terme de rejet ou pas de H0
#'6 - Conclure sur la question initiale (exposé dans le point 1)
#'
#'Du coup si on dit 7 points par exo, cela fait 1 point pour chacun des 6 points ci-dessus plus 1 pour la coherence total de l’exo.
#'
#'7 x 3 = 21 mais on considèrera que 7 x 3 = 20 donc 1 point cadeau

# raccourcis clavier R ####
# https://delladata.fr/huit-raccourcis-clavier-a-utiliser-sous-r-studio/
#' CTRL + ENTER : pour envoyer le code dans la console et l'exécuter
#' ALT + - : pour insérer automatiquement la flèche d'assignation <-
#' 

# retour sur le TD 1 #####

rm(list=ls()) # start with an empty environment
graphics.off()


#' Combien d'arbres plantes l'annee 1 ont subi le traitement 4 ?
#' 

#setwd('./Documents/Enseignement/TD Stats Coissac/')
setwd('~/Documents/Enseignement/TD Stats Coissac/')
list.files()

peuplier <-  read.table('./peuplier.txt', header = TRUE)
#' combien de traitements differents ?

#' Combien d'arbres plantes l'annee 1 ont subi le traitement 4 ? 33

#' Combien d'arbres plantes l'annee 1 ont subi le traitement 3 ou 4 ? 68



#' Combien d'arbres plantes l'annee 1 ont un diametre inferieur a 4 ?
#' inferieur ou egal a 4.05 ? 


#' Combien d'arbres plantes l'annee 2 ont un diametre compris entre 6 et 8 ? 56


#' combien de sites, en excluant le premier ? UN

# Y a t'il des arbres de deux ans ? NON

dim(peuplier[peuplier$Annee==1 & 
               (peuplier$Traitement >= 3)
                ,])[1] # 68


#' Combien d'arbres ont subi le traitement 3, reponse par annee ?

peuplier.traitement3 <- peuplier[peuplier$Traitement==3,]
dim(peuplier.traitement3 )
length(peuplier.traitement3$Annee==1) # 74 , pas la reponse voulue !

peuplier.traitement3$Annee==1
which(peuplier.traitement3$Annee==1)
length(which(peuplier.traitement3$Annee==1)) # 35

sum(peuplier.traitement3$Annee==1)           # 35
length(which(peuplier.traitement3$Annee==1)) # 35

length(which(peuplier.traitement3$Annee==2)) # 39

table(peuplier.traitement3$Annee)

# TD2 @@@@@#####
rm(list=ls()) # start with an empty environment
graphics.off()

# #setwd('./Documents/Enseignement/TD Stats Coissac/')
 #setwd('F:/TD Stats Coissac/')
# list.files()

# getwd()
help(runif)
sample30 <- runif(30) 
sample30
sample300 <- runif(300) 
sample300

?par
par(mfrow=c(1,2))
hist(sample30) 
hist(sample300)

# Creer le dossier Graphics dans le repertoire courant; puis

dev.copy(pdf,'./Graphics/sample-unif.pdf',width=12,height=6)
dev.off() # en deux lignes

## ALTERNative:

dev.copy2pdf(file='./Graphics/sample-unif2.pdf') # en une seule ligne

# nombres aleatoires issus d'une distrib normale ou exponentielle

rnorm(30)                # affiche
nor30                       # erreur: objet pas (encore) cree
nor30= rnorm(30)         # attribue 
nor30                    # affiche un objet deja cree
rm(nor30)

nor30
(nor30= rnorm(30))       # atribue et affiche
nor30                    #   nombres idem
(nor30= rnorm(30))      # nouveaux nombres
nor30

nor3000 = rnorm(3000)

expo30= rexp(30)
expo3000= rexp(3000)

par(mfcol=c(2,2))
hist(nor30)
hist(nor3000)
hist(expo30)
hist(expo3000)

# 1.2 Comparaison de deux tests de normalite ################

shapiro.test(sample30)
shapiro.test(runif(30)) # repeter plusieurs fois cette ligne

for(i in 1:4){
  mytest=shapiro.test(runif(30))
  print( mytest)
}
  shapiro.test(runif(30)) # repeter plusieurs fois cette ligne

shapiro.test(runif(300)) # repeter plusieurs fois cette ligne
## Conclusion sur la puissance du test ?

shapiro.test(sample30)$p.value

names(shapiro.test(sample30))

shapiro.test(sample300)$p.value

help(pearson.test)
#install.packages("nortest"),
#install.packages("nortest", repos="http://cran.r-project.org" ) 
library(nortest)
pearson.test(sample30)
pearson.test(sample300) 
# noter les puissances differentes des 2 tests aux 2 effectifs
## 


#1.3 Signification de la pvalue associee a un test

#1.3.1 construction du generateur d'echantillons aleatoires

normalea <- function(x) 
  rnorm(x,1,1) 

mean(normalea(10000))

sd(normalea(10000))

#1.3.2 Constructiondesfonctionsdetest 

shapiro.normtest.pvalue <- function(x) 
  shapiro.test(x)$p.value

pearson.normtest.pvalue <- function(x) 
  pearson.test(x)$p.value

shapiro.normtest.pvalue(sample30)
## 
rep(30,20)


#' On va repeter le test pour 20 nouveaux echantillons de 30 nombres tires d?une distrib normale

# une ligne de code compliquee...
mapply(function(x) shapiro.normtest.pvalue(normalea(x)),rep(30,20))

# pour apprivoiser mapply...
moitie <- function(x)
  x/2
moitie(100)

double= function(x) 2*x

double(3)
double(1:10)
mapply(double, x=1:10) # mapply avec une fonction pre-definie

mapply(function(x) 3*x, x=1:10) # mapply avec fonction  triple definie a la volee


normalea(x=30)
normalea(30)
as.matrix(normalea(30), ncol=1)

normalea(11)
as.matrix(normalea(11), ncol=1)
mapply(function(x) normalea(x),rep(11,20))

mapply(function(x) normalea(x),rep(30,20))

mapply(function(x) shapiro.normtest.pvalue(normalea(x)),rep(30,20))


#le nombre d'echantillons test?s est de 20 
# la taille des echantillons est de 30
# on repete donc 20 fois un test effectue sur 30 nouvelles valeurs

mapply(function(x) shapiro.normtest.pvalue(normalea(x)),rep(30,20)) > 0.05
mapply(function(x) shapiro.normtest.pvalue(normalea(x)),rep(30,20)) > 0.1

table(mapply(function(x) shapiro.normtest.pvalue(normalea(x)),rep(30,20)) > 0.05)

table(mapply(function(x) shapiro.normtest.pvalue(normalea(x)),rep(30,20)) > 0.05)


##
table(mapply(function(x) shapiro.normtest.pvalue(normalea(x)),rep(30,1000)) > 0.05)
table(mapply(function(x) shapiro.normtest.pvalue(normalea(x)),rep(30,1000)) > 0.05)


#table(mapply(function(x) shapiro.normtest.pvalue(normalea(x)),rep(30,1000)) > 0.01)

table(mapply(function(x) shapiro.normtest.pvalue(normalea(x)),rep(300,20)) > 0.05)

?shapiro.test

## Pearson test
table(mapply(function(x) pearson.normtest.pvalue(normalea(x)),rep(30,20)) > 0.05)
table(mapply(function(x) pearson.normtest.pvalue(normalea(x)),rep(30,1000)) > 0.05)
table(mapply(function(x) pearson.normtest.pvalue(normalea(x)),rep(300,20)) > 0.05)


rep = factor(mapply(function(x) shapiro.normtest.pvalue(normalea(x)), 
                    rep(30,1000) ) > 0.05)
table(rep)

plot(rep)

rep = factor(mapply(function(x) shapiro.normtest.pvalue(normalea(x)), 
                    rep(300,1000) ) > 0.05)
table(rep)

rep = factor(mapply(function(x) pearson.normtest.pvalue(normalea(x)), 
                    rep(30,1000) ) > 0.05)
table(rep)


rep = factor(mapply(function(x) pearson.normtest.pvalue(normalea(x)), 
                    rep(300,1000) ) > 0.05)
table(rep)

# 1.4 Estimation de la puissance d'un test ##############

#' Notion de puissance : capacite d'un test a rejeter Ho quand elle est fausse
#' directement associee au risque de second espece
#' puissance = 1-beta, ou beta est le risque de second espece
#' le risque beta ne depend pas de votre echantillon,
#' il est intrinseque au test choisi
#' 
#' risque alpha : rejeter Ho alors qu'elle est vraie
#' risque beta : accepter Ho alors qu'elle est fausse
#' 
#' #' le risque beta ne depend pas de votre echantillon
#' alors que le risque alpha peut etre ajuste (p=0.05, p=0,01...) 
#' en fonction de votre echantillon.
#' 
#' Parmi les tests appropries pour une question stat, 
#' on cherche toujours a utiliser le plus puissant possible
#' ex: comparaison de deux moyennes, 
#'  test parametrique ou non
#

# 1.4.1 construction des generateurs d'echantillons aleatoires
unifalea <- function(x) runif(x,1-sqrt(3),1+sqrt(3)) 
expoalea <- function(x) rexp(x,1) 

mean(unifalea(1000))
sd(unifalea(1000))

mean(expoalea(1000))
sd(expoalea(1000))

# Ces fonctions sont elles centrees et reduites ?
#' donner un exemple de loi aleatoire uniforme
#' donner un exemple de loi aleatoire exponentielle

# 1.4.2 Realisation du test de puissance 

#' Ho testee: distribution des valeurs selon une loi normale
#' echantillons tires d'une distribution alternative H1,
#'  loi uniforme ou exponentielle

shap.unif.30 <- factor(mapply(function(x) shapiro.normtest.pvalue(unifalea(x)),
                              rep(30,1000) ) > 0.05)
shap.unif.300 <- factor(mapply(function(x) shapiro.normtest.pvalue(unifalea(x)),
                              rep(300,1000) ) > 0.05)
shap.expo.30 <- factor(mapply(function(x) shapiro.normtest.pvalue(expoalea(x)),
                              rep(30,1000) ) > 0.05)
shap.expo.300 <- factor(mapply(function(x) shapiro.normtest.pvalue(expoalea(x)),
                              rep(300,1000) ) > 0.05)


pear.unif.30 <- factor(mapply(function(x) pearson.normtest.pvalue(unifalea(x)),
                              rep(30,1000) ) > 0.05)
pear.unif.300 <- factor(mapply(function(x) pearson.normtest.pvalue(unifalea(x)),
                               rep(300,1000) ) > 0.05)
pear.expo.30 <- factor(mapply(function(x) pearson.normtest.pvalue(expoalea(x)),
                              rep(30,1000) ) > 0.05)
pear.expo.300 <- factor(mapply(function(x) pearson.normtest.pvalue(expoalea(x)),
                              rep(300,1000) ) > 0.05)

table(shap.unif.30)
## 
table(shap.unif.300)
## 
table(shap.expo.30)

table(shap.expo.300)


table(pear.unif.30)
## 
table(pear.unif.300)
## 
table(pear.expo.30)

table(pear.expo.300)

#X11(w=10)
graphics.off()
# quartz()

# marges
parmar <-  par()$mar
par(mar =parmar + c (1,1,1,1))

# graphiques multiples
par(mfrow=c(2,4))

plot(shap.unif.300,) 
plot(shap.unif.300,) 
plot(shap.expo.30,) 
plot(shap.expo.300,) 
plot(pear.unif.30,) 
plot(pear.unif.300,) 
plot(pear.expo.30,) 
plot(pear.expo.300,)


plot(shap.unif.30,) 
mtext('runif, sample=30', cex = 0.75, side=3, line=1)
mtext(text = 'Shapiro', side = 2,line = 3, font = 2)

plot(shap.unif.300) 
mtext('runif, sample=300', cex = 0.75, side=3, line=1)

plot(shap.expo.30,) 
mtext('rexp, sample=30', cex = 0.75, side=3, line=1)

plot(shap.expo.300) 
mtext('rexp, sample=300', cex = 0.75, side=3, line=1)



plot(pear.unif.30) 
mtext('runif, sample=30', cex = 0.75, side=3, line=1)
mtext(text = 'Pearson', side = 2,line = 3, font=2)

plot(pear.unif.300,) 
mtext('runif, sample=300', cex = 0.75, side=3, line=1)

plot(pear.expo.30,) 
mtext('rexp, sample=30', cex = 0.75, side=3, line=1)

plot(pear.expo.300) 
mtext('runif, sample=300', cex = 0.75, side=3, line=1)

# impact de l'effectif ( 30 vs 300) ?


# impact du test (shapiro vs pearson) ?


# impact de la distribution d'origine H1 (uniforme ou exponentielle) ?



