#tp6 pierre-Louis

library(MASS)
#question 1 Test de student

echantillon = bottles$Volume
mu0=500
t.test(echantillon,mu = mu0,alternative = "less",conf.level = 0.95)
#on a une pValue=0.07243 > 1- conf.level=O.05
#donc on ne peut pas rejetter l'hypothese que mu0=500 au niveau a*=0.05

t.test(echantillon,mu = mu0,alternative = "less",conf.level = 0.9)
#on a une pValue=0.07243 > 1-conf.level=O.1 
#donc on rejette l'hypothese que mu0=500 au niveau de confiance a*=10%

#Question 2 Test sur une proportion

effectifTotal=1713
p0=1/6

for (i in 1:ncol(MM)){
  print(prop.test(MM[1,i],effectifTotal,p0,conf.level = 0.95))
}
#dans le cas du rouge on a une Pvalue proche de 1 donc on ne peut pas rejetter l'hypothese p0 au niveau de confiance a*=0.05

#dans le cas du vert, bleu et orange, 
#on a p-value<a*=0.05 et la proportion est inferieure à 1.666
#on conclu qu'ils sont sous représentés

#dans le cas du jaune, et du marron,
#on a p-value<a* et la proportion est inférieure à p0
#on conclu qu'ils sont sur représentés

#Tests d’homogénéité
#Question 3 test de Student apparié

t.test(immer$Y1,immer$Y2,paired=TRUE,conf.level = 0.95)
#on a p-value<a*=0.05
#on rejette donc l'hypothese selon laquelle les rendements sont les memes
#d'une annee sur l'autre

#Question 4 test du signe
negatif= (immer$Y1-immer$Y2)<0
#on cherche à tester la proportion de fois ou la différence des récoltes est négative
p0=0.5
prop.test(length(negatif[negatif==TRUE]),length(negatif),p=p0,conf.level = 0.99)
#on a p-value<a*=0.01
#on rejette l'hypothèse selon laquelle on a autant de récolte dans les deux annees
#au niveau a*=0.01

#Question 5
var.test(shoes$A,shoes$B,conf.level = 0.95)
#p-value est proche de 1 et le ratio des variance est proche de 1
#on peut conclure que les deux echantillons ont les meme variances

#Question 6
t.test(shoes$A,shoes$B,conf.level = 0.95)
#la p value est suffisament proche de 1
#on ne peut pas rejeter l'hypothese selon laquelle
#les espérances sont les meme
#au niveau a*=0.05

#Qestion 7 adéquation à la loi normale
shapiro.test(galaxies)
#la p-value est très proche de 0
#on conclu donc par le rejet de l'hypothese
#selon laquelle la distribution de la vitesse des galaxies
#suit une loi normale

#Question 8 adéquation à une loi 
#un estimateur du parametre lambda est:
lambda=1/mean(delai.data$delai)

ks.test(delai.data$delai,"pexp",lambda)
#par defaut le a*=0.05 sur ce test
#or p-value>a*
#on ne peut pas rejetter l'hypothese selon laquelle
#le delai suis une loi expo de parametre lambda

#Question 9-10 Test du X2
gouts <- data.frame( row.names =c("homme", "femme"),chocolat = c(100, 350), vanille = c(120, 200), fraise = c(60, 90))
xt=chisq.test(gouts)
#la p-value est suffisament faible pour conclure
#que l'on rejette l'hypothese selon laquelle
#les différences entre les gouts des hommes et de femmes est due au hasard

#Question 11-12
ct <- chisq.test(glace)
ct$observed
#c'est la dataframe que l'on a rentré
ct$expected
#c'est les valeurs qu'on aurrait du avoir si les hommes avaient 
#des gouts qui suivent une loi similaire

#Question 13
d2=sum((ct$expected-gouts)**2/ct$expected)
d2-xt$statistic
#on a bien retrouvé la statistique du X2

#Question 14
vitamine= data.frame(c=cold$Cold ,nc=cold$NoCold)

chisq.test(vitamine)
#la pvalue est suffisament faible pour rejetter
#l'hypothese selon laquelle la vitamine n'a pas d'effet

#Etude Du Medicament
#on prend pour hypothese alternative que mu>mu0 
mu0=0
#(on veut que le médicament ait un effet positif sur la durée de sommeil )
for (i in 1:2){
  print(t.test(sleep$extra[sleep$group == i], mu = mu0, alternative = "greater"))
}
#pour le médicament 1
#p-value=0.1>a*=0.05
#donc on ne peut pas dire que le médicament a un effet

#pour le médicament 2
#p-value=0.002<a*=0.05
#la moyenne de l'echantillon est supérieure
#on rejette donc l'hypothese selon laquelle
#le médicament n'a pas d'effet
