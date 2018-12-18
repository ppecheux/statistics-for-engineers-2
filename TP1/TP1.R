#2 Premiers pas

#Op?rations
6*7
2**3
5*6/2
#foctions
sqrt(4)
sin(pi/15)**2+cos(pi/15)**2
log2(8)
log(exp(0))
identical(1,exp(0))#revoie true or false

#3 Structures de donn?es usuelles

#pour les qualitatives les vecteurs
c(1,2,3,4,5)-c(1:5)+c(1,c(2,3),4,5)#Vecteurs equivalents

v <-c(1, 2, 3, 4, 5)
u <-c(5, 4, 3, 2, 1)
u==2#donne un vecteur de bool avec true quand u[i]==2
u*v# donne 5 8 9 8 5

#pour les sous vecteurs
v[2:4]==v[c(2,3,4)] #r?cup?re un sous vecteur

mean(v)
sup3v <- v[v>=3] #on prends sous vecteur avec les valeurs du vecteur masque bool.
length(sup3v)
concate <- c(u,v)
concate[concate>4] <- 0.5 #change la valeur de certaines valeurs
any(concate!=floor(concate))
floor(concate!=floor(concate))
floor(concate[concate!=floor(concate)])
length(concate[concate!=floor(concate)])
min(concate)
any(concate<1)

#pour les qualitatives Facteurs

f = factor(c("R","V","B","R"))
f = ordered(f)# donne une relation d'ordre alphabetique
f <- factor(c("R","V","B","R","B"), ordered =TRUE  , levels =c  ("R", "V", "B")) #pour donner un autre ordre
f
f < "R"
length(f[f=="R"])

#Les dataframe
data.frame(v,f,v>3)

#pour lier des fichiers - session, working directory
#setwd("~/Documents/Cours/Branche/GI01/sy02/tps/data")

X <- read.csv("sy02.data")



length(X)-ncol(X)#equivalent
names(X)#pour les entetes
nrow(X)
head(X)
summary(X)
mean(X[X$correcteur.median=="EG","median"])
nrow(X[X$median<X$final,])/nrow(X)

#4 Statistiques descriptives

nf = X[,"final"]
var(X[,"final"])
sd(X[,"final"])
min(X[,"final"])
max(X[,"final"])
summary((X[,"final"]))

-quantile(nf,0.25)+quantile(nf,0.75)
IQR(nf)#?quart interquartile

sortednf=sort(nf)
mean(sortednf[11:length(nf)-10]) #moyenne tronqu?e ? l'ordre 10

#5 Analyse univari?e

table(f) #associe le nombre d'occurence avec chaque variable
barplot(table(f))
table(X$correcteur.median) #table qui compte le nombre d'occurence du correcteur dans la colonne
barplot(table(X$correcteur.median))

boxplot(X$final)
#pour trouver le nombre de valeurs ab?rantes:
bound = quantile(nf,0.25)-IQR(nf)*1.5
length(nf[nf<bound])

stem(X$final)

hist(X$moyenne, breaks = c(0,15,20))

#pour retrouver la densit? de l'histogramme:
length(X$moyenne[X$moyenne<15])/length(X$moyenne)/(15-0)

#6 Analyse bivari?e

plot(X$median,X$final)
plot(final~median,data=X)

boxplot(final~correcteur.final,data=X) #mettre en relation deux grandeurs

stripchart(final~correcteur.final,data=X, method = "jitter")
