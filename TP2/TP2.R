#TP 2 - SY02 Probabilités Corrigé

#1 Étude de peintres

library(MASS)
head(painters)


hist(painters$Composition, main = "Composition", xlab = "Note")
hist(painters$Drawing, main = "Dessin", xlab = "Note")
hist(painters$Colour, main = "Couleur", xlab = "Note")
hist(painters$Expression, main = "Expression", xlab = "Note")

moyennes <- (painters[,2]+painters[,3]+painters[,4]+painters[,1])/4
#étude du vecteur moyennes
n<-length(moyennes)

x_<-sum(moyennes)/n

s2<-sum((moyennes-x_)**2)/n
s2-var(moyennes)*(n-1)/n

s<-sqrt(s2)
s-sd(moyennes)*sqrt((n-1)/n)

s2_<-s2*n/(n-1)
s2_-var(moyennes)

s_<-sqrt(s2_)
s_-sd(moyennes)

hist(moyennes)

#2 Calcul de probabilités
1-pnorm(3)
pnorm((42-35)/6)
pnorm((50-35)/6)-pnorm((40-35)/6)
n=c(5,10,30)
dbinom(n-1,n,0.5)
1-pbinom(14,20,1/2)
pbinom(15,20,1/2)-pbinom(9,20,1/2)#attention c'est une loi discrete donc on doit prendre en compte le <=

a=c(0.05,0.1,0.9)

qnorm(a)
qchisq(a,10)
qt(a,5)
qf(a,2,5)

#3 Implémentation d'une loi de probabilité
dloi<-function(x,b){
  f<-(2/b**2)*x
  f[x<0]<-0
  f[x>b]<-0
  return(f)
}

dloi(-1:5,3)

curve(dloi(x, 3), from = -5, to = 5)

ploi <- function(x, b) {
  F <- x^2/b^2
  F[x < 0] <- 0
  F[x >= b] <- 1
  return(F)
}

curve(ploi(x,3),from = -5, to =5)

qloi <- function(a, b) {
  x<-sqrt(a)*b
  x[a<=0]<-0
  x[a >= 1] <- b
  return(x)
}

curve(qloi(x, 3), from = 0, to = 1)

#pour générer des nombres à partir d'un loi 
rloi<-function(n,b){
  u<-runif(n)
  x<-qloi(u,b)
}

par(mfrow = c(2, 2))
for (n in c(10, 50, 100, 1000)) {
  hist(rloi(n, 3), breaks = round(1 + 10/3 * log10(n)), freq = FALSE, main = n, xlim = c(-1, 4))
  curve(dloi(x, 3), add = TRUE, col = "red")
}
