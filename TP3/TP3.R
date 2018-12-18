runifa <- function(n) {
  if(!exists("param")) param <<- sample(10:20, 1)
  runif(n, min = 0, max = param)
}

estim<-function(x){#x etant un vecteur
  2*mean(x)
}

n=100


k=c(1:5)
a = replicate(n,estim(runifa(n)))
boxplot(a)

estimk<-function(x,k){
  (mean(x**k)*(k+1))**(1/k)
}

a = replicate(n,estimk(runifa(n),1))
boxplot(a)

runknown <- function(n) {
  bn <- rbinom(n, 1, 0.2)
  bn * rnorm(n, mean=-4, sd=1) + (1 - bn) * rnorm(n, mean=10, sd=1)
}

xn=runknown(n)
mean(xn)
sd(xn)**2

hist(xn)
plot(ecdf(xn))#pour tracer la fonction de répatition
#empirique de xn

#pour une seule valeur de ecdf il faut (exemple en x=2):
ecdf(xn)(2)

nu = 7.2
sigma=sqrt(32.36)
(mean(xn)-nu)/(sigma/sqrt(n))

random.T <- function(n) {
  xn=runknown(n)
  nu = 7.2
  sigma=sqrt(32.36)
  c= (mean(xn)-nu)/(sigma/sqrt(n))# Calculer une réalisation de la loi T
  return(c)
}

t.1000= replicate(1000,random.T(10))
mean(t.1000)
var(t.1000)

plot(ecdf(t.1000))
curve(pnorm,add=TRUE)

lambda=3
f <- function(lambda, x) {
  #f=lambda*exp(-lambda*x)
  #f[0>x]=0
  f=dexp(x,rate = lambda)
  return(f)
}

L<-function(lambda,x){
  prod(f(lambda,x))
}


l<-function(lambda,x){
  sum(log(f(lambda,x)))
}

x= rexp(n,rate=lambda)
l(3.1,x)#ne correspond pas au corrigé
l(2.8,x)

lambdas <- seq(0, 6, 0.01)
logL.lambdas <- sapply(lambdas, function(lambda) l(lambda, x))
plot(lambdas, logL.lambdas, type = "l")

x <- rexp(n, rate = 3)
opt <- optimize(l, lower = 0, upper = 6, maximum = TRUE, x = x)
opt$maximum

sim.EMV <- function() {
  x <- rexp(n, 3)
  # Maximization de la log-vraisemblance
  opt <- optimize(l, lower = 0, upper = 10, maximum = TRUE, x = x)
  opt$maximum
}

sim.EMV()
sim.EMV.100 <- replicate(100, sim.EMV())
boxplot(sim.EMV.100)

mean(sim.EMV.10000) - 3
n/(n - 1) * 3 - 3

install.packages("pracma")
library(pracma)

sim.Fisher <- function() {
  x <- rexp(n, rate = 3)
  # Log-vraisemblance par rapport à x
  logLx <- function(lambda) l(lambda, x)
  # Information de Fisher
  (grad(logLx, 3))^2
}

(inf.Fisher <- mean(replicate(10000, sim.Fisher())))

n/3^2
(1/inf.Fisher)
var(sim.EMV.10000)
n <- 100
grad2 <- function(f, x) {
  df <- function(x) {
    grad(f, x)
  }
  grad(df, x)
}
sim.Fisher <- function() {
  x <- rexp(n, 3)
  # Log-vraisemblance par rapport à x
  logLx <- function(lambda) l(lambda, x)
  # Information de Fisher
  grad2(logLx, 3)
}
(  -mean(replicate(1000, sim.Fisher())))
