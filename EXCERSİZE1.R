
## inverse transformation applied Let X∼N(0,1)
#and the probability density function of |X| is;
#f(x)=22π−−√e−x22,x>0

rejection.sampling <- function(f, c, g, n) {
  set.seed(2022)
  k <- 0 
  i <- 0 
  s <- c() 
   while (k < n) {
    x <- rexp(1, rate = 1)
    u <- runif(1)
    i <- i+1
    
    if (u <= f(x) / (c*g(x))) {
      k <- k + 1
      s[k] = x 
    }
    
  }
  print(summary(s))
  return(s)
  
}
f <- function(x) 2/(sqrt(2*pi))*exp(-(x^2)/2)
g <- function(x) sqrt((2*exp(1))/pi)*exp(-x)
c <- 1 # an upper bound for f(x)/g(x)
result <- rejection.sampling(f, c, g, n=1000)
hist(result, prob = T)
a<- seq(0,5,0.01)
points(a, 2/(sqrt(2*pi))*exp(-(a^2)/2) ,type = "l", col="red", lwd = 2.5)
plot(x, u, pch=".", col="maroon", xlim = c(0,5))
points(s, u[u <= (2/(sqrt(2*pi))*exp(-(x^2)/2)) / (sqrt((2*exp(1))/pi)*exp(-x))], pch=".", col="green3")
##obtain cauchy distribition using inverse gamma 
library(invgamma)
set.seed(361)

n <- 10^4
mu <- 5
s <- 3

y <- rnorm(n, mean = 0, sd = 1)
z <- rinvgamma(n, shape = 1/2 , rate = (s^2)/2)
cauchy <- mu + y*sqrt(z)
hist(cauchy, prob=TRUE, xlim = c(- 100, 100),
     breaks = 100000,
     main = "Cauchy(5, 3)")
y = seq(-100,100,0.01)
lines(y,dcauchy(y,location = mu, scale = s),col="red",lwd = 2.5)

## obtain laplace distribition using double exponential dist and inverse transformation by uniform  dist.
double.exponentail <- function(n) {
  set.seed(361)
  x <- numeric(n)
  theta <- 2
  u <- runif(n)
  if(u <= (1/2)){
    x <- theta + log(2*u)
    
  } else if (u > (1/2)) {
    x <- theta - log(2-(2*u))
  }
  hist(x, prob = TRUE, main = 'Double Exponential Distribution', breaks=20, col="darkolivegreen1")
  y = seq(-10,10,0.001)
  lines(y, (1/2)*exp(-abs(y-theta)), col="red",lwd = 2.5)
  print(summary(x))
  return(x)
}

result<- double.exponentail(10^4)
library(ExtDist)
y = seq(-100,100,0.01)
hist(result, prob=TRUE,
     breaks = 20,
     main = "Laplace(double exponential with)")
lines(y, dLaplace(y, mu=2, b=1 ), col="red",lwd = 2.5)


##f(x) is probability density function and non-zero on [-1, 1] and f(x)/g(x)≤c
##Simulated random sample from f(x) by taking y uniformly on [0, c]. obtain use rejection sampling by taking sample size n=104
.
set.seed(2022)
n = 10^6
i <- 0 
k <- 0 
s <- numeric(n) 
while (k < n) {
  u = runif(1, 0, 2/pi)
  i <- i+1
  x = runif(1, -1,1)
  if (u <= (2/pi)*sqrt(1-x^2)) {
    k <- k + 1
    s[k] <- x
  }
}
i;k;length(s)
hist(s, prob=T, breaks = 50, main="f(x)=(2/pi)*sqrt(1-x^2)", col="pink") 
a <- seq(-1,1,0.1) 
lines(a,(2/pi)*sqrt(1-a^2), col="red", lwd=2.5)
## obtain geometric dist using inverse transformation method
n <- 10000
p <- 0.05
u<- runif(n) #generate uniform random variables
x<- log(1-u)/log(1-p) #apply inverse transformation method for geometric distribution
y<- x[x >= 6] 
hist(y)
geom_sample <- rgeom(n= 10000, p=0.05) 
geom_sample_bigger_than_six <- geom_sample[geom_sample >= 6] 

