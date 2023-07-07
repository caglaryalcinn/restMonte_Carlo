set.seed(357)
n<-10000 
t<- 4
lambda<-4 ##exp parametresi 
u <- matrix(runif(n*t),nrow = t,ncol=n) ### I have n times t uniform number generators, t*n is in the matrix
logU <- -log(u)/lambda ## I log my uniforms and split them into lambdas
#bu exp daðýldý
gamma_y<- apply(logU, 2,sum) ##we collect the columns we get n exp gamma

hist(gamma_y,prob = TRUE ,main="Gamma Dist")
y<- seq(0,2.5,0.01) ##forline
lines(y,dgamma(y,t,lambda),col="red",lwd=2.5)

##theoretical parameters

mean<-t/lambda
var <- t/lambda^2

est.mean <- mean(gamma_y)
est.var<-var(gamma_y)

#####
library(sads)
set.seed(357)
n<-10^5 ##n times try
t<-18 ##depend on sitituation
lambda<-8 ##in this stitituation is 8
u <- matrix(runif(n*t),nrow = t,ncol = n) ###n tries ,t for obtain pareto
logU2<--log(u)/lambda ###uniform trans for exp
erlang <- apply(logU2, 2,sum) ## sumation exp equal erlang
pareto <- (logU2/erlang)+1 ##equal pareto
hist(pareto,probability = TRUE,xlim = c(0,10),
     breaks = 100,main = "PARETO")
y<-seq(0,10,0.01)
lines(y,dpareto(y,scale= 1,shape=t,log=FALSE),col="red",lwd=1.5)
##########


set.seed(357)
n<-10^5
t<-10 
u <- matrix(rnorm(n*t,0,1),nrow = t,ncol = n) 
x<-u^2
chi <- apply(x, 2,sum) 

hist(chi,probability = TRUE,main = "chi")
y = seq(0,35,0.01)

lines(y,dchisq(y,t),col="red",lwd = 2.5)
##########

############## montecarlo

f <- function(x)  3*x^5+4*x^2+8*x ##define func
n<-1000
a<-0  
b<-1 ##define range
x <- runif(n,a,b)
gbar =mean(sapply(x,f))### I looped each x and  took the average

estimate <- (b-a)*gbar ## done
estimate

y1 <- seq(a, b,0.001)
y.low1 <- rep(0,times = length(y1))
plot(y1, f(y1), type = "n", main = "f(x) = 3x^5+4x^2+8x", lwd = 2.5)
lines(y1,f(y.low1), col = 'grey')
lines(y1,f(y1), col = 'grey')
polygon(c(y1, rev(y1)), c(f(y1), rev(f(y.low1))), col = "darkred", border = NA)



 ######

f <- function(x) (1/lambda)*exp(-x/lambda)
lambda<-0.5

n<-1000
a<-0
b<- 2
x<-runif(n,a,b)
g<-mean(sapply(x, f))*(b-a)
estimate <- 1- g
estimate


f <- function(x) (1/lambda)*exp(-x/lambda)
## önce fonksiyonu yaptýk 
 g<- function(x,p){
   if(x>p) return(1)
   else return(0)
 }
### Return 1 if the given critical value is greater than probability, otherwise 0
 n <-1000
 lambda<-0.5 ## soruda verdi
 p<-2 ### given critical probability value

 x<- rexp(n,0.5)
 ##I generated from exp, I will get the pdf it gave and try to generate it from it

 estimate = mean(sapply(x,g,p)) ###apply p to g insert each x into g so that each exp number greater than the critical value I say one

 ### and I'm averaging them, which gives me a ratio, which is probability anyway

 
 
 
 ##########
 set.seed(357)
 n<- 1000
 beta.cdf <- function(x,alpha,beta){
   u<-runif(n,0,x)### 0 dan x e integral teknik olarak 
   g = factorial(alpha+beta-1)/(factorial(alpha-1)*factorial(beta-1))*u^(alpha-1)*(1-u)^(beta-1) ### cdf of beta
   estimate<-mean(x*g) ### u is already written inside the g function, x is b minus a
   #### find cdf
   ####### instead of the other step function, it uses straight integral calculation instead of probability calculation.

   
   
 }
 
x<- seq(0.1,0.9,0.1)
for(i in x){
  est<-beta.cdf(i,3,3)
  cat(est)
}
######## amazing
####################





f <- function(x) (exp(-2*x)/(1+x^2)) + (4*x+2)^2

n<-10000
a<-0
b<-3
u<-runif(n,a,b)
var.est<-var(sapply(u,f))*(b-a)^2
var.est
###
f <- function(x) (exp(-2*x)/(1+x^2)) + (4*x+2)^2
###  defined fonksiyon
n<-10000
a<-0
b<-3
x <- runif(n/2, a, b)
z1<-f(x)
z2<-f(b-x)
antithetic<- (z1+z2)/2 ###Anthitetic 
theta1<-mean(sapply(x,f))*(b-a)
theta2 <- mean(sapply(b-x,f))*(b-a)
ant.estimate <- (theta1+theta2)/2
## ant.estimate <- mean(anthitetic)*(b-a)
ant.estimate
var.anthitetic.est<-var(antithetic)
var.anthitetic.est

var.r<-100*((var.est-var.anthitetic.est)/var.est)
var.r

######bias 

set.seed(357)
bias.mse<- matrix(0,ncol = 2,nrow = 2,dimnames = list(c("bias","mse"),c("alpha","beta")))
### 2* 2  matrix for save values
MSE <- function(n,alpha,beta){
M <- 10^5
alpha.hat<- c()
beta.hat<-c()

for(i in 1:M){
   x <- rgamma(n,alpha,beta) ####
   beta.hat[i]<-mean(x)/var(x)
   alpha.hat[i]<- mean(x)^2/var(x)
   
}
bias.mse[1,1] <- mean(alpha.hat)-alpha
bias.mse[1.2] <- mean(beta.hat)-beta

bias.mse[2,1] <- var(alpha.hat)-(mean(alpha.hat)-alpha)^2
bias.mse[2,2] <- var(beta.hat)-(mean(beta.hat)-beta)^2
print(bias.mse)
}

MSE(100,4,7)
#### cute code
CL<-function(n,M,alpha){
   mu<-240
   sigma<-25
   matrix <- matrix(0, nrow = M, ncol = 2, dimnames = list(c(1:M), c("Lower","Upper")))
   for(i in 1:M){
      x <- rnorm(n,mu,sigma)
      x.bar <- mean(x)
      x.sd <- sd(x)
      matrix[i,1] <-x.bar-qnorm(1-alpha/2)*x.sd/sqrt(n)
      matrix[i,2] <-x.bar-qnorm(1-alpha/2)*x.sd/sqrt(n)
      
      
   }
   out <- list(CI = apply(matrix,2,mean),conf.level = 1-alpha)
   print(out)
   
}
CL(100,10000,0.05)


#### it was said that so many people had a mean, their standard deviation was also given
### determines my sanple size n, I create sample size M times
##### I also put alpha in the function n m alpha
#### I created a matrix to keep lower and upper separately, not necessary
### I create M samples inside the for loop n size
###calculate the mean and sd of each sample and replace it in the formula
### the qnorm(1-alpha/2 ) in the formula is important
####results I have m confidence intervals I take the average of them with the apply function







#########
CL <- function(n,M,alpha){
   mu <- mean(iris$Sepal.Width)
   sigma <- var(iris$Sepal.Width)
   matrix <- matrix(0,nrow = M,ncol = 2,dimnames = list(c(1:M),c("u","l")))
   for(i in 1:M){
      matrix[i,1] <- (n-1) * var(x) / qchisq(1 - alpha/2, df=n-1) #upper confidence limit
      matrix[i,2] <- (n-1) * var(x) / qchisq(alpha/2, df=n-1) #lower confidence limit
      
   }
   out <- list(CI = apply(matrix,2,mean), conf_level = 1-alpha) #mean of rows
   return(out)
   
}
CL(n= length(iris), M=10^5, alpha=0.05)



#########
 cv <- function(n,xbar,Mu0,sigma,M,alpha){
    S.error <- (sigma/sqrt(n))
    test.stat <- (xbar - Mu0)/ S.error
    test<-c() ## empty vec
    for ( i in 1 :M){
       x<- rnorm(n,Mu0,sigma) ##I generated random number from r norm in given values

       test[i]<-(mean(x)-Mu0)/S.error
       #### I calculated m test stats for each x

       
    }
    critic<- quantile(test,1-alpha) ### rejection region 
    
    if(abs(test.stat)>abs(critic)){
       cat("From MC Simulation we get an estimated Critical Value of",
           round(critic,3),"\n", "Test Statistic is",
           round(test.stat,3),"\n","\n",
           "Therefore, we have enough evidence to reject the null hypothesis",
           "\n","\n")
       
    }
    else{
       cat("From MC Simulation we get an estimated Critical Value of",
           round(critic,3),"\n", "Test Statistic is",
           round(test.stat,3),"\n",
           "Therefore, we do not have enough evidence to reject the null hypothesis",
           "\n","\n")
    }
#### I reject depending on whether my test stat is greater or less than the critical value 
#### I defined test stat at first, I have nothing else to do with it, then I calculate m retests from n normal values, take its quantile and compare the two.
    
    
    
    
    
   
 }
cv(n=50, xbar=1.7, Mu0=3.057, sigma=sqrt(0.19), M=10^5, alpha=0.05)

 
################ pvalue
   
pvalue <- function(n,xbar,Mu0,sigma,M,alpha){
   S.error <- (sigma/sqrt(n))
   test.stat <- (xbar -Mu0)/S.error
   TestScores<-c()
   for (i in 1:M) {
      x <- rnorm(n,Mu0,sigma)
      TestScores[i]<-(mean(x)-Mu0)/S.error
      
   }
   p <- length(which(TestScores >= test.stat))/M
   if(p< alpha){
      cat("Since P-value",p,"which is less than the significance level",
          alpha, "\n",
          "Therefore, we can reject the null hypothesis.","\n","\n")
   }
   else{
      cat("Since P-value",p,
          "which is greater than the significance level",
          alpha, "\n",
          "Therefore, we can not reject the null hypothesis.",
          "\n","\n")
      
   }
   
}   
pvalue(n=30, xbar=112.5, Mu0=100, sigma=15, M=10^5, alpha=0.05)
##### same logic p <- length(which(TestScores >= test.stat))/M bunu diyoruz.

help("attach")




####exercise
set.seed(357)
n<-100

tmean<-c()

for (i in 1:9) {
   for(j in 1:n){
      x <- rcauchy(20,0,1)
      tmean [ j ] <- sum( x[2:(20-i)]) / (n -2i)
      
   }
   bias<-mean(tmean)-mean(x)
   mse<-var(tmean)+bias^2
   cat(i,"level trimmed mean mse is ",mse,"\n")
}


######


#########
n <- 20
m <- 1000
mu0 <- 500
sigma <- 100
mu <- c(seq(450, 650, 10)) #xbar yok o üzden biz de x bar yaptýk
M <- length(mu)
power <- numeric(M)
for (i in 1:M) {
   mu1 <- mu[i]
   pvalues <- replicate(m, expr = {
      x <- rnorm(n, mean = mu1, sd = sigma)
      ttest <- t.test(x,
                      alternative = "greater", mu = mu0)
      ttest$p.value } )
   ####### bu replicat þu demek n kadar bir ifadeyi iþlemi yap ve bir dizi olarak sakla bir deðeri
   
   power[i] <- mean(pvalues <= .05)
   ######  t testteki p valuelarýn 0.5 den küçük olanlarýnýn meani powerý veriyormuþ.
}
library(Hmisc) #for errbar
plot(mu, power)
abline(v = mu0, lty = 1)
abline(h = .05, lty = 1)
#add standard errors
se <- sqrt(power * (1-power) / m)
errbar(mu, power, yplus = power+se, yminus = power-se,
       xlab = bquote(theta))
lines(mu, power, lty=3)
detach(package:Hmisc)
help(replicate)
###conf 
n<-20
alpha<-0.05
cl<-replicate(n,expr = {
   x <- rnorm(n,mean=2,sd=2) ##soruda verilir
   (n-1)*var(x)/qchisq(alpha,n-1)   #### replicate ile yaptýðýmýz þey n kere exp hesaplayýp clin içine depolamak
   
})
sum(cl>4)
mean(cl>4)
#### yüzde 90 4 eþit varyans


##### bootsrap

M<-10000
med_BT =c()
n <- nrow(faithful)
for (i in 1:M) {
   
   c<-faithful$eruptions[-i]
   med_BT[i]<-median(c)
   
}
hist(med_BT)
## oluþturduðumuz vektörün medyanýný alýyor boþ vektörümðze atýyoruz sonra daðýlým dönüþtürmesi yapýyoruz

upper <- median(faithful$eruptions) + qnorm(0.95)*sd(med_BT)
lower <- median(faithful$eruptions) - qnorm(0.95)*sd(med_BT)
cat("90% confidence interval for eruptions is= (", lower, ",", upper, ")")
### bu hep ayný a1 niþi eksi artý qnorm095 çarpý gerçek

set.seed(357)
c<-cor(faithful$eruptions,faithful$waiting)
n <- nrow(faithful)
M<-10000
for(i in 1:M){
   w<-sample(n,n,replace = TRUE)
   BOOST<-faithful[w,]
   BOOST.c[i]<-cor(BOOST$eruptions,BOOST$waiting)
}
est.cor<-mean(BOOST.c)
hist(BOOST.c)

upper<-est.cor+qnorm(0.95)*c
upper<-est.cor-qnorm(0.95)*c
upper
###########
M<-10000
n<-nrow(iris)##  bunu unutma
sd<-sd(iris$Sepal.Length)
est.sd<-c()
for (i in 1:M) {
   w<-sample(n,n,replace = TRUE)
   x<-iris$Sepal.Length[w]
   est.sd[i]<-sd(x)
   
}
esti.sd<-mean(est.sd)

bias <- esti.sd-sd
mse<-var(est.sd)+bias^2

hist(est.sd)
upper <- esti.sd+(qnorm(0.90)*sd)
lower<- esti.sd-(qnorm(0.90)*sd)


#####


library(e1071)


M<<-1000
n<-nrow(iris)
ske <- c()
skew<-skewness(iris$Sepal.Width)
for (i in 1:M) {
   w <- sample(n,n,replace= TRUE)
   e <- iris$Sepal.Width[w]
   ske[i]<-skewness(e)
   
   
}
est.ske<-mean(ske)
bias.ske<-est.ske -skew
mse<-var(ske)+bias^2
hist(ske)
upper <- est.ske + qnorm(0.95)*skew
lower <- est.ske - qnorm(0.95)*skew


#####
M<-1000
est.co<-c()
n<-nrow(mtcars)
co<-cor(mtcars$disp,mtcars$hp)
for (i in 1:M) {
   w<-sample(n,n,replace = TRUE)
   e<-mtcars[w,]
   est.co[i]<-cor(e$disp,e$hp)
   
}
est.corr<-mean(est.co)
est.corr
co
hist(est.co)
######## model selection
data <- airquality[,c("Ozone","Solar.R","Wind", "Temp")]
data <- as.data.frame(scale(data))
data <- na.omit(data)
head(data)
attach(data)
models <- list(as.formula(Ozone ~ Solar.R + Wind + Temp),
               as.formula(Ozone ~ Solar.R*Wind*Temp),
               as.formula(Ozone ~ Solar.R + Wind + Temp + Wind*Temp),
               as.formula(Ozone ~ Wind + Temp + poly(Solar.R,2)),
               as.formula(Ozone ~ Wind + poly(Solar.R,2)+ poly(Temp,3)))

errors <- matrix(0, ncol = length(models), nrow = nrow(data)) #
for (i in 1:nrow(data)) {
   for (j in 1:length(models)) {
      model <-lm(models[j],data=data[-i,])
      errors[i,j]<-predict(model,data[i,])-data$Ozone[i]
      
      
   }

}
detach(data)
comparison<-apply(errors^2,2,mean)
comparison

help(matrix)
##########


sturges <- function(x){
   n <- length(x) #sample size
   #uses Sturges' formula, implicitly basing bin sizes on the range of the data;
   #Compute the Number of Classes for a Histogram
   nclass <- ceiling(1 + log2(n)) #denominator part of the formula
   #diff: used to find the difference between each consecutive pair of elements of a vector; bin width
   cwidth <- diff(range(x) / nclass)
   #then, calculate the optimal number of breaks (bin)
   breaks <- min(x) + cwidth * 0:nclass
   return(list(nclass = nclass, cwidth = cwidth, breaks = breaks))


}
set.seed(1234)
x <- rnorm(25, 2, 10)
h.sturges <- hist(x, breaks = sturges(x)$breaks, prob = TRUE, main = "Sturges Rule n=25")
########

scotts <- function(x){
   n <- length(x) #sample size
   h <- 3.5 * sd(x) * n^(-1/3) #bin width according to scotts rule
   nclass <- ceiling(diff(range(x)) / h) #number of classes for histogram
   breaks <- min(x) + h * 0:nclass #breaks for his...............................................................................................togram
   return(list(nclass = nclass, h = h, breaks = breaks))
}
x <- iris$Sepal.Width
h.scott <- hist(x, breaks = scotts(x)$breaks, freq = FALSE, main = "Scotts")

########

FD <- function(x){
   n <- length(x) #sample size
   h <- 2 * IQR(x) * n^(-1/3) # bin width
   nclass <- ceiling(diff(range(x)) / h) # number of classes in histogram
   breaks <- min(x) + h * 0:nclass #breaks of histogram
   return(list(nclass = nclass, h = h, breaks = breaks))
}
#########
