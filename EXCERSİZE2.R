### create coin throw simulation using uniform random number generator
n = 10^3;
U = runif(n, min = 0, max = 1);
toss = U < 0:7;
a = numeric(n + 1);
avg = numeric(n);

for(i in 2 : n + 1){
  a[i] = a[i - 1] + toss[i - 1];
  avg[i - 1] = a[i]/(i - 1);
}


plot(1 : n, avg[1 : n], type = "l", lwd = 5, col = "blue", ylab = "ProportionofHeads",
     xlab = "CoinT ossNumber", cex.main = 1.25, cex.lab = 1.5, cex.axis = 1.75)
## generating gamma distribitons using transformations esponential to gamma
set.seed(2022)  
n = 10^4      
i <- 0  
k <- 0 
y <- numeric(n) 
c <- 5/3
while (k < n) {
  u = runif(1)
  i <- i+1
  x = 5 - 2*log(runif(1))
  if (u <= (x/5) * exp(-(x-5)/2 * c)) {
    k <- k + 1
    y[k] <- x
  }
}
i;k;length(y)
hist(y, prob = TRUE, main ="f(x)=(a*exp(-a))/(6*exp(-5))")
a = seq(min(y), max(y), 0.01)
lines(a, (a*exp(-a))/(6*exp(-5)), col="red",lwd = 2.5)


###used classic mote carlo integration calculate approximate prabablity under cauchy distribiton
set.seed(361)
f <- function(x) function(x, s, mu){ 1/pi*s*(1+((x-mu)/s)^2)}
g1 <- function(x, p1){
  if (x < p1) return(1)
  else return(0)
}
g2 <- function(x, p2){
  if (x > p2) return(1)
  else return(0)
}
n <-10^5
mu <- -2
s <- 4
p1 <- 0.9
p2 <- 3
x <- rcauchy(n = n, scale= s, location  = mu)
probability1 = mean(sapply(x ,g1, p1))
probability2 = mean(sapply(x ,g2, p2))
estimate_classical = probability1 + probability2
cat(estimate_classical,"\n")
cat("probability is:",pcauchy(p1, location = mu, scale = s) + (1- pcauchy(p2, location = mu, scale = s)),"\n")

var_prob1 <- (mean(sapply(x ,g1, p1)^2) - (mean(sapply(x ,g1, p1)))^2) # Var(x) = E(X^2) - E(x)^2
var_prob2 <- (mean(sapply(x ,g2, p2)^2) - (mean(sapply(x ,g2, p2)))^2) # Var(x) = E(X^2) - E(x)^2
var_cauchy <- var_prob1 + var_prob2 + 2*cov(sapply(x ,g1, p1), sapply(x ,g1, p1))
cat("Variance of classical monte carlo is:" , var_cauchy)

#### crated a funciton automatic controol variates method desicion on classic monte carlo integration
a<- function(f,n,low,up,graph=F,var.reduc=F,type, f.cont=1){
  set.seed(1234)
  start1 <- proc.time()
  x<-runif(n,low,up)
  Estimate<-mean(sapply(x,f))*(up-low)
  Theoritical<-integrate(f,low,up)$value
  Error<-abs(Estimate-Theoritical)
  var.estimate <-mean(sapply(x,f)^2)*(up-low) - (mean(sapply(x,f))*(up-low))^2
  run.time <- proc.time() - start1
  outcome<-round(rbind(Estimate,Theoritical,Error),5)
  if(graph){
    y <- seq(low,up,0.001)
    y.low <- rep(0,times = length(y))
    plot(y, fun(y), type = "n",
         main = "f(x)",
         lwd = 2.5)
    lines(y,fun(y.low))
    lines(y,fun(y))
    polygon(c(y, rev(y)), c(fun(y), rev(fun(y.low))), border = NA)}
  if(var.reduc){
    if(type=="Antithetic") {
      start2 <- proc.time()
      x2 <- runif(n/2,low,up)
      theta1.1 <- mean(sapply(x2,f)) * (up-low)
      theta1.2 <- mean(sapply(up-x2,f)) * (up-low)
      exp.ant.estimate1 <- (theta1.1 + theta1.2) / 2
      Error.ant<-abs(exp.ant.estimate1-Theoritical)
      z1.1 <- f(x2)
      z1.2 <- f(up-x2)
      var.ant.estimate1 <- (var(z1.1)+var(z1.2)+2*cov(z1.1,z1.2)) / 4
      run.time.ant <- proc.time() - start2  
      ant.outcome<-matrix(c(Estimate,exp.ant.estimate1,
                            var.estimate, var.ant.estimate1),nrow=2,dimnames = list(c("classical approach","Antithetic"),c("estimated mean","variance")))
      print(ant.outcome)
      print(matrix(c(Error.ant),nrow=1,dimnames = list(c("Error"))))
      print(run.time)
      print(run.time.ant)
      
    }
    else if (type=="Control") {
      start3 <- proc.time()
      g.cont<-f
      f.cont <-f.cont
      correlation<-cor(g.cont(x),f.cont(x))
      if (correlation<0.5) {print("correlation < 0.5")}
      else {print(cat("Correlation between g and f",
                      round(correlation,4),""))}
      c_star <- -cov(g.cont(x),f.cont(x)) / var(f.cont(x))
      expected.fu <- integrate(f.cont,low,up)$value
      exp.cont.estimate1 <- mean(g.cont(x) +c_star * (f.cont(x) - expected.fu)) * (up-low)
      Error.cont<-abs(exp.cont.estimate1-Theoritical)
      var.cont.estimate1 <- var(g.cont(x) + c_star * (f.cont(x) - expected.fu))
      var.cont.estimate1
      cont.outcome <- matrix(c(Estimate,exp.cont.estimate1,
                               var.estimate, var.cont.estimate1),nrow=2,dimnames = list(c("classical approach","control variates"),c("estimated mean","variance")))
      print(cont.outcome)
      print(matrix(c(Error.cont),nrow=1,dimnames = list(c("Error"))))
      run.time.cont <- proc.time() - start3
      print(run.time)
      print(run.time.cont)
      
      
    }}
  
  
  else {return(outcome)}
  
}

fun<-function(x)  exp(x) / (1+x^2) 
fun1<- function(x) 1/1+x
a(fun,10^5,0,1,var.reduc = F, graph = T)




