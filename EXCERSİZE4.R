data <- read.csv("UKDriverDeaths.csv")
head(data, 10)
### this data distibitude poisson 
#obtained mle with monte carlo
n <- nrow(data)
lambda.hat <- sum(data$x) / n
B <- 1000
lambda.boot <- numeric(B)
for(i in 1:B){
  index <- sample(n, size=n, replace = T)
  lambda.boot[i] <- sum(data$x[index]) / n 
}
estimate.lambda <- mean(lambda.boot)
SE <- sd(lambda.boot)
bias <- estimate.lambda - lambda.hat
out <- c(lambda.hat, estimate.lambda, SE, bias)
names(out) <- c("lambda.hat","estimated lambda", "Std.Err", "Bias")
out
## obtain sample kurtosis using boostrap
kurtosis_fnc <- function(x,i){
  n <- length(x)
  sample_krts <- mean((x[i] - mean(x[i]))^4) / var(x[i])^2
  return(sample_krts)
  
}
n <- nrow(data)
B <- 1000
kurtosis <- numeric(B)
sample_kurt <- kurtosis_fnc(data$x)
for(i in 1:B){
  index <- sample(n, size=n, replace = T)
  x <- data[index,"x"]
  kurtosis[i] <- kurtosis_fnc(x)
}
est_kurt <- mean(kurtosis)
se_kurt <- sd(kurtosis)
bias_kurt <- (est_kurt - sample_kurt)
hist(kurtosis, col="pink") # the distribution from the bootstrap
abline(v=sample_kurt, col="blue",lwd=4)
result <- c(sample_kurt, est_kurt, se_kurt, bias_kurt)
names(result) <- c("sample", "estimated", "std.error","bias")
result
### obtained skewwness using jacknife
skew <- function(x,i){
  n <- length(x)
  sample_skw <- mean((x[i] - mean(x[i]))^3) / mean((x[i] - mean(x[i]))^2)^(3/2)
  return(sample_skw)
  
}
n <- nrow(data)
Skew <-c()
sample_skew <- skew(data$x)
for(i in 1:n){
  Skew[i] <- skew(data$x[-i])
}
est_skew <- mean(Skew)
se_skew <- sqrt((n-1) * mean((sample_skew - mean(Skew))^2))
bias_skew <- (est_skew - sample_skew)*(n-1)
hist(Skew, col="orange") # the distribution from the bootstrap
abline(v=sample_skew, col="blue",lwd=4)
result <- c(sample_skew, est_skew, se_skew, bias_skew)
names(result) <- c("sample", "estimated", "std.error","bias")
result
## obtained ½95 confidence interval with boostrap
alpha <- 0.05
B <- 1000
sample_var <- var(data$x)
n <- nrow(data)
S1 <- S2 <- numeric(B)
for(i in 1:B){
  index1 <- index2 <- sample(1:n, size = n, replace = TRUE)
  S1[i] <- var(data[index1,"x"])
  S2[i] <- var(data[index2,"x"])
}
lower_CI <- (n-1) * mean(S1) / qchisq(1-alpha/2, df=n-1)
upper_CI <- (n-1) * mean(S1) / qchisq(alpha/2, df=n-1)
lower_PI <- quantile(S2, alpha/2)
upper_PI <- quantile(S2, 1-alpha/2)
result <- matrix(c(lower_CI,lower_PI,upper_CI,upper_PI), ncol = 2, dimnames = list(c("Conf.Int","Perc.Int"), c("Lower","Upper")))
result




