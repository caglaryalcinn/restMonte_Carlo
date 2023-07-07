### construct hypotehesis test using monte carlo 
##H0:μ=8, H1:μ≠8
v <- function(n,xbar, Mu0, sigma, M, alpha){
  
  S.Error <- (sigma / sqrt(n))
  test.stat <- (xbar - Mu0) / S.Error
  
  TestScores <- numeric(M)
  
  for(i in 1:M){
    x <- rnorm(n, Mu0, sigma) # generate random sample under H0
    TestScores[i] <- (mean(x) - Mu0) / S.Error
  }
 
  Critical.Value_lower <- quantile(TestScores, alpha/2)
  Critical.Value_upper <- quantile(TestScores,1-(alpha/2))
  
  if(abs(test.stat) > abs(Critical.Value_lower) & abs(test.stat) < abs(Critical.Value_upper)){
    cat("From MC Simulation we get an estimated Critical Value of", 
        round(Critical.Value_upper,3),"and", round(Critical.Value_lower,3)  ,"\n", "Since Test Statistic is", 
        round(test.stat,3),"\n", "which is between critical values, namely acceptence region" ,"\n",
        "we do not have enough evidence to reject the null hypothesis",
        "\n","\n")
  }else{
    cat("From MC Simulation we get an estimated Critical Value of", 
        round(Critical.Value_upper,3),"and", round(Critical.Value_lower,3)  ,"\n", "Since Test Statistic is", 
        round(test.stat,3),"\n", "which is not between critical values, namely test statistics is in rejection region,"
        ,"\n","we have enough evidence to reject the null hypothesis",
        "\n","\n")
  }
  
  a <- seq(-4,4,0.01)
  plot(a, dnorm(a), type = "l",lwd = 2.5, ylab = "", xlab = "")
  abline(h=0, lwd = 2.5)
  abline(v = Critical.Value_upper, col = "Red")
  abline(v = Critical.Value_lower, col = "Red")
  abline(v = test.stat, col = "Dark Blue")
  points(x = Critical.Value_upper, y = 0, pch = 1, cex = 2, col = "Red")
  points(x = Critical.Value_lower, y = 0, pch = 1, cex = 2, col = "Red")
  points(x = test.stat, y = 0, pch = 2, cex = 2, col = "Dark Blue")
  legend("topright",legend = c("Critical Values","Test Stat"), 
         col = c("Red","Dark Blue"), pch = 1:2)
  
  return(list(test_stat = test.stat,
              Critical_Value_upper = as.numeric(Critical.Value_upper),
              Critical_Value_lower = as.numeric(Critical.Value_lower)))
}

M <- 10^5
n <- 35
alpha <- 0.01
Mu0 <- 7.91
xbar <- 8
sigma <- sqrt(0.03)

cv(n, xbar, Mu0, sigma,  M, alpha)