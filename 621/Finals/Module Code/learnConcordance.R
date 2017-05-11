set.seed(2017)
N = 100
x = rnorm(N)
y = rnorm(N)

# Three Ways to Compute Correlation
# 1. Pearson computes linear association
(N*sum(x*y) - sum(x)*sum(y) )/
 sqrt ( ( N*sum(x*x)- (sum(x))^2 ) * ( N*sum(y*y)- (sum(y))^2 ) )
cov(x,y) / sqrt((var(x)*var(y)))
cor.test(x,y, method="pearson")$est

# Spearman's Rho ----
# 2. Spearman Correlation is Pearson Correlation of Ranked Data
cor( rank(x), rank(y))
1 - (6* sum ( (rank(x)-rank(y))^2 ))/(N*(N*N-1))
cor.test(x,y,method="spearman")$est

# Kendall's Tau ---- 
# 3. Kendall computes all possible pairs -- choose(N, 2)
kendall = numeric(choose(100,2))
length(kendall)
# Brute force comuptation: Compute each pair
k = 1
for(i in 1:(N-1)) {
  for(j in (i+1):N) {
    kendall[k] = (x[i]-x[j])*(y[i]-y[j])
    k=k+1
  }
}
(sum(kendall>0) - sum(kendall<0))/length(kendall)
cor.test(x,y,method="kendall")$est

# Relationship
tau = seq(-1,1, len=201)
rhoLower = 0.5 * ifelse(tau < 0, tau^2+2*tau-1, 3*tau-1)
rhoUpper = 0.5 * ifelse(tau < 0, 1+3*tau, 1+2*tau-tau^2)
par(pty='s')
plot(tau, rhoLower, type="l", xlab="tau", ylab="rho", col='red', lwd=2)
title("Attainable Region for Kendall's Tau and Spearman's Rho")
lines(tau, rhoUpper, col='blue', lwd=2)
