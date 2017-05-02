#Without Importance Sampling
set.seed(2017)
X <- runif(1e5,0,10)
Y <- 10*exp(-2*abs(X-5))
c( mean(Y), var(Y) )

# What is the function
h = function(x) exp(-2*abs(x-5))
x = seq(0,10,length=500)
plot(x, h(x), type="l")
# Note: peak is around 5
# what function is centered around 5?  A Guassian ~ N(5, sigma)


#Importance sampling Monte Carlo
w <- function(x) dunif(x, 0, 10)/dnorm(x, mean=5, sd=1)
f <- function(x) 10*exp(-2*abs(x-5))
set.seed(2017)
X=rnorm(1e4,mean=5,sd=1)
Y=w(X)*f(X)
c( mean(Y), var(Y) )


# Let's graph the 2 functions
par(mfrow=c(1,2))
x = seq(0,10,length=500)

h = function(x) exp(-2*abs(x-5))
plot(x, h(x), type="l", main="Regular Monte Carlo")
lines(x, rep(1/10, 500), col='red')

g <- function(x) sqrt(2*pi)*exp(.5*(x-5)^2)
plot(x, h(x), type="l", main="MC with Importance Sampling")
lines(x, dnorm(x, mean=5, sd=1), col='red')
