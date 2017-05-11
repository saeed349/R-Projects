set.seed(07630)
nIterations = 10000  #should be high 
nSteps = 30
mu = 0.02
dt = 1/nSteps
sigma = 0.03
S0 = 100

S = matrix(0, nrow=nSteps+1, ncol=nIterations)

for( j in 1:nIterations) {
  S[1,j] = S0
  w = rnorm(nSteps)   # Random Number Generation
  for( i in 1:nSteps){
    dS = mu*S[i,j]*dt + sigma*S[i,j]*w[i]*sqrt(dt)
    S[i+1,j] = S[i,j] + dS
  }
}

# Averages
mean(S[31,])
var(S[31,])
