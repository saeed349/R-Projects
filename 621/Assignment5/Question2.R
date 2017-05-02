#One way of doing correlation(Correct Way)----
X1 = rnorm(10000,0,1) 
X2 = rnorm(10000,0,1) 

Y = X1 
Z = 0.4*X1+sqrt(1-0.4)*X2 

par(mfrow=c(1,2))
plot(Y)
plot(Z)


#Testing another way of doing correlation(It works)-----
Y=Z={}
for(i in 1:10000)
{
  X1 = rnorm(1) 
  X2 = rnorm(1) 
  
  Y[i]=X1
  Z[i] = 0.8*X1+sqrt(1-0.4)*X2 
}


#Monte Carlo Method----

set.seed(07630)
nIterations = 10000  #should be high 
nSteps = 30
mu = 0.02
dt = 1/nSteps
sigma = 0.03
S0 = 100


S_0=100.0
K = 100.0
r = 0.0319
v_0 = 0.010201
T = 1.00

rho = -0.7
kappa = 6.21
theta = 0.019
xi = 0.61

# S = matrix(0, nrow=nSteps+1, ncol=nIterations)
payoff_sum=0
for( j in 1:nIterations) {
  S[1,j] = S0
  W1=W2={}
  vol_path=v_0
  vmax=0
  spot_path=S_0
  for( i in 1:nSteps){
    X1 = rnorm(1) 
    X2 = rnorm(1) 
    W1[i]=X1
    W2[i] = 0.8*X1+sqrt(1-0.4)*X2 
    
    vmax= max(vol_path,0)
    
    vol_path=vol_path+kappa*T*(theta-vmax)+xi*sqrt(vmax*T)*W1[i] #W1[i-1]
    # print(paste("Vol=",vol_path))
    
    spot_path=spot_path* exp((r - 0.5*vmax)*T +sqrt(vmax*T)*W2[i])
    # print(paste("Spot=",spot_path))
  }
  payoff_sum=payoff_sum+max(spot_path-K,0)
}
option_price=(payoff_sum/nIterations)*exp(-r*T)
print(option_price)

# Averages
mean(S[31,])
var(S[31,])

