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
  Z[i] = 0.8*X1+sqrt(1-0.8^2)*X2 
}


#Monte Carlo Method----

# set.seed(07630)
nIterations =  100000  #should be high #100000 in question
nSteps = 1000

# mu = 0.02
# # dt = 1/nSteps
# sigma = 0.03
# S0 = 100


S_0=100.0
K = 100.0
r = 0.0319
v_0 = 0.010201
T = 1.00

dt = T/nSteps

rho = -0.7
kappa = 6.21
theta = 0.019
xi = 0.61

# S = matrix(0, nrow=nSteps+1, ncol=nIterations)
payoff_sum=0
for( j in 1:nIterations) {
  W1=W2={}
  vol_path=v_0
  vmax=0
  spot_path=S_0
  W1=W2=0
  for( i in 1:nSteps){
    
    

    vmax= max(vol_path,0)
    # print(paste("vol_path Before=",vol_path))
    vol_path=vol_path+kappa*dt*(theta-vmax)+xi*sqrt(vmax*dt)*W1 #W1[i-1]
    # print(paste("Vol=",vol_path))
    # print(paste("vol_path After=",vol_path))
    spot_path=spot_path* exp((r - 0.5*vmax)*dt +sqrt(vmax*dt)*W2)
    # print(paste("Spot=",spot_path))
    
    X1 = rnorm(1,0,1) 
    X2 = rnorm(1,0,1) 
    W1=X1
    W2= rho*X1+sqrt(1-rho^2)*X2
  }
  payoff_sum=payoff_sum+max(spot_path-K,0)
  print(j)
}
option_price=(payoff_sum/nIterations)*exp(-r*dt)
print(option_price)


