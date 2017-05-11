## @knitr Heston_Analytical

source("HestonPrice-1.R")
source("HestonProb.R")

HestonPriceExample = function(
  S = 100,         # Spot price
  K = 100,         # Strike price
  tau = 1,        # Maturity
  r = 0.0319,        # Risk free rate
  div = 0.0,         # Dividend yield
  kappa = 6.21,       # Heston parameter : reversion speed
  sigma = 0.61,     # Heston parameter : volatility of variance
  rho   = -0.7,    # Heston parameter : correlation
  theta = 0.019,    # Heston parameter : reversion level
  v0    = 0.010201,    # Heston parameter : initial variance
  lambda = 0       # Heston parameter : risk preference
)
{
  # Expression for the characteristic function ----
  Trap = 0;   # 0 = Original Heston formulation
              # 1 = Albrecher et al formulation
  
  # rmse=0
  # payoff_sum=0
  # Integration range				 
  Lphi = 0.000001;  # Lower limit
  dphi = 0.01;      # Increment
  Uphi = 50;        # Upper limit
  
  # Obtain the Heston put and call
  HPut  = HestonPrice('P',kappa,theta,lambda,rho,sigma,tau,K,S,r,div,v0,Trap,Lphi,Uphi,dphi);
  HCall = HestonPrice('C',kappa,theta,lambda,rho,sigma,tau,K,S,r,div,v0,Trap,Lphi,Uphi,dphi);
  
  # Output the result
  list(Call = HCall, Put = HPut)
}


ptm <- proc.time()
call_price=as.numeric(HestonPriceExample()[1])
time=as.double((proc.time() - ptm)[3])

K=100
rmse=(call_price-6.8061)^2
bias=(max(call_price,0)-6.8061)
rmse=sqrt(rmse)
print(paste("Call Option Price=",round(call_price,2)," Time taken=",round(time,2)," seconds"
            ,"RMSE=",round(rmse,2),"Bias=",round(bias,2)))
