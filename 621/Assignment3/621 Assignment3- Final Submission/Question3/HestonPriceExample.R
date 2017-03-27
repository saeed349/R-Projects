HestonPriceExample = function(
  S = 1,         # Spot price
  K = 1,         # Strike price
  tau = 5,        # Maturity
  r = 0.0,        # Risk free rate
  div = 0.0,         # Dividend yield
  kappa = 1,       # Heston parameter : reversion speed
  sigma = 0.2,     # Heston parameter : volatility of variance
  rho   = -0.3,    # Heston parameter : correlation
  theta = 0.1,    # Heston parameter : reversion level
  v0    = 0.1,    # Heston parameter : initial variance
  lambda = 0       # Heston parameter : risk preference
)
{
  # Expression for the characteristic function ----
  Trap = 1;   # 0 = Original Heston formulation
              # 1 = Albrecher et al formulation
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