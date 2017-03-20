HestonProb = function(phi,kappa,theta,lambda,rho,sigma,tau,K,S,r,div,v0,Pnum,Trap)
{
# Returns the integrand for the risk neutral probabilities P1 and P2.
# phi = integration variable
# Pnum = 1 or 2 (for the probabilities)
# Heston parameters:
  #    kappa  = volatility mean reversion speed parameter
  #    theta  = volatility mean reversion level parameter
  #    lambda = risk parameter
  #    rho    = correlation between two Brownian motions
  #    sigma  = volatility of variance
  #    v      = initial variance
  # Option features.
  #    PutCall = 'CC'all or 'P'ut
  #    K = strike price
  #    S = spot price
  #    r = risk free rate
  #    div = dividend yield
  #    Trap = 1 "Little Trap" formulation 
  #           0  Original Heston formulation
  
  # Log of the stock price.
  x = log(S);
  
  # Parameter "a" is the same for P1 and P2.
  a = kappa*theta;
  
  # Parameters "u" and "b" are different for P1 and P2.
  if (Pnum==1) {
    u = 0.5;
    b = kappa + lambda - rho*sigma;
  } else {
    u = -0.5;
    b = kappa + lambda;
  }
  
  
  d = sqrt((rho*sigma*1i*phi - b)^2 - sigma^2*(2*u*1i*phi - phi^2));
  g = (b - rho*sigma*1i*phi + d) / (b - rho*sigma*1i*phi - d);
  
  if (Trap==1) {
    # "Little Heston Trap" formulation
    cc = 1/g;
    D = (b - rho*sigma*1i*phi - d)/sigma^2*((1-exp(-d*tau))/(1-cc*exp(-d*tau)));
    G = (1 - cc*exp(-d*tau))/(1-cc);
    CC = (r-div)*1i*phi*tau + a/sigma^2*((b - rho*sigma*1i*phi - d)*tau - 2*log(G));
   } else if (Trap==0) {
    # Original Heston formulation.
    G = (1 - g*exp(d*tau))/(1-g);
    CC = (r-div)*1i*phi*tau + a/sigma^2*((b - rho*sigma*1i*phi + d)*tau - 2*log(G));
    D = (b - rho*sigma*1i*phi + d)/sigma^2*((1-exp(d*tau))/(1-g*exp(d*tau)));
  }
  
  # The characteristic function.
  f = exp(CC + D*v0 + 1i*phi*x);
  
  # Return the real part of the integrand.
  y = Real(exp(-1i*phi*log(K))*f/1i/phi);
  y
}
  
  