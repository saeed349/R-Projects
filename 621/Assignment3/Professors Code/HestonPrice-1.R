HestonPrice = function(PutCall,kappa,theta,lambda,rho,
                      sigma,Tm,K,S,r,div,v0,trap,Lphi,Uphi,dphi)
{
# Heston (1993) price of a European option.
# Uses the original formulation by Heston
# Heston parameters:
  #    kappa  = volatility mean reversion speed parameter
  #    theta  = volatility mean reversion level parameter
  #    lambda = risk parameter
  #    rho    = correlation between two Brownian motions
  #    sigma  = volatility of variance
  #    v0     = initial variance
  # Option features.
  #    PutCall = 'C'all or 'P'ut
  #    K = strike price
  #    S = spot price
  #    r = risk free rate
  #    div = dividend yield
  #    Tm = maturity
  # Integration features
  #    L = lower limit
  #    U = upper limit
  #    dphi = integration increment
  
  # Build the integration grid
  phi = seq(from=Lphi, by=dphi, to=Uphi)
  N = length(phi);
  int1 = int2 = numeric(N)
  
  # Build the integrands for P1 and P2;
  for (k in 1:N) {
    int1[k] = HestonProb(phi[k],kappa,theta,lambda,rho,sigma,Tm,K,S,r,div,v0,1,trap);
    int2[k] = HestonProb(phi[k],kappa,theta,lambda,rho,sigma,Tm,K,S,r,div,v0,2,trap);
  }
  
  # The integrals
  I1 = 100*trapz(phi, int1) * dphi;
  I2 = 100*trapz(phi, int2) * dphi;

  # The probabilities P1 and P2
  P1 = 1/2 + 1/pi*I1;
  P2 = 1/2 + 1/pi*I2;
  
  # The call price
  HestonC = S*exp(-div*Tm)*P1 - K*exp(-r*Tm)*P2;
  
  # The put price by put-call parity
  HestonP = HestonC - S*exp(-div*Tm) + K*exp(-r*Tm);
  
  # Output the option price
  ifelse ("C" == PutCall, HestonC, HestonP)
  #list(int1=int1, int2=int2)
}

    