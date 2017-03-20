Rouah_Run_HestonExplicitPDE = function()
{
  # Strike price, risk free rate, dividend yield, and maturity
  K = 100
  r = 0.02
  div = 0.05
  Mat = 0.15

  # Heston parameters
  kappa =  1.5
  theta =  0.04
  sigma =  0.3
  rho   = -0.9
  v0    =  0.05
  lambda = 0
  params = c(kappa, theta, sigma, v0, rho, lambda)

  # Minimum and maximum values for the Stock Price, Volatility, and Maturity
  Smin = 0;  Smax = 2*K
  Vmin = 0;  Vmax = 0.5
  Tmin = 0;  Tmax = Mat

  # Number of grid points for the stock, volatility, and maturity
  nS = 79        # Stock price
  nV = 39        # Volatility
  nT = 3000      # Maturity

  # The maturity time increment and grid
  dt = (Tmax-Tmin)/nT
  Tm = (0:nT)*dt

  # Pricing Using a Uniform Grid
  # Increment for Stock Price and volatility
  ds = (Smax-Smin)/nS
  dv = (Vmax-Vmin)/nV

  # The stock price and volatility grids
  S = (0:nS) * ds
  V = (0:nV) * dv

  # Solve the PDE
  HestonExplicitPDE(params,K,r,div,S,V,Tm)
}
