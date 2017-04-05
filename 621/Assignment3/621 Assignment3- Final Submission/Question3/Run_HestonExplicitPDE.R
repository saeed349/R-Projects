Rouah_Run_HestonExplicitPDE = function()
{
  # Strike price, risk free rate, dividend yield, and maturity
  K = 1
  r = 0.0
  div = 0.0
  Mat = 5

  # Heston parameters
  kappa =  1
  theta =  0.1
  sigma =  0.2
  rho   = -0.3
  v_0    =  0.1
  lambda = 0
  params = c(kappa, theta, sigma, v_0, rho, lambda)

  # Minimum and maximum values for the Stock Price, Volatility, and Maturity
  Smin = 0;  Smax = 2*K
  Vmin = 0;  Vmax = 0.5
  Tmin = 0;  Tmax = Mat

  # Number of grid points for the stock, volatility, and maturity
  nS = 40        # Stock price
  nV = 40        # Volatility
  nT = 5000      # Maturity

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
  U=HestonExplicitPDE(params,K,r,div,S,V,Tm)
  s_0 = 1
  v_0 = 0.1
  UniformPrice = interp2(V, S , U,v_0,s_0)

}
