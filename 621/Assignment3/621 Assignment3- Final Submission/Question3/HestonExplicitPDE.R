HestonExplicitPDE = function(params,K,r,div,S,V,Tm) {

  # Finite differences for the Heston PDE for a European Call ----
  #  Uses even grid sizes
  #  In 'T Hout and Foulon "ADI Finite Difference Schemes for Option Pricing
  #  in the Heston Modelo with Correlation" Int J of Num Analysis and Modeling, 2010.
  #  Thesis by Sensi Li and paper by Vassilis Galiotos
  #  INPUTS
  #     params = 6x1 vector of Heston parameters
  #     K = Strike price
  #     r = risk free rate
  #     div = Dividend yield
  #     S = vector for stock price grid
  #     V = vector for volatility grid
  #     Tm = vector for maturity grid
  # OUTPUT
  #     2-D array of size (nS+1)x(nV+1) for the call price

  # Heston parameters ----
  kappa = params[1];
  theta = params[2];
  sigma = params[3];
  v0    = params[4];
  rho   = params[5];
  lambda = params[6];
  
  # Length of stock, volatility, and maturity ----
  NS = length(S);
  NV = length(V);
  NT = length(Tm);
  Smin = S[1];  Smax = S[NS];
  Vmin = V[1];  Vmax = V[NV];
  Tmin = Tm[1];  Tmax = Tm[NT];
  
  # Increment for Stock Price, Volatility, and Maturity ----
  ds = (Smax-Smin)/(NS-1);
  dv = (Vmax-Vmin)/(NV-1);
  dt = (Tmax-Tmin)/(NT-1);
  
  # Initialize the 2-D grid with zeros ----
  U = matrix(0, nrow=NS, ncol=NV)
  
  # Temporary grid for previous time steps ----
  u = matrix(0, nrow=NS, ncol=NV)
  
  # Solve the PDE ----
  # Round each value of U(S,v,Tm) at each step
  # Boundary condition for tm = Maturity ----
  for (s in 1:NS) {
    for (v in 1:NV) {
      U[s,v] = max( K-S[s], 0);
    }
  }
  # Boundary Conditions for everything else ----
  for (tm in 1:(NT-1))  {
    # Boundary condition for Smin and Smax ----
    for (v in 1:(NV-1)) {
      U[1,v] = 0;
      U[NS,v] = max(0, K-Smax) # Galiotos uses U(NS-1,v) + ds;
    }
    # Boundary condition for Vmax ----
    for (s in 1:NS) {
      U[s,NV] = max(0, K-S[s]) # Galiotos uses U(s,NV-1);
    }
    # Update temp grid u[s,tm] with the boundary conditions ----
      u = U;
    # Boundary condition for Vmin ----
    # Previous time step values are in the temporary grid u(s,tm)
    for (s in 2:(NS-1)) {
      DerV = (u[s,2] - u[s,1]) / dv;                	# PDE Points on the middle of the grid (non boundary)
      DerS = (u[s+1,1] - u[s-1,1])/2/ds;              # Central difference for dU/dS
      U[s,1] = u[s,1]*(1 - r*dt - kappa*theta*dt/dv) +
              dt*(0.5*(r-div)*s*u[s+1,1] - u[s-1,1]) + 
               kappa*theta*dt/dv*u[s,2];
    }
    # Update the temporary grid u(s,tm) with the boundary conditions ----
    u = U;
    # Interior points of the grid (non boundary) ----
    # Previous time step values are in the temporary grid u(s,tm) ----
    for (s in 2:(NS-1)) {
      for (v in 2:(NV-1)) {
      A = (1 - dt*(s-1)^2*(v-1)*dv - sigma^2*(v-1)*dt/dv - r*dt);
      B = (1/2*dt*(s-1)^2*(v-1)*dv - 1/2*dt*(r-div)*(s-1));
      CC = (1/2*dt*(s-1)^2*(v-1)*dv + 1/2*dt*(r-div)*(s-1));
      D = (1/2*dt*sigma^2*(v-1)/dv - 1/2*dt*kappa*(theta-(v-1)*dv)/dv);
      E = (1/2*dt*sigma^2*(v-1)/dv + 1/2*dt*kappa*(theta-(v-1)*dv)/dv);
      F = 1/4*dt*sigma*(s-1)*(v-1);
      U[s,v] = A*u[s,v] +   B*u[s-1,v] + CC*u[s+1,v] +
               D*u[s,v-1] + E*u[s,v+1] +
               F*(u[s+1,v+1]+u[s-1,v-1]-u[s-1,v+1]-u[s+1,v-1])
      }
    }
    
  }
  # Return  2-D array of size (nS+1)x(nV+1) ----
  U
}
