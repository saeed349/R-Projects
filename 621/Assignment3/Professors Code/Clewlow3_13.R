Clewlow3_13 = function(isCall, K=100, Tm=1, 
                      S0=100, r=0.06, sig=0.2, N=3, div=0.03, dx=0.2)
{
  # Implicit Finite Difference Method: i times, 2*i+1 final nodes
  # Precompute constants ----
  dt = Tm/N
  nu = r - div - 0.5 * sig^2
  edx = exp(dx)
  # got the constants formulas from clewlow 3.33,3.34,3.35
  pu = -0.5 * dt * ( (sig/dx)^2 + nu/dx )
  pm =  1.0 + dt *   (sig/dx)^2 + r*dt 
  pd = -0.5 * dt * ( (sig/dx)^2 - nu/dx)
  firstRow = 1
  nRows = lastRow = 2*N+1
  firstCol = 1
  middleRow = nCols = lastCol = N+1
  
  cp = ifelse(isCall, 1, -1)
  
  # Intialize asset price, derivative price, primed probabilities  ----
  pp = pmp = V = S = matrix(0, nrow=nRows, ncol=nCols, dimnames=list(
    paste("NumUps=",(nCols-1):-(nCols-1), sep=""),
    paste("Time=",round(seq(0, 1, len=nCols),4),sep="")))
  S[middleRow, firstCol] = S0
  for (i in 1:(nCols-1)) {
    for(j in (middleRow-i+1):(middleRow+i-1)) {
      S[j-1, i+1] = S[j, i] * exp(dx)
      S[j ,  i+1] = S[j, i] 
      S[j+1, i+1] = S[j, i] * exp(-dx)
    }
  }
  # Intialize option values at maturity ----
  for (j in firstRow:lastRow) {
    V[j, lastCol] = max( 0, cp * (S[j, lastCol]-K))
  }
  # Compute Derivative Boundary Conditions ----
  # From equation 3.38 and 3.39 in Clewlow
  lambdaL = -1 * (S[lastRow-1, lastCol] - S[lastRow,lastCol])
  lambdaU = 0
  
  # Step backwards through the lattice ----
  for (i in (lastCol-1):firstCol) {
    h = solveImplicitTridiagonal(V, pu, pm, pd, lambdaL, lambdaU, i)
    pmp[,i] = h$pmp  # collect the pm prime probabilities
    pp [,i] = h$pp   # collect the p prime probabilities
    V = h$V
    # Apply Early Exercise condition ----
    for(j in lastRow:firstRow) {
      V[j, i] = max(V[j, i], cp * (S[j, lastCol] - K))
    }
  }
  # Return the price ----
  list(Type = paste( "American", ifelse(isCall, "Call", "Put")),Price = V[middleRow,firstCol],
       Probs=round(c(pu=pu, pm=pm, pd=pd),middleRow), pmp=round(pmp,4), pp=round(pp,4),
       S=round(S,2), V=round(V,middleRow))
}

Clewlow3_13(isCall = FALSE)


