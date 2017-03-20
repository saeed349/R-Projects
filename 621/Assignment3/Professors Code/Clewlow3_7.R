Clewlow3_7 = function(isCall, K=100, Tm=1,S0=100, r=0.06, sig=0.2,
                      N=3, div=0.03, dx=0.2)
{
  # Finite Difference Method: i times, 2*i+1 final nodes
  # Precompute constants ----
  dt = Tm/N
  nu = r - div - 0.5 * sig^2
  edx = exp(dx)
  # got the constant formulas from clewlow 3.18, 3.19, 3.20
  pu = 0.5 * dt * ( (sig/dx)^2 + nu/dx )
  pm = 1.0 - dt *   (sig/dx)^2 - r*dt 
  pd = 0.5 * dt * ( (sig/dx)^2 - nu/dx)
  firstRow = 1
  r = nRows = lastRow = 2*N+1
  firstCol = 1
  middleRow = s = nCols = lastCol = N+1
  cp = ifelse(isCall, 1, -1)
  
  # Intialize asset prices  ----
  V = S = matrix(0, nrow=nRows, ncol=nCols, dimnames=list(
    paste("NumUps=",(middleRow-1):-(middleRow-1), sep=""),
    paste("Time=",round(seq(0, 1, len=nCols),4),sep="")))
  S[middleRow, firstCol] = S0
  for (i in 1:(lastCol-1)) {
    for(j in (middleRow-i+1):(middleRow+i-1)) {
      S[j-1, i+1] = S[j, i] * exp(dx)
      S[j ,  i+1] = S[j, i] 
      S[j+1, i+1] = S[j, i] * exp(-dx)
    }
  }
  # Intialize option values at maturity ----
  for (j in 1:lastRow) {
    V[j, lastCol] = max( 0, cp * (S[j, lastCol]-K))
  }
  # Step backwards through the tree ----
  for (i in N:1) {
      for(j in (middleRow+N-1):(middleRow-N+1)) {
         V[j, i] = pu*V[j-1,i+1] + pm*V[j, i+1] + pd*V[j+1,i+1]
      }
      # Boundary Conditions ----
      stockTerm = ifelse(isCall, S[1, lastCol]-S[2,lastCol], S[nRows-1,lastCol]-S[nRows,lastCol])
      V[lastRow,  i] = V[lastRow-1,  i] + ifelse(isCall, 0, stockTerm)
      V[firstRow, i] = V[firstRow+1, i] + ifelse(isCall, stockTerm, 0)
  }
  # Return the price ----
  list(Type = paste( "European", ifelse(isCall, "Call", "Put")),
       Price = V[middleRow,firstCol], Probs=round(c(pu=pu, pm=pm, pd=pd),4), S=round(S,2), V=round(V,4))
}

Clewlow3_7(isCall=T)





