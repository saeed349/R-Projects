solveImplicitTridiagonal=function(V, pu, pm, pd, lambdaL, lambdaU, colI)
{
  # Initalize values ----
  firstRow = 1
  secondRow = 2
  thirdRow = 3
  lastRow = nRows = nrow(V)
  lastCol = ncol(V)
  # Substitute boundary condition at j = -Nj into j = -Nj+1 ----
  pp = pmp = numeric(nRows)
  pmp[lastRow-1] = pm + pd
  pp[lastRow-1]  = V[lastRow-1, lastCol] + pd*lambdaL
  
  # Eliminate upper diagonal ----
  for (j in (lastRow-2):(secondRow)) {
    pmp[j] = pm - pu*pd/pmp[j+1]
    pp[j] = V[j, colI+1] - pp[j+1]*pd/pmp[j+1]
  }
    # Use boundary conditions at j = Nj and equation at j=Nj-1 ----
  V[firstRow, colI] = (pp[secondRow] + pmp[secondRow]*lambdaU)/(pu + pmp[secondRow])
  V[secondRow, colI] = V[firstRow,colI] - lambdaU
  # Back-substitution ----
  for(j in thirdRow:lastRow) {
    V[j, colI] =  (pp[j] -pu*V[j-1, colI])/pmp[j]
  }
  V[lastRow, colI] = V[lastRow-1, colI] - lambdaL
  # Return values ----
  list(V=V, pmp=pmp, pp=pp)
}
