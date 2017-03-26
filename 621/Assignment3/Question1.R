#Explicit Finite Difference method----
  # Explicit Implementation----
Explicit <- function(isAmerican, isCall, K, Tm,S0, r, sig, N, div, dx,returnGreeks=FALSE){
  # Finite Difference Method: i times, 2*i+1 final nodes
  # Precompute constants ----
  dt = Tm/N
  nu = r - div - 0.5 * sig^2
  edx = exp(dx)
  # got the constants formulas from clewlow 3.18, 3.19, 3.20
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
      # This inner for loop is only stepping through the 2 to rowsize-1, to avoid the boundaries
      V[j, i] = pu*V[j-1,i+1] + pm*V[j, i+1] + pd*V[j+1,i+1]
      # print(paste(i,j))
    }
    # Boundary Conditions ----
    # print(paste("First =",lastRow,i,"Last=",firstRow,i))
    stockTerm = ifelse(isCall, S[1, lastCol]-S[2,lastCol], S[nRows-1,lastCol]-S[nRows,lastCol])
    # print(stockTerm)
    # print(paste("i=",i))
    # The last row contains the discounted value of V[lastRow, lastCol] and since 
    # this is zero for Call, we adopt the below method
    V[lastRow,  i] = V[lastRow-1,  i] + ifelse(isCall, 0, stockTerm)
    # Doing interpolation for Filling the first rows of each column
    V[firstRow, i] = V[firstRow+1, i] + ifelse(isCall, stockTerm, 0)
    if(isAmerican)
      for(j in lastRow:firstRow) {
        V[j, i] = max(V[j, i], cp * (S[j, lastCol] - K))
      }
  }
  # Calculating the Greeks and returning ----
  if(returnGreeks && isCall){
    delta = (V[middleRow-1,firstCol+1]-V[middleRow+1,firstCol+1])/
      (S[middleRow-1,firstCol+1]-S[middleRow+1,firstCol+1])
    delta1 =(V[middleRow-1,firstCol+1]-V[middleRow,firstCol+1])/
      (S[middleRow-1,firstCol+1]-S[middleRow,firstCol+1])
    delta2 =(V[middleRow,firstCol+1]-V[middleRow+1,firstCol+1])/
      (S[middleRow,firstCol+1]-S[middleRow+1,firstCol+1])
    gamma = 2*(delta1-delta2)/((S[middleRow-1,firstCol+1]-S[middleRow+1,firstCol+1]))
    theta =(V[middleRow,firstCol+1]-V[middleRow,firstCol])/dt
    return(list(Price=V[middleRow,firstCol],Delta=delta,Gamma=gamma,Theta=theta))
  }
  # Return the price ----
  # list(Type = paste( "European", ifelse(isCall, "Call", "Put")),
  #      Price = V[middleRow,firstCol], Probs=round(c(pu=pu, pm=pm, pd=pd),4), S=round(S,2), V=round(V,4))
  return(V[middleRow,firstCol])
}
  #Implementation of Explicit----
Explicit(isAmerican=FALSE,isCall=TRUE, K=100, Tm=1, S0=100, r=0.06, sig=0.2, N=3, div=0.03, dx=0.2,returnGreeks = TRUE)
#European put value is =5.6217
#European call value is =8.5455 (verified)
#Ameican put value = 6.0058 (verified)


#Implicit method----
  # Implicit Implementation----
Implicit = function(isAmerican,isCall, K, Tm,S0, r, sig, N, div, dx){
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
  if(isCall){ 
    lambdaU =(S[1, lastCol] - S[2, lastCol])
    lambdaL = 0
  }else{ #clewlows way
    lambdaU = 0
    lambdaL = -1 * (S[lastRow-1, lastCol] - S[lastRow,lastCol])
  }
  # stockTerm = ifelse(isCall, S[1, lastCol]-S[2,lastCol], S[nRows-1,lastCol]-S[nRows,lastCol])
  # # The last row contains the discounted value of V[lastRow, lastCol] and since 
  # # this is zero for Call, we adopt the below method
  # lambdaL = V[lastRow-1,  i] + ifelse(isCall, 0, stockTerm)
  # # Doing interpolation for Filling the first rows of each column
  # lambdaU = V[firstRow+1, i] + ifelse(isCall, stockTerm, 0)
  
  
  # Step backwards through the lattice ----
  for (i in (lastCol-1):firstCol) {
    h = solveImplicitTridiagonal(V, pu, pm, pd, lambdaL, lambdaU, i)
    pmp[,i] = h$pmp  # collect the pm prime probabilities
    pp [,i] = h$pp   # collect the p prime probabilities
    V = h$V
    # Apply Early Exercise condition ----
    if(isAmerican)
      for(j in lastRow:firstRow) {
        V[j, i] = max(V[j, i], cp * (S[j, lastCol] - K))
      }
  }
  # Return the price ----
  # list(Type = paste( "American", ifelse(isCall, "Call", "Put")),Price = V[middleRow,firstCol],
  #      Probs=round(c(pu=pu, pm=pm, pd=pd),middleRow), pmp=round(pmp,4), pp=round(pp,4),
  #      S=round(S,2), V=round(V,middleRow))
  return(list(Price=V[middleRow,firstCol],Probs=round(c(pu=pu, pm=pm, pd=pd),middleRow)))
}
  # Solving the tridiagonal matrix---- 
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
  # Testing the implicit method ----
x=Implicit(isAmerican=TRUE,isCall=FALSE, K=100, Tm=1, S0=100, r=0.06, sig=0.2, N=3, div=0.03, dx=0.2)
#Implicit American Put = 4.9221 (verified from textbook)
#Implicit American Call=7.5608 (not verified)
#Implicit European Call=7.5608 (not verified)
#Implicit European Put =4.768 (not verified)
#CHECK FOR EUROPEAN as well and verify. Also check for American Call and verify

#Crank Nicholson Method----
CrankNicholson = function(isAmerican, isCall, K, Tm,S0, r, sig, N, div, dx){
  # Crank Nicholson Finite Difference Method: i times, 2*i+1 final nodes
  # Precompute constants ----
  dt = Tm/N
  nu = r - div - 0.5 * sig^2
  edx = exp(dx)
  pu = -0.25     *dt * ( (sig/dx)^2 + nu/dx )  
  pm =  1.0 + 0.5*dt *   (sig/dx)^2 + 0.5*r*dt 
  pd = -0.25     *dt * ( (sig/dx)^2 - nu/dx)   
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
  if(isCall){
    lambdaU =(S[1, lastCol] - S[2, lastCol])
    lambdaL = 0
  }else{
    lambdaU = 0
    lambdaL = round(-1 * (S[lastRow-1, lastCol] - S[lastRow,lastCol]),2)
  }
  # Step backwards through the lattice ----
  for (i in (lastCol-1):firstCol) {
    h = solveCrankNicholsonTridiagonal(V, pu, pm, pd, lambdaL, lambdaU, i)
    pmp[,i] = round(h$pmp,4)  # collect the pm prime probabilities
    pp [,i] = round(h$pp, 4)  # collect the p prime probabilities
    V = h$V
    # Apply Early Exercise condition for American Options ----
    for(j in lastRow:firstRow) {
      V[j, i] = max(V[j, i], cp * (S[j, lastCol] - K))
    }
  }
  # Return the price ----
  list(Type = paste(ifelse(isCall, "Call", "Put")),Price = V[middleRow,firstCol],
       Probs=round(c(pu=pu, pm=pm, pd=pd), 4), pmp=pmp, pp= pp,
       S=round(S,2), V=round(V,middleRow))
}
  #TridiagonalMatrix solver for crank nicholson----
#it was solveICrankNicholsonTridiagonal
solveCrankNicholsonTridiagonal=function(V, pu, pm, pd, lambdaL, lambdaU, colI)
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
  pp[lastRow-1]  = (- pu   *V[lastRow-2, lastCol] 
                    -(pm-2)*V[lastRow-1, lastCol]
                    - pd   *V[lastRow  , lastCol] + pd*lambdaL)
  # Eliminate upper diagonal ----
  for (j in (lastRow-2):(secondRow)) {
    pmp[j] = pm - pu*pd/pmp[j+1]
    pp[j] = ( - pu   *V[j-1, colI+1] 
              -(pm-2) *V[j  , colI+1]
              - pd    *V[j+1, colI+1] 
              -pp[j+1]*pd/pmp[j+1])
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


  #Implementation of Crank Nicholson----
CrankNicholson(isAmerican=FALSE,isCall=TRUE, K=100, Tm=1, S0=100, r=0.06, 
               sig=0.25, N=1128, div=0.03, dx=0.2)


#American call = 8.0301 (not verified)
#American put  = 5.4184 (verified)








#BlackSholes----
BSM<-function(S, K, t, r, sigma,type){
  d1 <- (log(S/K)+(r+sigma^2/2)*t)/(sigma*sqrt(t))
  d2 <- d1 - sigma * sqrt(t)
  if (type == "c")
    result <- S*pnorm(d1) - K*exp(-r*t)*pnorm(d2)
  if (type == "p")
    result <- K*exp(-r*t) * pnorm(-d2) - S*pnorm(-d1)
  return(result)
}
BSM(S=100,K=100,t=1,r=.06,sigma=.2,type="c")


# Question1.C----
  # Carried out an implementation from clewlow pg 65---- 
QuestionC<-function(N=3,sig,Tm,nsd,error){
  repeat{
    dt=Tm/N
    # Nj=((nsd*sig*sqrt(Tm)) / (2*(error-(Tm/N))) ) - .5 # Doesnt work
    Nj=(sqrt(N)*nsd)/(2*sqrt(3)) -.5
    dx=(nsd*sig*sqrt(Tm))/(2*Nj+1)
    # print(paste(dt,Nj,dx))
    N=N+1
    if(((dx*dx)+dt)<=error){
      print(paste("ANSWER:",dt,Nj,dx,N))
      return(list(dt,Nj,dx,N))
      break
    }
  }
}
  # Testing it for the above conditions ----
c=QuestionC(N=3,sig=.2,Tm=1,nsd=6,error = .001)
Implicit(isAmerican=FALSE,isCall=FALSE, K=100, Tm=1, S0=100, r=0.06, sig=0.2, N=1121, div=0.0, dx=0.0103509833901353)




# Question1.D ----

Explicit(isAmerican=FALSE,isCall=TRUE, K=100, Tm=1, S0=100, r=0.06, sig=0.25, N=1121, div=0.03, dx=0.0103509833901353)
# 11.01241
Explicit(isAmerican=FALSE,isCall=FALSE, K=100, Tm=1, S0=100, r=0.06, sig=0.25, N=1121, div=0.03, dx=0.0103509833901353)
# 8.144172
Implicit(isAmerican=FALSE,isCall=TRUE, K=100, Tm=1, S0=100, r=0.06, sig=0.25, N=1121, div=0.03, dx=0.0103509833901353)
# 11.00983
Implicit(isAmerican=FALSE,isCall=FALSE, K=100, Tm=1, S0=100, r=0.06, sig=0.25, N=1121, div=0.03, dx=0.0103509833901353)
# 8.141817
  # Verify the above answers, Also I have ASSUMED that dx is obtained from the above
  # Also verify which N to use, because in the problem below I am getting different results


# QuestionE ----
# print(QuestionC(sig=.25,Tm=1,nsd=6,error = .001))
QuestionE<-function(error,N=100,Tm,nsd,isAmerican, isCall, K, S0, r, sig, div, dx){
  BS_price = BSM(type=ifelse(isCall, "c", "p"), K=K, t=Tm, S=S0, r=r, sigma=sig)
  print(BS_price)
  repeat{
    dt=Tm/N
    Nj=(sqrt(N)*nsd)/(2*sqrt(3)) -.5
    dx=(nsd*sig*sqrt(Tm))/(2*Nj+1)
    # print(paste(dt,Nj,dx))
    N=N+1
    print(N)
    explicit_price=Explicit(isAmerican=FALSE,isCall=isCall, K=K, Tm=Tm, 
                            S0=S0, r=r, sig=sig, N=N, div=div, dx=dx)
    print(explicit_price)
    if(abs(explicit_price-BS_price)<=error){
      print(paste("ANSWER:",dt,Nj,dx,N))
      return(list(dt,Nj,dx,N))
      break
    }
  }
}
BSM<-function(S, K, t, r, sigma,type){
  d1 <- (log(S/K)+(r+sigma^2/2)*t)/(sigma*sqrt(t))
  d2 <- d1 - sigma * sqrt(t)
  if (type == "c")
    result <- S*pnorm(d1) - K*exp(-r*t)*pnorm(d2)
  if (type == "p")
    result <- K*exp(-r*t) * pnorm(-d2) - S*pnorm(-d1)
  return(result)
}

E=QuestionE(error=0.001,Tm=1,nsd=6,isAmerican=FALSE,isCall=TRUE, 
            K=100, S0=100, r=0.06, sig=0.25, div=0.03)


# QuestionF ----
library(reshape)
library(ggplot2)
sig.list = seq(.05,.6,.05)
pu={}
pm={}
pd={}
for(sig in sig.list){
  implicit=Implicit(isAmerican=FALSE,isCall=TRUE, K=100, Tm=1, S0=100,
                    r=0.06, sig=sig, N=100, div=0.03, dx=0.0103509833901353)
  pu=append(pu,implicit$Probs[1])
  pm=append(pm,implicit$Probs[2])
  pd=append(pd,implicit$Probs[3])
  print(sig)
}
probability=data.frame(sig.list,pu,pm,pd)
names(probability)=c('Sigma','pu','pm','pd')
melted_prob=melt(probability,id.vars ="Sigma")
ggplot(data = melted_prob, aes(x=Sigma, y=value)) + geom_line(aes(colour=variable))


















# QuestionF Proper Implementation ----
pu = function(sig, dt=1/3, dx=0.2, r=0.06, div=0.03, nu=r-div-0.5*sig^2) {
  0.5 * dt * ( (sig/dx)^2 + nu/dx )
}

pm =  function(sig, dt=1/3, dx=0.2, r=0.06, div=0.03, nu=r-div-0.5*sig^2) {
  1.0 - dt *   (sig/dx)^2 - r*dt 
}
pd =  function(sig, dt=1/3, dx=0.2, r=0.06, div=0.03, nu=r-div-0.5*sig^2) {
  0.5 * dt * ( (sig/dx)^2 - nu/dx)
}

x = seq(from=0.01, to=0.5, by=0.01)
ux = pu(x)
mx = pm(x)
dx = pd(x)
theTitle = expression(paste("Figure 3.11: Explicit Finite Difference Probabilities", 
                            " as a Function of ", sigma, sep=""))
xlab1 = expression(paste("Volatility (", sigma, ")", sep=""))
xlab2 = expression(paste("K = 100, T = 1.0, S =100, r = 6%, ", delta, " = 3%"))
plot(x, ux, ylim=c(-1,1), type="n", xlab="")
title(theTitle, cex=3)
mtext(text=xlab1, side=1, line=2, cex=1.25)
mtext(text=xlab2, side=1, line=3, cex=1.25)
lines(x, ux, lwd=2, col=2)
lines(x, mx, lwd=2, col=3)
lines(x, dx, lwd=2, col=4)
abline(h=0)
legend(0.4, 0.5, legend=c('pu', 'pm', 'pd'), col=2:4, lty=1, lwd=3)


# QuestionH ----
sigma=.25
explicit=Explicit(isAmerican=FALSE,isCall=TRUE, K=100, Tm=1, S0=100, r=0.06, sig=0.25, N=3, div=0.03, dx=0.2,returnGreeks = TRUE)
delta=explicit$Delta
gamma=explicit$Gamma
theta=explicit$Theta
dsigma = .001* sigma
Vega = (Explicit(isAmerican=FALSE,isCall=TRUE, K=100, Tm=1, S0=100, r=0.06, sig=sigma+dsigma, N=3, div=0.03, dx=0.2) 
-Explicit(isAmerican=FALSE,isCall=TRUE, K=100, Tm=1, S0=100, r=0.06, sig=sigma-dsigma, N=3, div=0.03, dx=0.2))/(2*dsigma)
  
  # Delta = 0.58179
  # Gamma = 0.0172
  # Theta = -6.6315
  # Vega  = 43.1158

  #Made an assumption in the cloumn index in the delta calculations

library(fOptions)
sapply(c('delta', 'gamma', 'vega', 'theta', 'rho'), function(greek)
GBSGreeks(Selection = greek, TypeFlag = "c", S = 100, X = 100,
              Time = 1, r = 0.06, b = 0.00, sigma = 0.25))

  
  
  
  
  