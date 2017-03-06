#professors implementation
BinomialTreeOld= function(isCall, isAmerican, K=100, Tm=1, 
                        S0=100, r=0.06, N=3, u=1.1, d=1/u)
{
  # Precompute constants ----
  dt = Tm/N # diving the time into partition
  p = (exp(r*dt)-d)/(u-d) #calculating the probability of up move
  disc = exp(-r*dt) #discount factor
  M = N+1 #in r, index starts from 0, whereas state in our tree starts from 0
  cp = ifelse(isCall, 1, -1) # 1 for CE or -1 for PE 
  
  # Intialize asset prices  ----
  # Intitialize an empty array of size 
  V = S = matrix(0, nrow=M, ncol=M, dimnames=list(
    paste("State", 1:(N+1),sep=""), paste("T=",0:N,sep="")))
  print(V)
  S[1,1] = S0
  for (j in 2:M) {
    S[1, j] = S[1, j-1]*u # multiplying the previous column member in same row to get the up move
    for(i in 2:j) {
      S[i, j] = S[i-1, j-1]*d #multiplying d with the diagonal element to get the down move
    }
  }
  S
  # Intialize option values at maturity ----
  for (j in 1:M) {
    V[M-j+1, M] = max( 0, cp * (S[M-j+1, M]-K))
  }
  
  # Step backwards through the tree ----
  for (j in (M-1):1) {
    for (i in 1:j) {
      V[i, j] = disc * ( p*V[i, j+1] + (1-p)*V[i+1, j+1] )
      if(isAmerican) {
        V[i, j] = max(V[i, j], cp * (S[i, j] - K))
      }
    }
  }
  V
  # Return the price ----
  list(Type = paste( ifelse(isAmerican, "American", "European"), 
                     ifelse(isCall, "Call", "Put")),
       Price = V[1,1], S=round(S,2), V=round(V,4))
}

#inspired from clewlaws trinomial tree
BinomialTree = function(isCall, isAmerican, K=100, Tm=1, 
                      S0=100, r=0.06, sig=0.2, N=3, u=1.1, d=1/u)
{
  # Precompute constants ----
  dt = Tm/N # diving the time into partition, to get time for each state
  disc = exp(-r*dt)
  p = (exp(r*dt)-d)/(u-d) #calculating the probability of up move
  nRows = 2*N+1 #number of rows for the matrix
  nCols = N+1 #number of columns
  cp = ifelse(isCall, 1, -1) # to check ifts a call or a put
  
  # Intialize asset prices  ----
  # Creating a matrix of nRows*nColumns with zeros and headings 
  V = S = matrix(0, nrow=nRows, ncol=nCols, dimnames=list(
    paste("NumUps", N:-N, sep="="), paste("T", 0:3, sep="=")))
  S[nCols, 1] = S0 # initial stock price
  
  # iterating the elements of the matrix in a conical manner starting
  # from the position of initial stock price
  # For n=3, S[i,j]= S0, then update the forward diagonal elements 
  # Code is similar to the one used for trinomial tree
  for (j in 1:N) {
    for(i in (nCols-j+1):(nCols+j-1)) {
      S[i-1, j+1] = S[i, j] * u
      S[i+1, j+1] = S[i, j] * d
    }
  }
  for (i in 1:nRows) {
    V[i, nCols] = max( 0, cp * (S[i, nCols]-K))
  }
  print(V)
  # V
  # Step backwards through the tree ----
  for (j in (nCols-1):1) {
    for(i in (nCols-j+1):(nCols+j-1)) {
      print((nCols-j+1):(nCols+j-1))
      V[i, j] = disc * (p*V[i-1,j+1] + (1-p)*V[i+1,j+1])
      if(isAmerican) {
        # if american option, then take the Value at each node as the max of the
        # value of option or the payoff at that period
        V[i, j] = max(V[i, j], cp * (S[i, j] - K))
      }
    } 
  }
  #Returning all the calculated values as a list.
  list(Type = paste( ifelse(isAmerican, "American", "European"), 
                     ifelse(isCall, "Call", "Put")),
       Price = V[nCols,1], S=round(S,2), V=round(V,4))
}


BinomialTree(isCall=T, isAmerican=F)

# BinomialTree(isCall=T, isAmerican=F)
# Clewlow2_3n5(isCall=F, isAmerican=T)

  

