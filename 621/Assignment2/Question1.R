BinomialTree= function(isCall, isAmerican, K=100, Tm=1, 
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
  
  print("--------")
  step=3
  m=(step-1)*2+3
  n=step+1
  u=1.1
  d=1/u
  s.tree =matrix(NA,nrow=m,ncol=n,dimnames=list(
    paste("State", 1:(m),sep=""), paste("T=",0:step,sep="")))
  s.tree
  s.tree[step+1,1]=100
  s.tree
  for(j in 2:n){
    for(i in 1:m){
      if(i+1<=m)
        if(!is.na(s.tree[i+1,j-1]))
          s.tree[i,j]=s.tree[i+1,j-1]*u
      if(i-1>=1)
        if(!is.na(s.tree[i-1,j-1]))
          s.tree[i,j]=s.tree[i-1,j-1]*d
    }
  }
  print(s.tree)
  
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

BinomialTree(isCall=T, isAmerican=F)
# Clewlow2_3n5(isCall=F, isAmerican=T)



