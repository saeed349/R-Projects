#Exotic using old binomial tree method----
BinomialTreeExotic = function(isCall, isAmerican=FALSE, K=100, Tm=1, 
                           S0=100, r=0.06, sig=0.2, N=3, u=1.1, d=1/u,div=0,type,barrier=100)
{
  dt = Tm/N
  nu=r-div-0.5*sig*sig
  dxu=sqrt(sig*sig*dt+((nu*dt)^2))
  dxd=-dxu
  pu=0.5+0.5*(nu*dt/dxu)
  pd=1-pu
  disc=exp(-r*dt)
  #
  # dpu=disc*pu
  # dpd=disc*pd
  # edxud=exp(dxu-dxd)
  # edxd=exp(dxd)
  
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
    paste("NumUps", N:-N, sep="="), paste("T", 0:N, sep="=")))
  S[nCols, 1] = S0 # initial stock price
  
  # iterating the elements of the matrix in a conical manner starting
  # from the position of initial stock price
  # For n=3, S[i,j]= S0, then update the forward diagonal elements 
  # Code is similar to the one used for trinomial tree
  for (j in 1:N) {
    for(i in (nCols-j+1):(nCols+j-1)) {
      S[i-1, j+1] = S[i, j]*exp(dxu)
      S[i+1, j+1] = S[i, j] *exp(dxd)
    }
  }
  print(S)
  for (i in 1:nRows) {
    if(type=="UO"){
      V[i,nCols] <- ifelse((S[i,nCols]<barrier),max(0,cp * (S[i, nCols]-K)),0)
    }
    else if(type=="UI")
      V[i,nCols] <- ifelse((S[i,nCols]>barrier),max(0,cp * (S[i, nCols]-K)),0)
    # V[i, nCols] = max( 0, (cp * (S[i, nCols]-K))
      
  }
  print(V)
  # V
  # Step backwards through the tree ----
  for (j in (nCols-1):1) {
    for(i in (nCols-j+1):(nCols+j-1)) {
      # V[i, j] = disc * (p*V[i-1,j+1] + (1-p)*V[i+1,j+1])
      V[i, j] = disc * (pu*V[i-1,j+1] + pd*V[i+1,j+1])
      # if(isAmerican) {
      #   # if american option, then take the Value at each node as the max of the
      #   # value of option or the payoff at that period
      #   V[i, j] = max(V[i, j], cp * (S[i, j] - K))
      if(type=="UO" && S[i,j]>=barrier){
        V[i,j] <- 0
      }
      else if(type=="UI" && S[i,j]<=barrier){
        
        V[i,j] <- 0
      # V[i, nCols] = max( 0, (cp * (S[i, nCols]-K))
      }
    } 
  }
  print(V)
  return(V[nCols,1])
}
  
BinomialTreeExotic(isCall=TRUE,K=10,Tm =.3 ,S0 =10 ,sig = .2,N =200,r=.01,type = "UO",barrier = 11)
BinomialTreeOld(isCall=FALSE,K=100,Tm =1 ,S0 =100 ,sig = .2,N =200,r=.06)
BinomialTreeOld(isCall=TRUE,isAmerican = TRUE,K=100,Tm =1 ,S0 =100 ,sig = .2,N =200,r=.06)
BinomialTreeOld(isCall=FALSE,isAmerican = TRUE,K=100,Tm =1 ,S0 =100 ,sig = .2,N =200,r=.06)




#Binomial Tree using multiplicative method----
ExoticBinomialNew = function(isCall, isAmerican=FALSE, K, Tm, 
                        S0, r, sig, N,div=0,type,barrier=100)
{
  # Precompute constants ----
  dt = Tm/N
  nu=r-div-0.5*sig*sig
  dxu=sqrt(sig*sig*dt+((nu*dt)^2))
  dxd=-dxu
  pu=0.5+0.5*(nu*dt/dxu)
  pd=1-pu
  disc=exp(-r*dt)
  
  dpu=disc*pu
  dpd=disc*pd
  edxud=exp(dxu-dxd)
  edxd=exp(dxd)
  
  #to check if its a call or a put
  cp = ifelse(isCall, 1, -1) 
  
  #setting an initial matrix
  S = V = matrix(0, nrow=N+1, ncol=1)
  
  #setting the last up move of the stock at state N
  S[1]=S0*exp(N*dxd) 
  
  for(i in 1:N+1){
    S[i]=S[i-1]*edxud
  }
  
  #calculating the payoff at maturity
  for (j in 1:N+1) {
    V[j] = max( 0, cp * (S[j]-K))
    if (type=="UO" && S[j]>=barrier)
      V[j]=0
    else if (type=="UI" && S[j]<=barrierLevel)
      V[j]=0
  }
  # for (j in 1:N+1) {
  #   if(isCall)
  #     V[j]=max(0.0,S[j]-K)
  #   else if(!isCall)
  #     V[j]=max(0.0,K-S[j])
  #   
  #   if (type=="UO" && S[j]>=barrier)
  #     V[j]=0
  #   else if (type=="DO" && S[j]<=barrier)
  #     V[j]=0
  # }
  for (i in seq(from=N, to=1, by=-1)){
    for( j in 1:i){
      V[j]=disc*(pu*V[j+1]+pd*V[j])
      S[j]=S[j]/edxd
      if(isAmerican) {
        V[j] = max( V[j], cp * (S[j]-K))
      if (type=="UO" && S[j]>=barrier)
        V[j]=0
      else if (type=="UI" && S[j]<=barrierLevel)
        V[j]=0
      }
    }
    print(V)
  }
  # for (i in seq(from=N, to=1, by=-1)){
      
  return(V[1])
}

ExoticBinomialNew(isCall=TRUE,K=10,Tm =.3 ,S0 =10 ,sig = .2,N =5,r=.01,type = "UO",barrier = 11)
BinomialTree(isCall=FALSE,K=100,Tm =1 ,S0 =100 ,sig = .2,N =200,r=.06)
BinomialTree(isCall=TRUE,isAmerican = TRUE,K=100,Tm =1 ,S0 =100 ,sig = .2,N =200,r=.06)
BinomialTree(isCall=FALSE,isAmerican = TRUE,K=100,Tm =1 ,S0 =100 ,sig = .2,N =200,r=.06)


#Black sholes merton pricing function----
BSM<-function(S, K, t, r, sigma,type){
  d1 <- (log(S/K)+(r+sigma^2/2)*t)/(sigma*sqrt(t))
  d2 <- d1 - sigma * sqrt(t)
  if (type == "c")
    result <- S*pnorm(d1) - K*exp(-r*t)*pnorm(d2)
  if (type == "p")
    result <- K*exp(-r*t) * pnorm(-d2) - S*pnorm(-d1)
  return(result)
}




#Exotic using analytical----
ExoticAnalytical<-function(S0,Tm,K,div=0,r,isCall=TRUE,sig,barrier,type){
  lamda=(r-div+sig*sig/2)/(sig*sig)
  y=log(barrier*barrier/(S0*strike))/(sig*sqrt(Tm))+lamda*sig*sqrt(Tm)
  
  x1=log(S0/barrier)/(sig*sqrt(Tm)+lamda*sig*sqrt(Tm))
  y1=log(barrier/S0)/(sig*sqrt(Tm)+lamda*sig*sqrt(Tm))
  
  cui=(S0*pnorm(x1)*exp(-div*Tm)-K*exp(-r*Tm)*pnorm(x1-sig*sqrt(Tm)))
  -S0*exp(-div*Tm)*((barrier/S0)^(2*lamda))*(pnorm(-y)-pnorm(-y1))
  +K*exp(-r*Tm)*((barrier/S0)^(2*lamda-2))*(pnorm(-y+sig*sqrt(Tm)) -pnorm(-y1+sig*sqrt(Tm))) 
  
  cuo= BSM(S=S0,K=K,t=Tm,r=r,sigma=sig,type="c")-cui
  if(type=="UO")
    return(cuo)
  else if(type=="UI")
    return(cui)
  
}
ExoticAnalytical(S0=stockPrice,T=.3,K=strike,div=div,r=interestRate,
                 sig=volatility,isCall=TRUE,barrier=barrierLevel,type="UO")

stockPrice=10.0
strike=10.0
T=0.3
interestRate=0.01
volatility=.20
barrierLevel=11.0
div=0

