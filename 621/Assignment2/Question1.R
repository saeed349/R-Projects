library(quantmod)
library(fOptions)
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
  # print(V)
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
BinomialTree = function(isCall, isAmerican=FALSE, K=100, Tm=1, 
                      S0=100, r=0.06, sig=0.2, N=3, u=1.1, d=1/u,div=0)
{
  # dt = Tm/N
  # nu=r-div-0.5*sig*sig 
  # dxu=sqrt(sig*sig*dt+((nu*dt)^2))
  # dxd=-dxu
  # pu=0.5+0.5*(nu*dt/dxu)
  # pd=1-pu
  # disc=exp(-r*dt)
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
      S[i-1, j+1] = S[i, j] * u
      S[i+1, j+1] = S[i, j] * d
      # S[i-1, j+1] = S[i, j]*exp(dxu)
      # S[i+1, j+1] = S[i, j] *exp(dxd)
    }
  }
  for (i in 1:nRows) {
    V[i, nCols] = max( 0, cp * (S[i, nCols]-K))
  }
  # print(S)
  # V
  # Step backwards through the tree ----
  for (j in (nCols-1):1) {
    for(i in (nCols-j+1):(nCols+j-1)) {
      V[i, j] = disc * (p*V[i-1,j+1] + (1-p)*V[i+1,j+1])
      # V[i, j] = disc * (pu*V[i-1,j+1] + pd*V[i+1,j+1])
      if(isAmerican) {
        # if american option, then take the Value at each node as the max of the
        # value of option or the payoff at that period
        V[i, j] = max(V[i, j], cp * (S[i, j] - K))
      }
    } 
  }
  #Returning all the calculated values as a list.
  # list(Type = paste( ifelse(isAmerican, "American", "European"), 
  #                    ifelse(isCall, "Call", "Put")),
  #      Price = V[nCols,1], S=round(S,2), V=round(V,4))
  return(V[nCols,1])
}

#Black sholes merton pricing function
BSM<-function(S, K, t, r, sigma,type){
  d1 <- (log(S/K)+(r+sigma^2/2)*t)/(sigma*sqrt(t))
  d2 <- d1 - sigma * sqrt(t)
  if (type == "c")
    result <- S*pnorm(d1) - K*exp(-r*t)*pnorm(d2)
  if (type == "p")
    result <- K*exp(-r*t) * pnorm(-d2) - S*pnorm(-d1)
  return(result)
}

#Implementation of secant method that was used in assignment 1
Secant <- function(S, K, t, r, type, option_price
                             , x0=0.1, x1=3, tolerance=1e-07, max.iter=10000){
  x1=3
  theta=.00001
  fun.x1=BSM(S=S,K=K,t=t,r=r,sigma=x1,type=type)-option_price
  count=1
  start.time <- Sys.time()
  while(abs(fun.x1) > tolerance && count<max.iter) {
    x2=x1-theta
    fun.x1=BSM(S=S,K=K,t=t,r=r,sigma=x1,type=type)-option_price
    fun.x2=BSM(S=S,K=K,t=t,r=r,sigma=x2,type=type)-option_price
    x1 <- x1- fun.x1/((fun.x1-fun.x2)/theta)   
    count <-count+1
  }
  end.time <- Sys.time()
  time.taken <- end.time - start.time
  if(x2<0 || count>=max.iter)
    return(list(NA,time.taken,count))
  else
    return(list(x2,time.taken,count))
}


Secant_Jacob <- function(S, K, t, r, type, option_price
                                  , x0=0.1, x1=3, tolerance=1e-07, max.iter=10000){

  error=1e-04
  iter=1
  volGuess=1.0
  deltavol=1e-04
  BSMvolGuess=1000.0
  volLower=0.0
  volMid=0.5
  # startime
  while(abs(BSMvolGuess)>error){
    if(is.infinite(volGuess)){
      print(paste("volGuess_temp=",volGuess_temp,
                  "BSMvolGuess_temp=",BSMvolGuess_temp,
                  "volLower_temp=",volLower_temp,
                  "BSMvolGuess_temp=",BSMvolGuess_temp,
                  "BSMvolLower_temp=",BSMvolLower_temp))
    }
    BSMvolGuess= BSM(S=S,K=K,t=t,r=r,sigma=volGuess,type=type)-option_price
    # t=volGuess
    volLower=volGuess - deltavol
    BSMvolLower= BSM(S=S,K=K,t=t,r=r,sigma=volLower,type=type)-option_price
    
    #Testing out by printing----
    volGuess_temp=volGuess
    BSMvolGuess_temp=BSMvolGuess
    volLower_temp=volLower
    BSMvolGuess_temp=BSMvolGuess
    BSMvolLower_temp=BSMvolLower
    #----
    if(BSMvolGuess==BSMvolLower){
      print("-------------------SSSSSSSSSSSSSS------")
      # volLower=volGuess + 1e-1
      volLower=runif(1, min=0, max=1)
      volGuess=runif(1, min=0, max=1)
      BSMvolLower= BSM(S=S,K=K,t=t,r=r,sigma=volLower,type=type)-option_price
      print(paste("volGuess=",volGuess,
                  "BSMvolGuess=",BSMvolGuess,
                  "volLower=",volLower,
                  "BSMvolGuess=",BSMvolGuess,
                  "BSMvolLower=",BSMvolLower))
      print("=======================================")
    }
    volGuess=volGuess-BSMvolGuess/((BSMvolGuess-BSMvolLower)/(volGuess-volLower))
    
    iter=iter+1
    if(iter == 5000){
      return (volGuess)
    }
    if(is.na(BSMvolGuess)){
      # print(paste("volguess previous=","volGuess=",volGuess))
      # print(BSM(S=S,K=K,t=t,r=r,sigma=volGuess,type=type))
      # print(paste("BSMvolGuess",BSMvolGuess))
    }
    
  }
  return (volGuess)
}

Secant_Jacob(S=100,K=100,t=30/252,r=.05,type='c',option_price = 3.051184)

#Calculating IV on the option chain using secant method
ImpliedVol_Secantfunction<-function(symbol="AAPL",option_chain=option_chain_csv,rate=.75/100){
  symbol="AAPL"
  stock_df<-as.data.frame(getSymbols(symbol,from = as.Date("2017-01-01"),to=as.Date("2017-02-19"), env = NULL))

  iv <- {}
  original_iv <-{}
  optionName <-{}
  strike <-{}
  days_till_expiry <-{}
  time.taken <- 0
  iterations <- 0
  type <-{}
  bid<-{}
  ask<-{}
  for (i in 1:nrow(option_chain)) 
  {
    try({
       secant <- Secant(
       S = as.numeric(tail(stock_df,1)[6]),
       K = as.numeric(option_chain[i,"Strike"]),
       t = as.numeric(option_chain[i,"days_till_expiry"])/252,
       r = rate,
       type = ifelse((option_chain[i,"Type"]=="Call"), "c", "p"),
       option_price = as.numeric(option_chain[i,"premium"]))

       iv <- append(iv,as.numeric(secant[1]))

       if(!is.na(secant[1])){
        time.taken <- as.numeric(secant[2])+time.taken
        iterations <- as.numeric(secant[3])+iterations
       }
      
      #FOptions Implementation ----
      # ivol <- GBSVolatility(option_chain[i,"premium"],
      #         ifelse((option_chain[i,"Type"]=="Call"), "c", "p"), S = as.numeric(tail(stock_df,1)[6]),
      #         X = as.numeric(option_chain[i,"Strike"]),
      #         Time = as.numeric(option_chain[i,"days_till_expiry"])/252,
      #         r = rate, b=0, maxiter = 1000)
      # iv <- append(iv,as.numeric(ivol))
      # time.taken <- as.numeric(0)
      # iterations <- as.numeric(0)

      
      #Inspired from Jacob chetta----
      # secant <- Secant_Jacob(
      #   S = as.numeric(tail(stock_df,1)[6]),
      #   K = as.numeric(option_chain[i,"Strike"]),
      #   t = as.numeric(option_chain[i,"days_till_expiry"])/252,
      #   r = rate,
      #   type = ifelse((option_chain[i,"Type"]=="Call"), "c", "p"),
      #   option_price = as.numeric(option_chain[i,"premium"]))
      # 
      # iv <- append(iv,as.numeric(secant[1]))
      # time.taken <- as.numeric(0)
      # iterations <- as.numeric(0)
      #----
      
      
      type <- append(type,as.character(option_chain[i,"Type"]))
      
      strike<-append(strike,as.numeric(option_chain[i,"Strike"]))
      
      optionName <- append(optionName,paste(option_chain[i,"Strike"],"-",
                                            option_chain[i,"Type"],"Expiring On:",
                                            option_chain[i,"Expiry"]))
      days_till_expiry <- append(days_till_expiry,as.numeric(option_chain[i,"days_till_expiry"]))
      
      bid<-append(bid,as.numeric(option_chain[i,"Bid"]))
      ask<-append(ask,as.numeric(option_chain[i,"Ask"]))
      
      # original_iv <- append(original_iv,as.numeric(option_chain[i,"Implied.Volatility"]))
    })
  }
  option_chain_df <- data.frame(days_till_expiry,type,optionName,iv,strike,bid,ask)
  names(option_chain_df)<-c("Days_till_Expiry","Type","Specification","Implied_Volatility","Strike","Bid","Ask")
  time.taken <- time.taken/as.numeric(colSums(!is.na(option_chain_df))[3])
  iterations <- iterations/as.numeric(colSums(!is.na(option_chain_df))[3])
  list(option_chain_df,time.taken,iterations)
}

BinomialTree(isCall=T, isAmerican=F)


#Importing and using the option data ----
#Importing the option chain downloaded from yahoo finance
option_chain_csv <- read.csv(file="call.csv",header=TRUE, sep=",")
#call.csv and put.csv are available in the project folder
option_chain_csv$days_till_expiry <- as.Date(option_chain_csv$Expiry,"%m/%d/%Y")-as.Date("2017-02-19")
#Calculating the days till expiry

option_chain_csv$premium<-(option_chain_csv$Bid+option_chain_csv$Ask)/2

head(option_chain_csv)


#----
jacob_secant
foption_secant
assignment1_secant


df=ImpliedVol_Secantfunction()
df=as.data.frame(df[1])
# option.df=complete.cases(df[,Implied_Volatility])
options.df=df[complete.cases(df$Implied_Volatility),]

BSM(S=100,K=100,t=30/252,r=.75/100,sigma=.2,type="c")

bin_price=BinomialTree(isCall=TRUE,K=100,Tm =30/252 ,S0 =100 ,sig = .2,N =200,r=.06,u=1.01)
# BinomialTree(isCall=T, isAmerican=F)
# Clewlow2_3n5(isCall=F, isAmerican=T)

#
QuestionC<-function(options.df){
  eur_binomial <-{}
  eur_bsm <-{}
  ame_binomial <-{}
  ame_bsm <-{}
  
  for (i in 1:nrow(options.df)){
    stock_df<-as.data.frame(getSymbols("AAPL",from = as.Date("2017-01-01"),
                                       to=as.Date("2017-02-19"), env = NULL))
    eur_binomial <- append(eur_binomial,BinomialTree(
                       isCall = as.logical(options.df[i,"Type"]=="Call"),
                           K  = as.numeric(options.df[i,"Strike"]),
                           Tm = as.numeric(options.df[i,"Days_till_Expiry"]),
                           S0 = as.numeric(tail(stock_df,1)[6]),
                          sig = as.numeric(options.df[i,"Implied_Volatility"]),
                           r  = .75/100,
                            N = 200))
    # print(eur_binomial)
                           
  }
  return(eur_binomial)
  
}
  
df <- QuestionC(options.df)
