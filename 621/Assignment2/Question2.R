library(quantmod)
library(rCharts)
library(reshape)

TrinomialTree = function(isCall, isAmerican=FALSE, K, Tm, 
                      S0, r, sig, N, div=0, dx=0)
{
  # Precompute constants ----
  dt = Tm/N 
  nu = r - div - 0.5 * sig^2
  if(dx==0)
    dx=sig*sqrt(3*T/N)
  pu = 0.5 * ( (sig^2*dt + nu^2 *dt^2)/dx^2 + nu*dt/dx ) #up move probability
  pm = 1.0 -   (sig^2*dt + nu^2 *dt^2)/dx^2 #Side move probability
  pd = 0.5 * ( (sig^2*dt + nu^2 *dt^2)/dx^2 - nu*dt/dx ) # down move probability.
  #pu+pm+pd not necessarily equal to 1
  disc = exp(-r*dt) #Discount rate
  nRows = 2*N+1 #number of rows 
  nCols = N+1 #number of columns
  cp = ifelse(isCall, 1, -1) #to check if call or put
  # Intialize an empty matrix  ----
  V = S = matrix(0, nrow=nRows, ncol=nCols, dimnames=list(
    paste("NumUps", N:-N, sep="="), paste("T", 0:N, sep="=")))
  #Initial stock value
  S[nCols, 1] = S0
  #Fill in the stock matrix with the stock values over different states
  for (j in 1:N) {
    for(i in (nCols-j+1):(nCols+j-1)) {
      S[i-1, j+1] = S[i, j] * exp(dx)
      S[i ,  j+1] = S[i, j] 
      S[i+1, j+1] = S[i, j] * exp(-dx)
    }
  }
  # Intialize option values at maturity ----
  for (i in 1:nRows) {
    V[i, N+1] = max( 0, cp * (S[i, N+1]-K))
  }
  V
  # Step backwards through the tree ----
  for (j in (nCols-1):1) {
    for(i in (nCols-j+1):(nCols+j-1)) {
      #converging from N to N-1 state diagonally
      V[i, j] = disc * (pu*V[i-1,j+1] + pm*V[i, j+1] + pd*V[i+1,j+1])
      if(isAmerican) {
        V[i, j] = max(V[i, j], cp * (S[i, j] - K))
      }
    }
  }
  V
  # Return the value
  # list(Type = paste( ifelse(isAmerican, "American", "European"), 
  #                    ifelse(isCall, "Call", "Put")),
  #      Price = V[1,1], S=round(S,2), V=round(V,4))
  return(V[N+1,1])
}


TrinomialTree(isCall=T, isAmerican=F, K=100, T=1.0, div=0.03, S0=100, sig=0.2, r=0.06, N=200)
TrinomialTree(isCall=T, isAmerican=T, K=100, T=1.0, div=0.03, S0=100, sig=0.2, r=0.06, N=200)
TrinomialTree(isCall=F, isAmerican=F, K=100, T=1.0, div=0.03, S0=100, sig=0.2, r=0.06, N=200)
TrinomialTree(isCall=F, isAmerican=T, K=100, T=1.0, div=0.03, S0=100, sig=0.2, r=0.06, N=200)

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

#Calculating the IV----
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

#Calculating IV on the option chain using secant method
ImpliedVol_Secant<-function(symbol="AAPL",option_chain,rate=.75/100){
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
      #Myoldmethod----
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


#Importing and using the option data ----
#Importing the option chain downloaded from yahoo finance
option_chain_csv <- read.csv(file="put.csv",header=TRUE, sep=",")
#call.csv and put.csv are available in the project folder
#IMP to remove #both call and put have different format in the csv
#Call
# option_chain_csv$days_till_expiry <- as.Date(option_chain_csv$Expiry,"%m/%d/%Y")-as.Date("2017-02-19")
#Put
option_chain_csv$days_till_expiry <- as.Date(option_chain_csv$Expiry,"%Y/%m/%d")-as.Date("2017-02-19")
#Calculating the days till expiry
option_chain_csv$premium<-(option_chain_csv$Bid+option_chain_csv$Ask)/2


#Plotting the values as per question B----
df=0
df=ImpliedVol_Secant(option_chain = option_chain_csv)
df=as.data.frame(df[1])
# option.df=complete.cases(df[,Implied_Volatility])
options.df=df[complete.cases(df$Implied_Volatility),]


#QuestionC-Calculating tree on option data----
QuestionC<-function(options.df){
  eur_binomial <-{}
  eur_bsm <-{}
  ame_binomial <-{}
  stock_df<-as.data.frame(getSymbols("AAPL",from = as.Date("2017-01-01"),
                                     to=as.Date("2017-02-19"), env = NULL))
  for (i in 1:nrow(options.df)){
    eur_binomial <- append(eur_binomial,TrinomialTree(
      isCall = as.logical(options.df[i,"Type"]=="Call"),
      K  = as.numeric(options.df[i,"Strike"]),
      Tm = as.numeric(options.df[i,"Days_till_Expiry"])/252,
      S0 = as.numeric(tail(stock_df,1)[6]),
      sig = as.numeric(options.df[i,"Implied_Volatility"]),
      r  = .75/100,
      N = 200))
    ame_binomial <- append(ame_binomial,TrinomialTree(
      isAmerican = TRUE, 
      isCall = as.logical(options.df[i,"Type"]=="Call"),
      K  = as.numeric(options.df[i,"Strike"]),
      Tm = as.numeric(options.df[i,"Days_till_Expiry"])/252,
      S0 = as.numeric(tail(stock_df,1)[6]),
      sig = as.numeric(options.df[i,"Implied_Volatility"]),
      r  = .75/100,
      N = 200))
    eur_bsm <- append(eur_bsm,BSM(
      type = ifelse(options.df[i,"Type"]=="Call",'c','p'),
      K  = as.numeric(options.df[i,"Strike"]),
      t  = as.numeric(options.df[i,"Days_till_Expiry"])/252,
      S  = as.numeric(tail(stock_df,1)[6]),
      sigma = as.numeric(options.df[i,"Implied_Volatility"]),
      r  = .75/100))
    # BSM(S=100,K=100,t=30/252,r=.75/100,sigma=.2,type="c")
    
  }
  options.df$BinomialEuropean <- eur_binomial
  options.df$BinomialAmerican <- ame_binomial
  options.df$BlackSholesMertonEuropean <- eur_bsm
  return(options.df)
  
}

df <- QuestionC(options.df)
df[1]
plot(df$Strike,df$BinomialAmerican)

#QuestionD -Error Comparison----
QuestionD<-function(){
  bsm_price <- {}
  binomial_price <-{}
  error <-{}
  iter <-c(10, 20, 30, 40, 50, 100, 150, 200, 250, 300, 350, 400)
  for(i in iter){
    binomial_temp <-TrinomialTree(isCall=FALSE,K=100,Tm =1 ,S0 =100 ,sig = .2,N =i,r=.06)
    bsm_temp <-BSM(S=100,K=100,t=1,r=.06,sigma=.2,type="p")
    binomial_price <- append(binomial_price,binomial_temp)
    bsm_price <- append(bsm_price,bsm_temp)
    error <- append(error,bsm_temp-binomial_temp)
  }
  print(error)
  plot(iter,error)
}
QuestionD()


# Error using Trinomial Tree= 0.00460599886943669
# Error using Binomial Tree = 0.00460598068031715
