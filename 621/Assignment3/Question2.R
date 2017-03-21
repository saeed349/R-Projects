# Question2.A
# Retrieving Data and Calculating the Implied Volatility 
  # Calculation for Implied Volatility
    # Black sholes merton pricing function ----
library(quantmod)
BSM<-function(S, K, t, r, sigma,type){
  d1 <- (log(S/K)+(r+sigma^2/2)*t)/(sigma*sqrt(t))
  d2 <- d1 - sigma * sqrt(t)
  if (type == "c")
    result <- S*pnorm(d1) - K*exp(-r*t)*pnorm(d2)
  if (type == "p")
    result <- K*exp(-r*t) * pnorm(-d2) - S*pnorm(-d1)
  return(result)
}
    # Secant function ----
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
      type <- append(type,as.character(option_chain[i,"Type"]))
      
      strike<-append(strike,as.numeric(option_chain[i,"Strike"]))
      
      optionName <- append(optionName,paste(option_chain[i,"Strike"],"-",
                                            option_chain[i,"Type"],"Expiring On:",
                                            option_chain[i,"Expiry"]))
      days_till_expiry <- append(days_till_expiry,as.numeric(option_chain[i,"days_till_expiry"]))
      
      bid<-append(bid,as.numeric(option_chain[i,"Bid"]))
      ask<-append(ask,as.numeric(option_chain[i,"Ask"]))
    })
  }
  option_chain_df <- data.frame(days_till_expiry,type,optionName,iv,strike,bid,ask)
  names(option_chain_df)<-c("Days_till_Expiry","Type","Specification","Implied_Volatility","Strike","Bid","Ask")
  time.taken <- time.taken/as.numeric(colSums(!is.na(option_chain_df))[3])
  iterations <- iterations/as.numeric(colSums(!is.na(option_chain_df))[3])
  list(option_chain_df,time.taken,iterations)
}    
  # Retrieving data from CSV and formating data ----
option_chain_call_csv <- read.csv(file="call.csv",header=TRUE, sep=",")
option_chain_put_csv <-read.csv(file="put.csv",header=TRUE, sep=",")
#Call
option_chain_call_csv$days_till_expiry <- as.Date(option_chain_call_csv$Expiry,"%m/%d/%Y")-as.Date("2017-02-19")
#Put
option_chain_put_csv$days_till_expiry <- as.Date(option_chain_put_csv$Expiry,"%Y/%m/%d")-as.Date("2017-02-19")
#Calculating the days till expiry
option_chain_call_csv$premium<-(option_chain_call_csv$Bid+option_chain_call_csv$Ask)/2

option_chain_put_csv$premium<-(option_chain_put_csv$Bid+option_chain_put_csv$Ask)/2

df=ImpliedVol_Secant(option_chain = option_chain_call_csv)
df=as.data.frame(df[1])
options.df_call=df[complete.cases(df$Implied_Volatility),]

df=ImpliedVol_Secant(option_chain = option_chain_put_csv)
df=as.data.frame(df[1])
options.df_put=df[complete.cases(df$Implied_Volatility),]




# Question2.B
# Calculating option price and dx
PriceCalculator<-function(options.df,show=FALSE){
  explicit.price <-{}
  implicit.price <-{}
  cranknic.price <-{}
  delta_explicit <-{}
  gamma_explicit <-{}
  theta_explicit <-{}
  vega_explicit  <-{}
  stock_df<-as.data.frame(getSymbols("AAPL",from = as.Date("2017-01-01"),
                                     to=as.Date("2017-02-19"), env = NULL))
  # Calculating dx,N for explicit and implicit based on the error ----
  for (i in 1:nrow(options.df)){
    dt=0
    dx=0
    N=3 #Initial arbitary value
    Tm = as.numeric(options.df[i,"Days_till_Expiry"])/252
    sig = as.numeric(options.df[i,"Implied_Volatility"])
    nsd=3
    error=3
    repeat{
      dt=Tm/N
      # Nj=((nsd*sig*sqrt(Tm)) / (2*(error-(Tm/N))) ) - .5 # Doesnt work
      Nj=(sqrt(N)*nsd)/(2*sqrt(3)) -.5
      dx=(nsd*sig*sqrt(Tm))/(2*Nj+1)
      # print(paste(dt,Nj,dx))
      N=N+1
      if(((dx*dx)+dt)<=error){
        # print(paste("ANSWER:",dt,Nj,dx,N))
        # return(list(dt,Nj,dx,N))
        break
      }
    }
    
    # Calculating the option price using Explicit method ----
    explicit = Explicit(isAmerican = FALSE,
                            isCall = as.logical(options.df[i,"Type"]=="Call"),
                                K  = as.numeric(options.df[i,"Strike"]),
                                Tm = as.numeric(options.df[i,"Days_till_Expiry"])/252,
                                S0 = as.numeric(tail(stock_df,1)[6]),
                               sig = as.numeric(options.df[i,"Implied_Volatility"]),
                                r  = .75/100,
                               div = 0,
                                dx = dx,
                                 N = N,
                      returnGreeks = TRUE)
    
    explicit.price <- append(explicit.price,explicit$Price)
    delta_explicit = append(delta_explicit,explicit$Delta)
    gamma_explicit = append(gamma_explicit,explicit$Gamma)
    theta_explicit = append(theta_explicit,explicit$Theta)
    
    sigma=as.numeric(options.df[i,"Implied_Volatility"])
    dsigma = .001* sigma
    Vega = Explicit(isAmerican = FALSE,
                    isCall = as.logical(options.df[i,"Type"]=="Call"),
                    K  = as.numeric(options.df[i,"Strike"]),
                    Tm = as.numeric(options.df[i,"Days_till_Expiry"])/252,
                    S0 = as.numeric(tail(stock_df,1)[6]),
                    sig = sigma+dsigma,
                    r  = .75/100,
                    div = 0,
                    dx = dx,
                    N = N) -
          Explicit(isAmerican = FALSE,
                   isCall = as.logical(options.df[i,"Type"]=="Call"),
                   K  = as.numeric(options.df[i,"Strike"]),
                   Tm = as.numeric(options.df[i,"Days_till_Expiry"])/252,
                   S0 = as.numeric(tail(stock_df,1)[6]),
                   sig = sigma-dsigma,
                   r  = .75/100,
                   div = 0,
                   dx = dx,
                   N = N)
    vega_explicit = append(vega_explicit,Vega)
    
    # Calculating the option price using Implicit method ----
    implicit.price <- append(implicit.price,Implicit(
      isAmerican = FALSE,
      isCall = as.logical(options.df[i,"Type"]=="Call"),
      K  = as.numeric(options.df[i,"Strike"]),
      Tm = as.numeric(options.df[i,"Days_till_Expiry"])/252,
      S0 = as.numeric(tail(stock_df,1)[6]),
      sig = as.numeric(options.df[i,"Implied_Volatility"]),
      r  = .75/100,
      div = 0,
      dx = dx,
      N = N)$Price)
    
    
    #calculating dx and N using convergence condition for crank nicholson----
    dt=0
    dx=0
    N=3 #Initial arbitary value
    Tm = as.numeric(options.df[i,"Days_till_Expiry"])/252
    sig = as.numeric(options.df[i,"Implied_Volatility"])
    nsd=3
    error=3
    repeat{
      dt=Tm/N
      # Nj=((nsd*sig*sqrt(Tm)) / (2*(error-(Tm/N))) ) - .5 # Doesnt work
      Nj=(sqrt(N)*nsd)/(2*sqrt(3)) -.5
      dx=(nsd*sig*sqrt(Tm))/(2*Nj+1)
      # print(paste(dt,Nj,dx))
      N=N+1
      if(((dx*dx)+(dt/2))<=error){
        # print(paste("ANSWER:",dt,Nj,dx,N))
        # return(list(dt,Nj,dx,N))
        break
      }
    }
    
    # Calculating the option price using Cranknicholson method ----
    cranknic.price <- append(cranknic.price,CrankNicholson(
      isAmerican = FALSE,
      isCall = as.logical(options.df[i,"Type"]=="Call"),
      K  = as.numeric(options.df[i,"Strike"]),
      Tm = as.numeric(options.df[i,"Days_till_Expiry"])/252,
      S0 = as.numeric(tail(stock_df,1)[6]),
      sig = as.numeric(options.df[i,"Implied_Volatility"]),
      r  = .75/100,
      div = 0,
      dx = dx,
      N = N)$Price)

  }
  options.test = data.frame(explicit.price,implicit.price,cranknic.price,options.df$Bid,
                            delta_explicit,gamma_explicit,vega_explicit)
  print(options.test)
  options.df$Explicit.price <- explicit.price
  options.df$Implicit.price <- implicit.price
  options.df$CrankNicholson.price <- cranknic.price
  options.df$Delta.explicit = delta_explicit
  options.df$Gamma.explicit = gamma_explicit
  options.df$Theta.explicit = theta_explicit
  options.df$Vega.explicit  = vega_explicit
  if(show){
    head(options.df)
    return(options.df)
  }
  else
    return(options.df)
}

price=PriceCalculator(options.df_call,show=FALSE)
option_chain_call <-PriceCalculator(options.df_call)
option_chain_put <-PriceCalculator(options.df_put)
