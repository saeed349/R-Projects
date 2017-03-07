library(quantmod)
library(fOptions)
library(rCharts)
library(reshape)

#Binomial Tree using multiplicative method----
BinomialTree = function(isCall, isAmerican=FALSE, K, Tm, 
                      S0, r, sig, N,div=0)
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
  }
  
  for (i in seq(from=N, to=1, by=-1)){
    for( j in 1:i){
      V[j]=disc*(pu*V[j+1]+pd*V[j])
      S[j]=S[j]/edxd
      if(isAmerican) {
        V[j] = max( V[j], cp * (S[j]-K))
      }
    }
  }
  return(V[1])
}

BinomialTree(isCall=TRUE,K=100,Tm =1 ,S0 =100 ,sig = .2,N =200,r=.06)
BinomialTree(isCall=FALSE,K=100,Tm =1 ,S0 =100 ,sig = .2,N =200,r=.06)
BinomialTree(isCall=TRUE,isAmerican = TRUE,K=100,Tm =1 ,S0 =100 ,sig = .2,N =200,r=.06)
BinomialTree(isCall=FALSE,isAmerican = TRUE,K=100,Tm =1 ,S0 =100 ,sig = .2,N =200,r=.06)

#----

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


#Calculating Implied Vol----
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

#Jacob chettans secant method----
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

#-----


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

head(option_chain_csv)


#Plotting the values as per question B----



df=0
df=ImpliedVol_Secant(option_chain = option_chain_csv)
df=as.data.frame(df[1])
# option.df=complete.cases(df[,Implied_Volatility])
options.df=df[complete.cases(df$Implied_Volatility),]

QuestionC<-function(options.df){
  eur_binomial <-{}
  eur_bsm <-{}
  ame_binomial <-{}
  stock_df<-as.data.frame(getSymbols("AAPL",from = as.Date("2017-01-01"),
                                     to=as.Date("2017-02-19"), env = NULL))
  for (i in 1:nrow(options.df)){
    eur_binomial <- append(eur_binomial,BinomialTree(
                       isCall = as.logical(options.df[i,"Type"]=="Call"),
                           K  = as.numeric(options.df[i,"Strike"]),
                           Tm = as.numeric(options.df[i,"Days_till_Expiry"])/252,
                           S0 = as.numeric(tail(stock_df,1)[6]),
                          sig = as.numeric(options.df[i,"Implied_Volatility"]),
                           r  = .75/100,
                            N = 200))
    ame_binomial <- append(ame_binomial,BinomialTree(
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


df_melted <- melt(df,id.vars=c("Days_till_Expiry","Strike"), 
                  measure.vars = c("BinomialEuropean","BlackSholesMertonEuropean",
                                   "BinomialAmerican","Bid","Ask"))

h2=hPlot( BinomialAmerican ~ Strike, data =df, type ='line',
         group = 'Days_till_Expiry')
h2

h1=hPlot(value ~ Strike, data =df_melted, type ='line',
          group = 'variable')
# h1$legend(enabled=FALSE)
h1$chart(zoomType="xy")
h1

h3=hPlot( BlackSholesMertonEuropean ~ Strike, data =df, type ='line',
          group = 'Days_till_Expiry')
h3
# h1$show('iframesrc',cdn=TRUE)
# h1$print(include_assets = TRUE)


#----QuestionD -Error Comparison
QuestionD<-function(){
  bsm_price <- {}
  binomial_price <-{}
  error <-{}
  iter <-c(10, 20, 30, 40, 50, 100, 150, 200, 250, 300, 350, 400)
  for(i in iter){
    binomial_temp <-BinomialTree(isCall=FALSE,K=100,Tm =1 ,S0 =100 ,sig = .2,N =i,r=.06)
    bsm_temp <-BSM(S=100,K=100,t=1,r=.06,sigma=.2,type="p")
    binomial_price <- append(binomial_price,binomial_temp)
    bsm_price <- append(bsm_price,bsm_temp)
    error <- append(error,bsm_temp-binomial_temp)
  }
  print(error)
  plot(iter,error)
}
QuestionD()

#BonusQuestion----

Secant_Binomial <- function(S, K, t, r, type, option_price
                   , x0=0.1, x1=3, tolerance=1e-07, max.iter=10000){
  x1=3
  theta=.00001
  fun.x1=BinomialTree(isCall=is.logical(type=="Call"),K=K,Tm =t ,S0 =S ,sig = x1,N =200,r=r)-option_price
  count=1
  start.time <- Sys.time()
  while(abs(fun.x1) > tolerance && count<max.iter) {
    x2=x1-theta
    fun.x1=BinomialTree(isCall=is.logical(type=="Call"),K=K,Tm =t ,S0 =S ,sig = x1,N =200,r=r)-option_price
    fun.x2=BinomialTree(isCall=is.logical(type=="Call"),K=K,Tm =t ,S0 =S ,sig = x2,N =200,r=r)-option_price
    x1 <- x1- fun.x1/((fun.x1-fun.x2)/theta)   
    count <-count+1
    print(count)
  }
  end.time <- Sys.time()
  time.taken <- end.time - start.time
  if(x2<0 || count>=max.iter)
    return(list(NA,time.taken,count))
  else
    return(list(x2,time.taken,count))
}
ImpliedVol_Secant_Binomial<-function(symbol="AAPL",option_chain,rate=.75/100){
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
      secant <- Secant_Binomial(
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
      
      # original_iv <- append(original_iv,as.numeric(option_chain[i,"Implied.Volatility"]))
    })
  }
  option_chain_df <- data.frame(days_till_expiry,type,optionName,iv,strike,bid,ask)
  names(option_chain_df)<-c("Days_till_Expiry","Type","Specification","Implied_Volatility","Strike","Bid","Ask")
  time.taken <- time.taken/as.numeric(colSums(!is.na(option_chain_df))[3])
  iterations <- iterations/as.numeric(colSums(!is.na(option_chain_df))[3])
  list(option_chain_df,time.taken,iterations)
}

df=ImpliedVol_Secant_Binomial(option_chain = option_chain_csv)
df=as.data.frame(df[1])
options.df=df[complete.cases(df$Implied_Volatility),]
