library(flipsideR) #to get the option-chain from yahoo
library(quantmod)#to get the stock prices
library(rCharts)
library(gsubfn) #for multiple returns from functions

# Black sholes pricing forumala
QuestionA<-function(S, K, t, r, sigma,type){
  d1 <- (log(S/K)+(r+sigma^2/2)*t)/(sigma*sqrt(t))
  d2 <- d1 - sigma * sqrt(t)
  if (type == "c")
    result <- S*pnorm(d1) - K*exp(-r*t)*pnorm(d2)
  if (type == "p")
    result <- K*exp(-r*t) * pnorm(-d2) - S*pnorm(-d1)
  return(result)
}


QuestionA(S=100,K=100,t=30/252,r=.05,sigma=.2,type="p")

QuestionB<-function(S, K, t, r, sigma,type){
  call <- QuestionA(S,K,t,r,sigma,type="c")
  put <- QuestionA(S,K,t,r,sigma,type="p")
  
  print("Call - Put = So - Ke^(rt)")
  print(paste(call," - ",put, " = ",S," - ",(K*exp(r*t))))

  if(abs((abs(call-put)-abs(S-((K*exp(r*t))))))<.01)
    print("Put-Call Parity holds")
}


#correct bisection method -but its not giving me the answer :-(
QuestionC_impliedVol<-function(S, K, t, r, type, option_price, max.iter=100000)
{
  sigma.upper <- 2
  sigma.lower <- 0.001
  sigma.mid <- .5
  count <- 0
  fun.mid <- QuestionA(S=S,K=K,t=t,r=r,sigma=sigma.mid,type=type)- option_price
  start.time <- Sys.time()
  while(abs(fun.mid) > 0.0001 && count<max.iter){
    
    fun.upper=QuestionA(S=S,K=K,t=t,r=r,sigma=sigma.upper,type=type)-option_price
    fun.lower=QuestionA(S=S,K=K,t=t,r=r,sigma=sigma.lower,type=type)-option_price
    fun.mid=QuestionA(S=S,K=K,t=t,r=r,sigma=sigma.mid,type=type)-option_price

    if(fun.mid*fun.lower < 0){
      sigma.upper <-sigma.mid
      sigma.mid <- (sigma.upper + sigma.lower)/2
    }else{
      sigma.lower<- sigma.mid
      sigma.mid  <- (sigma.lower + sigma.upper)/2
    }
    count <- count + 1
  }
  print(sigma.mid)
  end.time <- Sys.time()
  time.taken <- end.time - start.time
  print(time.taken)
  if(count>=max.iter){
    return(list(NA,time.taken,count))
  }else{
    print(count)
    return(list(sigma.mid,time.taken,count))
  }
}



b <- QuestionC_impliedVol(135.355,140,30/365,(1.70511/100),"c",option_price = .93,max.iter =100000) #real IV=16.94


a<- QuestionC_impliedVol(134.97,120,2/365,(1.70511/100),"c",15,max.iter =100000) #iv shouldbe=72.27



#done on 2/19/2017
QuestionC<-function(symbol){
  stock_df<-as.data.frame(getSymbols(symbol,from = as.Date("2017-01-01"), env = NULL))

  option_chain <-option_chain_csv

  libor <-.05/100
  iv <- {}
  original_iv <-{}
  optionName <-{}
  strike <-{}
  days_till_expiry <-{}
  time.taken <- 0
  iterations <- 0
  for (i in 1:nrow(option_chain)) 
  {
    try({
      bisection <- QuestionC_impliedVol(
        S = as.numeric(tail(stock_df,1)[6]),
        K = as.numeric(option_chain[i,"Strike"]),
        t = as.numeric(option_chain[i,"days_till_expiry"])/252,
        r = libor,
        type = ifelse((option_chain[i,"Type"]=="Call"), "c", "p"),
        option_price = as.numeric(option_chain[i,"premium"]))
       
      iv <- append(iv,as.numeric(bisection[1]))
      
      if(!is.na(bisection[1])){
        time.taken <- as.numeric(bisection[2])+time.taken
        print(paste("count=",bisection[3]))
        iterations <- as.numeric(bisection[3])+iterations
      }
      
      
      strike<-append(strike,as.numeric(option_chain[i,"Strike"]))
      
      optionName <- append(optionName,paste(option_chain[i,"Strike"],"-",
                                            option_chain[i,"Type"],"Expiring On:",
                                            option_chain[i,"Expiry"]))
      days_till_expiry <- append(days_till_expiry,as.numeric(option_chain[i,"days_till_expiry"]))
      
      original_iv <- append(original_iv,(option_chain[i,"Implied.Volatility"]))
    })
  }
  option_chain_df <- data.frame(days_till_expiry,optionName,iv,strike,original_iv)
  names(option_chain_df)<-c("Days_till_Expiry","variable","Implied_Volatility","strike","original_iv")
  time.taken <- time.taken/as.numeric(colSums(!is.na(option_chain_df))[3])
  iterations <- iterations/as.numeric(colSums(!is.na(option_chain_df))[3])
  return(list(option_chain_df,time.taken,iterations))
  # return(option_chain_df)
}



QuestionD_Secant <- function(S, K, t, r, type, option_price
                             , x0=0.1, x1=3, tol=1e-07, max.iter=10000){
  x1=3
  theta=.00001
  fun.x1=QuestionA(S=S,K=K,t=t,r=r,sigma=x1,type=type)-option_price
  count=1
  start.time <- Sys.time()
  while(abs(fun.x1) > 0.0001 && count<max.iter) {
    x2=x1-theta
    fun.x1=QuestionA(S=S,K=K,t=t,r=r,sigma=x1,type=type)-option_price
    fun.x2=QuestionA(S=S,K=K,t=t,r=r,sigma=x2,type=type)-option_price
    x1 <- x1- fun.x1/((fun.x1-fun.x2)/theta)   
    print(paste("x1=",x1))
    count <-count+1
  }
  end.time <- Sys.time()
  time.taken <- end.time - start.time
  if(x2<0 || count>=max.iter)
    return(list(NA,time.taken,count))
  else
    return(list(x2,time.taken,count))
}


#old one
QuestionD<-function(symbol){
  stock_df<-as.data.frame(getSymbols(symbol,from = as.Date("2017-01-01"), env = NULL))
  option_chain <-option_chain_csv
  
  print(head(option_chain))
  # option_chain$days_till_expiry <- as.Date(option_chain$expiry)-Sys.Date()
  libor <-.05/100
  iv <- {}
  original_iv <-{}
  optionName <-{}
  strike <-{}
  days_till_expiry <-{}
  for (i in 1:nrow(option_chain)) 
  {
    try({
      iv <-append(iv,100*QuestionD_Secant(
        S = as.numeric(tail(stock_df,1)[6]),
        K = as.numeric(option_chain[i,"Strike"]),
        t = as.numeric(option_chain[i,"days_till_expiry"])/252,
        r = libor,
        type = ifelse((option_chain[i,"Type"]=="Call"), "c", "p"),
        option_price = as.numeric(option_chain[i,"premium"])))
      
      strike<-append(strike,as.numeric(option_chain[i,"Strike"]))
      
      optionName <- append(optionName,paste(option_chain[i,"Strike"],"-",
                                            option_chain[i,"Type"],"Expiring On:",
                                            option_chain[i,"Expiry"]))
      days_till_expiry <- append(days_till_expiry,as.numeric(option_chain[i,"days_till_expiry"]))
      
      original_iv <- append(original_iv,(option_chain[i,"Implied.Volatility"]))
    })
  }
  option_chain_df <- data.frame(days_till_expiry,optionName,iv,strike,original_iv)
  names(option_chain_df)<-c("Days_till_Expiry","variable","Implied_Volatility","strike","original_iv")
  return(option_chain_df)
}

QuestionD<-function(symbol){
  stock_df<-as.data.frame(getSymbols(symbol,from = as.Date("2017-01-01"), env = NULL))
  
  option_chain <-option_chain_csv
  
  libor <-.05/100
  iv <- {}
  original_iv <-{}
  optionName <-{}
  strike <-{}
  days_till_expiry <-{}
  time.taken <- 0
  iterations <- 0
  for (i in 1:nrow(option_chain)) 
  {
    try({
      secant <- QuestionD_Secant(
        S = as.numeric(tail(stock_df,1)[6]),
        K = as.numeric(option_chain[i,"Strike"]),
        t = as.numeric(option_chain[i,"days_till_expiry"])/252,
        r = libor,
        type = ifelse((option_chain[i,"Type"]=="Call"), "c", "p"),
        option_price = as.numeric(option_chain[i,"premium"]))
      
      iv <- append(iv,as.numeric(secant[1]))
      
      if(!is.na(secant[1])){
        time.taken <- as.numeric(secant[2])+time.taken
        print(paste("count=",secant[3]))
        iterations <- as.numeric(secant[3])+iterations
      }
      
      
      strike<-append(strike,as.numeric(option_chain[i,"Strike"]))
      
      optionName <- append(optionName,paste(option_chain[i,"Strike"],"-",
                                            option_chain[i,"Type"],"Expiring On:",
                                            option_chain[i,"Expiry"]))
      days_till_expiry <- append(days_till_expiry,as.numeric(option_chain[i,"days_till_expiry"]))
      
      original_iv <- append(original_iv,(option_chain[i,"Implied.Volatility"]))
    })
  }
  option_chain_df <- data.frame(days_till_expiry,optionName,iv,strike,original_iv)
  names(option_chain_df)<-c("Days_till_Expiry","variable","Implied_Volatility","strike","original_iv")
  time.taken <- time.taken/as.numeric(colSums(!is.na(option_chain_df))[3])
  iterations <- iterations/as.numeric(colSums(!is.na(option_chain_df))[3])
  return(list(option_chain_df,time.taken,iterations))
  # return(option_chain_df)
}

#MSFT works
option_chain <- flipsideR::getOptionChain("FB")
  summary(option_chain)
y=QuestionD("AAPL")
options_IV <- y[1]
options_IV <- data.frame(options_IV)
#-----
library(rCharts)
h1=hPlot(Implied_Volatility ~ strike, data =options_IV, type ='line',
         group = 'Days_till_Expiry')
# h1$legend(enabled=FALSE)
h1$chart(zoomType="xy")
h1

h2=hPlot(Implied.Volatility ~ Strike, data =option_chain_csv, type ='line')
h2

 #highcharts for 2d plot
#----
library(rgl)

plot3d(x=options_IV$Days_till_Expiry,
       y=options_IV$Implied_Volatility,
       z=options_IV$strike) #3d scatter plot
#----


#----
library(plot3D)
optionsIV <- options_IV[complete.cases(options_IV),]
library(akima)
s=interp(optionsIV$Days_till_Expiry,
         optionsIV$Implied_Volatility,
         optionsIV$strike)
surface3d(s$x,s$y,s$z)
#--- Surface plot #3d surface plot
greeks=QuestionF(S=100,K=100,t=2/252,r=.05,sigma=.2,type="c")

QuestionF<-function(S, K, t, r, sigma,type)
{
 
  d1 <- (log(S/K)+(r+sigma^2/2)*t)/(sigma*sqrt(t))
  d2 <- d1 - sigma * sqrt(t)
#Delta----
  if (type == "c") 
    delta = exp((r)*t)*pnorm(d1)
  if (type == "p")
    delta = exp((r)*t)*(pnorm(d1)-1)
  
#Gamma---- 

  gamma = exp((r)*t)*dnorm(d1)/(S*sigma*sqrt(t)) # Call,Put


#Theta----      

    
  Theta1 = -(S*exp((r)*t)*dnorm(d1)*sigma)/(2*sqrt(t))
  if (type == "c") 
    theta = Theta1 -
    (r)*S*exp((r)*t)*pnorm(+d1) - r*K*exp(-r*t)*pnorm(+d2)
  if (type == "p")
    theta = Theta1 +
    (r)*S*exp((r)*t)*pnorm(-d1) + r*K*exp(-r*t)*pnorm(-d2)


#Vega----

  vega = S*exp((r)*t)*dnorm(d1)*sqrt(t) # Call,Put
  
  return(list(delta,gamma,theta,vega))

}

option_chain_csv <- read.csv(file="call.csv",header=TRUE, sep=",")
option_chain_csv$days_till_expiry <- as.Date(option_chain_csv$Expiry,"%m/%d/%Y")-Sys.Date()

option_chain_csv$premium<-(option_chain_csv$Bid+option_chain_csv$Ask)/2
option_chain_csv$Implied.Volatility<- as.numeric(sub("%","",option_chain_csv$Implied.Volatility))


