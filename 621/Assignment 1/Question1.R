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


QuestionA(S=100,K=100,t=30/252,r=.05,sigma=.2,type='c')

QuestionB<-function(S, K, t, r, sigma){
  call <- QuestionA(S,K,t,r,sigma,type="c")
  put <- QuestionA(S,K,t,r,sigma,type="p")
  
  print("Call - Put = So - Ke^(rt)")
  print(paste(call," - ",put, " = ",S," - ",(K*exp(r*t))))

  if(abs((abs(call-put)-abs(S-((K*exp(r*t))))))<.01)
    print("Put-Call Parity holds")
}



option_chain_csv <- read.csv(file="put.csv",header=TRUE, sep=",")
option_chain_csv$days_till_expiry <- as.Date(option_chain_csv$Expiry,"%m/%d/%Y")-Sys.Date()

option_chain_csv$premium<-(option_chain_csv$Bid+option_chain_csv$Ask)/2
option_chain_csv$Implied.Volatility<- as.numeric(sub("%","",option_chain_csv$Implied.Volatility))


#Implementation of bisection method
QuestionC_Bisection<-function(S, K, t, r, type, option_price, max.iter=100000,tolerance=.0001)
{
  sigma.upper <- 2
  sigma.lower <- 0.001
  sigma.mid <- .5
  count <- 0
  fun.mid <- QuestionA(S=S,K=K,t=t,r=r,sigma=sigma.mid,type=type)- option_price
  start.time <- Sys.time()
  while(abs(fun.mid) > tolerance && count<max.iter){
    
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
  end.time <- Sys.time()
  time.taken <- end.time - start.time
  if(count>=max.iter){
    return(list(NA,time.taken,count))
  }else{
    return(list(sigma.mid,time.taken,count))
  }
}


b <- QuestionC_Bisection(135.355,140,30/365,(1.70511/100),"c",option_price = .93,max.iter =100000) #real IV=16.94


a<- QuestionC_impliedVol(134.97,120,2/365,(1.70511/100),"c",15,max.iter =100000) #iv shouldbe=72.27


#Calculating IV on the option chain using bisection method
QuestionC<-function(symbol="AAPL"){
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
      bisection <- QuestionC_Bisection(
        S = as.numeric(tail(stock_df,1)[6]),
        K = as.numeric(option_chain[i,"Strike"]),
        t = as.numeric(option_chain[i,"days_till_expiry"])/252,
        r = libor,
        type = ifelse((option_chain[i,"Type"]=="Call"), "c", "p"),
        option_price = as.numeric(option_chain[i,"premium"]))
       
      iv <- append(iv,as.numeric(bisection[1]))
      
      if(!is.na(bisection[1])){
        time.taken <- as.numeric(bisection[2])+time.taken
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
}


#Implementation of secant method
QuestionD_Secant <- function(S, K, t, r, type, option_price
                             , x0=0.1, x1=3, tolerance=1e-07, max.iter=10000){
  x1=3
  theta=.00001
  fun.x1=QuestionA(S=S,K=K,t=t,r=r,sigma=x1,type=type)-option_price
  count=1
  start.time <- Sys.time()
  while(abs(fun.x1) > tolerance && count<max.iter) {
    x2=x1-theta
    fun.x1=QuestionA(S=S,K=K,t=t,r=r,sigma=x1,type=type)-option_price
    fun.x2=QuestionA(S=S,K=K,t=t,r=r,sigma=x2,type=type)-option_price
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
QuestionD<-function(symbol="AAPL"){
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
  type <-{}
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
        iterations <- as.numeric(secant[3])+iterations
      }
      
      type <- append(type,as.character(option_chain[i,"Type"]))
      
      strike<-append(strike,as.numeric(option_chain[i,"Strike"]))
      
      optionName <- append(optionName,paste(option_chain[i,"Strike"],"-",
                                            option_chain[i,"Type"],"Expiring On:",
                                            option_chain[i,"Expiry"]))
      days_till_expiry <- append(days_till_expiry,as.numeric(option_chain[i,"days_till_expiry"]))
      
      original_iv <- append(original_iv,(option_chain[i,"Implied.Volatility"]))
    })
  }
  option_chain_df <- data.frame(days_till_expiry,type,optionName,iv,strike,original_iv)
  names(option_chain_df)<-c("Days_till_Expiry","Type","Specification","Implied_Volatility","strike","original_iv")
  time.taken <- time.taken/as.numeric(colSums(!is.na(option_chain_df))[3])
  iterations <- iterations/as.numeric(colSums(!is.na(option_chain_df))[3])
  return(list(option_chain_df,time.taken,iterations))
}




y=QuestionD("AAPL")
options.data <- y[1]
options.data <- data.frame(options.data)
options.data <- options_IV[complete.cases(options.data$Implied_Volatility),]
#-----
library(rCharts)
h1=hPlot(Implied_Volatility ~ strike, data =options.data, type ='line',
         group = 'Days_till_Expiry')
# h1$legend(enabled=FALSE)
h1$chart(zoomType="xy")
h1

h1=hPlot(original_iv ~ strike, data =options.data, type ='line',
         group = 'Days_till_Expiry')
h1

#highcharts for 2d plot
#----
library(rgl)

plot3d(x=options_IV$Days_till_Expiry,
       y=options_IV$Implied_Volatility,
       z=options_IV$strike) #3d scatter plot
#----


#----
library(plot3D)
x=options.data

library(akima)
s=interp(options.data$Days_till_Expiry,
         options.data$Implied_Volatility,
         options.data$strike)
surface3d(s$x,s$y,s$z)
#--- Surface plot #3d surface plot



#greeks----


QuestionF_PDE<-function(S, K, t, r, sigma,type)
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


QuestionF_gamma<-function(S, K, t, r, sigma,type)
{
  gamma <- (QuestionF_delta(S+1, K, t, r, sigma,type)-QuestionF_delta(S, K, t, r, sigma,type))/10
}

QuestionF_delta<-function(S, K, t, r, sigma,type)
{
  delta <- (QuestionA(S*1.0001, K, t, r, sigma,type)-QuestionA(S, K, t, r, sigma,type))/
    (S*1.0001-S)
  return(delta)
  
}

QuestionF_vega<-function(S, K, t, r, sigma,type)
{
  vega <- (QuestionA(S, K, t, r, sigma*1.01,type)-QuestionA(S, K, t, r, sigma,type))/
    (sigma*1.01-sigma)
  return(abs(vega))
}

#Q-G----

QuestiongG<-function(options.data)
{

  delta <-{}
  gamma <-{}
  vega <-{}
  libor <- .05/100
  stock_df<-as.data.frame(getSymbols("AAPL",from = as.Date("2017-01-01"), env = NULL))
  for (i in 1:nrow(options.data)) 
  {

    greeks<-QuestionF_PDE(
        S = as.numeric(tail(stock_df,1)[6]),
        K = as.numeric(options.data[i,"strike"]),
        t = as.numeric(options.data[i,"Days_till_Expiry"])/252,
        r = libor,
     type = ifelse((options.data[i,"Type"]=="Call"), "c", "p"),
    sigma = as.numeric(options.data[i,"Implied_Volatility"]))
    print(paste("Greeks=",greeks))

    delta <- append(delta, greeks[1])
    gamma <- append(gamma,greeks[2])
    vega <- append(vega, greeks[4])
    
  }
  options.data$Delta <-delta
  options.data$Gamma <-gamma
  options.data$Vega <-vega
  return(options.data)
}

options.data.greeks <- QuestiongG(data.frame(options.data))


