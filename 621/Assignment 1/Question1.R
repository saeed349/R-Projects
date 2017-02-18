library(flipsideR) #to get the option-chain from yahoo
library(quantmod)#to get the stock prices
library(rCharts)

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



#incorrect bisection method
QuestionC_impliedVol<-function(S, K, t, r, type, option_price,max.iter){
  sigma.mid<- 0.2
  sigma.upper <- 1
  sigma.lower <- 0.0001
  count <- 0
  error <-QuestionA(S, K, t, r, sigma.mid, type) - option_price  
    
  while(abs(error) > 0.0001 && count<max.iter){
    if(error < 0){
      sigma.lower <-sigma.mid
      sigma.mid <- (sigma.upper + sigma.mid)/2
    }else
    {
      sigma.upper<- sigma.mid
      sigma.mid  <- (sigma.lower + sigma.mid )/2
    }
    error <- QuestionA(S, K, t, r, sigma.mid, type) - option_price
    print(paste("Error-",error))
    print(paste("Count-",count))
    count <- count + 1
    
    # if(is.na((abs(error) > 0.0001)))
    #   break
  }
  print(sigma.mid)
  if(count>=max.iter){
    return(NA)
  }else{
    return(sigma.mid)
  }
}

#correct bisection method -but its not giving me the answer :-(
QuestionC_impliedVol<-function(S, K, t, r, type, option_price, max.iter)
{
  sigma.upper <- 3
  sigma.lower <- 0.0001
  sigma.mid <- .5
  count <- 0
  fun.mid <- QuestionA(S=S,K=K,t=t,r=r,sigma=sigma.mid,type=type)- option_price
  
  while(abs(sigma.mid) > 0.0001 && count<max.iter){
    
    fun.upper=QuestionA(S=S,K=K,t=t,r=r,sigma=sigma.upper,type=type)-option_price
    fun.lower=QuestionA(S=S,K=K,t=t,r=r,sigma=sigma.lower,type=type)-option_price
    fun.mid=QuestionA(S=S,K=K,t=t,r=r,sigma=sigma.mid,type=type)-option_price
    
    # print(paste(fun.upper,fun.mid, fun.lower))
    
    # if(count==10)
    #   break
    if(fun.mid*fun.lower < 0){
      sigma.upper <-sigma.mid
      sigma.mid <- (sigma.upper + sigma.lower)/2
      # print(paste(sigma.mid,sigma.upper))
    }else{
      sigma.lower<- sigma.mid
      sigma.mid  <- (sigma.lower + sigma.upper)/2
      # print(paste(sigma.lower,sigma.mid))
    }
    count <- count + 1
  }
  print(sigma.mid)
  if(count>=max.iter){
    return(NA)
  }else{
    return(sigma.mid)
  }
}

QuestionC_impliedVol(S=100,K=100,t=2/252,r=.05,type="p",option_price = .69100,max.iter =10000)

QuestionC_impliedVol(135.72,128,5/252,(1.70511/100),"c",7.55,max.iter =100000) #IV=27.25

test<-function(sigma)
{
  QuestionA(S=135.72,K=128,t=5/252,r=(.70511/100),sigma,type="c")-7.55 
}
curve(test)

QuestionC_impliedVol(134.97,120,2/365,(1.70511/100),"c",15,max.iter =100000) #iv shouldbe=72.27

library(fOptions)

libor=1.70511/100
GBSVolatility(15,"c", S = 134.97, X =120,
                t = 2/365,
                r = libor, b=0, maxiter = 1000)

QuestionC<-function(symbol){
  stock_df<-as.data.frame(getSymbols(symbol,from = as.Date("2017-01-01"), env = NULL))
  option_chain <- flipsideR::getOptionChain(symbol)
  print(head(option_chain))
  option_chain$days_till_expiry <- as.Date(option_chain$expiry)-Sys.Date()
  libor <-.70511/100
  iv <- {}
  optionName <-{}
  strike <-{}
  days_till_expiry <-{}
  for (i in 1:nrow(option_chain)) 
  {
    # if(as.numeric(option_chain[i,"days_till_expiry"])<120)
    # {
    try({
      iv <-append(iv,100*QuestionC_impliedVol(
                        S = as.numeric(tail(stock_df,1)[6]),
                        K = option_chain[i,"strike"],
                        t = as.numeric(option_chain[i,"days_till_expiry"])/252,
                        r = libor,
                     type = ifelse((option_chain[i,"type"]=="Call"), "c", "p"),
             option_price = option_chain[i,"premium"],
                 max.iter = 10000))
      
      # iv <-append(iv,100*GBSVolatility(option_chain[i,"premium"],
      #                                  ifelse((option_chain[i,"type"]=="Call"), "c", "p"), S = as.numeric(tail(stock_df,1)[6]),
      #                                  X =option_chain[i,"strike"],
      #                                  t = as.numeric(option_chain[i,"days_till_expiry"])/252,
      #                                  r = libor, b=0, maxiter = 1000))
      
      
      # function(S, K, T, r, market, type)
      # iv <-append(iv,100*implied.vol(
      #     S = as.numeric(tail(stock_df,1)[6]),
      #     K = option_chain[i,"strike"],
      #     T = as.numeric(option_chain[i,"days_till_expiry"])/252,
      #     r = libor,
      #     type = ifelse((option_chain[i,"type"]=="Call"), "c", "p"),
      #     market = option_chain[i,"premium"]))
      strike<-append(strike,as.numeric(option_chain[i,"strike"]))
        
      optionName <- append(optionName,paste(option_chain[i,"strike"],"-",
                                         option_chain[i,"type"],"Expiring On:",
                                         option_chain[i,"expiry"]))
      days_till_expiry <- append(days_till_expiry,as.numeric(option_chain[i,"days_till_expiry"]))
    # }
    })
    
    
  }
  option_chain_df <- data.frame(days_till_expiry,optionName,iv,strike)
  names(option_chain_df)<-c("Days_till_Expiry","variable","Implied_Volatility","strike")
  return(option_chain_df)
}



{
  # QuestionD_fun<-function(S,K,t,r,sigma,type='c',option_price){
  #   # option_price=3.051184
  #   BS <- QuestionA(S=S,K=K,t=t,r=r,sigma,type=type)
  #   option_price-BS
  # }
  # curve(QuestionD, xlim=c(-1,1), col='blue')
  # abline(h=0)
  # abline(v=0)
  # 
  # require(NLRoot)
  # SMfzero(Implied_Vol_function, 0, 1)
} #testing 


#1st working
QuestionD_Secant <- function(S, K, t, r, type, option_price
                   , x0=0.1, x1=3, tol=1e-07, maxiter=500){
  for ( i in 1:maxiter ) {
    fun.x1=QuestionA(S=S,K=K,t=t,r=r,sigma=x1,type=type)-option_price
    fun.x0=QuestionA(S=S,K=K,t=t,r=r,sigma=x0,type=type)-option_price
    x2 <- x1-((fun.x1*(x1-x0))/(fun.x1-fun.x0))
    fun.x2=QuestionA(S=S,K=K,t=t,r=r,sigma=x2,type=type)-option_price
    print(paste("x2=",x2," fun.x2=",fun.x2))
    if (abs(fun.x2) < tol)
      return(x2)
    x0 <- x1
    x1 <- x2
  }
  # stop("Exceeded Maximum number of iteractions")
  return(x2)
}

#working 
QuestionD_Secant <- function(S, K, t, r, type, option_price
                             , x0=0.1, x1=3, tol=1e-07, max.iter=500){
  x1=.9
  theta=.0000001
  
  fun.x1=QuestionA(S=S,K=K,t=t,r=r,sigma=x1,type=type)-option_price
  print(fun.x1)
  while(abs(fun.x1) > 0.0001 && count<max.iter) {
    x2=x1-theta
    fun.x1=QuestionA(S=S,K=K,t=t,r=r,sigma=x1,type=type)-option_price
    fun.x2=QuestionA(S=S,K=K,t=t,r=r,sigma=x2,type=type)-option_price
    x1 <- x1- fun.x1/((fun.x1-fun.x2)/theta)   
    print(paste("x1=",x1))
    # fun.x1=QuestionA(S=S,K=K,t=t,r=r,sigma=x1,type=type)-option_price
    # fun.x2=QuestionA(S=S,K=K,t=t,r=r,sigma=x2,type=type)-option_price
    # print(paste("x2=",x2," fun.x2=",fun.x2))
    # if (abs(fun.x1) < tol)
    #   break
    # x0 <- x1
    # x1 <- x2
    count <-count+1
  }
  # stop("Exceeded Maximum number of iteractions")
  return(x2)
}


#Not working 
QuestionD_Secant <-function(S, K, t, r, type, option_price,
                          tol=1e-07, maxiter=500){
  x0=1
  theta=.1
  x1=x0-theta
  for ( i in 1:maxiter ) {
    fun.x0=QuestionA(S=S,K=K,t=t,r=r,sigma=x0,type=type)
    fun.x1=QuestionA(S=S,K=K,t=t,r=r,sigma=x1,type=type)
    dx=(fun.x1-fun.x0)/theta
    if(abs(dx)<tol)
      break
    x0=x0-(option_price-fun.x0)/dx
  }
  return(x0)
}


QuestionD_Secant(S=100,K=100,t=30/252,r=.05,type='c',option_price=3.5)
QuestionD_Secant(S=134.97,K=120,t=30/265,r=(2.7011/100),type='c',option_price=15)
{
  # secant2 <- function(fun, x0, x1, tol=1e-07, niter=500){
  #   for ( i in 1:niter ) {
  #     # x2<-(x1-fun(x1) * (fun(x1)-fun(x0)))/(x1-x0)
  #     # x2 <- x1-fun(x1)*(x1-x0)/(fun(x1)-fun(x0))
  #     x2 <- x1 - fun(x1) / ((fun(x1) - fun(x0)) / (x1 - x0))
  #     print(paste("x0=",x0," x1=",x1," x2=",x2))
  #     print(paste("-------x0=",fun(x0)," x1=",fun(x1)," x2=",fun(x2)))
  #     print("")
  #     # if (abs(fun(x2)) < tol)
  #     if(abs(x2 - x1) < tol)
  #       return(x2)
  #     x0 <- x1
  #     x1 <- x2
  #   }
  #   stop("exceeded allowed number of iteractions")
  # }
  # fun<-function(sigma){
  #   option_price=15
  #   # BS<-QuestionA(S=100,K=100,t=30/252,r=.05,sigma,type='c')
  #   BS <- QuestionA(S=134.97,K=120,t=2/365,r=(1.70511/100),sigma,type='c')
  #   return 
  # }
  
}


QuestionD<-function(symbol){
  stock_df<-as.data.frame(getSymbols(symbol,from = as.Date("2017-01-01"), env = NULL))
  option_chain <- flipsideR::getOptionChain(symbol)
  print(head(option_chain))
  option_chain$days_till_expiry <- as.Date(option_chain$expiry)-Sys.Date()
  libor <-1.70511/100
  iv <- {}
  optionName <-{}
  days_till_expiry <-{}
  for (i in 1:nrow(option_chain)) 
  {
    try({
      iv <-append(iv,100*QuestionD_Secant(
        S = as.numeric(tail(stock_df,1)[6]),
        K = option_chain[i,"strike"],
        t = as.numeric(option_chain[i,"days_till_expiry"])/252,
        r = libor,
        type = ifelse((option_chain[i,"type"]=="Call"), "c", "p"),
        option_price = option_chain[i,"premium"]))
      
      
      optionName <- append(optionName,paste(option_chain[i,"strike"],"-",
                                            option_chain[i,"type"],"Expiring On:",
                                            option_chain[i,"expiry"]))
      days_till_expiry <- append(days_till_expiry,as.numeric(option_chain[i,"days_till_expiry"]))
    })
  }
  option_chain_df <- data.frame(days_till_expiry,optionName,iv)
  names(option_chain_df)<-c("Days_till_Expiry","variable","value")
  return(option_chain_df)
}


options_IV <- y
#-----
library(rCharts)
h1=hPlot(Implied_Volatility ~ strike, data =options_IV, type ='line',
         group = 'Days_till_Expiry')
# h1$legend(enabled=FALSE)
h1$chart(zoomType="xy")
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


