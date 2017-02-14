library(flipsideR) #to get the option-chain from yahoo
library(quantmod) #to get the stock prices


# Black sholes pricing forumala
QuestionA<-function(S, K, t, r, sigma,type)
{
  d1 <- (log(S/K)+(r+sigma^2/2)*t)/(sigma*sqrt(t))
  d2 <- d1 - sigma * sqrt(t)
  if (type == "c")
    result = S*pnorm(d1) - K*exp(-r*t)*pnorm(d2)
  if (type == "p")
    result = K*exp(-r*T) * pnorm(-d2) - S*pnorm(-d1)
  return(result)
}
# blackscholes <- function(S, X, rf, T, sigma) {
#   values <- c(2)
#   
#   
#   
#   values[1] <- S*pnorm(d1) - X*exp(-rf*T)*pnorm(d2)
#   values[2] <- X*exp(-rf*T) * pnorm(-d2) - S*pnorm(-d1)
#   
#   return(values)
# }

# library(fOptions)
# GBSOption(TypeFlag = "c", S = 100, X = 100,
#           Time = 1, r = 0.1, b = 0.1, sigma = 0.25)
# 
# blackscholes(100,100,.1,1,.25)


QuestionA(S=100,K=100,t=1,r=.1,sigma=.25,type="p")

#Calculating the implied volatility
QuestionC_impliedVol<-function(S, K, t, r, type, option_price,max.iter){
  sigma.mid<- 0.50
  sigma.uppper <- 1
  sigma.lower <- 0.001
  count <- 0
  error <-QuestionA(S, K, t, r, sigma.mid, type) - option_price  
    
  while(abs(error) > 0.00001 && max.iter<1000){
    if(error < 0){
      sigma.lower <-sigma.mid
      sigma.mid <- (sigma.upper + sigma.mid)/2
    }else{
      sigma.upper<- sigma.mid
      sigma.mid  <- (sigma.lower + sigma.mid )/2
    }
    error <- QuestionA(S, K, T, r, sigma.mid, type) - option_price
    print(paste("Error-",error))
  }
  print(sigma.mid)
  if(count>=1000){
    return(NA)
  }else{
    return(sigma.mid)
  }
}

QuestionC_impliedVol(100,100,1,.1,"p",5.459533)

QuestionC<-function(symbol){
  stock_df<-as.data.frame(getSymbols(symbol,from = as.Date("2017-01-01"), env = NULL))
  option_chain = getOptionChain(symbol)
  option_chain$days_till_expiry=as.Date(option_chain$expiry)-Sys.Date()
  libor=1.70511/100
  iv={}
  optionName={}
  days_till_expiry={}
  for (i in 1:nrow(option_chain)) 
  {
    if(as.numeric(option_chain[i,"days_till_expiry"])<120)
    {
      # S, K, t, r, type, option_price
      iv=append(iv,100*QuestionC_impliedVol(
                        S = as.numeric(tail(stock_df,1)[6]),
                        K = option_chain[i,"strike"],
                        t = as.numeric(option_chain[i,"days_till_expiry"])/252,
                        r = libor,
                     type = ifelse((option_chain[i,"type"]=="Call"), "c", "p"),
             option_price = option_chain[i,"premium"]))
             
      optionName=append(optionName,paste(option_chain[i,"strike"],"-",
                                         option_chain[i,"type"],"Expiring On:",
                                         option_chain[i,"expiry"]))
      days_till_expiry=append(days_till_expiry,as.numeric(option_chain[i,"days_till_expiry"]))
    }
    
    
  }
  option_chain_df=data.frame(days_till_expiry,optionName,iv)
  names(option_chain_df)<-c("Days_till_Expiry","variable","value")
  return(option_chain_df)
}


