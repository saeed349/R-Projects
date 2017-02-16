library(flipsideR) #to get the option-chain from yahoo
library(quantmod)#to get the stock prices


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


QuestionB(S=100,K=100,t=30/252,r=.05,sigma=.2,type="c")

QuestionB<-function(S, K, t, r, sigma,type){
  call <- QuestionA(S,K,t,r,sigma,type="c")
  put <- QuestionA(S,K,t,r,sigma,type="p")
  
  print("Call - Put = So - Ke^(rt)")
  print(paste(call," - ",put, " = ",S," - ",(K*exp(r*t))))

  if(abs((abs(call-put)-abs(S-((K*exp(r*t))))))<.01)
    print("Put-Call Parity holds")
}



#Calculating the implied volatility
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



QuestionC_impliedVol(100,100,1,.1,"c",3.051184,max.iter =10000 )



QuestionC_impliedVol(134.97,120,2/365,(1.70511/100),"c",15,max.iter =100000) #iv shouldbe=72.27

library(fOptions)

libor=1.70511/100
GBSVolatility(15,"c", S = 134.97, X =120,
                Time = 2/365,
                r = libor, b=0, maxiter = 1000)

QuestionC<-function(symbol){
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
    # if(as.numeric(option_chain[i,"days_till_expiry"])<120)
    # {
      iv <-append(iv,100*QuestionC_impliedVol(
                        S = as.numeric(tail(stock_df,1)[6]),
                        K = option_chain[i,"strike"],
                        t = as.numeric(option_chain[i,"days_till_expiry"])/252,
                        r = libor,
                     type = ifelse((option_chain[i,"type"]=="Call"), "c", "p"),
             option_price = option_chain[i,"premium"],
                 max.iter = 1000))
      
      # iv <-append(iv,100*GBSVolatility(option_chain[i,"premium"],
      #                                  ifelse((option_chain[i,"type"]=="Call"), "c", "p"), S = as.numeric(tail(stock_df,1)[6]),
      #                                  X =option_chain[i,"strike"],
      #                                  Time = as.numeric(option_chain[i,"days_till_expiry"])/252,
      #                                  r = libor, b=0, maxiter = 1000))
      
      
      # function(S, K, T, r, market, type)
      # iv <-append(iv,100*implied.vol(
      #     S = as.numeric(tail(stock_df,1)[6]),
      #     K = option_chain[i,"strike"],
      #     T = as.numeric(option_chain[i,"days_till_expiry"])/252,
      #     r = libor,
      #     type = ifelse((option_chain[i,"type"]=="Call"), "c", "p"),
      #     market = option_chain[i,"premium"]))      
        
      optionName <- append(optionName,paste(option_chain[i,"strike"],"-",
                                         option_chain[i,"type"],"Expiring On:",
                                         option_chain[i,"expiry"]))
      days_till_expiry <- append(days_till_expiry,as.numeric(option_chain[i,"days_till_expiry"]))
    # }
    
    
  }
  option_chain_df <- data.frame(days_till_expiry,optionName,iv)
  names(option_chain_df)<-c("Days_till_Expiry","variable","value")
  return(option_chain_df)
}

#----this below code works
# implied.vol <-
#   function(S, K, T, r, market, type){
#     sig <- 0.20
#     sig.up <- 1
#     sig.down <- 0.001
#     count <- 0
#     err <- QuestionA(S, K, T, r, sig, type) - market #(S, K, t, r, sigma,type)
# 
#     ## repeat until error is sufficiently small or counter hits 1000
#     while(abs(err) > 0.00001 && count<1000){
#       if(err < 0){
#         sig.down <- sig
#         sig <- (sig.up + sig)/2
#       }else{
#         sig.up <- sig
#         sig <- (sig.down + sig)/2
#       }
#       err <- QuestionA(S, K, T, r, sig, type) - market
#       count <- count + 1
#     }
# 
#     ## return NA if counter hit 1000
#     if(count==1000){
#       return(NA)
#     }else{
#       return(sig)
#     }
#   }





