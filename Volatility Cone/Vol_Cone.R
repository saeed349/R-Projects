cone_call<-function(symbol)
{
  library(quantmod)
  library(reshape2)
  library(ggplot2)
  library(flipsideR) #library to get the option chain
  library(zoo)
  library(xts)
  library(fOptions)
  library(rCharts) #to plot the nvd3 plot
  
  # symbol="AMZN"
  
  stock_df<-as.data.frame(getSymbols(as.character(symbol),from = as.Date("2016-01-01"),env = NULL))
  
  time_friday=options.expiry(as.xts(stock_df))
  
  stock=stock_df[,6];
  
  stock.ret <- diff(log(stock), lag = 1)
  stock.ret <- stock.ret[-1]
  
  stdev=sd(stock.ret)
  ann_vol=stdev*sqrt(252)
  
  
  temp.max={}
  temp.min={}
  temp.mean={}
  temp.stdev={}
  temp.1sd.upper={}
  temp.1sd.lower={}
  temp.2sd.upper={}
  temp.2sd.lower={}
  
  
  duration=seq(150,1,-1)
  for(n in duration)
  {
    temp.ann_vol={}
    for(i in time_friday)
    {
      if((i-n)>0)
      {
        temp.price=stock[(i-n):i]
        temp.ret <- diff(log(temp.price), lag = 1)
        temp.ret <- temp.ret[-1]
        temp.ann_vol=append(temp.ann_vol,sd(temp.ret)*sqrt(252))
      }
    }
    
    temp.max=append(temp.max,(max(temp.ann_vol)*100))
    temp.min=append(temp.min,(min(temp.ann_vol)*100))
    temp.mean=append(temp.mean,(mean(temp.ann_vol)*100))
    temp.stdev=sd(temp.ann_vol)
    temp.1sd.upper=append(temp.1sd.upper,((tail(temp.mean,1))+(temp.stdev*100)))
    temp.1sd.lower=append(temp.1sd.lower,((tail(temp.mean,1))-(temp.stdev*100)))
    temp.2sd.upper=append(temp.2sd.upper,((tail(temp.mean,1))+(2*temp.stdev*100)))
    temp.2sd.lower=append(temp.2sd.lower,((tail(temp.mean,1))-(2*temp.stdev*100)))
  }
  vol_cone=data.frame(duration,temp.max,temp.min,temp.mean,temp.1sd.lower,temp.1sd.upper
                      ,temp.2sd.lower,temp.2sd.upper)
  names(vol_cone)=c('Days_till_Expiry','Max','Min','Mean','first_sd_lower','first_sd_upper',
                    'second_sd_lower','second_sd_upper')
  na.omit(vol_cone)
  # for plotting in ggplot, making it into a single column frame
  melted_cone=melt(vol_cone,id.vars ="Days_till_Expiry")
  print(melted_cone)
  
  option_chain = getOptionChain(symbol)
  head(option_chain)
  
  option_chain$days_till_expiry=as.Date(option_chain$expiry)-Sys.Date()
  
  libor=1.70511/100
  
  iv={}
  optionName={}
  days_till_expiry={}
  
  for (i in 1:nrow(option_chain)) 
  {
    if(as.numeric(option_chain[i,"days_till_expiry"])<120)
    {
      try(
        {
          iv=append(iv,100*GBSVolatility(option_chain[i,"premium"],
                                         TypeFlag = ifelse((option_chain[i,"type"]=="Call"), "c", "p"),
                                         S = as.numeric(tail(stock_df,1)[6]), 
                                         X =option_chain[i,"strike"],
                                         Time = as.numeric(option_chain[i,"days_till_expiry"])/245,
                                         r = libor, b=0, maxiter = 1000))
          optionName=append(optionName,paste(option_chain[i,"strike"],"-",
                                             option_chain[i,"type"],"Expiring On:",
                                             option_chain[i,"expiry"],".Price=",
                                             option_chain[i,"premium"]))
          days_till_expiry=append(days_till_expiry,as.numeric(option_chain[i,"days_till_expiry"]))
        }
      )
      
      
    }
    
    
  }
  
  option_chain_df=data.frame(days_till_expiry,optionName,iv)
  names(option_chain_df)<-c("Days_till_Expiry","variable","value")
  
  
  melted_cone=melt(vol_cone,id.vars ="Days_till_Expiry")
  
  # return(vol_cone)
  do.call(rbind.data.frame, option_chain_df)
  do.call(rbind.data.frame, melted_cone)
  
  test_df=merge(melted_cone,option_chain_df,all=TRUE)
  
  h1=hPlot(value ~ Days_till_Expiry, data =test_df, type ='line', 
           group = 'variable')
  
  h1$legend(enabled=FALSE)
  h1$chart(zoomType="x")
  h1
}


# fit = tryCatch(
#   GBSVolatility(option_chain$premium[3],
#                         TypeFlag = ifelse((option_chain$type[3]=="Call"), "c", "p"),
#                         S = 0, 
#                         X =option_chain$strike[3],
#                         Time = as.numeric(option_chain$days_till_expiry[3])/365,
#                         r = libor, b=0, maxiter = 1000))

# if(is(fit, "warning")) {
#   return(1)
# } else {
#   #forecasting 20 days ahead
#   return(24)
# }
# fit
