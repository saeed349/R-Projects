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
  
  symbol="AAPL"
  # dataEnv<-new.env()
  # getSymbols(toString(symbol_name),env=dataEnv,from = as.Date("2016-01-01"), to = as.Date("2017-02-01"))
  
  stock_df<-as.data.frame(getSymbols(symbol,from = as.Date("2016-01-01"), env = NULL))
  
  # stock=(getSymbols(toString(symbol_name),from = as.Date("2016-01-01"), to = as.Date("2017-02-01")))
  time_friday=options.expiry(as.xts(stock_df))
  
  stock=stock_df[,6];
  
  # spReturns = diff(log(stock_data))
  # stdev=sd(coredata(spReturns))
  
  
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
    # print((tail(temp.mean,1)))
    # print("-----")
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

  # ggplot(data=vol_cone,aes(x=duatation))
  ggplot(data=melted_cone, aes(x=Days_till_Expiry, y=value, group=variable)) + geom_line()
  # 
  # This brings the option chain with the dates
  # library(flipsideR)
  # symbol="AAPL"
  option_chain = getOptionChain(symbol)
  head(option_chain)
  
  option_chain$days_till_expiry=as.Date(option_chain$expiry)-Sys.Date()
  
  libor=1.70511/100
                                          # GBSVolatility(option_price_P100[i], TypeFlag = "p", S = stock_price[i], X =105,
                                          #               Time = (as.numeric(expiry_date-date[i],units="days"))/365, 
                                          #               r = libor[i]/100, b=0, maxiter = 10000)
                                          # 
  # option_chain$impl_vol=GBSVolatility(option_chain$premium, TypeFlag = ifelse((option_chain$type=="Call"), "c", "p"), 
  #                                     S = as.numeric(tail(stock_df,1)[6]), X =option_chain$strike, Time = as.numeric(option_chain$days_till_expiry)/365,
  #                                     r = libor, b=0, maxiter = 1000)
  # 
  x=GBSVolatility(20, TypeFlag ="c",S = 100, X =90, Time = 60/365,
                  r = libor, b=0, maxiter = 10000)
  vol_cone$Days_till_Expiry=NULL 
  
  vol_cone$option1[vol_cone$second_sd_upper>50 ]=55 #attempto to plot the option price
  return(vol_cone[rev(rownames(vol_cone)),])
  # return(melted_cone)
  
}


# p8 <- nPlot(value ~ Days_till_Expiry, group =  'variable', data = melted_cone, 
#             type = 'stackedAreaChart', id = 'chart'
# )


# p9 <- nPlot(value ~ Days_till_Expiry, group =  'variable', data = melted_cone, 
#             type = 'lineChart', id = 'chart'
# )

# p9$xAxis(axisLabel='Number of days till expiry')
# p9$yAxis(axisLabel='Volatility')
