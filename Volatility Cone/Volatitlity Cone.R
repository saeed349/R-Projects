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

  test_iv=GBSVolatility(option_chain$premium[3],
                        TypeFlag = ifelse((option_chain$type[3]=="Call"), "c", "p"),
                        S = as.numeric(tail(stock_df,1)[6]), 
                        X =option_chain$strike[3],
                        Time = as.numeric(option_chain$days_till_expiry[3])/365,
                        r = libor, b=0, maxiter = 1000)
  iv={}
  optionName={}
  days_till_expiry={}
  for (i in 1:nrow(option_chain)) 
  {
    if(as.numeric(option_chain[i,"days_till_expiry"])<120)
    {
      iv=append(iv,100*GBSVolatility(option_chain[i,"premium"],
                                     TypeFlag = ifelse((option_chain[i,"type"]=="Call"), "c", "p"),
                                     S = as.numeric(tail(stock_df,1)[6]), 
                                     X =option_chain[i,"strike"],
                                     Time = as.numeric(option_chain[i,"days_till_expiry"])/365,
                                     r = libor, b=0, maxiter = 1000))
      optionName=append(optionName,paste(option_chain[i,"strike"],"-",
                                         option_chain[i,"type"],"Expiring On:",
                                         option_chain[i,"expiry"]))
      days_till_expiry=append(days_till_expiry,as.numeric(option_chain[i,"days_till_expiry"]))
    }
    
    
  }
  
  option_chain_df=data.frame(days_till_expiry,optionName,iv)
  names(option_chain_df)<-c("Days_till_Expiry","variable","value")
  
  
  melted_cone=melt(vol_cone,id.vars ="Days_till_Expiry")
  
  # for some reason apply is not working
  # df={}
  # f<-function(x)
  # {
  #   print(GBSVolatility(x[5],TypeFlag = ifelse((x[2]=="Call"), "c", "p"),
  #                    S = as.numeric(tail(stock_df,1)[6]), X =x[4], 
  #                    Time = as.numeric(x[11])/365,
  #                    r = libor, b=0, maxiter = 1000))
  # }
  # 
  # apply(option_chain,1,f)
  
  # 
  x=GBSVolatility(20, TypeFlag ="c",S = 100, X =90, Time = 60/365,
                  r = libor, b=0, maxiter = 10000)
  # return(vol_cone)
  
  ##to remove the column name and use it as index, I don't know why I have removed it
  # rownames(vol_cone) <- vol_cone$Days_till_Expiry
  # vol_cone$Days_till_Expiry<-NULL
  return(vol_cone)
}


# p8 <- nPlot(value ~ Days_till_Expiry, group =  'variable', data = melted_cone, 
#             type = 'stackedAreaChart', id = 'chart'
# )


# p9 <- nPlot(value ~ Days_till_Expiry, group =  'variable', data = melted_cone, 
#             type = 'lineChart', id = 'chart'
# )

# p9$xAxis(axisLabel='Number of days till expiry')
# p9$yAxis(axisLabel='Volatility')
melted_cone$Days_till_Expiry=as.numeric(melted_cone$Days_till_Expiry)
melted_cone$value=as.numeric(melted_cone$value)

melted_cone[complete.cases(melted_cone),]
melted_cone=x

# datm <- transform(melted_cone,  Days_till_Expiry= to_jsdate(Days_till_Expiry))
# melted_cone$Days_till_Expiry <- as.double(as.POSIXct(as.Date(melted_cone$Days_till_Expiry),origin="1970-01-01"))

# Not working
melted_cone$Days_till_Expiry<-as.Date(as.Date(Sys.Date())
                                      +as.integer(melted_cone$Days_till_Expiry))

melted_cone$Days_till_Expiry <- as.double(as.POSIXct(as.Date(melted_cone$Days_till_Expiry), origin="1970-01-01"))
 


to_jsdate <- function(date_){
  val = as.POSIXct(as.Date(date_), origin="1970-01-01")
  as.numeric(val)
}

# test_df=melted_cone
test_df = reshape2::melt(
  vol_cone[,c('Days_till_Expiry','Max','Min')],
  id = 'Days_till_Expiry'
)
# do.call(rbind.data.frame, test_df)
test_df$Days_till_Expiry<-as.Date(as.Date(Sys.Date())
                                      +as.integer(test_df$Days_till_Expiry))
test_df <- transform(test_df, Days_till_Expiry = to_jsdate(Days_till_Expiry))
# test_df$Days_till_Expiry <- as.double(as.POSIXct(as.Date(test_df$Days_till_Expiry), origin="1970-01-01"))
test_df=test_df[1:20,]



# df <- data.frame(matrix(unlist(melted_cone)),stringsAsFactors=FALSE)
# do.call(rbind.data.frame, test)

p6 <- Rickshaw$new()
p6$layer(value ~ Days_till_Expiry,group = 'variable', data = test_df, type = 'line', 
         colors = c("darkred", "darkslategrey"))
p6$set(slider = TRUE)
p6

p3$set(
  xAxis = FALSE,
  yAxis = FALSE,
  shelving = FALSE,
  legend = FALSE,
  slider = FALSE,
  highlight = FALSE
)
p3$show('iframesrc',cdn=TRUE)


# This works

r1<-rPlot(value ~ Days_till_Expiry, data = melted_cone,
          color = 'variable',type = 'line')

# r1$guides(x = list(title = "", ticks = unique(melted_cone$Days_till_Expiry)))
# r1$guides(y = list(title = "", max = 18))


# p12 <- nPlot(value ~ Days_till_Expiry, group = 'variable', data = test_df
#              , type = 'multiChart')
# p12$set(multi = list(
#   Max= list(type="line", yAxis=1),
#   Min = list(type="line", yAxis=2)
# ))
# p12$setTemplate(script = system.file(
#   "/libraries/nvd3/layouts/multiChart.html",
#   package = "rCharts"
# ))
# p12
# 
# h12 <- hPlot(value ~ Days_till_Expiry, group = 'variable', data = test_df
#              , type = 'multiChart')
# p12
test_df<-vol_cone
test_df$spot[test_df$Days_till_Expiry==10]=50
test_df$spot2[test_df$Days_till_Expiry==100]=25


test_df_melted=melt(test_df,id.vars ="Days_till_Expiry")

#-----------------------------------
#the highchart works

h1=hPlot(value ~ Days_till_Expiry, data =test_df_melted, type ='line', 
      group = 'variable')

h1$legend(enabled=FALSE)
#-----------------------------------

n1=nPlot(value ~ Days_till_Expiry, group =  'variable', data = test_df_melted, 
                       type = 'lineChart', id = 'chart')

n1$chart(showLegend=FALSE)
#=================================

do.call(rbind.data.frame, option_chain_df)
do.call(rbind.data.frame, melted_cone)

test_df=merge(melted_cone,option_chain_df,all=TRUE)

h1=hPlot(value ~ Days_till_Expiry, data =test_df, type ='line', 
         group = 'variable')

h1$legend(enabled=FALSE)
h1

