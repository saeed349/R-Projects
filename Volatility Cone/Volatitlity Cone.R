library(quantmod)
library(reshape2)
library(ggplot2)

getSymbols("EBAY",from = as.Date("2016-01-01"), to = as.Date("2017-02-01"))

time_friday=options.expiry(EBAY)

stock<<-EBAY[,6];

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


duration=seq(90,1,-1)
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
      # print(sd(temp.ret)*sqrt(252))
      # print(i)
      print("----")
      temp.ann_vol=append(temp.ann_vol,sd(temp.ret)*sqrt(252))
    }
  }
  
  temp.max=append(temp.max,(max(temp.ann_vol)*100))
  temp.min=append(temp.min,(min(temp.ann_vol)*100))
  temp.mean=append(temp.mean,(mean(temp.ann_vol)*100))
  temp.stdev=sd(temp.ann_vol)
  print((tail(temp.mean,1)))
  print("-----")
  temp.1sd.upper=append(temp.1sd.upper,((tail(temp.mean,1))+(temp.stdev*100)))
  temp.1sd.lower=append(temp.1sd.lower,((tail(temp.mean,1))-(temp.stdev*100)))
  temp.2sd.upper=append(temp.2sd.upper,((tail(temp.mean,1))+(2*temp.stdev*100)))
  temp.2sd.lower=append(temp.2sd.lower,((tail(temp.mean,1))-(2*temp.stdev*100)))
}


# temp.1sd.lower=temp.1sd.lower[!is.na(temp.1sd.lower)]


vol_cone=data.frame(duration,temp.max,temp.min,temp.mean,temp.1sd.lower,temp.1sd.upper
                    ,temp.2sd.lower,temp.2sd.upper)

names(vol_cone)=c('Days_till_Expiry','Max','Min','Mean','1st sd lower','1st sd upper',
                  '2nd sd lower','2nd sd upper')

na.omit(vol_cone)

melted_cone=melt(vol_cone,id.vars ="Days_till_Expiry")

# ggplot(data=vol_cone,aes(x=duatation))
ggplot(data=melted_cone, aes(x=Days_till_Expiry, y=value, group=variable)) + geom_line()
# 
# This brings the option chain with the dates
# library(flipsideR)
# AAPL = getOptionChain('AAPL') 