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

temp.ann_vol={}

temp.max={}
temp.min={}
temp.mean={}
temp.stdev={}
temp.1sd.upper={}
temp.1sd.lower={}
temp.2sd.upper={}
temp.2sd.lower={}


duration=1
for(i in time_friday)
{
  if((i-10)>0)
  {
    temp.price=stock[(i-10):i]
    temp.ret <- diff(log(temp.price), lag = 1)
    temp.ret <- temp.ret[-1]
    print(sd(temp.ret)*sqrt(252))
    print(i)
    print("----")
    temp.ann_vol=append(temp.ann_vol,sd(temp.ret)*sqrt(252))
  }
  
}

# temp.max=max(temp.ann_vol)
# temp.min=min(temp.ann_vol)
# temp.mean=mean(temp.ann_vol)
# temp.stdev=sd(temp.ann_vol)
# temp.1sd.upper=(temp.mean*100)+(temp.stdev*100)
# temp.1sd.lower=temp.mean*100-temp.stdev*100
# temp.2sd.upper=temp.mean*100+(2*temp.stdev*100)
# temp.2sd.lower=temp.mean*100-(2*temp.stdev*100)

temp.max=append(temp.max,max(temp.ann_vol))
temp.min=append(temp.min,min(temp.ann_vol))
temp.mean=append(temp.mean,mean(temp.ann_vol))
temp.stdev=append(temp.stdev,sd(temp.ann_vol))
temp.1sd.upper=append(temp.1sd.upper,((temp.mean*100)+(temp.stdev*100)))
temp.1sd.lower=append(temp.1sd.lower,((temp.mean*100)-(temp.stdev*100)))
temp.2sd.upper=append(temp.2sd.upper,((temp.mean*100)+(2*temp.stdev*100)))
temp.2sd.lower=append(temp.2sd.lower,((temp.mean*100)-(2*temp.stdev*100)))


vol_cone=data.frame(duration,temp.max,temp.min,temp.mean,temp.1sd.lower,temp.1sd.upper
                    ,temp.2sd.lower,temp.2sd.upper)

names(vol_cone)=c('Days till Expiry','Max','Min','Mean','1st sd lower','1st sd upper',
                  '2nd sd lower','2nd sd upper')

melted_cone=melt(vol_cone,id.vars ="Days till Expiry")

ggplot(data=melted_cone, aes(x=duration, y=value, group=variable)) + geom_line()

