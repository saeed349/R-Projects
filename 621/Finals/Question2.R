# Part 1----


library(quantmod)
getSymbols("XLY",from="2012-01-01")

kurtosi(dailyReturn(XLY))

plot(XLY[,3])

stockData <- new.env()
lookup.symb=c("AMZN","HD","CMCSA","DIS","MCD","PCLN","SBUX","TWX","LOW","NKE","CHTR","NFLX","TJX","GM","F","MAR","TGT",
              "FOXA","CCL","ROST","NWL")

getSymbols("XLV",from="2012-01-01")
getSymbols(lookup.symb, from="2012-01-01", env=stockData, src="yahoo")


ReturnMatrix=NULL


for(i in 1:length(lookup.symb))
{
  tmp <- get(lookup.symb[i], pos=stockData)   # get data from stockData environment  
  ReturnMatrix=cbind(ReturnMatrix,   (Cl(tmp)-Op(tmp)) / Op(tmp)   )
  colnames(ReturnMatrix)[i]=lookup.symb[i]
}

cor(ReturnMatrix)



Prin.Comp = prcomp(ReturnMatrix, scale = T) #by default R centers the variables. Scale also makes then sd=1
summary(Prin.Comp)

names(Prin.Comp)


Prin.Comp$center
Prin.Comp$scale

#The rotation contains the directions of the principal components (eigenvectors)
Prin.Comp$rotation
Prin.Comp$sdev

eigen(cor(ReturnMatrix))
Prin.Comp$sdev^2 # the eigenvalues

biplot(Prin.Comp,scale=0) #scale=0 makes arrows proportional with eigenvalues from top to bottom

#To create the Scree plot:
propvariance = Prin.Comp$sdev^2/sum(Prin.Comp$sdev^2)
cumsum(propvariance)

plot(propvariance, type="b")



plot(cumsum(propvariance), type="b") # PCA does not work well for this data


number_best_pca= which(cumsum(propvariance)>.8)[1]

# match(propvariance[1],propvariance)

## Using another function

Prin.Comp1=princomp(ReturnMatrix,cor=T) #Cor=T says use correlation matrix cor=F says use covariance
summary(Prin.Comp1)
loadings(Prin.Comp1)

plot(Prin.Comp1,type="l") #Scree Plot
Prin.Comp1$scores # The PCA's
biplot(Prin.Comp1)


library(psych)
fit <- principal(ReturnMatrix, nfactors=number_best_pca, rotate="varimax", n.obs=dim(ReturnMatrix)[1])
fit # print results






#Part 2----

rank(fit$loadings[,1])

rank(fit$loadings[,2])

# pca_component 1 - TWX, FOXA, DIS, CMCSA
# pca_component 2 - ROST, TJX, TGT, HD 

getSymbols("TWX",from="2012-01-01")
getSymbols("FOXA",from="2012-01-01")
getSymbols("ROST",from="2012-01-01")
getSymbols("TJX",from="2012-01-01")
stock1=ts(TWX[,6])
stock2=ts(FOXA[,6])
stock3=ts(ROST[,6])
stock4=ts(TJX[,6])


library(Sim.DiffProc)
library(Ecdat)

fx <-{}
gx <-{}
#model 1 drift and diffusion 
fx[1] <- expression( theta[1]*x )
gx[1]<- expression( theta[2]*x )

#model 2 drift and diffusion 
fx[2] <- expression( theta[1]+theta[2]*x )
gx[2]<- expression( theta[3]*x^theta[4] )

#model 3 drift and diffusion 
fx[3] <- expression( theta[1]*x )
gx[3]<- expression (theta[2] + ( theta[3]*x^theta[4]) )

#model 4 drift and diffusion 
fx[4]<- expression( theta[1]*x)
gx[4] <- expression( theta[2]*x^(3/2))

#model 5 drift and diffusion 
fx[5] <- expression( theta[1]+theta[2]*x )
gx[5] <- expression(theta[3] + (theta[4]*log(x)) )

pmle=eval(formals(fitsde.default)$pmle)
print("We'll use euler method for our Maximum Likelyhood")

Best.fit<-function(data,pmle)
{
  #model1
  mod1 <- fitsde(data=data,drift=fx[1],diffusion=gx[1],start = 
                   list(theta1=1, theta2=1,theta3=1),pmle=pmle)
  #model 2
  mod2 <- fitsde(data=data,drift=fx[2],diffusion=gx[2],start = 
                   list(theta1=1, theta2=1,theta3=1,theta4=1),pmle=pmle)
  #model 3
  mod3 <- fitsde(data=data,drift=fx[3],diffusion=gx[3],start = 
                   list(theta1=1, theta2=1,theta3=1),pmle=pmle)
  #model 4
  mod4 <- fitsde(data=data,drift=fx[4],diffusion=gx[4],start = 
                   list(theta1=1, theta2=1,theta3=1),pmle=pmle)
  
  #model 5
  mod5 <- fitsde(data=data,drift=fx[5],diffusion=gx[5],start = 
                   list(theta1=1, theta2=1,theta3=1, theta4=1),pmle=pmle)
  #Computes AIC
  AIC <- c(AIC(mod1),AIC(mod2),AIC(mod3),AIC(mod4),AIC(mod5))
  Test <- data.frame(AIC,row.names = c("Model 1","Model 2","Model 3", "Model 4","Model 5"))
  Test
  # Bestmod <- rownames(Test)[which.min(Test[,1])]
  Bestmod <- which.min(Test[,1])
  list('best.model'=Bestmod,'AIC.results'=Test)
}


Diff.mle <-function(fx,gx,data)
{
  pmle <- eval(formals(fitsde.default)$pmle)
  fitres <- lapply(1:4, function(i) fitsde(data=data,drift=fx,diffusion=gx,pmle=pmle[i],
                                           start = list(theta1=1,theta2=1,theta3=1,theta4=1)))
  Coef <- data.frame(do.call("cbind",lapply(1:4,function(i) coef(fitres[[i]]))))
  Info <- data.frame(do.call("rbind",lapply(1:4,function(i) AIC(fitres[[i]]))),
                     row.names=pmle)
  names(Coef) <- c(pmle)
  names(Info) <- c("AIC")
  list("Info"=Info,"Coef"=Coef)
}

pmle_type=2 
best_model={}
params={}

## Finding the best model for the top 4 stocks----
### For Stock 1
fit1=Best.fit(data =stock1,pmle = pmle[pmle_type])
print(paste("Best model = model ",fit1$best.model))
best_model[1]=fit1$best.model

print("The parameter estimates are:")
ls1=Diff.mle(fx=fx[fit1$best.model],gx=gx[fit1$best.model],data = stock1)
ls1=ls1$Coef[,pmle_type]
params[1]=list(ls1)
print(ls1)

### For Stock 2

fit2=Best.fit(data =stock2,pmle = pmle[2])
print(paste("Best model = model ",fit2$best.model))
best_model[2]=fit2$best.model

print("The parameter estimates are:")
ls2=Diff.mle(fx=fx[fit2$best.model],gx=gx[fit2$best.model],data = stock2)
ls2=ls2$Coef[,pmle_type]
params[2]=list(ls2)


print(ls2)

### For Stock 3

fit3=Best.fit(data =stock3,pmle = pmle[pmle_type])
print(paste("Best model = model ",fit3$best.model))
best_model[3]=fit3$best.model


print("The parameter estimates are:")
ls3=Diff.mle(fx=fx[fit3$best.model],gx=gx[fit3$best.model],data = stock3)
ls3=ls3$Coef[,pmle_type]
params[3]=list(ls3)
print(ls3)

### For Stock 4

fit4=Best.fit(data =stock4,pmle = pmle[pmle_type])
print(paste("Best model = model ",fit4$best.model))
best_model[4]=fit4$best.model


print("The parameter estimates are:")
ls4=Diff.mle(fx=fx[fit4$best.model],gx=gx[fit4$best.model],data = stock4)
ls4=ls4$Coef[,pmle_type]
params[4]=list(ls4)
print(ls4)

#Part 3----


stockData <- new.env()
lookup.symb=c("TWX","FOXA","ROST","TJX")
getSymbols(lookup.symb, from="2012-01-01", env=stockData, src="yahoo")

ReturnMatrix=NULL


for(i in 1:length(lookup.symb))
{
  tmp <- get(lookup.symb[i], pos=stockData)   # get data from stockData environment  
  ReturnMatrix=cbind(ReturnMatrix,   (Cl(tmp)-Op(tmp)) / Op(tmp)   )
  colnames(ReturnMatrix)[i]=lookup.symb[i]
}
cor_matrix=cor(ReturnMatrix)

chol_upper=chol(cor_matrix)



#MonteCarlo Simulation----
n_iterations=10
n_steps=252
stocks_sim=matrix(0,n_iterations,4)
stocks_sim[,1]=stock1[1]
stocks_sim[,2]=stock2[1]
stocks_sim[,3]=stock3[1]
stocks_sim[,4]=stock4[1]

sim1={}

dt=1/n_steps

for(i in 1:n_iterations)
{
  print(i)
  for(j in 1:n_steps)
  {
    w=as.vector(matrix( rnorm(1*4,mean=0,sd=1), 1, 4))
    cor_w=chol_upper%*%w
    for(k in 1:4)
    {
     if(best_model[k]==1)
      {
        stocks_sim[i,k]=stocks_sim[i,k]+(params[[k]][1]*dt*stocks_sim[i,k])+(params[[k]][2]*stocks_sim[i,k]*w[1])
        if(i==1 && k==3 && length(sim1)<300){
          sim1=append(sim1,stocks_sim[i,k])
          print(paste("i=",i,"j=",j,"k=",k,"Price=",stocks_sim[i,k]))
          print(length(sim1))
        }
      }
      else if(best_model[k]==2)
      {
        stocks_sim[i,k]=stocks_sim[i,k]+(params[[k]][1]+params[[k]][2]*stocks_sim[i,k])*dt+(params[[k]][3]*stocks_sim[i,k]^params[[k]][4]*w[i])
      }
      else if(best_model[k]==3)
      {
        stocks_sim[i,k]=stocks_sim[i,k]+(params[[k]][1]*stocks_sim[i,k]*dt)+(params[[k]][2]+(params[[k]][3]*stocks_sim[i,k]^params[[k]][4]*w[i]))
      }
      else if(best_model[k]==4)
      {
        stocks_sim[i,k]=stocks_sim[i,k]+(params[[k]][1]*stocks_sim[i,k]*dt)+(params[[k]][2]*stocks_sim[i,k]^(3/2)*w[i])
      }
      else if(best_model[k]==5)
      {
        stocks_sim[i,k]=stocks_sim[i,k]+(params[[k]][1]+params[[k]][2])*dt+(params[[k]][3]+params[[k]][4]*log(stocks_sim[i,k]))*stocks_sim[i,k]*w[i]
      }
    }
    
  }
}
stocks_sim

plot(sim1)

#Part5-----

etf=ts(XLY[,6])
fit_etf=Best.fit(data =etf,pmle = pmle[pmle_type])
print(paste("Best model = model ",fit4$best.model))


print("The parameter estimates are:")
ls_etf=Diff.mle(fx=fx[fit_etf$best.model],gx=gx[fit_etf$best.model],data = etf)
ls_etf=ls4$Coef[,pmle_type]



require(yuima)
D=setYuima(data=setData(as.double(TWX[,6])))
str(D@data)
m1=setModel(drift="theta*x",diffusion="sigma*x", state.var="x",time.var="t",solve.var="x",xinit=0.5)
X=simulate(m1,true.param=list(sigma=0.4,theta=4))
initialise=list(sigma=0.5,theta=1)
lowbound=list(sigma=0,theta=0)
upbound=list(sigma=2,theta=3)
mle=qmle(D@data,start=initialise,lower=lowbound,upper=upbound)
summary(mle)

