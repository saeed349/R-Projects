library(quantmod)
stockData <- new.env()
lookup.symb=c("YHOO","GOOG","IBM","INTC","MSFT")
# lookup.symb=c("AAPL","AXP","BA", "CAT","CSCO","CVX","KO","DD","XOM","GE","GS","HD",
# "IBM","INTC","JNJ","JPM","MCD","MMM","MRK","MSFT","NKE","PFE","PG","TRV","UNH","UTX",
# "VZ","WMT","DIS")
#VISA problem
getSymbols("V",from="2005-01-03")
getSymbols(lookup.symb, from="2005-01-03", env=stockData, src="yahoo")

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

#[1] "sdev"     "rotation" "center"   "scale"    "x"  
# center and scale are the parameters used for normalization prior to PCA
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

## Using another function

Prin.Comp1=princomp(ReturnMatrix,cor=T) #Cor=T says use correlation matrix cor=F says use covariance
summary(Prin.Comp1)
loadings(Prin.Comp1)

plot(Prin.Comp1,type="l") #Scree Plot
Prin.Comp1$scores # The PCA's
biplot(Prin.Comp1)


#The principal( ) function in the psych package can be used to extract 
#and rotate principal components.
# Varimax Rotated Principal Components
# retaining 2 components 
install.packages("psych")
library(psych)
fit <- principal(cor(ReturnMatrix), nfactors=2, rotate="varimax", n.obs=dim(ReturnMatrix)[1])
fit # print results


#Exploratory Factor Analysis
#The factanal( ) function produces maximum likelihood factor analysis.

# Maximum Likelihood Factor Analysis
# entering raw data and extracting 2 factors, 
# with varimax rotation 
fit <- factanal(ReturnMatrix, 2, rotation="varimax")
print(fit, digits=2, cutoff=.3, sort=TRUE)
# plot factor 1 by factor 2 (the loadings of these factors)
load <- fit$loadings[,1:2] 
plot(load,type="n") # set up plot 
text(load,labels=names(ReturnMatrix),cex=.7) # add variable names

# Principal Axis Factor Analysis
library(psych)
fit <- fa(cor(ReturnMatrix), nfactors=2, n.obs=dim(ReturnMatrix)[1])
#I need to incliude the n.obs to get the correct test statistics. 
#This is because the function always uses the correlation matrix
fit # print results

install.packages("GPArotation")


# Determine Number of Factors to Extract
install.packages("nFactors")
library(nFactors)
ev <- eigen(cor(ReturnMatrix)) # get eigenvalues
ap <- parallel(subject=nrow(ReturnMatrix),var=ncol(ReturnMatrix),
               rep=100,cent=.05)
nS <- nScree(x=ev$values, aparallel=ap$eigen$qevpea)
plotnScree(nS)


##Different 

fit=factanal(ReturnMatrix,factors=2,rotation="varimax")
print(fit)

