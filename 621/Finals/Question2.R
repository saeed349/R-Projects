# Part 1----


library(quantmod)
getSymbols("XLV",from="2012-01-01")

plot(XLV[,3])

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
fit <- principal(cor(ReturnMatrix), nfactors=number_best_pca, rotate="varimax", n.obs=dim(ReturnMatrix)[1])
fit # print results


#Part 2----

