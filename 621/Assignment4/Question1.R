library(Sim.DiffProc)
library(Ecdat)
library(yuima)

df=read.csv(file="sample_data.csv",header=TRUE, sep=",")

X=ts(df[1])

# plot(X)
# 
fx <- expression( theta[1]*x)
gx <- expression( theta[2]*x^theta[3] )

coef,info=diff.mle(fx=fx,gx=gx,X)

diff.mle <-function(fx,gx,data)
{
  pmle <- eval(formals(fitsde.default)$pmle)
  fitres <- lapply(1:4, function(i) fitsde(data=data,drift=fx,diffusion=gx,pmle=pmle[i],
                                           start = list(theta1=1,theta2=1,theta3=1,theta4=1)))
  Coef <- data.frame(do.call("cbind",lapply(1:4,function(i) coef(fitres[[i]]))))
  Info <- data.frame(do.call("rbind",lapply(1:4,function(i) AIC(fitres[[i]]))),
                     row.names=pmle)
  names(Coef) <- c(pmle)
  names(Info) <- c("AIC")
  list("Info"=,Info,"Coef"=Coef)
}



#Part1----
#model1
fx1 <- expression( theta[1]*x )
gx1 <- expression( theta[2]*x^theta[3] )
mod1 <- fitsde(data=X,drift=fx1,diffusion=gx1,start = 
                    list(theta1=1, theta2=1,theta3=1),pmle="euler")
# competing model 2
fx2 <- expression( theta[1]+theta[2]*x )
gx2 <- expression( theta[3]*x^theta[4] )
mod2 <- fitsde(data=X,drift=fx2,diffusion=gx2,start = 
                 list(theta1=1, theta2=1,theta3=1,theta4=1),pmle="euler")
# competing model 3
fx3 <- expression( theta[1]+theta[2]*x )
gx3 <- expression( theta[3]*sqrt(x) )
mod3 <- fitsde(data=X,drift=fx3,diffusion=gx3,start = 
                 list(theta1=1, theta2=1,theta3=1),pmle="euler")
## competing model 4
fx4 <- expression( theta[1] )
gx4 <- expression( theta[2]*x^theta[3] )
mod4 <- fitsde(data=X,drift=fx4,diffusion=gx4,start = 
                 list(theta1=1, theta2=1,theta3=1),pmle="euler")

## competing model 5
fx5 <- expression( theta[1]*x )
gx5 <- expression(theta[2] + (theta[3]*x^theta[4]) )
mod5 <- fitsde(data=X,drift=fx5,diffusion=gx5,start = 
                 list(theta1=1, theta2=1,theta3=1, theta4=1),pmle="euler")
#  Computes AIC
AIC <- c(AIC(mod1),AIC(mod2),AIC(mod3),AIC(mod4),AIC(mod5))
Test <- data.frame(AIC,row.names = c("Model 1","Model 2","Model 3", "Model 4","Model 5"))
Test
Bestmod <- rownames(Test)[which.min(Test[,1])]
Theta1 <- c(coef(mod1)[[1]],coef(mod2)[[1]],coef(mod3)[[1]],coef(mod4)[[1]],coef(mod5)[[1]])
Theta2 <- c(coef(mod1)[[2]],coef(mod2)[[2]],coef(mod3)[[2]],coef(mod4)[[2]],coef(mod5)[[2]])
Theta3 <- c(coef(mod1)[[3]],coef(mod2)[[3]],coef(mod3)[[3]],coef(mod4)[[3]],coef(mod5)[[3]])
Theta4 <- c("",coef(mod2)[[4]],coef(mod5)[[4]],"","")
Parms <- data.frame(Theta1,Theta2,Theta3,Theta4,
                    row.names = c("Model1","Model2","Model3","Model4","Model5"))
Parms

Model

#Part2----
