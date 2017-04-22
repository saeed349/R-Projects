# Real Data ---- 
library(Sim.DiffProc)
library(Ecdat)
library(yuima)


data(Irates)
rates <- Irates[, "r1"]
X <- window(rates, start = 1964.471, end = 1989.333)
plot(X)
fx <- expression( theta[1]+theta[2]*x ) ## drift coefficient of CKLS model
gx <- expression( theta[3]*x^theta[4] ) ## diffusion coefficient of CKLS model
pmle <- eval(formals(fitsde.default)$pmle)
fitres <- lapply(1:4, function(i) fitsde(X,drift=fx,diffusion=gx,pmle=pmle[i],
                                         start = list(theta1=1,theta2=1,theta3=1)))
Coef <- data.frame(do.call("cbind",lapply(1:4,function(i) coef(fitres[[i]]))))
Info <- data.frame(do.call("rbind",lapply(1:4,function(i) logLik(fitres[[i]]))),
                   do.call("rbind",lapply(1:4,function(i) AIC(fitres[[i]]))),
                   do.call("rbind",lapply(1:4,function(i) BIC(fitres[[i]]))),
                   row.names=pmle)
names(Coef) <- c(pmle)
names(Info) <- c("logLik","AIC","BIC")
Coef

# ----
f <- expression( (2.076-0.263*x) )
g <- expression( 0.130*x^1.451 )
mod <- snssde1d(drift=f,diffusion=g,x0=X[1],M=200, N=length(X),t0=1964.471,
                T=1989.333)
mod

plot(mod,plot.type="single",type="n",ylim=c(0,30))
lines(X,col=4,lwd=2)
lines(time(mod),mean(mod),col=2,lwd=2)
lines(time(mod),bconfint(mod,level=0.95)[,1],col=5,lwd=2)
lines(time(mod),bconfint(mod,level=0.95)[,2],col=5,lwd=2)
legend("topleft",c("real data","mean path",paste("bound of", 95,"% confidence")),inset = .01,col=c(4,2,5),lwd=2,cex=0.8)
