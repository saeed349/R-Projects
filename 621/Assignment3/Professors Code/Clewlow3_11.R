pu = function(sig, dt=1/3, dx=0.2, r=0.06, div=0.03, nu=r-div-0.5*sig^2) {
  0.5 * dt * ( (sig/dx)^2 + nu/dx )
}

pm =  function(sig, dt=1/3, dx=0.2, r=0.06, div=0.03, nu=r-div-0.5*sig^2) {
  1.0 - dt *   (sig/dx)^2 - r*dt 
}
pd =  function(sig, dt=1/3, dx=0.2, r=0.06, div=0.03, nu=r-div-0.5*sig^2) {
  0.5 * dt * ( (sig/dx)^2 - nu/dx)
}

x = seq(from=0.01, to=0.5, by=0.01)
ux = pu(x)
mx = pm(x)
dx = pd(x)
theTitle = expression(paste("Figure 3.11: Explicit Finite Difference Probabilities", 
                 " as a Function of ", sigma, sep=""))
xlab1 = expression(paste("Volatility (", sigma, ")", sep=""))
xlab2 = expression(paste("K = 100, T = 1.0, S =100, r = 6%, ", delta, " = 3%"))
plot(x, ux, ylim=c(-1,1), type="n", xlab="")
title(theTitle, cex=3)
mtext(text=xlab1, side=1, line=2, cex=1.25)
mtext(text=xlab2, side=1, line=3, cex=1.25)
lines(x, ux, lwd=2, col=2)
lines(x, mx, lwd=2, col=3)
lines(x, dx, lwd=2, col=4)
abline(h=0)
legend(0.3, 0.9, legend=c('pu', 'pm', 'pd'), col=2:4, lty=1, lwd=3)

