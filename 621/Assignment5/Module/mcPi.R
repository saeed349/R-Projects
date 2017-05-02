# Simulate pi
mcPI = function(nIterations= 100) 
{
   # Generate random numbers
  x = runif(nIterations, min=0, max=1)
  y = runif(nIterations, min=0, max=1)
  AreaCircle = x*x + y*y <= 1
  AreaSquare = nIterations
  list(x=x, y=y, inside=AreaCircle,
       est=4 * sum(AreaCircle)/AreaSquare)
}
round( mcPI(100)$est , 3)
round( mcPI(10000)$est,3)
round( mcPI(1000000)$est,3)
par(mfrow=c(1,2))
plot(z$x[!z$inside], z$y[!z$inside])
plot(z$x[z$inside], z$y[z$inside])

