#used first- decided to use .Not working on the given function
TrapRule = function(a, b, m, f)  # alternate version  #### the one in the fusoi
{
  h = (b-a)/(m-1)
  print(h)
  x = seq(from=a, to=b, length=m)
  print(x)
  y = f(x)
  print(y)
  z=(h * (0.5*y[1] + sum(y[2:(m-1)]) + (.5*y[m]))) #.5 should be multiplied with y[m] ?
  print(paste((0.5*y[1]),sum(y[2:(m-1)]), (.5*y[m])))
  print(z)
  print("---------")
  return(z)
}

TrapezoidRule = function(a, b, m, f)
{
  h = (b-a)/(m-1)
  Sum = 0.5 * h * (f(a)+f(b))
  for (i in 1:(m-2)) {
    ai = a + i*h
    Sum = Sum + h*f(ai)
  }
  return(Sum)
}

TrapezoidRule(b=10^6, a=-(10^6), m=12000,f1)

f1<-function(x){
  if(x==0)
    return(1)
  else
    return(sin(x)/x)
}

f2<-function(x){
  return(1+(exp(-x)*sin(8*(x^(2/3)))))
}

# b=10^4; a=-(10^4); n=15
Trapezoidal_Integration<-function(a,b,n,f)
{
  table = matrix(0, nrow=n, ncol=4, dimnames=list(
    c(1:n), c('m', 'Integration Result', 'Error', 'Error Ratio')))
  for(i in 1:n) {
    table[i,1] = ifelse(1 == i, yes=2, no=2*table[i-1,'m']-1)
    table[i,2] = TrapezoidRule(a, b, table[i,'m'], f) 
    table[i,3] = pi - table[i,2]  
    table[i,4] = ifelse(1 == i, NA, table[i-1,3] /table[i,3] )
  }
  return(table)
  
}

SimpsonRule(a=0,b=2, m=20000,f2)


#professors code
SimpsonRule = function(a, b, m, f)
{
  m = m-1
  h = (b-a)/m
  x = seq(from=a, to=b, by=h/2)
  print(x)
  y = f(x)
  ix1 = seq(from=3, by=2, to=2*m+1)
  ix2 = seq(from=2, by=2, to=2*m  )
  return(h/6 * (y[1] + 2*sum(y[ix1]) + 4*sum(y[ix2]) + y[2*m+1]))
  
}

#working code
SimpsonRule= function(a, b, m, f)
{
  h = (b-a)/(m-1)
  Sum = (1/3) * h * (f(a)+f(b)) 
  for (i in 1:(m-2)) {
    if(i%%2 != 0){
      Sum = Sum + h*f(a + i*h) *(4/3)
    }else{
      Sum = Sum + h*f(a + i*h) *(2/3)
    }
  }
  return(Sum)
}
SimpsonRule(a=-(10^6),b=10^6, m=12000,f1)

Simpson_Integration<-function(a,b,n,f)
{
  table = matrix(0, nrow=n, ncol=4, dimnames=list(
    c(1:n), c('m', 'Integration Result', 'Error', 'Error Ratio')))
  for(i in 1:n) {
    table[i,1] = ifelse(1 == i, yes=2, no=2*table[i-1,'m']-1)
    table[i,2] = SimpsonRule(a, b, table[i,'m'], f1)
    table[i,3] = 1 - table[i,2]  
    table[i,4] = ifelse(1 == i, NA, table[i-1,3] /table[i,3] )
  }
  return(table)
}

Simpson_Integration(b=10^4, a=-(10^4), n=15,f1)
