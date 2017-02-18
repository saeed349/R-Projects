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
  theSum = 0.5 * h * (f(a)+f(b)) #why -f(b), shouldnt it be + ?
  for (i in 1:(m-2)) {
    ai = a + i*h
    theSum = theSum + h*f(ai)
  }
  theSum
}


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
trap<-function(a,b,n,f)
{
  table6_5 = matrix(0, nrow=n, ncol=4, dimnames=list(
    c(1:n), c('m', 'I_2m_(f)', 'E_2m_(f)', 'E_2m_(f)/E_2m-1_(f)')))
  for(i in 1:n) {
    table6_5[i,1] = ifelse(1 == i, yes=2, no=2*table6_5[i-1,'m']-1)
    table6_5[i,2] = TrapezoidRule(a, b, table6_5[i,'m'], f) 
    table6_5[i,3] = pi - table6_5[i,2]  
    table6_5[i,4] = ifelse(1 == i, NA, table6_5[i-1,3] /table6_5[i,3] )
  }
  return(table6_5)
  
}



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
  print(paste("m=",m," h=",h," return=",
              (h/6 * (y[1] + 2*sum(y[ix1]) + 4*sum(y[ix2]) + y[2*m+1]))))
  # print(paste(y))
  # print(paste(ix1))
  # print(paste(ix2))
  return(h/6 * (y[1] + 2*sum(y[ix1]) + 4*sum(y[ix2]) + y[2*m+1]))
  
}

#working code
SimpsonRule2= function(a, b, m, f)
{
  h = (b-a)/(m-1)
  theSum = (1/3) * h * (f(a)+f(b)) #why -f(b), shouldnt it be + ?
  print(theSum)
  for (i in 1:(m-2)) {
    if(i%%2 != 0){
      theSum = theSum + h*f(a + i*h) *(4/3)
    }else{
      theSum = theSum + h*f(a + i*h) *(2/3)
    }
  }
  theSum
}

simson<-function(a,b,n,f)
{
  table6_5 = matrix(0, nrow=n, ncol=4, dimnames=list(
    c(1:n), c('m', 'I_2m_(f)', 'E_2m_(f)', 'E_2m_(f)/E_2m-1_(f)')))
  for(i in 1:n) {
    table6_5[i,1] = ifelse(1 == i, yes=2, no=2*table6_5[i-1,'m']-1)
    table6_5[i,2] = SimpsonRule2(a, b, table6_5[i,'m'], f)
    table6_5[i,3] = 1 - table6_5[i,2]  
    table6_5[i,4] = ifelse(1 == i, NA, table6_5[i-1,3] /table6_5[i,3] )
  }
  return(table6_5)
}


