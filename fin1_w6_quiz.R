remove(a)
remove(bel_price)
remove(short_rates)
remove(swaption_price)

a<-vector(length = 15)
bel_price<-matrix(nrow=15,ncol=15)
short_rates<-matrix(nrow=15,ncol=15)
swaption_price<-matrix(nrow=11,ncol=11)

#Quiz: fin 1 week6.
n<-10
q<-0.5
b<-0.05
S<-1
spot_rates<-c(3.0, 3.1, 3.2, 3.3, 3.4, 3.5, 3.55, 3.6, 3.65, 3.7)

#elementary prices with Ho-Lee model i.e. rates are variable.
for(j in 0:n)#j - periods.
{
  f<-function(x)
  {
    (-1)*S/(1+spot_rates[j]/100.)^j
  }
  
  subst<-substitute((-1)*S/(1+spot_rates[j]/100.)^j, list(j=j,S=S))#BDT
  body(f)[[2]]<-substitute(a, list(a=subst,i=i,j=j))
  
  for(i in j:0)#i - set at the same period.
  {
    if(j==0)
    {
      bel_price[j-i+1,j+1]<-S
    }
    else
    {
      #3 variants: top[i=0]-center[0<i<j]-bottom[i=j]
      if(0<i && i<j)
      {
        subst<-substitute(q*bel_price[j-i,j]/(1+x*exp(b*(j-i-1))) + (1-q)*bel_price[j-i+1,j]/(1+x*exp(b*(j-i))), list(i=i,j=j))#BDT
        body(f)[[2]]<-substitute(a+b, list(a=subst,b=body(f)[[2]],i=i,j=j))
      }
      
      if(i == 0)#bottom
      {
        subst<-substitute((q*bel_price[1,j])/(1+x), list(j=j))#BDT - the same as Ho-Lee!
        body(f)[[2]]<-substitute(a+b, list(a=subst,b=body(f)[[2]],i=i,j=j))
      }
      
      if(i == j)#top
      {
        subst<-substitute((q*bel_price[j,j])/(1+x*exp(b*(j-1))), list(j=j))#BDT
        body(f)[[2]]<-substitute(a+b, list(a=subst,b=body(f)[[2]],i=i,j=j))
      }
    }
  }#i

  if(j != 0)
  {
    root<-uniroot(f,lower = 0, upper = 1, tol = 1e-20)
    a[j]<-root$root
  }
  
  for(i in 1:j)
  {
    if(j != 0)
    {
      short_rates[i,j]<-a[j]*exp(b*(i-1))#BDT
    }
  }
  
  #el_price and bel_price are the same (but more accurate algo representation is bel_price!).
  for(i in j:0)#i - set at the same period.
  {
    if(j==0)
    {
      bel_price[j-i+1,j+1]<-S
    }
    else
    {
      #3 variants: top[i=0]-center[0<i<j]-bottom[i=j]
      if(0<i && i<j)# (j-i) vs (j-i+1)
      {
        bel_price[j-i+1,j+1]<-q*bel_price[j-i,j]/(1+a[j]*exp(b*(j-i-1))) + (1-q)*bel_price[j-i+1,j]/(1+a[j]*exp(b*(j-i)))#BDT
      }
      
      if(i == j)#bottom: min short_rate.
      {
        bel_price[j+1,j+1]<-(q*bel_price[j,j])/(1+a[j]*exp(b*(j-1)))#Ho-Lee and BDT (the same!).
      }
      
      if(i == 0)#top: max short_rate.
      {
        bel_price[1,j+1]<-(q*bel_price[1,j])/(1+a[j])#BDT
      }
    }
  }#i
}#j

#Quiz: fin 1 week6.
n<-9 # final payment at t=10
c<-0.039
N<-3 # start of swaption

#swaption
for(j in n:0)#j - periods: backwards.
{
  for(i in 0:j)#i - set at the same period.
  {
    if(j == n)
    {
      swaption_price[i+1,j+1]<-(short_rates[i+1,j+1]-c)/(1+short_rates[i+1,j+1])
    }
    else
    {
      if(j>=N)
      {
        swaption_price[i+1,j+1]<-(short_rates[i+1,j+1]-c)/(1+short_rates[i+1,j+1]) + (q*swaption_price[i+1,j+2] + (1-q)*swaption_price[i+2,j+2])/(1+short_rates[i+1,j+1])
      }
      
      if(j<N)
      {
        swaption_price[i+1,j+1]<-(q*swaption_price[i+1,j+2] + (1-q)*swaption_price[i+2,j+2])/(1+short_rates[i+1,j+1])
      }
      
      if(j == N)
      {
        swaption_price[i+1,j+1]<-max(swaption_price[i+1,j+1],0)
      }
    }
  }
}

