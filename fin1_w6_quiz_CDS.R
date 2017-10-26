#Quiz week 5: bonds pricing (Term Structure Models)
n<-10
S<-100
r<-0.05 # beginning short rate r(0,0)
u<-1.1
d<-0.9
q<-0.5

# a and b - for hazard rate (credit spread) =a*b^(j-i/1) where j-time and i-lattice.
a<-0.01
b<-1.01
R<-20

remove(short_rates)
remove(hazard_rates)
remove(zcb_price)

short_rates<-matrix(nrow=11,ncol=11)
hazard_rates<-matrix(nrow=11,ncol=11)
zcb_price<-matrix(nrow=11,ncol=11)

for(j in n:0)#j - periods
{
  #rate_lattice
  for(i in j:0)#i from N to 0
  {
    short_rates[j-i+1,j+1]<-u^i*d^(j-i)*r
    message(paste("rate: t,j",short_rates[j-i+1,j+1],j-i+1,j+1))
  }
}

#zcb (zero coupon bonds)
for(j in n:0)#j - periods.
{
  for(i in j:0)#i - set at the same period.
  {
    if(j==n)
    {
      zcb_price[j-i+1,j+1]<-S
    }
    else
    {
      h<-hazard_rates[j-i+1,j+1]<-a*b^(j-i-j/2)
      zcb_price[j-i+1,j+1]<-(1-h)*(q*zcb_price[j-i+1,j+2] + (1-q)*zcb_price[j-i+2,j+2])/(1+short_rates[j-i+1,j+1]) + R*h*(q+(1-q))/(1+short_rates[j-i+1,j+1])
    }
  }
}

