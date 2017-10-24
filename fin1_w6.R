#Quiz week 5: bonds pricing (Term Structure Models)
n<-10
S<-100
r<-0.05 # beginning short rate r(0,0)
u<-1.1
d<-0.9
q<-0.5
K<-80
rate_swap<-0.045

#test from metodic (week 5):TermStructure_Binomial_%231.pdf
# n<-5
# S<-100
# r<-0.06 # beginning short rate r(0,0)
# u<-1.25
# d<-0.9
# q<-0.5
# rate_swap<-0.05

#Book: 14.1 and 14.10 - rate lattice
n<-10
r<-0.07 # beginning short rate r(0,0)
u<-1.3
d<-0.9
q<-0.5
K<-80

remove(rate_lattice)
remove(zcb_price)
remove(el_price)
remove(bel_price)
remove(short_rates)
remove(all_zcb_prices)
remove(call_option_euro_zcb_price)
remove(call_option_amer_zcb_price)
remove(forward_swap_price)
remove(swaption_price)

remove(sec_price)
remove(fut_price)
remove(chooser_price)
remove(call_option_euro_fut_price)
remove(put_option_euro_price)
remove(put_option_amer_price)
remove(diff)
remove(diff_optons)

rate_lattice<-matrix(nrow=11,ncol=11)
zcb_price<-matrix(nrow=11,ncol=11)
el_price<-matrix(nrow=15,ncol=15)
bel_price<-matrix(nrow=15,ncol=15)
short_rates<-matrix(nrow=15,ncol=15)
all_zcb_prices<-vector(length=11)
call_option_euro_zcb_price<-matrix(nrow=11,ncol=11)
call_option_amer_zcb_price<-matrix(nrow=11,ncol=11)
forward_swap_price<-matrix(nrow=11,ncol=11)
swaption_price<-matrix(nrow=11,ncol=11)

sec_price<-matrix(nrow=16,ncol=16)
fut_price<-matrix(nrow=16,ncol=16)
chooser_price<-matrix(nrow=16,ncol=16)
call_option_euro_fut_price<-matrix(nrow=16,ncol=16)
put_option_euro_price<-matrix(nrow=16,ncol=16)
put_option_amer_price<-matrix(nrow=16,ncol=16)
diff<-matrix(nrow=16,ncol=16)
diff_options<-matrix(nrow=16,ncol=16)

#rate lattice
for(j in n:0)#j - periods
{
  #rate_lattice
  for(i in j:0)#i from N to 0
  {
    rate_lattice[j-i+1,j+1]<-u^i*d^(j-i)*r
    message(paste("rate: t,j",rate_lattice[j-i+1,j+1],j-i+1,j+1))
  }
}

#elementary prices (starting from $1 => "price of the money" during the time)
for(j in 0:n)#j - periods
{
  #rate_lattice
  for(i in j:0)#i from N to 0
  {
    if(j == 0)
    {
      el_price[j-i+1,j+1]<-1
    }
    else
    {
      #3 variants: top[i=0]-center[0<i<j]-bottom[i=j]
      if(0<i && i<j)
        el_price[j-i+1,j+1]<-q*el_price[j-i,j]/(1+rate_lattice[j-i,j]) + (1-q)*el_price[j-i+1,j]/(1+rate_lattice[j-i+1,j])
      
      if(i == 0)#bottom
        el_price[j+1,j+1]<-(q*el_price[j,j])/(1+rate_lattice[j,j])
      
      if(i == j)#top
        el_price[1,j+1]<-(q*el_price[1,j])/(1+rate_lattice[1,j])
    }
  }
}


for(j in 0:n)#j - periods.
{
  for(i in 0:j)
  {
    all_zcb_prices[j+1]<-all_zcb_prices[j+1] + el_price[i+1,j+1] 
  }
}

n<-4
S<-1
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
      zcb_price[j-i+1,j+1]<-(q*zcb_price[j-i+1,j+2] + (1-q)*zcb_price[j-i+2,j+2])/(1+rate_lattice[j-i+1,j+1])
    }
  }
}

#book page 399: Ho-Lee model picture 14.11
#element prices in k=1
a<-1/2*1/1.0767 
#short rate is a root of equation Z(2,0)=forward_equation - k=2 (3 nodes)
root<-uniroot(function(x) a/(1+x)+a/(1+x+0.0002)-1/1.0827^2, lower = 0, upper = 100)

n<-10
S<-1
q<-0.5
b<-0.0002
spot_rates<-c(7.67,8.27,8.81,9.31,9.75,10.16,10.52,10.85,11.15,11.42,11.67)
a<-vector(length = 15)

#Quiz: fin 1 - week 6.
b<-0.05
spot_rates<-c(7.67,8.27,8.81,9.31,9.75,10.16,10.52,10.85,11.15,11.42,11.67)

#Fin 1 week6: excel file.
n<-14
spot_rates<-c(7.3,	7.62,	8.1,	8.45,	9.2,	9.64,	10.12,	10.45,	10.75,	11.22,	11.55,	11.92,	12.2,	12.32)
b<-0.01

#elementary prices with Ho-Lee model i.e. rates are variable.
for(j in 0:n)#j - periods.
{
  f<-function(x)
  {
    (-1)*S/(1+spot_rates[j]/100.)^j
  }
  bf<-body(f)
  
  message("M0")
  
  for(i in j:0)#i - set at the same period.
  {
    if(j==0)
    {
#      el_price[j-i+1,j+1]<-S
      rate_lattice[j-i+1,j+1]<-spot_rates[j+1]/100.
    }
    else
    {
#      short_rates[j-i+1,j+1]<-el_price[]
      #3 variants: top[i=0]-center[0<i<j]-bottom[i=j]
      if(0<i && i<j)
      {
#        el_price[j-i+1,j+1]<-q*el_price[j-i,j]/(1+a[j]+b*(j-i)) + (1-q)*el_price[j-i+1,j]/(1+a[j]+b*(j-i-1))
#        subst<-substitute(q*el_price[j-i,j]/(1+x+b*(j-i)) + (1-q)*el_price[j-i+1,j]/(1+x+b*(j-i-1)), list(i=i))#Ho-Lee
        subst<-substitute(q*el_price[j-i,j]/(1+x*exp(b*(j-i))) + (1-q)*el_price[j-i+1,j]/(1+x*exp(b*(j-i-1))), list(i=i))#BDT
        body(f)[[2]]<-substitute(a+b, list(a=subst,b=body(f)[[2]],i=i,j=j))
      }
      
      if(i == 0)#bottom
      {
#        el_price[j+1,j+1]<-(q*el_price[j,j])/(1+a[j])
#        subst<-quote((q*el_price[j,j])/(1+x))#Ho-Lee
        subst<-quote((q*el_price[j,j])/(1+x))#BDT - the same as Ho-Lee!
        body(f)[[2]]<-substitute(a+b, list(a=subst,b=body(f)[[2]],i=i,j=j))
        #bf[[2]]<-substitute(expression(a+b), list(a=1,b=bf[[2]]))
        message("M1")
      }
      
      if(i == j)#top
      {
#        el_price[1,j+1]<-(q*el_price[1,j])/(1+a[j]+b*(j-1))
#        subst<-quote((q*el_price[1,j])/(1+x+b*(j-1)))#Ho-Lee
        subst<-quote((q*el_price[1,j])/(1+x*exp(b*(j-1))))#BDT
        body(f)[[2]]<-substitute(a+b, list(a=subst,b=body(f)[[2]],i=i,j=j))
        #bf[[2]]<-substitute(expression(a+b), list(a=1,b=bf[[2]]))
        message("M2")
      }
    }
  }#i
  message(paste("j=",j))
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
  
  for(i in j:0)#i - set at the same period.
  {
    if(j==0)
    {
      el_price[j-i+1,j+1]<-S
      rate_lattice[j-i+1,j+1]<-spot_rates[j+1]/100.
    }
    else
    {
      #      short_rates[j-i+1,j+1]<-el_price[]
      #3 variants: top[i=0]-center[0<i<j]-bottom[i=j]
      if(0<i && i<j)
      {
#        el_price[j-i+1,j+1]<-q*el_price[j-i,j]/(1+a[j]+b*(j-i)) + (1-q)*el_price[j-i+1,j]/(1+a[j]+b*(j-i-1))#Ho-Lee
        el_price[j-i+1,j+1]<-q*el_price[j-i,j]/(1+a[j]*exp(b*(j-i))) + (1-q)*el_price[j-i+1,j]/(1+a[j]*exp(b*(j-i-1)))#BDT
      }
      
      if(i == 0)#bottom
      {
        el_price[j+1,j+1]<-(q*el_price[j,j])/(1+a[j])#Ho-Lee and BDT (the same!).
      }
      
      if(i == j)#top
      {
#        el_price[1,j+1]<-(q*el_price[1,j])/(1+a[j]+b*(j-1))#Ho-Lee
        el_price[1,j+1]<-(q*el_price[1,j])/(1+a[j]*exp(b*(j-1)))#BDT
      }
    }
  }#i

  #el_price and bel_price are the same (but more accurate algo representation is bel_price!).
  for(i in j:0)#i - set at the same period.
  {
    if(j==0)
    {
      bel_price[j-i+1,j+1]<-S
      rate_lattice[j-i+1,j+1]<-spot_rates[j+1]/100.
    }
    else
    {
      #      short_rates[j-i+1,j+1]<-el_price[]
      #3 variants: top[i=0]-center[0<i<j]-bottom[i=j]
      if(0<i && i<j)
      {
        #        el_price[j-i+1,j+1]<-q*el_price[j-i,j]/(1+a[j]+b*(j-i)) + (1-q)*el_price[j-i+1,j]/(1+a[j]+b*(j-i-1))#Ho-Lee
        bel_price[j-i+1,j+1]<-q*bel_price[j-i,j]/(1+a[j]*exp(b*(j-i))) + (1-q)*bel_price[j-i+1,j]/(1+a[j]*exp(b*(j-i-1)))#BDT
      }
      
      if(i == j)#bottom
      {
        bel_price[j+1,j+1]<-(q*bel_price[j,j])/(1+a[j])#Ho-Lee and BDT (the same!).
      }
      
      if(i == 0)#top
      {
        #        el_price[1,j+1]<-(q*el_price[1,j])/(1+a[j]+b*(j-1))#Ho-Lee
        bel_price[1,j+1]<-(q*bel_price[1,j])/(1+a[j]*exp(b*(j-1)))#BDT
      }
    }
  }#i
  
  #find zcb for j => find short rate (by solving equation)
  for(i in 0:j)
  {
    zcb_price[j+1]<-zcb_price[j+1] + el_price[i+1,j+1]
  }
  
}#j

# f<-function(x)
# {
#   r+ ( x+10 + x^5)
# }
# 
# bf<-body(f)
# 
# for(i in 1:2)
# {
# 
#   sol<-uniroot(fun, lower = -100, upper = 100)
#   message(paste(sol))
# }
# 
# substitute(expression(a + b), list(a = bf[[2]]))
