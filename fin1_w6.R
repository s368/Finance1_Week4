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
el_price<-matrix(nrow=11,ncol=11)
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

