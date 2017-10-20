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

remove(rate_lattice)
remove(zcb_price)
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

for(j in n:0)#j - periods
{
  #rate_lattice
  for(i in j:0)#i from N to 0
  {
    rate_lattice[j-i+1,j+1]<-u^i*d^(j-i)*r
    message(paste("rate: t,j",rate_lattice[j-i+1,j+1],j-i+1,j+1))
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
      zcb_price[j-i+1,j+1]<-(q*zcb_price[j-i+1,j+2] + (1-q)*zcb_price[j-i+2,j+2])/(1+rate_lattice[j-i+1,j+1])
    }
  }
}

#(fin_1=>week_5=>quiz_question_5)forward-starting swap
for(j in n:0)#j - periods.
{
    for(i in j:0)#i - set at the same period.
  {
    message(paste("PRE:forward_swap_price: j,i",forward_swap_price[j-i+1,j+1],j,i))
      
    if(j==n)
    {
      forward_swap_price[j-i+1,j+1]<-(rate_lattice[j-i+1,j+1]-rate_swap)/(1+rate_lattice[j-i+1,j+1])
#      message(paste("forward_swap_price: t,j",forward_swap_price[j-i+1,j+1],j-i+1,j+1))
      message("A")
    }
    else if( 0<j && j<n)
    {
      forward_swap_price[j-i+1,j+1]<-((rate_lattice[j-i+1,j+1]-rate_swap)+q*forward_swap_price[j-i+1,j+2] + (1-q)*forward_swap_price[j-i+2,j+2])/(1+rate_lattice[j-i+1,j+1])
#      message(paste("forward_swap_price: t,j",forward_swap_price[j-i+1,j+1],j-i+1,j+1))
      message("B")
    }
    else if( j == 0)
    {
      forward_swap_price[j-i+1,j+1]<-(q*forward_swap_price[j-i+1,j+2] + (1-q)*forward_swap_price[j-i+2,j+2])/(1+rate_lattice[j-i+1,j+1])
#      message(paste("forward_swap_price: t,j",forward_swap_price[j-i+1,j+1],j-i+1,j+1))
      message("C")
    }
  }
}

n<-5
K<-0
#(fin_1=>week_5=>quiz_question_6) swaption: exec=5 till n=10 (last payment at 11).
for(j in n:0)#j - periods.
{
  for(i in j:0)#i - set at the same period.
  {
    if(j==n)
    {
      swaption_price[j-i+1,j+1]<-max(0,forward_swap_price[j-i+1,j+1] - K)
      message(paste("swaption:A",swaption_price[j-i+1,j+1]))
    }
    else
    {
      swaption_price[j-i+1,j+1]<-(q*swaption_price[j-i+1,j+2] + (1-q)*swaption_price[j-i+2,j+2])/(1+rate_lattice[j-i+1,j+1])
      message("swaption:B")
    }
  }
}


# n<-6
# #call option euro on zcb(zero coupon bonds)
# for(j in n:0)#j - periods.
# {
#   #zcb (zero coupon bonds)
#   for(i in j:0)#i - set ate the same period.
#   {
#     if(j==n)
#     {
#       call_option_euro_zcb_price[j-i+1,j+1]<-max(0,zcb_price[j-i+1,j+1] - K)
#       call_option_amer_zcb_price[j-i+1,j+1]<-max(0,zcb_price[j-i+1,j+1] - K)
#     }
#     else
#     {
#       call_option_euro_zcb_price[j-i+1,j+1]<-(q*call_option_euro_zcb_price[j-i+1,j+2] + (1-q)*call_option_euro_zcb_price[j-i+2,j+2])/(1+rate_lattice[j-i+1,j+1])
#       call_option_amer_zcb_price[j-i+1,j+1]<-(q*call_option_amer_zcb_price[j-i+1,j+2] + (1-q)*call_option_amer_zcb_price[j-i+2,j+2])/(1+rate_lattice[j-i+1,j+1])
#       call_option_amer_zcb_price[j-i+1,j+1]<-max(call_option_amer_zcb_price[j-i+1,j+1],zcb_price[j-i+1,j+1] - K)
#     }
#   }
# }
# 
# for(j in n:0)#j - periods.
# {
#   #american option: estimation if early will not loose!
#   for(i in j:0)
#   {
#     if((zcb_price[j-i+1,j+1]-K) > call_option_amer_zcb_price[j-i+1,j+1])
#     {
#       diff[j-i+1,j+1]<-(zcb_price[j-i+1,j+1]-K) - call_option_amer_zcb_price[j-i+1,j+1]
#     }
#   }
# }