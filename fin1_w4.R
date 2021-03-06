#Quiz week 4
#n<-15
T<-0.25
S<-100
r<-0.02
sigma<-0.3
c<-0.01
K<-100

#Excel
# n<-10
# T<-0.5
# S<-100
# r<-0.02
# sigma<-0.2
# c<-0.01
# K<-100

#metodic
# n<-3
# u<-1.07
# d<-1/u
# R<-1.01
# K<-100
# q<-(R-d)/(u-d)

#book page 336
# n<-6
# u<-1.02
# d<-0.99
# R<-1.01
# S<-100
# K<-102
# #q<-(R-d)/(u-d)
# q<-2/3

result<-c(0)

sum<-0
remove(sec_price)
remove(fut_price)
remove(chooser_price)
remove(call_option_euro_price)
remove(call_option_euro_fut_price)
remove(put_option_euro_price)
remove(put_option_amer_price)
remove(diff)
remove(diff_optons)

sec_price<-matrix(nrow=16,ncol=16)
fut_price<-matrix(nrow=16,ncol=16)
chooser_price<-matrix(nrow=16,ncol=16)
call_option_euro_price<-matrix(nrow=16,ncol=16)
call_option_euro_fut_price<-matrix(nrow=16,ncol=16)
put_option_euro_price<-matrix(nrow=16,ncol=16)
put_option_amer_price<-matrix(nrow=16,ncol=16)
diff<-matrix(nrow=16,ncol=16)
diff_options<-matrix(nrow=16,ncol=16)

N<-15
R<-exp(r*T/N)
u<-exp(sigma*sqrt(T/N))
d<-1/u
q<-(exp((r-c)*T/N) - d)/(u-d)

for(j in N:0)#j - periods
{
  #max<-max(S*u^j*d^(n-j) - K,0) # call price
  max<-max(K - S*u^j*d^(n-j),0) # put price
  
  #security price
  for(i in j:0)#cells in period time
  {
    sec_price[j-i+1,j+1]<-u^i*d^(j-i)*S
  }
  
  #futures price
  for(i in j:0)
  {
    fut_price[j-i+1,j+1]<-u^i*d^(j-i)*S/R^(j-N)
  }
}

n<-15
R<-exp(r*T/n)
u<-exp(sigma*sqrt(T/n))
d<-1/u
q<-(exp((r-c)*T/n) - d)/(u-d)

for(j in n:0)#j - periods
{
  #max<-max(S*u^j*d^(n-j) - K,0) # call price
  max<-max(K - S*u^j*d^(n-j),0) # put price
  
  # #security price
  # for(i in j:0)#cells in period time
  # {
  #   sec_price[j-i+1,j+1]<-u^i*d^(j-i)*S
  # }
  # 
  # #futures price
  # for(i in j:0)
  # {
  #   fut_price[j-i+1,j+1]<-u^i*d^(j-i)*S/R^(j-n)
  # }
  
  #put option price
  for(i in j:0)
  {
    if(j==n)
    {
 #     chooser_price[j-i+1,j+1]<-max(sec_price[j-i+1,j+1]-K,K-sec_price[j-i+1,j+1])
      #
      call_option_euro_price[j-i+1,j+1]<-max(sec_price[j-i+1,j+1]-K,0)
      call_option_euro_fut_price[j-i+1,j+1]<-max(fut_price[j-i+1,j+1]-K,0)
      #
      put_option_euro_price[j-i+1,j+1]<-max(K-sec_price[j-i+1,j+1],0)
      put_option_amer_price[j-i+1,j+1]<-max(K-sec_price[j-i+1,j+1],0)
    }
    else
    {
#      chooser_price[j-i+1,j+1]<-(q*chooser_price[j-i+1,j+2] + (1-q)*chooser_price[j-i+2,j+2])/R
      #
      call_option_euro_price[j-i+1,j+1]<-(q*call_option_euro_price[j-i+1,j+2] + (1-q)*call_option_euro_price[j-i+2,j+2])/R
      #
      call_option_euro_fut_price[j-i+1,j+1]<-(q*call_option_euro_fut_price[j-i+1,j+2] + (1-q)*call_option_euro_fut_price[j-i+2,j+2])/R
      call_option_euro_fut_price[j-i+1,j+1]<-max((fut_price[j-i+1,j+1]-K), call_option_euro_fut_price[j-i+1,j+1])
      #
      put_option_euro_price[j-i+1,j+1]<-(q*put_option_euro_price[j-i+1,j+2] + (1-q)*put_option_euro_price[j-i+2,j+2])/R
      put_option_amer_price[j-i+1,j+1]<-(q*put_option_amer_price[j-i+1,j+2] + (1-q)*put_option_amer_price[j-i+2,j+2])/R
      put_option_amer_price[j-i+1,j+1]<-max((K-sec_price[j-i+1,j+1]), put_option_amer_price[j-i+1,j+1])
      diff_options[j-i+1,j+1]<-put_option_amer_price[j-i+1,j+1]-put_option_euro_price[j-i+1,j+1]
    }
  }

  #american option: estimation if early will not loose!
  for(i in j:0)
  {
    if((K-sec_price[j-i+1,j+1]) > call_option_euro_fut_price[j-i+1,j+1])
    {
      diff[j-i+1,j+1]<-(K-sec_price[j-i+1,j+1]) - call_option_euro_fut_price[j-i+1,j+1]
    }
  }
  
#  max_exec<-max(K - S*u^j*d^(i-j),cell_option_price) # put exec
  
#  if(max_exec>max)
  {
    # message(paste("exec: j=",j))
    # message(paste("exec: max_exec=",max_exec," vs max=",max))
  }
  
#  sum<-sum+nchoosek(n,j)*q^j*(1-q)^(n-j)*max
}

result<-sum/R^n

#chooser n=10 on put/call with N=15: the same lattice as as for call/put
#n<-10
R<-exp(r*T/n)
u<-exp(sigma*sqrt(T/n))
d<-1/u
q<-(exp((r-c)*T/n) - d)/(u-d)
n<-10

for(j in n:0)#j - periods
{
  #max<-max(S*u^j*d^(n-j) - K,0) # call price
  max<-max(K - S*u^j*d^(n-j),0) # put price
  
  #put option price
  for(i in j:0)
  {
    if(j==n)
    {
      chooser_price[j-i+1,j+1]<-max(call_option_euro_price[j-i+1,j+1],put_option_euro_price[j-i+1,j+1])
    }
    else
    {
      chooser_price[j-i+1,j+1]<-(q*chooser_price[j-i+1,j+2] + (1-q)*chooser_price[j-i+2,j+2])/R
    }
  }
  
  #american option: estimation if early will not loose!
  for(i in j:0)
  {
    if((K-sec_price[j-i+1,j+1]) > call_option_euro_fut_price[j-i+1,j+1])
    {
      diff[j-i+1,j+1]<-(K-sec_price[j-i+1,j+1]) - call_option_euro_fut_price[j-i+1,j+1]
    }
  }
  
  #  max_exec<-max(K - S*u^j*d^(i-j),cell_option_price) # put exec
  
  #  if(max_exec>max)
  {
    # message(paste("exec: j=",j))
    # message(paste("exec: max_exec=",max_exec," vs max=",max))
  }
  
  #  sum<-sum+nchoosek(n,j)*q^j*(1-q)^(n-j)*max
}
