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

#question 4.
#first year
d1<-1/(1+0.05/2)
d2<-1/(1+0.05/2)^2
f<-function(x) {d1*(5*(1-x)+10*x)+d2*(105*(1-x)^2+10*(1-x)*x)-100.923497917906}
rrr<-uniroot(f,lower = 0, upper = 1, tol = 10e-10)
round(rrr$root,4)*100 # the answer = 2.12

# question 5 (CDS pricing).
n<-8
r<-0.01
S<-218.88953946553/4
nominal<-1000000
R<-0.45

remove(discount)
remove(hazard_rate)
remove(survival)
remove(exp_premium)
remove(pv_premium)
remove(default)
remove(ai)
remove(pv_ai)
remove(exp_protection)
remove(pv_protection)

discount<-vector(length = 21)
hazard_rate<-vector(length = 21)
survival<-vector(length = 21)
exp_premium<-vector(length = 21)
pv_premium<-vector(length = 21)
default<-vector(length = 21)
ai<-vector(length = 21)
pv_ai<-vector(length = 21)
exp_protection<-vector(length = 21)
pv_protection<-vector(length = 21)

for(i in 0:n)
{
  if(i == 0)
  {
    discount[i+1]<-1.
    hazard_rate[i+1]<-0.020140573040198/2.
    survival[i+1]<-100.
  }
  else
  {
    discount[i+1]<-1/(1+r/4)^i
    hazard_rate[i+1]<-0.020140573040198/2.
    
    if(0<=3*i && 3*i<12) hazard_rate[i+1]<-0.020140573040198/2. # first year
    if(12<=3*i && 3*i<24) hazard_rate[i+1]<-0.019443103338663/2. # second year
    if(24<=3*i && 3*i<36) hazard_rate[i+1]<-0.0199765188714693/2. # third year
    
    survival[i+1]<-survival[i]*(1-hazard_rate[i])
    
    exp_premium[i+1]<-S*survival[i+1]/100.
    pv_premium[i+1]<-exp_premium[i+1]*discount[i+1]*nominal*0.0001
    
    default[i+1]<-survival[i]*hazard_rate[i]
    ai[i+1]<-S/2*default[i+1]/100.
    pv_ai[i+1]<-ai[i+1]*discount[i+1]*nominal*0.0001
    
    exp_protection[i+1]<-(1-R)*default[i+1]/100.
    pv_protection[i+1]<-exp_protection[i+1]*discount[i+1]*nominal
  }
}

sum_protection<-0
sum_premium<-0

for(i in 1:n)
{
  sum_protection<-sum_protection + pv_protection[i+1]
  sum_premium<-sum_premium + pv_premium[i+1] + pv_ai[i+1]
}

spread<-sum_protection/sum_premium
diff<-sum_protection - sum_premium
