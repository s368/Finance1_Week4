# question 1: n=30 years, c= 5% yearly, principal M=400000

n<-360 # months i.e. 30 years.
c<-0.05/12 # i.e. monthly rate.
M<-400000

B<-c*(1+c)^n*M/((1+c)^n-1)
round(B,2) # answer on question 1.

#question 2.
n<-20*12
c<-0.06/12
r<-0.05/12
M<-400000000
season<-0

#Excel
# n<-30*12
# c<-0.08125/12
# r<-0.075/12
# M<-400
# season<-3

ans<-c*(1+c)^n*M/((1+c)^n-1) * ((1+r)^n-1)/r/(1+r)^n

#psa 100% for "prepaiment rate".
psa_beg<-0.002 #*2
psa_end<-0.06  #*2

cpr<-vector(length = n)
smm<-vector(length = n)
amt_beg<-vector(length = n)
amt_end<-vector(length = n)
pay_month<-vector(length = n)
interest_in<-vector(length = n)
interest_out<-vector(length = n)
principal_pay<-vector(length = n)
principal_pre<-vector(length = n)

for(i in 1:n)
{
  if(i <= 30)
  {
    cpr[i]<-psa_beg+(psa_end-psa_beg)*(i+season-1)/(30-1)
  }
  else
  {
    cpr[i]<-psa_end
  }
  
  smm[i]<-1-(1-cpr[i])^(1/12)
  
  if(i == 1)
  {
    amt_beg[i]<-M
  }
  else
  {
    amt_beg[i]<-amt_end[i-1]
  }
  
  if(i==1)
  {
    pay_month[i]<-c*M*(1+c)^(n-season)/((1+c)^(n-season)-1)
  }
  else
  {
    pay_month[i]<-c*amt_end[i-1]*(1+c)^(n-season-i+1)/((1+c)^(n-season-i+1)-1)
  }
  
  interest_in[i]<-amt_beg[i]*c
  interest_out[i]<-amt_beg[i]*r
  
  principal_pay[i]<-pay_month[i] - interest_in[i]
  principal_pre[i]<-(amt_beg[i]-principal_pay[i])*smm[i]
  
  amt_end[i]<-amt_beg[i] - principal_pay[i] - principal_pre[i]
}

#answer question 2 = 171.18
answer_2<-round(sum(interest_out, na.rm = TRUE)/1000000,2)

#answer question 3 = 181.09
answer_3<-round(sum(principal_pre,na.rm = TRUE)/1000000,2)

#book page 47: example 3.2 (Loan Calculation).
n<-12*5 # 5 years
r<-0.12/12
p<-1000
a<-r*(1+r)^n*p/((1+r)^n-1)
round(a,2) # answer on question 1.

#book page 404: example 14.9 (Quick, buy this CMO).
n<-12*30 # 30 years -> in months
r<-0.12/12 # year rate -> in month compounding
p<-100
a_monthly<-r*(1+r)^n*p/((1+r)^n-1)
round(a_monthly,2) # answer on question 1.

n<-30 # 30 years -> in months
r<-0.12 # year rate -> in month compounding
p<-100
a_yearly<-r*(1+r)^n*p/((1+r)^n-1)
round(a_yearly,2) # answer on question 1.

u<-0.98
d<-0.95
q<-0.5

#principal_left<-13.725*1.12-12.41*0.960+2*3.00-(0.960-0.941)*(13.725+50+25*1.12^3)
principal_left<-13.725*1.12-a_yearly*u^2+2*3.00-(u^2-u^3)*(13.725+50+25*1.12^3)
# 7.551 - book - in reality 7.550567!
# 7.580 - my

#value<-7.551*1.12/1.145+12.41*0.960-2*3.00+(0.960-0.941)*(13.725+50+25*1.12^3)
value<-principal_left*1.12/1.145+a_yearly*u^2-2*3.00+(u^2-u^3)*(13.725+50+25*1.12^3)
# 15.207 - book
# 15.178 - my
