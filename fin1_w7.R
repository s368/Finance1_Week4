# question 1: n=30 years, c= 5% yearly, principal M=400000

n<-360 # months i.e. 30 years.
c<-0.05/12 # i.e. monthly rate.
M<-400000

B<-c*(1+c)^n*M/((1+c)^n-1)
round(B,2) # answer on question 1.

#question 2.
n<-20*12

# for(i in 1:n)
# {
#   
# }

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

principal_left<-13.725*1.12-12.41*0.960+2*3.0-(0.960-0.941)*(13.725+50+25*1.12^3)

