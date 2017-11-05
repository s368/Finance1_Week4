# question 1: n=30 years, c= 5% yearly, principal M=400000

n<-360 # months i.e. 30 years.
c<-0.05/12 # i.e. monthly rate.
M<-400000

B<-c*(1+c)^n*M/((1+c)^n-1)
round(B,2) # answer on question 1.

#question 2.
n<-20*12

for(i in 1:n)
{
  
}