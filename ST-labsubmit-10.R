#problem-10

#stratum size 
N1=1580
N2=430
N=N1+N2

#starum mean and mean 
ybar=26.30 
ybar_1=19.40 
ybar_2=51.63

#variance 

S2_Y1=282
S2_Y2=922

#weighted stratum size 
W1=N1/N 
W2=N2/N 

#weighted average of within stratum variance of Y
Vn=W1*S2_Y1+W2*S2_Y2

#between stratum variance of stratum mean
Vn_prime = W1*(ybar_1-ybar)^2+W2*(ybar_2-ybar)^2

C=100 
c1=0.1 #cost of measure X(first stage) cheap 
c2=1 #cost of measure 2nd stage (expensive )

#optimal ratio of sample sizes 
r=sqrt((c1/c2)*(Vn/Vn_prime))
n_prime= round(C/(c1+c2*r))
n=round(r*n_prime)


#Optimal variance odf the estimator 

Vopt=(Vn/n)+(Vn_prime/n_prime)
V=620/100   #sample size=100

#relative efficiency
Re=V/Vopt 

gain=(Re-100)*100


cat ( "within stratum variance", Vn,"\n")
cat( "between stratum variance of stratum mean",Vn_prime,"\n")

cat("Gain of double sampling ", Re,"\n")


#interpretation : Gain=2.4% lower variance for same cost. 
#double sampling is more efficient 































