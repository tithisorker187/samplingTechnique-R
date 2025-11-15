#problem-8

district=c("Banderban","Chittagong","Khagrachari","Comila","Noakhali","Rangamati",
"Sylhet","Dhaka","Faridpur","Jamalpur","Kishorgonj","Mymensingh","Tangail","Barisal",
"Jessore","Khulna","Kustia","Patuakhali","Bogra","Dinajpur","Pabna","Rajshahi","Rangpur")

X=c(61,1079,30,1519,1036,48,2309,936,1018,811,1341,1715,561,133,1452,1134,567,1027,1169,1573,738,1799,2243)

Y=c(48, 994, 26, 1313, 779, 40, 1512, 859, 577, 723, 
       1121, 928, 483, 662, 1352, 853, 479, 543, 1093, 1069, 
       660, 1753, 1873)

#(I) #draw 5 random sample using srs and pps
N=length(X)
N
n=5
#random sample draw using srs 
srs=sample(1:N,n)
srs 
srs_district=district[srs]
srs_district
srs_Y=Y[srs]
srs_Y 

#pps sample draw 
pi=X/sum(X) 
pps=sample(1:N,n,prob=pi)
pps 
pps_district=district[pps]
pps_district
pps_X=X[pps]
pps_X
pps_Y=Y[pps]
pps_Y

##(ii) 

#srs average production and total production 
#without replacement 
ybar_srs=mean(srs_Y)
ybar_srs
var_srs_yi=var(srs_Y)
var_srs_yi 
var_srs=(1-n/N)*(var_srs_yi/n)
var_srs
se_srs=sqrt(var_srs)
se_srs
mean_total_srs=N*ybar_srs
mean_total_srs

se_total_srs=N*se_srs
se_total_srs



#with replacement

ybar_srswr=mean(srs_Y)
ybar_srswr
var_srswr_yi=var(srs_Y)
var_srswr_yi

var_srswr=var_srswr_yi/n
var_srswr

se_srswr=sqrt(var_srswr)
se_srswr

mean_total_srswr=N*ybar_srswr
se_total_srswr=N*se_srswr
se_total_srswr


#pps with replacement

zi=pps_Y/pi[pps]
zi 
ybar_pps=mean(zi)
ybar_pps 
var_pps_zi=var(zi)
var_pps_zi 
var_pps=var_pps_zi/n
var_pps
se_pps=sqrt(var_pps)
se_pps 

mean_total_pps=N*ybar_pps 
se_total_pps=N*se_pps


#pps without replacement 

total_hat=sum(pps_Y/pi[pps])  #Horvitz thompson estimator 
total_hat
mean_hat=total_hat/N
mean_hat

#Yates_Graundy style
var_ppswor=(1-n/N)*var(zi)/n
var_ppswor
se_ppswor=sqrt(var_ppswor)
se_ppswor

total_mean_ppswor=N*ybar_ppswor
total_mean_ppswor

##(iii)

CI_srs_avg=ybar_srs+c(-1,1)*1.96*se_srs
CI_srs_avg 
CI_srs_total=mean_total_srs+c(-1,1)*1.96*se_total_srs
CI_srs_total


RE_srs=(se_srs/ybar_srs)*100
RE_srs
RE_srswr=(se_srswr/ybar_srswr)*100
RE_srswr









































