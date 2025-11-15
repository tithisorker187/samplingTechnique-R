#problem-8

district = c("Banderban", "Chittagong", "Khagrachari", "Comilla", "Noakhali",
  "Rangamati", "Sylhet", "Dhaka", "Faridpur", "Jamalpur",
  "Kishorganj", "Mymensingh", "Tangail", "Barisal", "Jessore",
  "Khulna", "Kushtia", "Patuakhali", "Bogra", "Dinajpur",
  "Pabna", "Rajshahi", "Rangpur")


X = c(61, 1079, 30, 1519, 1036, 48, 2309, 936, 1018, 811, 
       1341, 1715, 561, 133, 1452, 1134, 567, 1027, 1169, 1573, 
       738, 1799, 2243)


Y = c(48, 994, 26, 1313, 779, 40, 1512, 859, 577, 723, 
       1121, 928, 483, 662, 1352, 853, 479, 543, 1093, 1069, 
       660, 1753, 1873)

N=length(X)
n=5
#========================================================
#(a) draw sample using srs and pps WOR
#=========================================================

###A SRSWOR sample draw 

set.seed(123)
srs=sample(1:N,n)
srs_district=district[srs]
srs_Y=Y[srs]
srs_Y


###PPS sample(size X)

set.seed(10)
pi=X/sum(X)
pps=sample(1:N,n,prob=pi)
pps_district=district[pps]


#=======================================================================
#######(B) average production and total production for both samples########
#=========================================================================


###mean srs ,variance, standard error

ybar_srs=mean(srs_Y)
var_srs_yi=var(srs_Y) #variance of yi

var_srs=(1-n/N)*var_srs_yi   #variance of ybar_srs
se_srs=sqrt(var_srs)

#Total production estimate 

ybar_total_srs=N*ybar_srs
se_total_srs=N*se_srs

####for pps with replacement average production,Total production, CI 

pps_Y=Y[pps]
pps_X=X[pps]

##hansen_hurwitz estimator:mean of zi=(yi/pi)

zi=pps_Y/pi[pps]
ybar_pps=mean(zi)

##variance of zi 
var_zi=var(zi)

#variance of ybar_pps : var(zi)/n
var_ybar_pps=var_zi/n
se_pps=sqrt(var_zi/n)

##total production 
ybar_total_pps=N*ybar_pps
se_total_pps=N*ybar_pps

#=============================================================================
#################(c)95% confidence interval and relative precision ##############
#============================================================================
#95% confidence interval for mean and total production===============

CI_mean_srs=ybar_srs+c(-1,1)*1.96*se_srs

CI_total=ybar_total_srs+c(-1,1)*1.96*se_total_srs


#95%Confidence interval for mean and total production==================
CI_mean_pps=ybar_pps+c(-1,1)*1.96*se_pps   #for average

CI_total_pps=total_ybar_pps+c(-1,1)*1.96*total_se_pps  #for total 

#####relative precision : re=(standard error/estimate mean)*100

RE_srs=(se_srs/ybar_srs)*100
RE_pps=(se_pps/ybar_pps)*100

cat("Confidence interval for average production of srs",CI_mean_srs,"\n")
cat("Confidence interval for total production of srs",CI_total_srs,"\n")
 
cat("Confidence interval for average production of pps",CI_mean_pps,"\n")
cat("Confidence interval for average production of pps",CI_mean_pps,"\n")
cat("Related efficiency of srs ",RE_srs,"\n")
cat("Related efficiency of pps",RE_pps,"\n")











































