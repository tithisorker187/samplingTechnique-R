M=c(102,105,200,88)
M
tehsil_name=c("Behrar","Bairath","Ajmer","Bansur")
tehsil_name 
#sheep population 
Behrar=c(266,890,311,46,174,31,17,186,224,31,102,46,31,109,275,128,125,267,153,152,84,21,52,10,0,48,94,123,87,89,109,0,310,3)
Behrar
Bairath=c(129, 57, 64, 11, 163, 77, 278, 50, 26, 127, 252, 194, 350, 0, 
572, 149, 275, 114, 387, 53, 34, 150, 224, 185, 157, 244, 466, 
203, 354, 816, 242, 140, 66, 590, 747, 147)
Bairath
Ajmer=c(247, 622, 225, 278, 181, 132, 659, 403, 281, 236, 595, 265, 
431, 190, 348, 232, 88, 1165, 831, 120, 987, 938, 197, 614, 
187, 896, 330, 485, 60, 60, 1051, 651, 552, 968, 987)
Ajmer

Bansur=c(347, 362, 34, 11, 133, 36, 34, 61, 249, 170, 112, 42, 161, 75, 
68, 0, 247, 186, 473, 0, 143, 198, 65, 0, 308, 122, 345, 0, 223,302, 219, 120, 199, 35, 0,0)
Bansur

#combine villages 
villages=list(Behrar,Bairath,Ajmer,Bansur)
villages
Mbar=124
wi=M/Mbar
wi
N=12
n=4

mi=sapply(villages,length)
mi 
ybar_i=sapply(villages,mean)
ybar_i
s_w2=sapply(villages,var) #variance within tehsil
s_w2
ybar_hat = sum(M * ybar_i) / (n * Mbar)
ybar_hat

# Between-cluster variance
 
s_b2= sum((wi * ybar_i - ybar_hat)^2) / (n - 1)
s_b2
term1=(1-n/N) * s_b2/n
term1
term2 =sum(wi^2 * (1-mi/M)* (s_w2/mi)) / (N * n)
term2
var_ybar <- term1 + term2
se_ybar <- sqrt(var_ybar)
se_ybar


CI_upper=ybar_hat+1.96*se_ybar
CI_upper
CI_lower=ybar_hat-1.96*se_ybar
CI_lower
sheep_total=N*Mbar*ybar_hat
sheep_total















































