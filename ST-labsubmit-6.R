#problem-6
# Given data
yields = matrix(c(
  4.32, 4.84, 3.96, 4.04,
  4.16, 4.36, 3.50, 5.00,
  3.06, 4.24, 4.76, 3.12,
  4.00, 4.84, 4.32, 3.72,
  4.12, 4.68, 3.46, 4.02,
  4.08, 3.96, 3.42, 3.08,
  5.16, 4.24, 4.96, 3.84,
  4.40, 4.72, 4.04, 3.98,
  4.20, 4.66, 3.64, 5.00,
  4.28, 4.36, 3.00, 3.52
),nrow=10,byrow=TRUE)
M=16; N=100

clusters_2ss_equal=function(N,M,yields){
n=nrow(yields)  #number of selected cluster
m=ncol(yields)  #number of sampled element per cluster

#field mean(cluster mean)
ybar_i=rowMeans(yields)

#overall mean 

ybar=mean(ybar_i)

#within cluster variance 

s_w2=mean(apply(yields,1,var))


#between cluster 

s_b2=var(ybar_i)

var_ybar=(1-n/N)*(s_b2/n)+(1-m/M)*(s_w2/(m*n))

se_ybar=sqrt(var_ybar)

CI_upper=ybar+1.96*se_ybar
CI_lower=ybar-1.96*se_ybar


#ii)

fract=1/(N*M-1)
part1=M*(N-1)*s_b2
part2=(N*(M-1)-(M-m)*(N-1)/m)*s_w2
s_2_srs=(part1+part2)*fract
fract1=(1/(n*m))-(1/(N*M))
var_srs=fract1*s_2_srs

#iii)

C=100
c1=4
c2=1
m_opt=round(sqrt((c1/c2)*(s_w2/(s_b2-s_w2/m))))
m_opt
n_opt=round((C/18))

#Return results 

return(list(
      Mean_per_hectare=round(ybar,4),
      Standard_error=round(se_ybar,4),
      Upper=round(CI_upper,4),
      Lower=round(CI_lower,4),
      Variance_SRS=var_srs,
      optimum_value_of_m=m_opt,
      optimum_value_of_n=n_opt
 ))
}
clusters_2ss_equal(M=16, N=100 ,yields)















































