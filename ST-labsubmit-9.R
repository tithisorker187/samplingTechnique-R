#problem-9

M=50
m=10 
Mi=c(52,56,60,46,49,51,50,61,60,45)
yi=list(
       c(12,11,12,10,13),
       c(10,9,7,9,8,10),
       c(6,5,7,5,6,4),
       c(7,8,7,7,6),
       c(10,11,13,12,12),
       c(14,15,13,12,13),
       c(6,7,6,8,7),
       c(9,10,8,9,9,10),
       c(7,10,8,9,9,10), 
       c(12,11,12,13,12,12))
yi

#(a)estimate average heights 
plot_mean=sappy(yi,mean) 
ybar=mean(plot_mean)

cat("Average heights of seedling in the field",ybar,"\n")

#(B) 95% CI 

#within variance 
s_w2=mean(sapply(yi,var))
#between variance 
s_b2=var(plot_mean)

#average number of trees per sampled plot 
M_bar=mean(Mi) 

#Total variance of estimate
var_est=((1-m/M)*s_b2/m)+ s_w2/(M_bar*m)
se=sqrt(var_est)

CI=ybar+c(-1,1)*1.96*se

cat("95% confidence interval for average",CI,"\n")

#(c) comment 


#=== (c) Interpretation & Comment ===
#• Estimated average seedling height: 9.24 units
#• 95% CI: (7.89, 10.59) → We are 95% confident the true mean lies here
#• Two-stage cluster sampling used: 10 out of 50 plots selected randomly
#• All trees measured in selected plots → no subsampling within plots
#• Variance split:
   #- Between plots: high (7.88) → plots differ a lot
   #- Within plots: low (1.89) → trees in same plot are similar
#• Wide CI due to high plot-to-plot variation
#• Method is cost-efficient: measure fewer plots, full data in each

#"The estimated average seedling height is 9.24 units with 95% confidence interval (7.89, 10.59).
# The wide interval is due to high between-plot variation. 
#Two-stage cluster sampling is efficient as it measures all trees in fewer plots,
# reducing cost and within-plot error."


















































