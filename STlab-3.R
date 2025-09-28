#problem-3 
#Lahiris Method 
set.seed(42)
size=c(150,50,100,200,160,40)
size
#N=total length of orchards size
N=length(size)
N
#M is the maximum size of orchards 
M=max(size)
M
sample_size=3
sample_size

lahiri_draw=function(){
            repeat{
#lets take a random number i (1 to N)
               i=sample.int(N,1)
#Lets take another random number j(1 to M)
               j=sample.int(M,1)
#condition j<=size[i]               
                if(j<=size[i]){
                
                return(c(i,j,size[i]))
                }
              }
            }
pairs=replicate(sample_size,lahiri_draw())
pairs=t(pairs)
colnames(pairs)=c("Orchard","Trialsize","size")
pairs



           