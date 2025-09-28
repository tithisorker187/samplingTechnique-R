#problem-4
#Lahiris Method

set.seed(42)

size=c(50,30,45,25,40,26,24,35,28,27)
size
sample_size=4
sample_size
N=length(size)   #N = length of the holdings
N
M=max(size)      #Maximum size of holding
M

lahiris_draw=function(){
       
                    repeat{
#lets take sample i (1 to N )
                   i=sample.int(N,1) 
#lets take another sample j( 1 to M)
                   j=sample.int(M,1)
#where j <=size[i]

                       if(j <= size[i]){
                       return(c(i,j,size[i]))[1]
            }
      }
 }

pairs=replicate(sample_size,lahiris_draw())
pairs=t(pairs)
colnames(pairs)=c("Orchard","Trialsize","Size")
pairs









