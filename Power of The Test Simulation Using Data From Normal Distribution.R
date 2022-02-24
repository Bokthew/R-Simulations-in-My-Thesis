# define variable to count the condition for each method
nbartlett=0
nlevene=0
nbf=0

# create a function with input sample size (n1,n2,n3),
  # standard deviation (sd1,sd2,sd3), and teoritical type 1 error (alpha)
normal<-function(n1,n2,n3,sd1,sd2,sd3,alpha){
  
# create a looping inside the functon to execute each method 10.000 times  
  for (i in 1:10000){
    
# generate data from normal distribution    
    DN1<-rnorm(n1,0,sd1)
    DN2<-rnorm(n2,0,sd2)
    DN3<-rnorm(n3,0,sd3)
    DN<-c(DN1,DN2,DN3)
    a<-rep("A",n1)
    b<-rep("B",n2)
    c<-rep("C",n3)
    populasi<-c(a,b,c)
    data=data.frame(DN,populasi)
    
# import library that needed
    library(car)
    library(onewaytests)
    
# run each method
    B=bartlett.test(data$DN~data$populasi, data=data)
    L=leveneTest(data$DN~data$populasi, data=data)
    BF=bf.test(data$DN~data$populasi, alpha=alpha, data=data)
    
# calculate the desired condition     
    if (B[3]>alpha){
      nbartlett=nbartlett+1}
    if (L[3]>alpha){
      nlevene=nlevene+1}	
    if (BF[3]>alpha){
      nbf=nbf+1}
  }
# calculate the probability for each method
    bartlett <- 1- (nbartlett/10000)
    levene <- 1- (nlevene /10000)
    bf <- 1- (nbf /10000)
    
# print the results
    hasil <- matrix(c(bartlett, levene, bf), nrow=1, ncol=3)
    colnames (hasil)= c("Bartlett", "Levene", "Brown-Forsythe")
    print (hasil)}

# example with sample size (10,10,10), variance(1,4,9), and type 1 error(0.01)  
normal(10,10,10, 1,2,3, 0.01)


