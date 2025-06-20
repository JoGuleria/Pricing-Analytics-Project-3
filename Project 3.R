

#####################################################################
#IMPORTANT NOTE:
#Most recent version of mlogit package has some compatibility issues
#with the gmnl package. Before you run the code in this file,
#please close Rstudio once, and launch it again AS ADMINISTRATOR,
#and run the following code.

install.packages("statmod")
install.packages("Rdpack")
install.packages("https://cran.r-project.org/src/contrib/Archive/mlogit/mlogit_1.0-2.tar.gz", repos=NULL,type="source", INSTALL_opts=c("--no-multiarch"))

#This will install mlogit version 1.0.2. To see if you have the correct
#version, run
packageVersion("mlogit")
#If it shows 1.0.2, you are good to go!
####################################################################

#Load packages - because we do some advanced stuff we need more packages
library("mlogit")
library("gmnl")
library("data.table")
library('RColorBrewer')
library("plotly")
library("dplyr")
rcol <- brewer.pal(9, 'Blues')

rm(list = ls());

#Change folder name to yours
setwd("C:\\Users\\jyoti\\Simon MSBA\\Spring 2025\\Spring A\\Pricing Analytics\\Project 3")


#Read data
data=fread("kiwi_bubbles_P2.csv",stringsAsFactors = F)

data=data[!(data$price.KB==99),]
data=data[!(data$price.KR==99),]
data=data[!(data$price.MB==99),]

meanPKB=mean(data$price.KB)
meanPKR=mean(data$price.KR)
meanPMB=mean(data$price.MB)



#Re-number the id - use only those who remain in the data.
#Unlike demographic-based segmenting, proportion is estimated along with other parameters.
#Hence no way we can include those without purchase data in calculating the proportion.
data$id <-1:nrow(data)

#Define mlogitdata in gmnl format
mlogitdata=mlogit.data(data,id="id",varying=4:7,choice="choice",shape="wide")

##
#Baseline model without any segmentation
#Run MLE.
mle= gmnl(choice ~  price, data = mlogitdata)
summary(mle)
BIC(mle)

##
#Run k-type model
#Set the number of segments 
NC=2

lc2=gmnl(choice~price|1|0|0|1,data=mlogitdata,model='lc',Q=NC,panel=TRUE)
#NOTE: if you see "argument not a matrix" error here, please close the Rstudio and follow
#the instruction at the beginning of this code file.

#If it runs but produces NaN, manupulate the starting value.
#,start=rep(0,4*NC+(NC-1))
summary(lc2)

BIC(lc2)
#Optimize the specification according to BIC



#Define a matrix of coefficient
coef.est=matrix(0L,NC,5)
coef.est = data.frame(segment = 1:NC, intercept.KB = NA, intercept.KR = NA, 
                      intercept.MB = NA, price.coef = NA) 
coef.est=as.matrix(coef.est)
for (i in 1:NC){
  coef.est[i,]=rbind(c(i,lc2$coefficients[((i-1)*4+1):(i*4)]))
}

#Define segment share
seg.share=matrix(0L,NC,1)
for (i in 2:NC){
  denom=1+sum(exp(lc2$coefficients[(NC*4+1):(NC*4+NC-1)]))
  seg.share[i]=exp(lc2$coefficients[(NC*4+i-1)])/denom
}
seg.share[1]=1/(1+sum(exp(lc2$coefficients[(NC*4+1):(NC*4+NC-1)])))
seg.share=as.double(seg.share)

#Estimation complete.


#Just like project 2, we define three function.
#DO NOT COPY FUNCTIONS from Project 2 - the definition of "NC" differs (because we used to have NC+1 segments back then, not anymore).
#Otherwise the way these functions work is identical to Project 2.

#Function 1 "demand"
#It takes (besides prices) estimated parameters of a given segment (4 x 1 vector)
#as inputs, and returns three choice probabilities for that segment (each for 
#KB, KR and MB) at that price.

demand=function(priceKB,priceKR,priceMB,para){
  probKB=exp(para[1]+para[4]*priceKB)/(1+exp(para[1]+para[4]*priceKB)+exp(para[2]+para[4]*priceKR)+exp(para[3]+para[4]*priceMB))
  probKR=exp(para[2]+para[4]*priceKR)/(1+exp(para[1]+para[4]*priceKB)+exp(para[2]+para[4]*priceKR)+exp(para[3]+para[4]*priceMB))
  probMB=exp(para[3]+para[4]*priceMB)/(1+exp(para[1]+para[4]*priceKB)+exp(para[2]+para[4]*priceKR)+exp(para[3]+para[4]*priceMB))
  return(cbind(probKB,probKR,probMB))
}

demand(1.381332,1.378087,1.345585, coef.est)
#Function 2 "demand.seg"
#This function takes the three prices and the matrix of
#estimated parameters (the "coef.est" matrix we created above: # of segments x 4 matrix)
#and returns the choice probability of all the segments stacked together.
#The first row corresponds to the three choice probabilities of segment 1,
#the second row corresponds to those of segment 2, and so on.
#These outputs correspond to "Pr{k}(y)" for all segment k and product y.
demand.seg=function(priceKB,priceKR,priceMB,para){
  #Define a matrix to store the result
  sharemat=matrix(0L,nrow = NC, ncol = 3)
  
  #Iterate the "demand" across segments and store the result in each row.
  for(seg in 1:(NC)){
    sharemat[seg,]=demand(priceKB,priceKR,priceMB,para[seg,2:5])
  }
  return(sharemat)
}
demand.seg(1.381332,1.378087,1.345585, coef.est)
#Function 3 "demand.agg"
#It takes three prices, the matrix of
#estimated parameters (the "coef.est" matrix we created above)
#and the segment share. It then returns the aggregate choice 
#probability at the market level
demand.agg=function(priceKB,priceKR,priceMB,para,segshare){
  demandagg=matrix(0L,nrow = max(length(priceKB),length(priceKR),length(priceMB)), ncol = 3)
  for(seg in 1:(NC)){
    demandagg=demandagg+segshare[seg]*demand(priceKB,priceKR,priceMB,para[seg,2:5])
  }
  return(demandagg)
}


demand.agg(1.381332,1.378087,1.345585, coef.est, seg.share)


#################################
#OPTIONAL: Consumer-level targeting with posterior
#################################


#Calculate each consumer's predicted beta0 and beta1 using posterior.
aux=effect.gmnl(lc2)
postcoef=aux$mean

head(postcoef)
#Histogram of WTP
#KB
hist(-postcoef[,1]/postcoef[,4],
     cex.lab=1.5, cex.axis=1.5, cex.main=1.5, cex.sub=1.5,
     xlab="Willingness to pay for KB",ylab=("Density"),main="")
#KR
hist(-postcoef[,2]/postcoef[,4],
     cex.lab=1.5, cex.axis=1.5, cex.main=1.5, cex.sub=1.5,
     xlab="Willingness to pay for KR",ylab=("Density"),main="")



#Scatterplot of parameters KB against KR

colorset2=c("blue","green","orange","red")
#Scatterplot of parameters KB - KR against KR - MB
colorset=c("red","blue","green")
plot(as.matrix(postcoef[,1]-postcoef[,2]),as.matrix(postcoef[,4]),cex=1,xlim=c(-3,3),ylim=c(-6.5,-1),
     col = "chocolate",pch=16,cex.lab=1.5, cex.axis=1.5, cex.main=1.5, cex.sub=1.5,
     xlab="beta0KB-beta0KR",ylab="beta1")
#overlay with model-predicted segments
for (seg in 1:NC) {
  points(coef.est[seg,2]-coef.est[seg,3],coef.est[seg,5],cex=20*seg.share[seg],col = colorset2[seg],pch=1)
}
legend(1.2, -0.7, legend=c("Type 1", "Type 2", "Type 3", "Type 4"),
       col=c( "blue","orange","red","green"), pch=1, cex=1.1)


c(KB = meanPKB, KR = meanPKR, MB = meanPMB)


