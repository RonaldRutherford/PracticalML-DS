library(lubridate)
library(PerformanceAnalytics)
library(rpart)
options(width=140)
## install.packages('neuralnet')
library("neuralnet")
library(randomForest)
trainML <- read.csv("~/Desktop/DS-PML/pml-training.csv",header=T)
testML  <- read.csv("~/Desktop/DS-PML/pml-testing.csv",header=T)
nTrain  <- names(trainML)
nTest   <- names(testML)
diffMLN <- which(!(nTest %in% nTrain))

nColTest <- length(testML)
factPCol <- rep(NA,160)
for (ii in 1:nColTest)
{
  factPCol[ii] <- length(levels(factor(testML[,ii])))
  
}

wColZ <- (factPCol < 2)
testMLRed  <- testML[,!(wColZ)]
trainMLRed <- trainML[,!(wColZ)]
testMLRed <- testMLRed[,-1]
trainMLRed <- trainMLRed[,-1]

lTr <- length(trainMLRed)

fit <- rpart(classe ~ . ,method="anova", data = trainMLRed[,6:lTr])	
	    
printcp(fit) # display the results
plotcp(fit) # visualize cross-validation results
summary(fit) # detailed summary of splits

# plot tree
plot(fit, uniform=TRUE,
   main="Classification Tree for Kyphosis")
text(fit, use.n=TRUE, all=TRUE, cex=.8)	    
preTest <- predict(fit,newdata=testMLRed)  ##, type="class")
preTestF <- round(preTest)
## switch(EXPR=round(preTest[1]),"A","B","C","D","E")

pml_write_files = function(preT){
  n = length(preT)
  for(i in 1:n)
  {
    ##xT <- switch(EXPR=round(preT[i]),"A","B","C","D","E")
    xT   <- preT[i]
    filename = paste0("problem_id_",i,".txt")
    write.table(xT,file=filename,quote=FALSE,row.names=FALSE,col.names=FALSE)
  }
}

## then create a folder where you want the files to be written. Set that to be your working directory and run:
pml_write_files(preTestF)
#################################################################
model    <- randomForest(classe ~ . , data = trainMLRed[,6:lTr])
preTestM <- predict(model,newdata=testMLRed) 
pml_write_files(preTestM)



