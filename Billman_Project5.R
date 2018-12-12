#####Project 5 Code#####
#####Thomas Billman#####

library(parallel)
library(foreach)
library(doParallel)
library('tidyverse')
library(randomForest)
no_cores <- detectCores() - 1
cl <- makeCluster(no_cores)
registerDoParallel(cl)

#Read in the full dataset to begin
dat <- read_csv(url("https://raw.githubusercontent.com/tbillman/Wang499/master/OrgNPVs2.csv"))
resp=as.factor(ifelse(dat$NPV<0,0,1)); 
dat <- data.frame(dat,resp)

set.seed(1)
ndat <- dat[,-26]
ndat <- ndat[complete.cases(ndat),]

for (x in c(3, 8,14,15, 18, 21 )){
  ndat[,x] <- as.factor(ndat[,x])
}
ndat <- ndat[,c(-2, -4, -5, -16, -17, -19, -20, -24, -25)]

for(x in 1:dim(ndat)[2]){
  print(length(unique(ndat[,x])))
}

p <- dim(ndat)[2]-2

set.seed(1)

f <- 5
folds <- rep_len(1:f, length.out = dim(ndat)[1])
folds <- sample(folds, size = dim(ndat)[1], replace = F)

for(i in 1:f){
  print("FOLD: ")
  print(i)
  test.id <- which(folds == i)
  bag.cat=randomForest(resp~.-NPV,data=ndat[-test.id,],
                       xtest = ndat[test.id,c(-17,-18)],
                       ytest = ndat[test.id,18],
                       mtry=floor(sqrt(p-1)),
                       ntree = 2,
                       importance=TRUE
                       )
  print(bag.cat)
}

for(i in 1:f){
  print("FOLD: ")
  print(i)
  test.id <- which(folds == i)
  bag.reg=randomForest(NPV~.-resp,data=ndat[-test.id,],
                       xtest = ndat[test.id,c(-17,-18)],
                       ytest = ndat[test.id,17],
                       mtry=floor(sqrt(p-1)),
                       ntree = 2,
                       importance=TRUE
  )
  print(bag.reg)
}


#Bagging 5f-CV

#RandomForest 5f-CV

#Ran in Bridges

acc <- c((111+34918)/sum(111,542, 170, 34918),
         (98+34933)/sum(98,534, 47, 34933),
         (91+34886)/sum(91,593, 42, 34886),
         (92+34948)/sum(92,526, 45, 34948),
         (88+34971)/sum(88,525, 27, 34971)
)

sens <- c( 1- .001172802,
           1- .001343625,
           1- .001202474,
           1- .001285971,
           1- .0007714727
)

spec <- c( 1- .830015314,
           1- .844936709,
           1- .866959064,
           1- .851132686,
           1- .8564437194
)
outm <- rbind(acc,sens,spec)
outm
boxplot(outm, use.cols = F)

outm <- cbind(outm,apply(outm,1,mean))
colnames(outm) <- c("Fold 1", "Fold 2", "Fold 3", "Fold 4", "Fold 5", "Mean")
outm


#Boosting 5f-CV

