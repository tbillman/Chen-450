#####Project 3 Code#####
#####Thomas Billman#####

#install.packages('tidyverse')
library(parallel)
library(foreach)
library(doParallel)
library(MASS)
library(class)
library(ISLR)
library('tidyverse')

catdat <- read_rds("catdat.rds")
print("Data Read")
no_cores <- detectCores() - 1
cl <- makeCluster(no_cores)
registerDoParallel(cl)
n <- dim(catdat)[1]

####################################
##### Cross Validation #############
####################################
print("QDA Beginning")
##### QDA Analysis #####
start.time <- Sys.time()
set.seed(1)
QDACC <- NULL
QDACC <- foreach(i = 1:n, .combine = c) %dopar% {
  qda.fit=MASS::qda(resp ~ ., data = catdat[-i,])
  qda.pred= predict(qda.fit, catdat[i,-8])
  qda.class=qda.pred$class
  as.integer(qda.class == catdat[i,8])
}
print("QDA Accuracy is:")
mean(QDACC)

print("QDA Sens, Spec:")
sum(which(QDACC == 1 & catdat$resp == 1))/sum(which(catdat$resp == 1))
sum(which(QDACC == 1 & catdat$resp == 0))/sum(which(catdat$resp == 0))

end.time <- Sys.time()

print("QDA LOOCV took:")
end.time - start.time
