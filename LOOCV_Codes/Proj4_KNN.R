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
print("KNN Beginning")
##### QDA Analysis #####
start.time <- Sys.time()

KACC <- NULL
KACC <- foreach(i = 1:n, .combine = c) %dopar% {
  knn.pred <- class::knn(train = catdat[-i,-8],
			 test = catdat[i,-8],
			 cl = catdat[-i,8],
			 k=3)
  pred <- as.integer(knn.pred) - 1
  as.integer(pred == catdat$resp[i])
}
print("KNN Accuracy is:")
mean(KACC)

print("KNN Sens, Spec:")
sum(which(KACC == 1 & catdat$resp == 1))/sum(which(catdat$resp == 1))
sum(which(KACC == 1 & catdat$resp == 0))/sum(which(catdat$resp == 0))

end.time <- Sys.time()

print("KNN LOOCV took:")
end.time - start.time
