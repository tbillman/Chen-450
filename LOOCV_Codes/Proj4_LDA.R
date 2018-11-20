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

print("Number of Cores")
print(no_cores)

cl <- makeCluster(no_cores)
registerDoParallel(cl)
n <- dim(catdat)[1]

####################################
##### Cross Validation #############
####################################
print("LDA Beginning")
##### LDA Analysis #####
start.time <- Sys.time()
set.seed(1)
LDACC <- NULL
LDACC <- foreach(i = 1:n, .combine = c) %dopar% {
  lda.fit=MASS::lda(resp ~ ., data = catdat[-i,])
  lda.pred= predict(lda.fit, catdat[i,-8])
  lda.class=lda.pred$class
  as.integer(lda.class == catdat[i,8])
}
print("LDA Accuracy is:")
print(mean(LDACC))

print("LDA Sens, Spec:")
print(sum(which(LDACC == 1 & catdat$resp == 1))/sum(which(catdat$resp == 1)))
sum(which(LDACC == 1 & catdat$resp == 0))/sum(which(catdat$resp == 0))

end.time <- Sys.time()

print("LDA LOOCV took:")
end.time - start.time
