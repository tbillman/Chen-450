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


####################################
##### Cross Validation #############
####################################

##### Logistic Regression #####

start.time = Sys.time()
set.seed(1)
f <- dim(catdat)[1]
folds <- rep_len(1:f, length.out = dim(catdat)[1])
folds <- sample(folds, size = dim(catdat)[1], replace = F)

print("Starting Logistic")

LACC <- foreach(i = 1:f,
		.combine = c) %dopar% 
{
  test.id <- which(folds == i)
  glm.fit <- glm(resp ~ . , family = "binomial", data = catdat[-test.id,])
  glm.pred <- predict.glm(glm.fit,catdat[test.id,-8])
  preds <- as.integer(glm.pred > .5)
  as.integer(preds == catdat[test.id,8])
}
sens <- sum(LACC == 1 & catdat$resp == 1)/ sum(catdat$resp == 1)
spec <- sum(LACC == 1 & catdat$resp == 0)/ sum(catdat$resp == 0)
print("Logistic Sens,Spec = ")
print(sens,spec)
print("Logistic Acc")
print(mean(LACC))
end.time <- Sys.time()
print("Logistic LOOCV took:")
print(end.time - start.time)

##### LDA Analysis #####
start.time <- Sys.time()
set.seed(1)
folds <- rep_len(1:f, length.out = dim(catdat)[1])
folds <- sample(folds, size = dim(catdat)[1], replace = F)


LDSENS <- NULL
LDSPEC <- NULL
LDACC <- NULL
for(i in 1:f){
  test.id <- which(folds == i)
  lda.fit=lda(resp ~ ., data = catdat[-test.id,])
  
  nu <- matrix(lda.fit$scaling[,1], nrow = 1)
  covar <- as.matrix(catdat[test.id,-8])
  OUT <- nu %*% t(covar)
  
  lda.pred= predict(lda.fit, catdat[test.id,])
  lda.class=lda.pred$class
  TAB <- table(lda.class,catdat$resp[test.id])
  LDSPEC <- c(SPEC, TAB[1,1]/sum(TAB[,1]))
  LDSENS <- c(SENS, TAB[2,2]/sum(TAB[,2]))
  LDACC <- c(ACC, sum(diag(TAB))/sum(TAB))
}
LDmat <- rbind(LDSENS,LDSPEC,LDACC)
mean(LDACC)
end.time <- Sys.time()
end.time - start.time
write.csv(LDmat, file = "LDA_LOOCV.csv")

#####QDA Analysis#####
start.time <- Sys.time()
set.seed(1)
folds <- rep_len(1:f, length.out = dim(catdat)[1])
folds <- sample(folds, size = dim(catdat)[1], replace = F)


QDSENS <- NULL
QDSPEC <- NULL
QDACC <- NULL
for(i in 1:f){
  test.id <- which(folds == i)
  qda.fit=qda(resp ~ ., data = catdat[-test.id,])
  qda.class=predict(qda.fit,catdat[test.id,])$class
  
  TAB = table(qda.class,catdat$resp[test.id])
  SPEC <- c(SPEC, TAB[1,1]/sum(TAB[,1]))
  SENS <- c(SENS, TAB[2,2]/sum(TAB[,2]))
  ACC <- c(ACC, sum(diag(TAB))/sum(TAB))
}
QDmat <- rbind(QDSENS,QDSPEC,QDACC)
mean(QDACC)
end.time <- Sys.time()
end.time - start.time
write.csv(QDmat, file = "QDA_LOOCV.csv")

#####KNN Analysis#####

set.seed(1)
folds <- rep_len(1:f, length.out = dim(catdat)[1])
folds <- sample(folds, size = dim(catdat)[1], replace = F)


KSENS <- NULL
KSPEC <- NULL
KACC <- NULL

start.time <- Sys.time()
for(i in 1:f){
  test.id <- which(folds == i)
  knn.pred=knn(train = catdat[-test.id,1:7],
               test = catdat[test.id,1:7],
               cl = catdat[-test.id,8],
               k=3)
  knn.pred
  TAB <- table(knn.pred,catdat[test.id,8])
  KSPEC <- c(SPEC, TAB[1,1]/sum(TAB[,1]))
  KSENS <- c(SENS, TAB[2,2]/sum(TAB[,2]))
  KACC <- c(ACC, sum(diag(TAB))/sum(TAB))
}
end.time <- Sys.time()
end.time - start.time
Kmat <- rbind(KSENS,KSPEC,KACC)
mean(KACC)

write.csv(Kmat, file = "KNN_LOOCV.csv")
