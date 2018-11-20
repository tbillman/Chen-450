#####Project 3 Code#####
#####Thomas Billman#####

#install.packages('tidyverse')
library('tidyverse')
library(MASS)
library('class')
catdat <- read_rds(url("https://github.com/tbillman/Chen-450/raw/master/catdat.rds"))


##### Testing for K values #####


set.seed(1)
f <- 5
folds <- rep_len(1:f, length.out = dim(catdat)[1])
folds <- sample(folds, size = dim(catdat)[1], replace = F)



start.time <- Sys.time()
for(j in c(1,3,5,7,9,11)){
  SENS <- NULL
  SPEC <- NULL
  ACC <- NULL
  
  for(i in 1:f){
    test.id <- which(folds == i)
    knn.pred=knn(train = catdat[-test.id,1:7],
                 test = catdat[test.id,1:7],
                 cl = catdat[-test.id,8],
                 k=3)
    knn.pred
    TAB <- table(knn.pred,catdat[test.id,8])
    SPEC <- c(SPEC, TAB[1,1]/sum(TAB[,1]))
    SENS <- c(SENS, TAB[2,2]/sum(TAB[,2]))
    ACC <- c(ACC, sum(diag(TAB))/sum(TAB))
  }
  KDAT <- rbind(SENS,SPEC,ACC)
  print(mean(ACC))
}
end.time <- Sys.time()
end.time - start.time



######################################
##### 5 - Fold Cross Validation ######
######################################

##### Logistic Regression #####

start.time = Sys.time()
set.seed(1)
f <- 5
folds <- rep_len(1:f, length.out = dim(catdat)[1])
folds <- sample(folds, size = dim(catdat)[1], replace = F)


LSENS <- NULL
LSPEC <- NULL
LACC <- NULL
for(i in 1:f){
  test.id <- which(folds == i)
  glm.fit <- glm(resp ~ . , family = "binomial", data = catdat[-test.id,])
  glm.pred <- predict.glm(glm.fit,catdat[test.id,-8])
  preds <- as.integer(glm.pred > .5)
  TAB <- table(preds,catdat[test.id,8])
  
  LSENS <- c(LSENS,1)
  LSPEC <- c(LSPEC,0)
  LACC <- c(LACC, TAB[1,2]/sum(TAB))
}
LOGDAT <- rbind(LSENS,LSPEC,LACC)
mean(LACC)

end.time <- Sys.time()
end.time - start.time


##### LDA Analysis #####
start.time <- Sys.time()
set.seed(1)
f <- 5
folds <- rep_len(1:f, length.out = dim(catdat)[1])
folds <- sample(folds, size = dim(catdat)[1], replace = F)


SENS <- NULL
SPEC <- NULL
ACC <- NULL
for(i in 1:f){
  test.id <- which(folds == i)
  lda.fit=lda(resp ~ ., data = catdat[-test.id,])
  
  nu <- matrix(lda.fit$scaling[,1], nrow = 1)
  covar <- as.matrix(catdat[test.id,-8])
  OUT <- nu %*% t(covar)
  
  lda.pred= predict(lda.fit, catdat[test.id,])
  lda.class=lda.pred$class
  TAB <- table(lda.class,catdat$resp[test.id])
  SPEC <- c(SPEC, TAB[1,1]/sum(TAB[,1]))
  SENS <- c(SENS, TAB[2,2]/sum(TAB[,2]))
  ACC <- c(ACC, sum(diag(TAB))/sum(TAB))
}
LDAT <- rbind(SENS,SPEC,ACC)
mean(ACC)
end.time <- Sys.time()
end.time - start.time


#####QDA Analysis#####
start.time <- Sys.time()
set.seed(1)
f <- 5
folds <- rep_len(1:f, length.out = dim(catdat)[1])
folds <- sample(folds, size = dim(catdat)[1], replace = F)


SENS <- NULL
SPEC <- NULL
ACC <- NULL
for(i in 1:f){
  test.id <- which(folds == i)
  qda.fit=qda(resp ~ ., data = catdat[-test.id,])
  qda.class=predict(qda.fit,catdat[test.id,])$class
  
  TAB = table(qda.class,catdat$resp[test.id])
  SPEC <- c(SPEC, TAB[1,1]/sum(TAB[,1]))
  SENS <- c(SENS, TAB[2,2]/sum(TAB[,2]))
  ACC <- c(ACC, sum(diag(TAB))/sum(TAB))
}
QDAT <- rbind(SENS,SPEC,ACC)
mean(ACC)
end.time <- Sys.time()
end.time - start.time


#####KNN Analysis#####

set.seed(1)
f <- 5
folds <- rep_len(1:f, length.out = dim(catdat)[1])
folds <- sample(folds, size = dim(catdat)[1], replace = F)


SENS <- NULL
SPEC <- NULL
ACC <- NULL

start.time <- Sys.time()
for(i in 1:f){
  test.id <- which(folds == i)
  knn.pred=knn(train = catdat[-test.id,1:7],
               test = catdat[test.id,1:7],
               cl = catdat[-test.id,8],
               k=3)
  knn.pred
  TAB <- table(knn.pred,catdat[test.id,8])
  SPEC <- c(SPEC, TAB[1,1]/sum(TAB[,1]))
  SENS <- c(SENS, TAB[2,2]/sum(TAB[,2]))
  ACC <- c(ACC, sum(diag(TAB))/sum(TAB))
}
end.time <- Sys.time()
end.time - start.time
KDAT <- rbind(SENS,SPEC,ACC)
mean(ACC)



#Results Boxplot
nms <- c("Logistic", "LDA", "QDA", "KNN")
boxplot(LOGDAT[3,], LDAT[3,], QDAT[3,], KDAT[3,],
        names = nms, col = 1:4, xlab = "Models", ylab = "Accuracy")
