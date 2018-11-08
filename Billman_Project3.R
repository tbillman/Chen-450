#####Project 3 Code#####
#####Thomas Billman#####

#install.packages('tidyverse')
library('tidyverse')

set.seed(1)

#Data Read in
fn <- "https://raw.githubusercontent.com/tbillman/Wang499/master/OrgNPVs2.csv"
dat <- read_csv(file = url(fn))
names(dat) <- make.names(names(dat))
#Data Cleaning/Exploration

str(dat)
attach(dat)

dat$FirstPmt <- as.factor(FirstPmt)
dat$FirstTimeHomebuyer <- as.factor(FirstTimeHomebuyer)
dat$Maturity.Date <- as.factor(Maturity.Date)
dat$MSA.Code <- as.factor(MSA.Code)
dat$Occupancy.Status <- as.factor(Occupancy.Status)
dat$Channel <- as.factor(Channel)
dat$PPM <- as.factor(PPM)
dat$Product <- as.factor(Product)
dat$State <- as.factor(State)
dat$Property.Type <- as.factor(Property.Type)
dat$Postal.Code <- as.factor(Postal.Code)
dat$Loan.Purpose <- as.factor(Loan.Purpose)
dat$Seller.Name <- as.factor(Seller.Name)
dat$Servicer.Name <- as.factor(Servicer.Name)
dat$Super.Conforming <- as.factor(Super.Conforming)

attach(dat)
which(
sapply(1:dim(dat)[2],function(x){
  length(unlist(unique(dat[,x])))
})
== 1
)
names(dat)[c(16,26)]
#Linear Models
#We remove many variables that have too many levels
lm1 <- lm(NPV ~ CreditScore + FirstPmt + FirstTimeHomebuyer + MI.Percentage + 
                Occupancy.Status + LTV + DTI + UPB + Interest.Rate + Channel + 
                PPM + Property.Type + Loan.Purpose + Original.Term + Borrower.Num, data = dat)
summary(lm1)
#checking which we should keep
keep <- which(summary(lm1)$coefficients[,4] < .05)

#keep significant covariates and NPV
keep1 <- which(names(dat) %in% c(names(keep),
                                 "FirstTimeHomebuyer","Property.Type","NPV"))

sigdat <- dat[,keep1]

detach(dat)
attach(sigdat)


#####Logistic Analysis Full Set#####
mn <- as.integer(NPV>0)
glm1 <- glm(formula = mn ~ . -NPV, family = binomial, data = sigdat)
mn <- as.integer(glm1$model$NPV > 0)
probs <- predict.glm(glm1, type = "response")
preds <- as.integer(probs > .5)
sum(as.integer(preds) == mn)/length(mn)
sum(mn)/length(mn)
tp <- which(mn == 1)
fp <- which(mn == 0)
sens <- sum(preds[tp] == 1)/length(tp)
spec <- sum(preds[fp] == 0)/length(fp)
sens;spec

#note that a threshold of .5 leads in all obervations marked as positive

#looking at an ROC curve for this model
library(pROC)
g <- roc(mn ~ probs)
auc(g)
plot(g)




#####LDA Analysis Full Set#####
#install.packages("MASS")
library(MASS)
mn <- as.integer(NPV>0)
lda.fit=lda(mn ~ . - NPV - Property.Type, data = sigdat)
lda.fit
nu <- matrix(lda.fit$scaling[,1], nrow = 1)
covar <- as.matrix(sigdat[-lda.fit$na.action,1:7])
covar[,2] <- as.integer(covar[,2] == "Y")
covar <- apply(covar,2,as.numeric)
OUT <- nu %*% t(covar)

lda.pred= predict(lda.fit, sigdat[-lda.fit$na.action,])
lda.class=lda.pred$class; lda.class

table(lda.class,mn1)
#This LDA predicts that all loans will have a positive NPV


#####QDA Analysis Full Set#####
sigdat1 <- sigdat[-lda.fit$na.action,]
sigdat1[,2] <- as.integer(sigdat1[,2] == "Y")
mn1 <- mn[-lda.fit$na.action]
qda.fit=qda(mn1 ~. -NPV - Property.Type ,data=sigdat1)
qda.fit
qda.class=predict(qda.fit,sigdat1)$class
TAB = table(qda.class,mn1); TAB
mean(qda.class==mn1)

spec = TAB[1,1]/sum(TAB[,1]);spec
sens = TAB[2,2]/sum(TAB[,2]);sens
acc <- sum(diag(TAB))/sum(TAB);acc


#####KNN Analysis Full Set#####
start.time = Sys.time()
set.seed(1)
knn.pred=knn(train = sigdat1[,1:7],
             test = sigdat1[,1:7],
             cl = mn1,
             k=3)
knn.pred
TAB <- table(knn.pred,mn1)
spec = TAB[1,1]/sum(TAB[,1]);spec
sens = TAB[2,2]/sum(TAB[,2]);sens
acc <- sum(diag(TAB))/sum(TAB);acc

end.time <- Sys.time()
end.time - start.time


####################################
##### Cross Validation #############
####################################

##### Logistic Regression #####

start.time = Sys.time()
sigdat1 <- sigdat1[,-8]
covar <- as.matrix(sigdat1[,1:7])
resp <- as.integer(sigdat1[,8] > 0, ncol = 1)

catdat <- data.frame(covar,resp)

set.seed(1)
glm.fit <- glm(resp ~ . , family = "binomial", data = catdat)
cv.error <- cv.glm(catdat,glm.fit,K=5)
cv.error.10=cv.glm(catdat,glm.fit,K=5)$delta[1]
cv.error.10
end.time <- Sys.time()
end.time - start.time




##### LDA Analysis #####
start.time <- Sys.time()
set.seed(1)
f <- 5
folds <- rep_len(1:f, length.out = dim(covar)[1])
folds <- sample(folds, size = dim(covar)[1], replace = F)


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
rbind(SENS,SPEC,ACC)
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
rbind(SENS,SPEC,ACC)
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
rbind(SENS,SPEC,ACC)
mean(ACC)

