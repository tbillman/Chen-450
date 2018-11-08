#install.packages('tidyverse')
library('tidyverse')

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

#Removing variables without contrasts
dat<- dat[,-16]
dat<- dat[,-25]
 
#Linear Models
#We remove many variables that have too many levels
lm1 <- lm(NPV ~ . -FirstPmt -Maturity.Date -MSA.Code -State -Postal.Code
                -Loan.Sequence.Number -Seller.Name -Servicer.Name, data = dat)
summary(lm1)
par(mfrow = c(2,2))
plot(lm1)
#checking which we should keep
keep <- which(summary(lm1)$coefficients[,4] < .05)

#keep significant covariates and NPV
keep1 <- which(names(dat) %in% c(names(keep),
                        "FirstTimeHomebuyer","Property.Type","NPV"))

sigdat <- dat[,keep1]

detach(dat)
attach(sigdat)
lm2 <- lm(NPV ~ ., data = sigdat)
plot(lm2)
summary(lm2)

#Now with interaction terms
lm3 <- lm(NPV ~ . + .^2, data = sigdat)
plot(lm3)
summary(lm3)
summary(lm3)$coefficients[
  which(summary(lm3)$coefficients[,4]< .05)
,]
#Surprising amount of significant interaction terms
#Seems that all of Credit score gets absorbed by interactions