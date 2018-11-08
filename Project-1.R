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
#####  
#Variable Selection

##1 Credit Score
summary(CreditScore)
hist(CreditScore, main = 'Credit Score Histogram')

##2 First Payment
summary(FirstPmt)
barplot(summary(as.factor(FirstPmt)), main = 'First Payment Bar Chart')

##3 First Time Homebuyer Flag
summary(FirstTimeHomebuyer)
barplot(summary(FirstTimeHomebuyer), main = 'First Time Homebuyer Flag')


##4 Maturity Date
summary(Maturity.Date)
barplot(summary(Maturity.Date), main = 'Maturity Date Bar Chart')

##5 Zip Code
summary(MSA.Code)
barplot(summary(MSA.Code), main = 'Metropolitan Statistical Area (MSA) Code Bar Chart')
##Note (Other) and "NA's" are the two spikes at the end

##6  Mortgage Insurance Percentage
summary(MI.Percentage)
hist(MI.Percentage, main = 'Mortgage Insurance % Histogram')

##7  Number of Units
summary(Number.of.Units)
unique(Number.of.Units)[5]
nou <- c(1,2,3,4,"NA")
noul <-  unlist(map(1:4, function(x){
  length(which(Number.of.Units == nou[x]))
}))
noul <- c(noul, sum(is.na(Number.of.Units)))
barplot(noul, names.arg = c(1,2,3,4,"NA"), main = "Number of Units")

##8 Occupancy Status
summary(Occupancy.Status)
barplot(summary(Occupancy.Status),
        names.arg = c("Investment", "Occupied", "Secondary")
        , main = 'Occupancy Status')

##9 Combined Loan to Value Ratio
summary(CLTV)
hist(CLTV, main = 'Combined Loan to Value Ratio')


##10 Debt to Income Ratio
summary(DTI)
hist(DTI, main = 'Debt to Income Ratio')

##11 Unpaid Balance
summary(UPB)
hist(UPB, main = 'Unpaid Balance')

##12 Loan to Value Ratio
summary(LTV)
hist(LTV, main = 'Loan to Value Ratio')

##13 Interest Rate
summary(Interest.Rate)
hist(Interest.Rate, main = 'Interest Rate')

##14 Channel
summary(Channel)
barplot(summary(Channel), 
        names.arg = c("Broker", 'Correspondent', 'Retail', 'Not Specified'),
        main = "Entry Channel")

##15 PPM
summary(PPM)
barplot(summary(PPM), main = "Prepayment Penalty Flag")

##16 Product Type
summary(Product)
barplot(summary(Product), main = "Product Type")

##17 State
summary(State)
barplot(summary(State), main = "Property State (Location)")

##18 Property Type
summary(Property.Type)
barplot(summary(Property.Type),
        names.arg = c("Condo", "Co-op", "Leasehold", "Manufacured",
                      "Planned Unit", "Single Family", "NA"),
        main = 'Property Type')

##19 Postal Code
summary(Postal.Code)
barplot(summary(Postal.Code), main = "Zip Code (First 3 digits)")


##20 Loan Sequence Number
#Just an index

##21 Loan Purpose
summary(Loan.Purpose)
barplot(summary(Loan.Purpose), 
        names.arg = c("Cash Refinance", 'No Cash Refinance', 'Purchase'),
        main = "Loan Purpose")

##22 Original Term
summary(Original.Term)
hist(Original.Term)

##23 Number of Borrowers
summary(Borrower.Num)
obar <- length(which(Borrower.Num == 1))
tbar <- length(which(Borrower.Num == 2))
barplot(height = c(obar,tbar), names.arg = c('One', 'Two'), main = 'Number of Borrowers')

##24 Seller Name
summary(Seller.Name)
barplot(summary(Seller.Name), main = "Seller Names", las = 2)

##25 Servicer Name
summary(Servicer.Name)
barplot(summary(Servicer.Name), main = "Servicer Names", las = 2)

##26 Super Conforming
summary(Super.Conforming)
barplot(summary(Super.Conforming))

##27
summary(NPV)
hist(NPV, main = "Histogram of Net Present Value")
#####
#Data Pairings
names(dat[,-c(16,20,26)])
newdat <- dat[,-c(16,20,26)]

svg(file = 'C://Users/Thomas/Desktop/image.svg')
pairs(newdat[1:100,])
dev.off()
names(newdat)
numlist <- unlist(map(names(newdat), function(x){
  if(is.numeric(newdat[[x]])){
    1
  }else{
    0
  }
}))
numlist <-which(numlist != 0)

cor(newdat[,numlist])
