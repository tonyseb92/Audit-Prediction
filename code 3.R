
library(gmodels)
library(gpairs)
library(stringr)
library(data.table)

###Importing the original dataset###

consensioset = read.csv("data_for_interview_test.csv", header=TRUE)
dim(consensioset)
#summary(consensioset)


####Splitting data - 80% for training, 20% for validation####
train.prop = 0.80
train.cases = sample(nrow(consensioset),nrow(consensioset)*train.prop)
length(train.cases);

#### Choosing variables for training and validation ####

class.train=consensioset[train.cases,c(3,6,8,20,22,23,25,26,28,33,62)]
#head(class.train)
class.valid=consensioset[-train.cases,c(3,6,8,20,22,23,25,26,28,33,62)]


####Removing rows with NULL values####

#dim(class.train)
class.train = class.train[complete.cases(class.train),]
#dim(class.train)

#dim(class.valid)
class.valid = class.valid[complete.cases(class.valid),]
#dim(class.valid)

##### Converting variables to different data types for processing #####

class.train$disposition<-as.character(class.train[,1])
class.train$site.id<-as.factor(class.train[,2])
class.train$ts.coder.id<-as.character(class.train[,3])
#class.train$icd.no<-as.factor(class.train[,4])
class.train$age.yrs<-as.numeric(class.train[,4])
class.train$MIPS.ts.error<-as.logical(class.train[,5])
class.train$MIPS.ts.coder.error<-as.logical(class.train[,6])
class.train$Dx.ts.error<-as.logical(class.train[,7])
class.train$mod.ts.error<-as.logical(class.train[,8])
class.train$primary.ts.cpt<-as.factor(class.train[,9])
class.train$ch.delete.ch.audit.em<-as.logical(class.train[,10])
class.train$ch.final.chg.diff<-as.numeric(class.train[,11])

attach(class.train)

########  Cleaning data ###########

## Reducing the variations of 'disposition'

for (x in 1:length(class.train[,1])) {if
  (grepl("discharge",class.train[x,1])==TRUE){
    class.train[x,1] <- "discharge"
  } else if
  (grepl("admit",class.train[x,1])==TRUE){
    class.train[x,1] <- "admit"
  } else if
  (grepl("ama",class.train[x,1])==TRUE){
    class.train[x,1] <- "ama"
  } else if
  (grepl("ed observation",class.train[x,1])==TRUE){
    class.train[x,1] <- "ed observation"
  } else if
  (grepl("eloped",class.train[x,1])==TRUE){
    class.train[x,1] <- "eloped"
  } else if
  (grepl("expired",class.train[x,1])==TRUE){
    class.train[x,1] <- "expired"
  } else if
  (grepl("lwbs after triage",class.train[x,1])==TRUE){
    class.train[x,1] <- "lwbs after triage"
  } else if
  (grepl("lwbs before triage",class.train[x,1])==TRUE){
    class.train[x,1] <- "lwbs before triage"
  } else if
  (grepl("none",class.train[x,1])==TRUE){
    class.train[x,1] <- "none"
  } else if
  (grepl("send to cath lab",class.train[x,1])==TRUE){
    class.train[x,1] <- "send to cath lab"
  } else if
  (grepl("send to l&d",class.train[x,1])==TRUE){
    class.train[x,1] <- "send to l&d"
  } else if
  (grepl("send to or",class.train[x,1])==TRUE){
    class.train[x,1] <- "send to or"
  } else if
  (grepl("send to specialty department",class.train[x,1])==TRUE){
    class.train[x,1] <- "send to specialty department" 
  } else if
  (grepl("transfer to",class.train[x,1])==TRUE){
    class.train[x,1] <- "TAF"
  } else {class.train[x,1] <- "Unknown"
  }
  
}

class.train$disposition<-as.factor(class.train[,1])

#Reducing the variations of 'ts.coder.id'

for (x in 1:length(class.train[,3])) {if
  (grepl("technosoft20",class.train[x,3])==TRUE){
    class.train[x,3] <- "technosoft 20"
  } else if
  (grepl("technsoft 33",class.train[x,3])==TRUE){
    class.train[x,3] <- "technosoft 33"
  } else if
  (grepl("techonosoft 32",class.train[x,3])==TRUE){
    class.train[x,3] <- "technosoft 32"
  } else
  {
    class.train[x,3] <- "Unknown"
  }
}

class.train$ts.coder.id<-as.factor(class.train[,3])

##### Replacing Final Charge difference with binary values #####
##### to reflect change in charge/no change in charge      #####

for (x in 1:length(class.train[,11])) {if
  (class.train[x,11] != 0){
    class.train[x,11] = 1
  } 
}

class.train$ch.final.chg.diff<-as.logical(class.train[,11])

##### Choosing DV/IV  ########

Y.train=class.train[,11]
X.train=class.train[,-11]


###### Fitting logistic regression model ######

fit.logit <- glm(ch.final.chg.diff ~ disposition + site.id + ts.coder.id + age.yrs + MIPS.ts.error + MIPS.ts.coder.error + Dx.ts.error + mod.ts.error + primary.ts.cpt + ch.delete.ch.audit.em, data=class.train,
                 family=binomial("logit"))


###### Predicting on training data #####
pred.train=predict(fit.logit, X.train, type="response", row.names = FALSE)
pred.train<-ifelse(pred.train>0.5,1,0)  #This line can be commented out to get probabilities


(ct=table(Y.train,pred.train))  #Confusioin matrix
diag(prop.table(ct,1))          #see hit rate
sum(diag(prop.table(ct)))       #Accuracy


#############################Validation###############################

class.valid$disposition<-as.character(class.valid[,1])
class.valid$site.id<-as.factor(class.valid[,2])
class.valid$ts.coder.id<-as.character(class.valid[,3])
#class.valid$icd.no<-as.factor(class.valid[,4])
class.valid$age.yrs<-as.numeric(class.valid[,4])
class.valid$MIPS.ts.error<-as.logical(class.valid[,5])
class.valid$MIPS.ts.coder.error<-as.logical(class.valid[,6])
class.valid$Dx.ts.error<-as.logical(class.valid[,7])
class.valid$mod.ts.error<-as.logical(class.valid[,8])
class.valid$primary.ts.cpt<-as.factor(class.valid[,9])
class.valid$ch.delete.ch.audit.em<-as.logical(class.valid[,10])
class.valid$ch.final.chg.diff<-as.numeric(class.valid[,11])


for (x in 1:length(class.valid[,1])) {if
  (grepl("discharge",class.valid[x,1])==TRUE){
    class.valid[x,1] <- "discharge"
  } else if
  (grepl("admit",class.valid[x,1])==TRUE){
    class.valid[x,1] <- "admit"
  } else if
  (grepl("ama",class.valid[x,1])==TRUE){
    class.valid[x,1] <- "ama"
  } else if
  (grepl("ed observation",class.valid[x,1])==TRUE){
    class.valid[x,1] <- "ed observation"
  } else if
  (grepl("eloped",class.valid[x,1])==TRUE){
    class.valid[x,1] <- "eloped"
  } else if
  (grepl("expired",class.valid[x,1])==TRUE){
    class.valid[x,1] <- "expired"
  } else if
  (grepl("lwbs after triage",class.valid[x,1])==TRUE){
    class.valid[x,1] <- "lwbs after triage"
  } else if
  (grepl("lwbs before triage",class.valid[x,1])==TRUE){
    class.valid[x,1] <- "lwbs before triage"
  } else if
  (grepl("none",class.valid[x,1])==TRUE){
    class.valid[x,1] <- "none"
  } else if
  (grepl("send to cath lab",class.valid[x,1])==TRUE){
    class.valid[x,1] <- "send to cath lab"
  } else if
  (grepl("send to l&d",class.valid[x,1])==TRUE){
    class.valid[x,1] <- "send to l&d"
  } else if
  (grepl("send to or",class.valid[x,1])==TRUE){
    class.valid[x,1] <- "send to or"
  } else if
  (grepl("send to specialty department",class.valid[x,1])==TRUE){
    class.valid[x,1] <- "send to specialty department" 
  } else if
  (grepl("transfer to",class.valid[x,1])==TRUE){
    class.valid[x,1] <- "TAF"
  } else {class.valid[x,1] <- "Unknown"
  }
}

class.valid$disposition<-as.factor(class.valid[,1])

#Cleaning data - reducing the variations of ts.coder.id

for (x in 1:length(class.valid[,3])) {if
  (grepl("technosoft20",class.valid[x,3])==TRUE){
    class.valid[x,3] <- "technosoft 20"
  } else if
  (grepl("technsoft 33",class.valid[x,3])==TRUE){
    class.valid[x,3] <- "technosoft 33"
  } else if
  (grepl("techonosoft 32",class.valid[x,3])==TRUE){
    class.valid[x,3] <- "technosoft 32"
  } else
  {
    class.valid[x,3] <- "Unknown"
  }
}

class.valid$ts.coder.id<-as.factor(class.valid[,3])

#Replacing Final Charge difference with binary values to reflect change in charge/no change in charge
for (x in 1:length(class.valid[,11])) {if
  (class.valid[x,11] != 0){
    class.valid[x,11] = 1
  } 
}

class.valid$audit.state<-as.logical(class.valid[,11])

#Choosing dependent variable
Y.valid=class.valid[,11]
X.valid=class.valid[,-11]

pred.valid=predict(fit.logit,class.valid[,-11],type="response", rownames= FALSE)
pred.valid<-ifelse(pred.valid>0.5,1,0) #This line can be commented out to get probabilities

(ctv=table(class.valid[,6],pred.valid))
diag(prop.table(ctv,1))
sum(diag(prop.table(ctv)))

library(gmodels)
Cross1=CrossTable(class.valid[,6],pred.valid)
Crosstv=table(class.valid[,6],pred.valid)

#######################OUTPUT############################

write.csv(class.train, file = "trainingset.csv")
write.csv(pred.train, file = "predictedtrain.csv")