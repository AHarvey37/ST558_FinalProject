library(plumber)
library(caret)
library(tidyverse)

rawData <- read.csv("diabetes_binary_health_indicators_BRFSS2015.csv")

cleaned<- rawData|>
  select(Diabetes_binary,HighBP,HighChol,HvyAlcoholConsump,Smoker,PhysActivity,Age,HeartDiseaseorAttack,MentHlth)|>
  mutate(Diabetes_binary = factor(Diabetes_binary,labels = c("No","Yes")),
         Age = as.factor(Age),
         HighBP = as.factor(HighBP),
         HighChol = as.factor(HighChol),
         HvyAlcoholConsump = as.factor(HvyAlcoholConsump),
         Smoker = as.factor(Smoker),
         PhysActivity = as.factor(PhysActivity),
         HeartDiseaseorAttack=as.factor(HeartDiseaseorAttack),
         MentHlth=as.factor(MentHlth))

# set seed for predictability
set.seed(8)

# Create a Vector to use to split data. Used createdatapartition to help maintain the ratio of diabates positive to diabetes negative
trainingVec <- createDataPartition(cleaned$Diabetes_binary,
                                   p = .7,
                                   list = FALSE)

# Split data into training and test sets
## Training set
diabetesTrain <- cleaned[trainingVec,]
## Test set
diabetesTest <- cleaned[-trainingVec,]

# make Train Control Variable with 5 fold cross-validation
trctrl<- trainControl(method = "cv",
                      number = 5,
                      classProbs = TRUE,
                      summaryFunction = mnLogLoss)


#Best model
cart_TreeFit3<- train(Diabetes_binary ~ .,
                      data = diabetesTrain,
                      method = "rpart",
                      trControl = trctrl,
                      metric = "logLoss",
                      #family = "binomial",
                      tuneGrid = expand.grid(cp = seq(0,1,by=.1))
)

# Return info
#* @param info 
#* @get /info
function(){
  list(Name = "Andrew Harvey",
       Links = "google.com")
}


# Pred end point
#* @param pred
#* @get /pred
function(Diabetes_binary=0.13933302,
         HighBP=0.42900110,
         HighChol=0.42412094,
         HvyAlcoholConsump=0.05619678,
         Smoker=0.44316856,
         PhysActivity=0.75654368,
         Age=8.03211921,
         HeartDiseaseorAttack=0.09418559,
         MentHlth=3.18477215){
  list(Diabetes_binary.Mean=as.numeric(Diabetes_binary),
  HighBP.Mean=as.numeric(HighBP),
  HighChol.Mean=as.numeric(HighChol),
  HvyAlcoholConsump.Mean=as.numeric(HvyAlcoholConsump),
  Smoker.Mean=as.numeric(Smoker),
  PhysActivity.Mean=as.numeric(PhysActivity),
  Age.Mean=as.numeric(Age),
  HeartDiseaseorAttack.Mean=as.numeric(HeartDiseaseorAttack),
  MentHlth.Mean=as.numeric(MentHlth))
}

## Test Functions

#Find multiple of two numbers
#* @param n1 1st number
#* @param n2 2nd number
#* @get /mult
function(n1=1, n2=1, msg="",msg2=""){
  result<-as.numeric(n1)*as.numeric(n2)
  list(msg = paste0("When n1 = ",n1," and n2 = ", n2 ),
       msg2 = paste0("The result = ", result ))
}

#Find the sum of 3 numbers
#* @param n1 1st number
#* @param n2 2nd number
#* @param n3 3rd number
#* @get /add
function(n1=1,n2=2,n3=3){
  result<- as.numeric(n1)+as.numeric(n2)+as.numeric(n3)
  list(msg = paste0("When n1 = ",n1,", n2 = ",n2,", and n3 = '\n'", "The result = ", result ))
}


#Print a Test message
#* @param test
#* @get /test
function(){
  "Hello, this is a test....... this is just a test."
}