#
# Exploratory Data Analysis of Titanic Incident #
# Data Source: Kaggle

# Importing R libaries

library(tidyverse)

# The data has been sorted into Train data and Test data

train <- read.csv('train.csv',header = TRUE, stringsAsFactors = FALSE,na.strings = c('','NA',''))
test <- read.csv('test.csv',stringsAsFactors = FALSE, na.strings = c('','NA',''))
head(train)
view(train)
str(train)
str(test)
test$Survived <- 0
table(train$Sex, train$Survived)
prop.table(table(train$Sex, train$Survived))

test <- test %>% select(-Survived) # to remove the added Survived col
test_1 <- test
test_1$Survived <- 0
test_1$Survived[test_1$Sex == 'female'] <- 1
str(test_1)

# test_1 %>% count(Sex)
# table(test_1$Sex)

my_sol <- data.frame(passengerID = test_1$PassengerId, Survived = test_1$Survived)
# view(my_sol) 
# write.csv(my_sol,'my_first_titan.csv')

# Data preparation
test$Survived <- 0
full <- rbind(train,test)
prop.table(table(full$Survived))

# checking for rows with na values
# sum(is.na(full))
colSums(is.na(full))
sapply(full, function(x) sum(is.na(x),na.rm = TRUE)/length(x)*100)

 #data conversion

full$Pclass<-as.factor(full$Pclass)

# another means of getting missing values (Amelia)

library(Amelia)
missmap(full, main = 'Missing Map')
full$Age[is.na(full$Age)] <- mean(full$Age, na.rm = T)
table(full$Embarked, useNA = 'always')
full$Embarked[is.na(full$Embarked)]<- 'S'
full$Fare[is.na(full$Fare)]<- mean(full$Fare, na.rm = T)

#dropping Cabin col, attributed 20% na

full <- full[-11]

# done with cleaning

# train & test splitting

cleaned_train<- full[1:891,]
cleaned_test<- full[892:1309,]

#data exploration (Visualization)

ggplot(cleaned_train,aes(x = Sex, fill = factor(Survived)))+
  geom_bar()

ggplot(cleaned_train, aes(x = Sex, fill = factor(Survived))) +
  theme_bw()+
  facet_wrap(~ Pclass)+
  geom_bar()+
  labs(y = "Passenger Count", 
       title = "Survival rate by Pclass & Gender")

# ggplot(cleaned_train, aes(x = Fare))+ geom_histogram()
ggplot(cleaned_train)+geom_histogram(aes(x=Fare), fill = 'white', colour = 'black')


# feature Engineering

#base on the age, want to get childs no; if age <18
full$Child <- NA
full$Child[full$Age<18]<-1
full$Child[full$Age>18]<-0
str(full)


full$Title <- sapply(full$Name, function(x) strsplit(x, split = '[,.]') [[1]][[2]])
full$Title <- sub(' ','', full$Title)# remove the blank & white space
table(full$Title)

table(full$Title)

# combine small title groups

full$Title[full$Title %in% c('Mlle','Mme')] <- 'Mlle'
full$Title[full$Title %in% c('Capt','Don','Major','Sir')] <- 'Sir'
full$Title[full$Title %in% c('the Countess','Dona','Lady','Jonkheer')] <- 'Lady'

# convert to a factor
full$Title <- factor(full$Title)

ggplot(full, aes(x=Title, fill = factor(Survived)))+geom_bar()

#  to get the family size
full$FamilySize <- full$SibSp + full$Parch + 1
table(full$FamilySize)

#split data into train and test
train_featured <- full[1:891,]
test_featured <- full[892:1309,]
train_featured$Survived <- as.factor(train_featured$Survived)
train_featured$Sex <- as.factor(train_featured$Sex)
train_featured$Embarked <- as.factor(train_featured$Embarked)

test_featured$Sex <- as.factor(test_featured$Sex)
test_featured$Embarked  <- as.factor(test_featured$Embarked)
table(test_featured)
str(test_featured)
view(test_featured)


###### MODEL Building ##############

# library(caTools)
# set.seed(390)
# split <- sample.split(train_featured,SplitRatio = 0.8)
# train.data <- subset(train_featured, split == T)
# test.data <- subset(train_featured, split == F)
# str(train.data)
# str(test.data)
# 
# # Train model with log ress using glm func
# # without feature engineering: removing passID, name, ticket, child,title,family size
# logit_model1 <- glm(Survived~.,family = binomial(link = 'logit'),data = train.data[-c(1,4,9,12,13,14)])
# summary(logit_model1)

#### We check on Mdel Building Later ###########

