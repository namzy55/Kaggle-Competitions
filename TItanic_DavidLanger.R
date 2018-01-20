#Import the dataset into R
train <- read.csv("D:/Study Material/R/dir/Sample Dataset/Titanic_DavidLanger/train.csv")
test <- read.csv("D:/Study Material/R/dir/Sample Dataset/Titanic_DavidLanger/test.csv")

#View the datasets
View(train)
View(test)

#Check the structure of both the datasets
str(train)
str(test)

#Append the column "survived" in the test dataset
test.survived <- data.frame(survived = rep("None", nrow(test)), test[,])
View(test.survived)

#Combine datasets
data.combined <- rbind(train, test.survived)
View(data.combined)

#Change the data types of the columns pclass and survived
str(data.combined)
data.combined$pclass <- as.factor(data.combined$pclass)
data.combined$survived <- as.factor(data.combined$survived)

#Take a look at gross survival rate
table(data.combined$survived)

#Distribution across class
table(data.combined$pclass)

#Load ggplot2 for data visualization
library(ggplot2)

#Hypothesis: Rich people has a higher chance of surviving
train$pclass <- as.factor(train$pclass)
train$survived <- as.integer(train$survived)

ggplot(train, aes(x = pclass, fill = factor(survived))) +
  geom_bar(width = 0.5) +
  xlab("Pclass") + 
  ylab("Total Count") +
  labs(fill = "Survived")

#Check the number of unique names in the combined data set
head(as.character(train$name))
length(unique(as.character(data.combined$name)))

#2 duplicate names
#First get the duplicate names and store them in a vevtor
dup_names <- as.character(data.combined[which(duplicated(as.character(data.combined$name))), "name"])

#Now, take a look at those records
data.combined[which(data.combined$name %in% dup_names),]

#String comparison/matching
library(stringr)

#Extract the records where name has Miss./Mrs.
str(data.combined)
misses <- data.combined[which(str_detect(data.combined$name, "Mrs.")),]
head(misses)

#Check the pattern for males
males <- data.combined[data.combined$sex == "male",]
head(males)

#Adding the Title column to the dataset
#Creating a Utility function

extractTitle <- function(name) {
  name <- as.character(name)
  
  if(length(grep("Miss.", name)) >0) {
    return ("Miss.")
  } else if (length(grep("Master.", name)) >0) {
    return ("Master.")
  } else if (length(grep("Mrs.", name)) >0) {
    return ("Mrs.")
  } else if (length(grep("Mr.", name)) >0) {
    return ("Mr.")
  } else {
    return ("Others")
  } 
}

titles <- NULL
for(i in 1:nrow(data.combined)) {
  titles <- c(titles, extractTitle(data.combined[i, "name"]))
}
data.combined$title <- as.factor(titles)

#Plot graph using pclass and title for the number of survivors
ggplot(data.combined[1:891,], aes(x = title, fill = survived)) +
  geom_bar(width = 0.5) + 
  facet_wrap(~pclass) + 
  ggtitle("Pclass") + 
  xlab("Title") +
  ylab("Total Count") +
  labs(fill = "Survived")


##########################################################################################

#Distribution of sex in the dataset
table(data.combined$sex)

#3 way visualization between pclass, sex and survival
str(data.combined)
ggplot(data.combined[1:891,], aes(x = sex, fill = survived)) +
  geom_bar(banwidth = 0.5) +
  facet_grid(~pclass) +
  xlab("Sex") +
  ylab("TOtal Count") +
  labs(fill = "Survived")

#Distribution of age across entire dataset
summary(data.combined$age)
summary(data.combined[1:891,"age"])

#Three way visualization between pclass, age and survival
na.omit(data.combined)
ggplot(data.combined[1:891,], aes(x = age, fill = survived)) +
  geom_histogram(binwidth = 10) +
  facet_grid(~sex + pclass) +
  xlab("Age") +
  ylab("TOtal Count")

#Validating that master is a good proxy for Male children
boys <- data.combined[which(data.combined$title == "Master."),]
summary(boys$age)

#Checking the same for Miss
misses <- data.combined[which(data.combined$title == "Miss."),]
summary(misses$age)

ggplot(misses[misses$survived != "None",], aes(x = age, fill = survived)) +
  facet_grid(~pclass) +
  geom_histogram(binwidth = 5) + 
  xlab("Age") +
  ylab("Total Count")

misses.alone <- misses[which(misses$sibsp == 0 & misses$parch == 0),]
summary(misses.alone$age)
length(which(misses.alone$age <= 14.5))

#Checking the sibsp variable
summary(data.combined$sibsp)

data.combined$sibsp <- as.factor(data.combined$sibsp)

#Visualization using variables - sibsp, pclass and title
ggplot(data.combined[1:891,], aes(x = sibsp, fill = survived)) +
  facet_grid(~pclass + title) +
  geom_bar(width = 50) + 
  xlab("Sibsp") +
  ylab("Total Count") +
  labs(fill = "Survived")


#Visualization using variables - parch, pclass and title
data.combined$parch <- as.factor(data.combined$parch)
ggplot(data.combined[1:891,], aes(x = parch, fill = survived)) +
  facet_grid(~pclass + title) +
  geom_bar(width = .5) + 
  xlab("Parch") +
  ylab("Total Count") +
  labs(fill = "Survived")
  
#Using Feature Engg, calculating the total family length
temp.sibsp<- c(train$sibsp, test$sibsp)
temp.parch<- c(train$parch, test$parch)

data.combined$family.size <- as.factor(temp.sibsp + temp.parch + 1)

ggplot(data.combined[1:891,], aes(x = family.size, fill = survived)) +
  facet_grid(~pclass + title) +
  geom_bar() + 
  xlab("Family Size") +
  ylab("Total Count") +
  labs(fill = "Survived")


##########################################################################################
#Exploring the ticket variable
str(data.combined$ticket)
data.combined$ticket <- as.character(data.combined$ticket)
data.combined$ticket[1:20]

#Extracting the first char value of the ticket
ticket.first.char <- as.factor(ifelse(data.combined$ticket == "", "", substr(data.combined$ticket,1,1)))
unique(ticket.first.char)

#Appending the column into the main dataset
data.combined$ticket.first.char <- as.factor(ticket.first.char)

#Looking at a high level plot
ggplot(data.combined[1:891,], aes(x = ticket.first.char, fill = survived)) +
  geom_bar() + 
  # xlab("Ticket.First.Char") +
  ylab("Total Count") +
  ylim(0,350) +
  labs(fill = "Survived")

#Drilling down a little
ggplot(data.combined[1:891,], aes(x = ticket.first.char, fill = survived)) +
  geom_bar() + 
  facet_wrap(~pclass + title) +
  xlab("Ticket.First.Char") +
  ylab("Total Count") +
  labs(fill = "Survived")


#Exploring the fare paid variable
summary(data.combined$fare)
length(unique(data.combined$fare))

#Treating fare as numeric, creating a histogram
ggplot(data.combined, aes(x = fare)) +
  geom_histogram(binwidth = 5) + 
  xlab("Fare") +
  ylab("Total Count") +
  ylim(0,500)
  
#Treating fare as numeric, creating a histogram
ggplot(data.combined[1:891,], aes(x = fare, fill =survived)) +
  geom_histogram(binwidth = 5) + 
  facet_wrap(~pclass + title) +
  xlab("Fare") +
  ylab("Total Count") +
  ylim(0,75)


#Exploring the cabin variable
str(data.combined$cabin)

data.combined$cabin <- as.character(data.combined$cabin)
data.combined$cabin[1:200]

#Replacing empty spaces by "U"
data.combined[which(data.combined$cabin == ""), "cabin"] <- "U"
data.combined$cabin[1:200]

#Extracting the 1st char from cabin
cabin.first.char <- as.factor(substr(data.combined$cabin,1,1))
str(cabin.first.char)
levels(cabin.first.char)

#Including this in the main dataset
data.combined$cabin.first.char <- cabin.first.char

#High level plot
ggplot(data.combined[1:891,], aes(x = cabin.first.char, fill =survived)) +
  geom_bar() + 
  xlab("cabin.first.char") +
  ylab("Total Count") +
  labs(fill = "Survived")

ggplot(data.combined[1:891,], aes(x = cabin.first.char, fill =survived)) +
  geom_bar() + 
  facet_wrap(~pclass) +
  xlab("Fare") +
  ylab("Total Count") +
  ylim(0,500) +
  labs(fill = "Survived")

ggplot(data.combined[1:891,], aes(x = cabin.first.char, fill =survived)) +
  geom_bar() + 
  facet_wrap(~pclass + title) +
  xlab("Fare") +
  ylab("Total Count") +
  ylim(0,500) +
  labs(fill = "Survived")

#Folks with multiple cabins
data.combined$cabin.multiple <- as.factor(ifelse(str_detect(data.combined$cabin, ""), "Y", "N"))


ggplot(data.combined[1:891,], aes(x = cabin.multiple, fill =survived)) +
  geom_bar() + 
  facet_wrap(~pclass + title) +
  xlab("cabin.multiple") +
  ylab("Total Count") +
  ylim(0,500) +
  labs(fill = "Survived")

#Exploring the Embarkment variable
str(data.combined$embarked)

ggplot(data.combined[1:891,], aes(x = embarked, fill =survived)) +
  geom_bar() + 
  facet_wrap(~pclass + title) +
  xlab("embarkement") +
  ylab("Total Count") +
  ylim(0,500) +
  labs(fill = "Survived")


##########################################################################################
##Exploratory Modelling - Using Random Forest
library(randomForest)


#Train a Random Forest with the default parameters - pclass and title
rf.train.1 <- data.combined[1:891, c("pclass","title")]
rf.label <- as.factor(train$survived)

set.seed(1234)
rf.1 <- randomForest(x = rf.train.1, y = rf.label, importance = TRUE, ntree = 500)
rf.1
varImpPlot(rf.1)


#Train a Random Forest with the default parameters - pclass, title and sibsp
rf.train.2 <- data.combined[1:891, c("pclass","title", "sibsp")]

set.seed(1234)
rf.2 <- randomForest(x = rf.train.2, y = rf.label, importance = TRUE, ntree = 500)
rf.2
varImpPlot(rf.2)


#Train a Random Forest with the default parameters - pclass, title and parch
rf.train.3 <- data.combined[1:891, c("pclass","title", "parch")]

set.seed(1234)
rf.3 <- randomForest(x = rf.train.3, y = rf.label, importance = TRUE, ntree = 500)
rf.3
varImpPlot(rf.3)


#Train a Random Forest with the default parameters - pclass, title, parch and sibsp
rf.train.4 <- data.combined[1:891, c("title","pclass", "parch", "sibsp")]

set.seed(1234)
rf.4 <- randomForest(x = rf.train.4, y = rf.label, importance = TRUE, ntree = 500)
rf.4
varImpPlot(rf.4)



#Train a Random Forest with the default parameters - pclass, title and family.size
rf.train.5 <- data.combined[1:891, c("title","pclass", "family.size")]

set.seed(1234)
rf.5 <- randomForest(x = rf.train.5, y = rf.label, importance = TRUE, ntree = 5000)
rf.5
varImpPlot(rf.5)



#Train a Random Forest with the default parameters - pclass, title, sibsp and family.size
rf.train.6 <- data.combined[1:891, c("pclass","title", "sibsp", "family.size")]

set.seed(1234)
rf.6 <- randomForest(x = rf.train.6, y = rf.label, importance = TRUE, ntree = 500)
rf.6
varImpPlot(rf.6)


#Train a Random Forest with the default parameters - pclass, title, parch and family.size
rf.train.7 <- data.combined[1:891, c("pclass","title", "parch", "family.size")]

set.seed(1234)
rf.7 <- randomForest(x = rf.train.7, y = rf.label, importance = TRUE, ntree = 500)
rf.7
varImpPlot(rf.7)


##########################################################################################
#Cross Validation

#As per the results from the above analysis, rf.5 gave the most optimum result
#Lets submit the above result to Kaggle

#Subset our test records and features
test.submit.df <- data.combined[892:1309, c("pclass", "title", "family.size")]

#Make predictions
rf.5.preds <- predict(rf.5, test.submit.df)
table(rf.5.preds)

#Write out a CSV file for submission in Kaggle
submit.df <- data.frame(PassengerId = rep(892:1309), Survived = rf.5.preds)
write.csv(submit.df, file = "Sample Dataset/Titanic_DavidLanger/Kaggle/SUB_RF_20170118_1.csv", row.names = FALSE)


#OOB score more than what kaggle predicted. Lets do the cross validation
#Using Caret library to do the cross validation
library(caret)
library(doSNOW) #Used for parallel processing (works both on Mac and Windows)

#DOing a 10 fold CV - means divide the dataset into 10 parts, use the 9 subsets to train the model 
#and run it in the 10th dataset. This repeats untill all the 10 subsets have been used as a test dataset
#once. Also, based on few researches, doing 10fold CV, 10 times, gives the most effective result. But also,
#remember, more the number of folds and number of times, more the computation resource will be used


#Leveraging caret to create 100 folds, also making sure, in each of these folds, the ratio of number of 
#people perished and survived is maintained based on the original train dataset. This concept is called
#Stratification
# set.seed(1456)
# cv.10.folds <- createMultiFolds(rf.label, k = 10, times = 10)
# 
# #Checking the Stratification
# table(rf.label)
# 342 / 549
# 
# table(rf.label[cv.10.folds[[20]]])
# 307/494  
# 
# #Setting up Caret's trainControl object
# ctrl.1 <- trainControl(method = "repeatedcv", number = 10, repeats = 10, index = cv.10.folds)
# 
# #Setting up doSnow package for multi-core taining (parallel processing). 
# c1 <- makeCluster(6, type = "SOCK")
# registerDoSNOW(c1)
# 
# 
# #Setting seed for reproducibilty and train
# set.seed(34324)
# fr.5.cv.1 <- train(x = rf.train.5, y = rf.label, method = "rf", tuneLength = 3, ntree = 1000, trControl = ctrl.1)
# 
# #Shutdown Cluster
# stopCluster(c1)
# 
# #Check out the result
# fr.5.cv.1


#Using a 5 fold CV repreated 10 times
set.seed(1456)
cv.5.folds <- createMultiFolds(rf.label, k = 5, times = 10)

#Setting up Caret's trainControl object
ctrl.2 <- trainControl(method = "repeatedcv", number = 5, repeats = 10, index = cv.5.folds)

#Setting up doSnow package for multi-core taining (parallel processing). 
c1 <- makeCluster(6, type = "SOCK")
registerDoSNOW(c1)


#Setting seed for reproducibilty and train
set.seed(1973)
fr.5.cv.2 <- train(x = rf.train.5, y = rf.label, method = "rf", tuneLength = 3, ntree = 1000, trControl = ctrl.2)

#Shutdown Cluster
stopCluster(c1)

#Check out the result
fr.5.cv.2



#Using a 3 fold CV repreated 10 times
set.seed(1456)
cv.3.folds <- createMultiFolds(rf.label, k = 3, times = 10)

#Setting up Caret's trainControl object
ctrl.3 <- trainControl(method = "repeatedcv", number = 3, repeats = 10, index = cv.3.folds)

#Setting up doSnow package for multi-core taining (parallel processing). 
c1 <- makeCluster(6, type = "SOCK")
registerDoSNOW(c1)


#Setting seed for reproducibilty and train
set.seed(1973)
fr.5.cv.3 <- train(x = rf.train.5, y = rf.label, method = "rf", tuneLength = 3, ntree = 1000, trControl = ctrl.3)

#Shutdown Cluster
stopCluster(c1)

#Check out the result
fr.5.cv.3


##########################################################################################
#Exploratory Modelling - Part2

#Exploration using Decision Trees rather than Random Forest

library(rpart)
library(rpart.plot)

#Creating a Utility function
rpart.cv <- function(seed, training, labels, ctrl) 
{
  c1 <- makeCluster(6, type = "SOCK")
  registerDoSNOW(c1)
  
  set.seed(seed)
  rpart.cv <- train(x = training, y = labels, method = "rpart", tuneLength = 30, trControl = ctrl)
  
  #Shutdown Cluster
  stopCluster(c1)
  
  return(rpart.cv)
}


#Grab features
features <- c("pclass", "title", "family.size")
rpart.train.1 <- data.combined[1:891, features]

#Run CV and check out result
rpart.1.cv.1 <- rpart.cv(1973, rpart.train.1, rf.label, ctrl.2)
rpart.1.cv.1

#Plot 
prp(rpart.1.cv.1$finalModel, type = 0, extra = 1, under = TRUE)


#Based on the Tree structure (possibility of over-fitting), exploring the title variable
table(data.combined$title)

#PArsing the last name and title
data.combined[1:25,"name"]

name.splits <- str_split(data.combined$name, ",")
name.splits[1]
last.names <- sapply(name.splits, "]", 1)
last.names[1:10]
name.splits

#Adding the last name to the dataset
data.combined$last.names <- last.names

#Now for titles
name.splits <- str_split(sapply(name.splits, "[", 2), " ")
titles <- sapply(name.splits, "[", 2)
unique(titles)

#Exploring the "the" title
data.combined[which(titles == "the"),]

#Remapping of the titles
titles[titles %in% c("Dona.", "the")] <- "Lady."
titles[titles %in% c("Ms.", "Mlle.")] <- "Miss."
titles[titles %in% c("Mme.")] <- "Mrs."
titles[titles %in% c("Don.", "Jonkheer.")] <- "Sir."
titles[titles %in% c("Major.", "Col.", "Capt.")] <- "Officer."

table(titles)

#MAke title as factor and include in the dataset
data.combined$new.title <- as.factor(titles)

#Visualize new version of title
ggplot(data.combined[1:891,], aes(x = new.title, fill = survived)) +
  geom_bar() + 
  facet_wrap(~pclass)

#Collapse titles even more
indexes <- which(data.combined$new.title == "Lady.")
data.combined$new.title[indexes] <- "Mrs."

indexes <-  which(data.combined$new.title %in% c("Dr.", "Rev.", "Sir.", "Officer."))
data.combined$new.title[indexes] <- "Mr." 
                  
#Visualize new version of titles
ggplot(data.combined[1:891,], aes(x = new.title, fill = survived)) +
  geom_bar() + 
  facet_wrap(~pclass)


#Grab features
features <- c("pclass", "new.title", "family.size")
rpart.train.2 <- data.combined[1:891, features]

#Run CV and check out the result
rpart.2.cv.1 <- rpart.cv(1973, rpart.train.2, rf.label, ctrl.2)
rpart.2.cv.1

#Plot
prp(rpart.2.cv.1$finalModel, type = 0, extra = 1, under = TRUE)

#Diving in on 1st class Mr.
indexes.first.mr <- which(data.combined$new.title == "Mr." & data.combined$pclass == "1")
first.mr.df <- data.combined[indexes.first.mr,]
summary(first.mr.df)

#One female. Who?
first.mr.df[first.mr.df$sex == "female",]

#Updating the new title feature
indexes <- which(data.combined$new.title == "Mr." & data.combined$sex == "female")
data.combined$new.title[indexes] <- "Mrs."

#Any other gender slip
length(which(data.combined$sex == "female" & data.combined$new.title %in% c("Master", "Mr.")))

#Refresh data frame
indexes.first.mr <- which(data.combined$new.title == "Mr." & data.combined$pclass == "1")
first.mr.df <- data.combined[indexes.first.mr,]
summary(first.mr.df)

#Looking at surviving 1st class Mr.
summary(first.mr.df[first.mr.df$survived == "1",])
View(first.mr.df[first.mr.df$survived == "1",])

#Looking at some high fares
indexes <- which(data.combined$ticket %in% c("PC 17755", "PC 17611", "113760"))
View(data.combined[indexes,])

#Visualize survival rate for 1st class Mr.
ggplot(first.mr.df, aes(x = fare, fill = survived)) +
  geom_density(alpha = 0.5)

#Engineering features based on all passengers with same ticket
ticket.party.size <- rep(0, nrow(data.combined))
avg.fare <- rep(0.0, nrow(data.combined))
tickets <- unique(data.combined$ticket)
length(tickets)

for(i in 1:length(tickets)) {
  current.ticket <- tickets[i]
  party.indexes <- which(data.combined$ticket == current.ticket)
  current.avg.fare <- data.combined[party.indexes[1], 'fare'] / length(party.indexes)
  
  for(k in 1:length(party.indexes)) {
    ticket.party.size[party.indexes[k]] <- length(party.indexes)
    avg.fare[party.indexes[k]] <- current.avg.fare 
  }
}

data.combined$ticket.party.size <- ticket.party.size
data.combined$avg.fare <- avg.fare

#Refresh 1st class Mr. df
first.mr.df <- data.combined[indexes.first.mr,]
summary(first.mr.df)


#Visualize new feature
ggplot(first.mr.df[first.mr.df$survived != "None",], aes(x = ticket.party.size, fill = survived)) +
  geom_density(alpha = 0.5)

ggplot(first.mr.df[first.mr.df$survived != "None",], aes(x = avg.fare, fill = survived)) +
  geom_density(alpha = 0.5)

#Hypothesis: ticket.party.size highly corelated to avg.fare
summary(data.combined$avg.fare)

#ONe missing value whiere avg.fare is na
data.combined[is.na(data.combined$avg.fare),]

#Get record of similar passenger and summarize avg. fare
indexes <- with(data.combined, which(pclass == "3" & title == "Mr." & family.size == 1 & 
                                       ticket != "3701"))

similar.na.passenger <- data.combined[indexes,]
summary(similar.na.passenger$avg.fare)

#Replacing the NA avg. fare value by median
data.combined[is.na(avg.fare), "avg.fare"] <- "7.840"

#Leverage cater's preProcess function to normalize data
preproc.data.combined <- data.combined[, c("ticket.party.size", "avg.fare")]
preProc <- preProcess(preproc.data.combined, method = c("center", "scale"))

postproc.data.combined <- predict(preProc, preproc.data.combined)

#Finding aorrelation between both the features
cor(postproc.data.combined$ticket.party.size, as.numeric(postproc.data.combined$avg.fare))

#Correlation only for 1st class
indexes <- which(data.combined$pclass == "1")
cor(postproc.data.combined$ticket.party.size[indexes], as.numeric(postproc.data.combined$avg.fare[indexes]))


#Okay, so finally checking if the new features have any major impact
features <- c("pclass", "new.title", "family.size", "ticket.party.size", "avg.fare")
rpart.train.3 <- data.combined[1:891, features]
rpart.train.3$avg.fare <- as.numeric(rpart.train.3$avg.fare)

#Run CV and check the result
rpart.3.cv.1 <- rpart.cv(1973, rpart.train.3, rf.label, ctrl.2)
rpart.3.cv.1

#Plot
prp(rpart.3.cv.1$finalModel, type = 0, extra = 1, under = TRUE)



#######################################################################################
#Submitting, Scoring and few more analysis

#Rpart Score - 83.56%

str(data.combined)
#Subset our test records and features
test.submit.df <- data.combined[892:1309, features]

test.submit.df$avg.fare <- as.numeric(test.submit.df$avg.fare)

#Make predictions
rpart.3.preds <- predict(rpart.3.cv.1$finalModel, test.submit.df, type = "class")
table(rpart.3.preds)

#Write out a CSV file for submission in Kaggle
submit.df <- data.frame(PassengerId = rep(892:1309), Survived = rpart.3.preds)
write.csv(submit.df, file = "Sample Dataset/Titanic_DavidLanger/Kaggle/SUB_RPART_20170119_1.csv", row.names = FALSE)


#Score on Kaggle from the above results - 0.80382




#Using the new features, again calculating the accuracy from Random Forest

features <- c("pclass", "new.title", "ticket.party.size", "avg.fare")
rf.train.temp <- data.combined[1:891, features]

set.seed(1234)
rf.temp <- randomForest(x = rf.train.temp, y = rf.label, ntree = 1000)
rf.temp

test.submit.df <- data.combined[892:1309, features]

#Make predictions
rf.preds <- predict(rf.temp, test.submit.df)
table(rf.preds)


#Write out a CSV file for submission in Kaggle
submit.df <- data.frame(PassengerId = rep(892:1309), Survived = rf.preds)
write.csv(submit.df, file = "Sample Dataset/Titanic_DavidLanger/Kaggle/SUB_RF_20170119_1.csv", row.names = FALSE)


