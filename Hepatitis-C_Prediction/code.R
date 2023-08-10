dataset <- read.csv("D:\\IDS_Project_Final\\HepatitisCdata.csv")
head(dataset)

ls(dataset)
str(dataset)
summary(dataset)

#removing first column
dataset <- subset(dataset, select = -1)

#Visualizing Unique Values
unique(dataset$Category)
unique(dataset$Sex)


#Removing NA values with their Mean

mean_CHOL <- mean(dataset$CHOL , na.rm=TRUE)
dataset$CHOL[is.na(dataset$CHOL)] = mean_CHOL

mean_ALB <- mean(dataset$ALB , na.rm=TRUE)
dataset$ALB[is.na(dataset$ALB)] = mean_ALB

mean_ALP <- mean(dataset$ALP , na.rm=TRUE)
dataset$ALP[is.na(dataset$ALP)] = mean_ALP

mean_ALT <- mean(dataset$ALT , na.rm=TRUE)
dataset$ALT[is.na(dataset$ALT)] = mean_ALT

mean_PROT <- mean(dataset$PROT , na.rm=TRUE)
dataset$PROT[is.na(dataset$PROT)] = mean_PROT


#Plotting The Data
hist(dataset$Age, main = "Age",xlab= 'Age', col = "lightblue", border = "black")
hist(dataset$ALB, main = "ALB",xlab= 'ALB', col = "lightblue", border = "black")
hist(dataset$ALP, main = "ALP",xlab= 'ALP', col = "lightblue", border = "black")
hist(dataset$ALT, main = "ALT",xlab= 'ALT', col = "lightblue", border = "black")
hist(dataset$AST, main = "AST",xlab= 'AST', col = "lightblue", border = "black")
hist(dataset$BIL, main = "BIL",xlab= 'BIL', col = "lightblue", border = "black")
hist(dataset$CHE, main = "CHE",xlab= 'CHE', col = "lightblue", border = "black")
hist(dataset$CHOL, main = "CHOL",xlab= 'CHOL', col = "lightblue", border = "black")
hist(dataset$CREA, main = "CREA",xlab= 'CREA', col = "lightblue", border = "black")
hist(dataset$GGT, main = "GGT",xlab= 'GGT', col = "lightblue", border = "black")
hist(dataset$PROT, main = "PROT",xlab= 'PROT', col = "lightblue", border = "black")


#Changing Categorical Attributes to Numeric
dataset$Category <- factor(dataset$Category, levels = c("0=Blood Donor", "0s=suspect Blood Donor", "1=Hepatitis", 
                                                        "2=Fibrosis", "3=Cirrhosis"), labels = c(0,1,2,3,4))
dataset$Category = as.numeric(dataset$Category)

dataset$Sex <- ifelse(dataset$Sex == "m", 0, 1)


barplot(table(dataset$Sex), xlab = "Sex",  main = "Sex", col = "lightblue", border = "black")
barplot(table(dataset$Category), xlab = "Category",  main = "Category", col = "lightblue", border = "black")



#Correlation 
cor(dataset$Category , dataset$Age, method="pearson")
cor(dataset$Category , dataset$Sex, method="pearson")
cor(dataset$Category , dataset$ALB, method="pearson")
cor(dataset$Category , dataset$ALP, method="pearson")
cor(dataset$Category , dataset$ALT, method="pearson")
cor(dataset$Category , dataset$AST, method="pearson")
cor(dataset$Category , dataset$BIL, method="pearson")
cor(dataset$Category , dataset$CHE, method="pearson")
cor(dataset$Category , dataset$CHOL, method="pearson")
cor(dataset$Category , dataset$CREA, method="pearson")
cor(dataset$Category , dataset$GGT, method="pearson")
cor(dataset$Category , dataset$PROT, method="pearson")
#Visualizing Correlation Matrix
library(corrplot)
corrplot(cor(dataset),
         method = "color",
)


#Applying KNN

install.packages("caret")
install.packages("class")

library(e1071)
library(class)
library(caret)

important_attributes <- dataset[, c("Category","Age", "Sex", "ALB" , "ALT", "AST", "BIL", "CHE", "CHOL", "CREA", "GGT" )]


#Dividing into Train & Test Set
set.seed(123)

train_indices <- sample(1:nrow(dataset), 0.80 * nrow(dataset))

train_data <- important_attributes[train_indices, ]
train_labels <- important_attributes$Category[train_indices]
test_data <- important_attributes[-train_indices, ]
test_labels <-  important_attributes$Category[-train_indices]
  
knn_model <- knn(train = train_data, test = test_data, cl = train_labels, k = 5)

accuracy <- sum(knn_model == test_labels) / length(test_labels)

confusion_matrix <- table(Actual = test_labels, Predicted = knn_model)

confusion_matrix

precision <- diag(confusion_matrix) / rowSums(confusion_matrix)
recall <- diag(confusion_matrix) / colSums(confusion_matrix)
recall[is.nan(recall)] <- 0

print(precision)
print(recall)

print(mean(precision))
print(mean(recall))

rowSums(confusion_matrix)


#10-Fold Cross Validation
set.seed(1234)

cv <- createFolds(dataset$Category, 10)



accuracies_for_10_fold <- sapply(cv, function(i) {
  train_data <- important_attributes[-i,]
  test_data <- important_attributes[i,]
  knn <- knn(train_data, test_data, cl = train_data$Category, k = 5)
  sum(knn == test_data$Category) / nrow(test_data)
})



print(accuracies_for_10_fold)

mean_accuracy <- mean(accuracies_for_10_fold)
print(mean_accuracy)


















