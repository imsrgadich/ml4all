#code for Breast Cancer dataset
library(caret)
library(pROC)

setwd("F:/Documents/ml4-all/")

#read the file
data <- read.csv("breast_Cancer_Dataset.txt",header = FALSE,na.strings = c("?"))
#missing values are given as ? in this dataset

#add column names
names(data) <- c("Sample_code_number","Clump_Thickness","Uniformity_Cell_Size","Uniformity_Cell_Shape",
                 "Marginal_Adhesion","Single_Epithelial_Cell_Size","Bare_Nuclei","Bland Chromatin",
                 "Normal_Nucleoli","Mitoses","Class")

#check for missing values
sapply(data,function(x){length(which(is.na(x)))})
#16 missing values in Bare_Nuclei column

#do mean imputation
ind <- which(is.na(data$Bare_Nuclei))

data$Bare_Nuclei[ind] <- mean(data$Bare_Nuclei,na.rm = TRUE)


data$Class <- as.factor(data$Class)
levels(data$Class) <- c("B","M")
#B - benign (negative class) and M Malignant (positive class)


#normalize numeric data
normalize <- function(x) {
  return ((x - min(x)) / (max(x) - min(x))) }

data_n <- as.data.frame(lapply(data[2:10], normalize))

#add the id variable and the class variable

data_n <- cbind(data$Sample_code_number,data_n)
names(data_n)[1] <- "Sample_code_number"

data_n <- cbind(data_n,data$Class)
names(data_n)[11] <- "Class"

summary(data_n)

#split into training and testing
set.seed(792) #to get identical training and test indexes on multiple runs
trainind <- createDataPartition(data$Class,p = 0.7,list = FALSE)

training <- data[trainind,]

testind <- setdiff(1:dim(data)[1],trainind)

test <- data[testind,]

#training the knn model 

trctrl <- trainControl(method = "repeatedcv", number = 10, repeats = 3)
set.seed(792)
knn_fit <- train(Class ~., data = training, method = "knn",
                 preProcess = c("center", "scale"),
                 tuneLength = 10)

knn_fit

#now predict on test set
test_pred <- predict(knn_fit, newdata = test)
test_pred

#confusion matrix
caret::confusionMatrix(test_pred,test$Class)
