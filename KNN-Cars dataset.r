#Classification using Nearest Neighbors 

#Classifying Cars data 


# import the CSV file
car <- read.csv("Car_Identification.csv", stringsAsFactors = FALSE)

# examine the structure of the car data frame
str(car)

# table of diagnosis
table(car$diagnosis)

# recode diagnosis as a factor
car$diagnosis <- factor(car$Category, levels = c("T", "J"),
                         labels = c("Toyota", "Jaguar"))

# table or proportions with more informative labels
round(prop.table(table(car$diagnosis)) * 100, digits = 1)

# summarize three numeric features
summary(car[c("BodyRoll", "DownForce", "Braking")])

# create normalization function
normalize <- function(x) {
  return ((x - min(x)) / (max(x) - min(x)))
}

# test normalization function - result should be identical
normalize(c(1, 2, 3, 4, 5))
normalize(c(10, 20, 30, 40, 50))

# normalize the car data
car_n <- as.data.frame(lapply(car[2:3], normalize))

# confirm that normalization worked
summary(car_n$area_mean)

# create training and test data
car_train <- car_n[1:469, ]
car_test <- car_n[470:569, ]

# create labels for training and test data

car_train_labels <- car[1:469, 1]
car_test_labels <- car[470:569, 1]

# visualize the data using labels

plot(car$Braking,car$DownForce, 
     main = 'Scatterplot',
     xlab = 'Braking',
     ylab = 'DownForce')

pairs(~Braking+DownForce+BodyRoll+Acceleration, 
      data = car,
      main = 'Scaterplot of many variables')
install.packages("car")
library(car)

scatterplot(DownForce ~ Braking | diagnosis, data = car,
     main = 'Scatterplot',
     xlab = 'Braking',
     ylab = 'DownForce')

scatterplotMatrix(~Braking+DownForce+BodyRoll+Acceleration | diagnosis, data=car)

#Training a model on the data ----

# load the "class" library
library(class)

car_test_pred <- knn(train = car_train, test = car_test,
                      cl = car_train_labels, k = 21)

head(car_test)
head(car_test_pred)

#Evaluating model performance ----

# load the "gmodels" library
library(gmodels)

# Create the cross tabulation of predicted vs. actual
CrossTable(x = car_test_labels, y = car_test_pred,
           prop.chisq = FALSE)

#Improving model performance ----

# use the scale() function to z-score standardize a data frame
car_z <- as.data.frame(scale(car[-1]))

# confirm that the transformation was applied correctly
summary(car_z$area_mean)

# create training and test datasets
car_train <- car_z[1:469, ]
car_test <- car_z[470:569, ]

# re-classify test cases
car_test_pred <- knn(train = car_train, test = car_test,
                      cl = car_train_labels, k = 21)

# Create the cross tabulation of predicted vs. actual
CrossTable(x = car_test_labels, y = car_test_pred,
           prop.chisq = FALSE)

# try several different values of k
car_train <- car_n[1:469, ]
car_test <- car_n[470:569, ]

#start time
strt<-Sys.time()

car_test_pred <- knn(train = car_train, test = car_test, cl = car_train_labels, k=1)
CrossTable(x = car_test_labels, y = car_test_pred, prop.chisq=FALSE)

car_test_pred <- knn(train = car_train, test = car_test, cl = car_train_labels, k=5)
CrossTable(x = car_test_labels, y = car_test_pred, prop.chisq=FALSE)

car_test_pred <- knn(train = car_train, test = car_test, cl = car_train_labels, k=11)
CrossTable(x = car_test_labels, y = car_test_pred, prop.chisq=FALSE)

car_test_pred <- knn(train = car_train, test = car_test, cl = car_train_labels, k=15)
CrossTable(x = car_test_labels, y = car_test_pred, prop.chisq=FALSE)

car_test_pred <- knn(train = car_train, test = car_test, cl = car_train_labels, k=21)
CrossTable(x = car_test_labels, y = car_test_pred, prop.chisq=FALSE)

car_test_pred <- knn(train = car_train, test = car_test, cl = car_train_labels, k=27)
CrossTable(x = car_test_labels, y = car_test_pred, prop.chisq=FALSE)

#end time
print(Sys.time()-strt)




