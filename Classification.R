
# Load Dataset in
seedsdata = read.csv('C:/Users/senth/Documents/RDatasets/seeds_dataset_class.csv', sep=",")

#----------------------------------------------------------Data Pre-Processing-------------------------------------------------------------------------

# Randomise Dataset - Ensures that the training/test set are both random samples 
# Biases in the ordering of the dataset are not retained in the samples we take for training and testing the models.
seeds_rand=seedsdata[sample(209,209),]

# Separate Class and Values into separate variables- [,1]- returns first column, [,-1] - returns all but first column
seedclass = seeds_rand[,1]
seedvalues = seeds_rand[,-1]


# Split Dataset into training/test set (70/30 split - highest level of accuracy)
# Set up a training set to learn model
seedclassTrain = seedclass[1:146]
seedvaluesTrain = seedvalues[1:146,]


# Set up test set to score the accuracy
seedclassTest = seedclass[147:209]
seedvaluesTest = seedvalues[147:209,]


#---------------------------------------------------Decision Trees----------------------------------------------------

# Create Decision Tree
install.packages("rpart") # Install rpart package
library(rpart) #  Load rpart package in
fit <- rpart(seedclassTrain~., method="class", data=seedvaluesTrain)

# Plot Decision Tree
plot(fit, uniform=TRUE, main="Decision Tree for Seedsdata")
text(fit, use.n=TRUE, all=TRUE, cex=.8)

# Calculate predictions for each testcase in the test set
treepred <-predict(fit, seedvaluesTest, type = 'class')

# Compare to actual test values to get accuracy
n = length(seedclassTest) #the number of test cases
ncorrect = sum(treepred==seedclassTest) #the number of correctly predicted
accuracy=ncorrect/n
print(accuracy)

# Accuracy = 0.9365079
# Print Results as Confusion Matrix 
table_mat = table(seedclassTest, treepred)
print(table_mat)

# Prune tree (with a pruning parameter cp, using different degrees of pruning)
pfit<- prune(fit, cp = 0.2)
plot(pfit, uniform=TRUE, main="Pruned Decision Tree for SeedsData")
text(pfit, use.n=TRUE, all=TRUE, cex=.8)

# Calculate predictions with pruned tree
prunetreepred <-predict(pfit, seedvaluesTest, type = 'class')

# Compare pruned tree to actual test values to get accuracy
n = length(seedclassTest) #the number of test cases
ncorrect = sum(prunetreepred==seedclassTest) #the number of correctly predicted
accuracy=ncorrect/n
print(accuracy)

# Results with different pruning parameters
# cp = 0.1        accuracy = 0.9206349
# cp = 0.3        accuracy = 0.9206349
# cp = 0.4        accuracy = 0.6666667
# cp = 0.5        accuracy = 0.3015873 
# cp = 0.9        accuracy = 0.3015873



# Scatterplot 2 selected variables of the data and colour code according to the decision tree output
plot(seeds_rand$Area, seeds_rand$GrooveLength, main="Area against GrooveLength", col = seeds_rand$Class,xlab = "Area", ylab = "GrooveLength")



#---------------------------------------------K-Nearest Neighbour-----------------------------------------------------

# Install class library
library(class)

# Generate Predicted Classes (dataframe of training cases, dataframe of testing cases,true classifications of training set)
knn3pred = knn(seedvaluesTrain, seedvaluesTest, seedclassTrain, k = 5)

# Compare to actual test values to get accuracy
n = length(seedclassTest) #the number of test cases
ncorrect = sum(knn3pred==seedclassTest) #the number of correctly predicted
accuracy=ncorrect/n
print(accuracy)

# Results with different values of K
# k = 2         Accuracy = 0.8571429
# k = 3         Accuracy = 0.8888889
# k = 4         Accuracy = 0.8730159
# k = 5         Accuracy = 0.9047619
# k = 7         Accuracy = 0.9047619


