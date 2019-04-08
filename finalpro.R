
company<-read.csv("pro.csv")
company
dim(company)
str(company$PerformanceRating)

# convert to factors
company$PerformanceRating<-factor(company$PerformanceRating,levels = c(2,3,4),
                                  labels=c("Good","Excellent","Outstanding"))

#checking the summary of the data
summary(company)

# EmpNumber is not useful so can be removed
company<-company[,c(-1)]

#converting the numerical data to categorical
company$EmpEducationLevel = as.ordered(company$EmpEducationLevel)
company$EmpEnvironmentSatisfaction = as.ordered(company$EmpEnvironmentSatisfaction)
company$EmpJobInvolvement = as.ordered(company$EmpJobInvolvement)
company$EmpJobSatisfaction = as.ordered(company$EmpJobSatisfaction)
company$EmpRelationshipSatisfaction = as.ordered(company$EmpRelationshipSatisfaction)
company$EmpWorkLifeBalance = as.ordered(company$EmpWorkLifeBalance)
company$EmpJobLevel = as.ordered(company$EmpJobLevel)

set.seed(2)

company$ind<-sample(2,nrow(company),replace=TRUE,prob=c(0.7,0.3))
company$ind
traincomp<-company[(company$ind==1),]
testcomp<-company[(company$ind==2),]

testcomp<-testcomp[,c(-28)]
traincomp<-traincomp[,c(-28)]
testcomp
nrow(traincomp)
nrow(testcomp)

install.packages("tree")
library(tree)

install.packages("randomForest")
library(randomForest)

#Construct Model
tree_model<- tree(traincomp$PerformanceRating~.,traincomp)
random_model<-randomForest(traincomp$PerformanceRating~.,traincomp)
# plot

plot(tree_model)
text(tree_model,pretty=0)
?pretty
# Test
testing<-predict(tree_model,testcomp[,1:26], type = "class")
mean(testing==testcomp$PerformanceRating)
testing

preds<-predict(random_model,testcomp[,1:26])
table(preds)


mean(preds==testcomp$PerformanceRating)
str(testing)
str(company$PerformanceRating)

importance(random_model)

