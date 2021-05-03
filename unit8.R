install.packages("tree")
library(tree)
library (ISLR)
attach(Carseats)


######################################################################
#      classification tree
######################################################################

## create a new variable
High=ifelse(Sales <=8," No"," Yes ")

## 
Carseats =data.frame(Carseats ,High)

##fit a classification tree in order to predict  High using all variables but Sales
tree.carseats =tree(High ~ .-Sales,Carseats )

## summary of the fitting
summary(tree.carseats )

### plotting
plot(tree.carseats )
text(tree.carseats ,pretty =0)
tree.carseats

set.seed (1021)
## training data
train=sample (1: nrow(Carseats ), 200)
## test data
Carseats.test=Carseats [-train ,]

High.test=High[-train ]

tree.carseats =tree(High ~ . -Sales ,Carseats ,subset =train )
tree.pred=predict (tree.carseats,Carseats.test ,type ="class")

### training error
table(tree.pred ,High.test)

## 
cv.carseats =cv.tree(tree.carseats ,FUN=prune.misclass)
names(cv.carseats)
cv.carseats

##plot the error rate as a function of both size and k.
par(mfrow =c(1,2))
plot(cv.carseats$size ,cv.carseats$dev ,type="b")
plot(cv.carseats$k ,cv.carseats$dev ,type="b")
prune.carseats =prune.misclass(tree.carseats ,best = 11)

## plot the classification after pruning 
plot(prune.carseats)
text(prune.carseats ,pretty =0)

## prediction
tree.pred=predict (prune.carseats , Carseats.test ,type="class")
table(tree.pred ,High.test)

###################################################################
# Fitting Regression Trees
###################################################################
library (MASS)
set.seed (11112)
train = sample (1: nrow(Boston ), nrow(Boston)/2)
tree.boston = tree(medv ~ .,Boston ,subset =train)
summary(tree.boston)

# fitted tree 
plot(tree.boston )
text(tree.boston ,pretty =0)

##USE THIS FOR Q1 OF THE ASSIGNMENT4##
##use the cv.tree() function to see whether pruning the tree will improve performance.
cv.boston =cv.tree(tree.boston )
plot(cv.boston$size,cv.boston$dev ,type='b')

#prune the tree
prune.boston =prune.tree(tree.boston ,best = 8)
plot(prune.boston )
text(prune.boston ,pretty =0)

# prediction 
yhat=predict(tree.boston,newdata =Boston [-train ,])
boston.test=Boston [-train ,"medv"]
plot(yhat,boston.test)
abline(0,1)
mean((yhat-boston.test)^2)
 

