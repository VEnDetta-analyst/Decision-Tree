download.file("https://ibm.box.com/shared/static/dpdh09s70abyiwxguehqvcq3dn0m7wve.data", "mushroom.data")
data=read.csv("C:/Users/VEDANG SAWANT/Downloads/agaricus-lepiota.data",header = F)
colnames(data) <- c("Class","cap.shape","cap.surface","cap.color","bruises","odor","gill.attachment","gill.spacing",
                         "gill.size","gill.color","stalk.shape","stalk.root","stalk.surface.above.ring",
                         "stalk.surface.below.ring","stalk.color.above.ring","stalk.color.below.ring","veil.type","veil.color",
                         "ring.number","ring.type","print","population","habitat")
head(data)

levels(data$Class)=c("Edible","Poissonous")
levels(data$odor) <- c("Almonds","Anise","Creosote","Fishy","Foul","Musty","None","Pungent","Spicy")
levels(data$print) <- c("Black","Brown","Buff","Chocolate","Green","Orange","Purple","White","Yellow")

install.packages("rpart")
install.packages("rpart.plot")
library(rpart)
library(rpart.plot)
#creating decision tree
myDecisionTree = rpart(Class ~ ., data = data, method = "class")
myDecisionTree

#plotting decision tree
rpart.plot(myDecisionTree, type = 3, extra = 2, under = TRUE, faclen=5, cex = .75)
newcase=data[10,-1]
predict(myDecisionTree,newcase,type="class")


#Accuracy of model
n=nrow(data)
n
smp_size=floor(0.75*n)
set.seed(123)
library(base)
train_ind=sample(c(1:n),size=smp_size)
mushrooms_train = data[train_ind, ]
mushrooms_test = data[-train_ind, ]
newDT= rpart(Class ~ ., data = mushrooms_train, method = "class")
result= predict(newDT, mushrooms_test[,-1], type = "class")
head(result)
