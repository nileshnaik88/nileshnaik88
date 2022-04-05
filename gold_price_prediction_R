#importing the dataset
gld_price_data <- read.csv("C:/Users/Amra/Desktop/gld_price_data.csv")
View(gld_price_data)
#creating subset
X<- subset(gld_price_data, select = -c(Date) )
View(X)
#calling randomforest library
library(randomForest)



#splitting training & testing dataset
X_set_size = floor(nrow(X) *0.80)
index <- sample(1:nrow(X), size = X_set_size)
training <-X[index,]
testing <-X[-index,]

#run1########################################################################
#applying regressor to dataset
rf <- randomForest(GLD ~ ., data = training, mtry= 4, ntree= 2001, importance= TRUE)
rf
plot(rf)
#run1 over######################################################################

#run2#########################################################################
#result variable for applying rf/ random forest to specified rows
res <- data.frame(testing$GLD, predict(rf, testing[1:458,], type= "response"))
#plots the difference between actual and predicted value
plot(res)
# accuracy about 97-98.5% #
#run2 over#####################################################################

#run3 break runs###############################################################
#taking input from user
v1 = readline("Enter SPX:")
v2 = readline("Enter USO:")
v3 = readline("Enter SLV:")
v4 = readline("Enter EUR.USD:")
#run3 over ####################################################################

#run4##########################################################################
#converting them into integers
v1 = as.integer(v1)
v2 = as.integer(v2)
v3 = as.integer(v3)
v4 = as.integer(v4)
#converting inputs into values for new dataframe
SPX<- c(v1)
USO<- c(v2)
SLV<- c(v3)
EUR.USD<- c(v4)
#creating new dataframe using users entered inputs
df <- data.frame(SPX, USO, SLV, EUR.USD)
print(df)
#using our declared prediction model to predict output of users entered values
p1 <- predict(rf, df)
#printing out the end result
print(p1)
#run4 over######################################################################
#phew project over :D ###########################################################
#plotting graph for 530 recent components

X_40 <- X[1760:2290,]


SPX <- c(X_40 $SPX)
USO <- c(X_40 $USO)
SLV <- c(X_40 $SLV)
EUR.USD <- c(X_40 $EUR.USD)
GLD <- c(X_40 $GLD)
library(ggplot2)
#GLD and SPX
sf <- as.data.frame(cbind(SPX, GLD))
ggplot(sf, aes(x=SPX, y=GLD))+geom_line()
#GLD and USO
sf1 <- as.data.frame(cbind(USO, GLD))
ggplot(sf1, aes(x=USO, y=GLD))+geom_line()
#GLD and SLV
sf2 <- as.data.frame(cbind(SLV, GLD))
ggplot(sf2, aes(x=SLV, y=GLD))+geom_line()
#GLD and EUR.USD
sf3 <- as.data.frame(cbind(EUR.USD, GLD))
ggplot(sf3, aes(x=EUR.USD, y=GLD))+geom_line()




