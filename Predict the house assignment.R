

# We first load the training and test set and remove the ID. We then proceeded to transform 
# the price to log price. We also added a dummy price to the test set so that the structure of both data frames are the same. 
data1=setwd("~/Desktop/train.csv")
df1a=data1
df1a$Id <- NULL
lgprice = log(df1a$price)
df1 = cbind.data.frame(lgprice, df1a[,-1])
df1[!complete.cases(df1),]

data2=setwd("~/Desktop/test.csv")
#create dummy price
l = length(data2[,1])
price = rep(1,l)
data2a = cbind.data.frame(price, data2)
data2a$Id<-NULL
df2=data2a
df2[!complete.cases(df2),]

# visualize the data.
library(ggplot2)
library(dplyr, warn.conflicts = FALSE)
df1 %>% 
  ggplot() + geom_point(aes(x=x, y=y, colour=lgprice/living_area),alpha=0.3) +
  scale_colour_gradient(low = "blue", high="red")

df1 %>%                                       
  ggplot(aes(x=factor(bedrooms), y=living_area)) +                   
  geom_boxplot() 

library("hexbin")
df1 %>%
ggplot(aes(x=living_area,y=lgprice)) +
stat_binhex(bins=50)



# Now since the data is multidimensional but we can only visualize it in at most 3D, let us apply some methods to reduce the dimensionality of the data.\\
# In particular, we will use T-SNE (t-distributed stochastic neighbor embedding)

library(Rtsne)
tsne <- Rtsne( select(df1, -lgprice), dims = 2, perplexity=30, max_iter=700, verbose=TRUE)

df3 = data.frame(df1$lgprice, tsne$Y)
df3 %>% 
  ggplot() + geom_point(aes(x=X1, y=X2, colour=df1.lgprice),alpha=0.3) +
  scale_colour_gradient(low = "blue", high="red")



# From the plots, we can see that there is indeed some structure in the data that we can use to predict the (log)prices.
# train our dataset.

library(randomForest)
lgprice.rf= randomForest( lgprice~., data=df1, mtry=7, ntree=1000, importance=FALSE)



# Now let us predict the prices.

lgprice.rf.pred = predict( lgprice.rf ,
                           newdata = df2[ , -1], type="response")


price.rf.pred = exp(lgprice.rf.pred)


# use the model to predict the prices on the training set.
lgprice.rf.pred_train = predict( lgprice.rf ,
                           newdata = df1[ , -1], type="response")
price.rf.pred_train= exp(lgprice.rf.pred_train)
plot(df1a[,"price"], price.rf.pred_train,
main="Prediction Training Set")


Now let us create the submission file.
submission_random_forest = data.frame(Id = data2[,"Id"],
                                      Prediction = price.rf.pred)
write.table(x = submission_random_forest,
            file = "random_forest_prediction_mtry7.csv",
            sep = ",", row.names = FALSE, col.names = TRUE)

# The RMSPE obtained is 0.31532.
