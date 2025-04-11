splt <- function(df,ratio){
  library(dplyr)
  library(caret)
  glimpse(df)
  if(ratio>1){
    ratio=ratio/100
  }
  n <- nrow(df)
  id <- sample(1:n,size = ratio*n)
  #Split Data
  train <- df[id,]
  test <- df[-id,]
  return(list(train=train,test=test))
}
prep_data <- splt(mtcars,0.8)
knn <- train(mpg~.,
             data=prep_data$train,
             method="knn",
             trControl=trainControl("repeatedcv",repeats = 5,5,verboseIter = T),
             matric="RMSE",
             tuneLength=2
             )

