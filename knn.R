
source("tangent.R") # including tangent function
################################################## Functions ############################################################
tan.distance <- function(d1, d2){
  # returns tangent distance between two images
  # takes two arguments imageOne and imageTwo
  # d1 is train dataframe 
  # d2 is one test datapoint
  # example distance(d1, d2)
  disT <- 0
  for(i in 1:nrow(d1)){disT[i] <- distance(d1[i,], d2)}
  return(disT)
}

euc.distance <- function(d1, d2){
  # return euclidean distance
  # takes two argument
  # example distance(df1, df2)
  # d1 is train dataframe
  # d2 is a test datapoint
  # returns a vector with all the distance of a test data point from each train data point
  train.row <- nrow(d1)
  dis <- sapply(apply((apply(data.frame(matrix(rep(d2,each=train.row),nrow=train.row)),2,as.numeric) - d1)^2,1,sum),sqrt)
  return(dis)
}

voted.label <- function(label.list){
  # returns the most voated label
  # example voted.label(c(1,1,-1))
  return(as.numeric(names(sort(summary(as.factor(c(label.list))), decreasing=T)[1])))
}

knn <- function(df.train, df.test, label.col, k = 1, method = "euclidean"){
  # returns predicted label for all the test data points
  # takes 5 arguments
  # df.train is Train Data Frame
  # df.test is Test Data Frame
  # label.col is columne number for label column
  # k is for k nearest neighbours
  # method is for specifying the distane method to use
  # example knn(ionosphere.train, ionosphere.test, 35, 1, "euclidean")
  train <- df.train[,-label.col]
  test <- df.test[,-label.col]
  df.test[,ncol(df.test)+1] <- 0 
  df.train[, ncol(df.train)+1] <- 0
  for(i in 1:nrow(df.test)){
    if (method == "euclidean"){
      df.train[,ncol(df.train)] <- euc.distance(train, test[i,])
    } else if (method == "tangent"){
      # implement tangent distance
      df.train[,ncol(df.train)] <- tan.distance(train, test[i,])
    }
    # df.train[,ncol(df.train)] <- distance(train, test[i,])
    train.order <- df.train[order(df.train[,ncol(df.train)]),]
    df.test[i,ncol(df.test)] <- voted.label(train.order[1:k, ncol(df.train)-1])
  }
  return(df.test[,ncol(df.test)])
}

accuracy <- function(test.label, pred.label){
  # returns ratio of correct prediction and total test dataset
  # takes two arguments 
  # test.label is label for test data set
  # pred.label is label predicted by knn
  bool <- test.label == pred.label
  return(length(bool[bool == TRUE])/length(test.label))
}
######################################### ####################### ####################################################
