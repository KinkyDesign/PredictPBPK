#' predict makes a PredictionResponse for Jaqpot
#' @param dataset
#' @param rawModel
#' @param additionalInfo

predictgeneric <- function(dataset, rawModel, additionalInfo){
  feats <- colnames(dataset$dataEntry[,2])
  rows_data <- length(dataset$dataEntry$values[,2])
  df <- data.frame(matrix(0, ncol = 0, nrow = rows_data))
  for(i in feats){
    fe <- additionalInfo$independentFeatures[i][[i]]
    feval <- dataset$dataEntry$values[i][,1]
    df[fe] <- feval
  }
  mod <- unserialize(base64_dec(rawModel))
  model <- mod$MODEL
  predFeat <- additionalInfo$predictedFeatures[1][[1]]
  predictions <- predict(model, df)
  for(i in 1:length(predictions)){
    prediction<- data.frame(predictions[i])
    colnames(prediction)<- predFeat
    if(i==1){lh_preds<- list(unbox(prediction))
    }else{
      lh_preds[[i]]<- unbox(prediction)
    }
  }
  datpred <-list(predictions=lh_preds)
  return(datpred)
}
