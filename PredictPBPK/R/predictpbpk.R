#' predict makes a PredictionResponse for Jaqpot
#' @param dataset
#' @param rawModel
#' @param additionalInfo

predictpbpk <- function(dataset, rawModel, additionalInfo){

  n_comp <- length(additionalInfo$predictedFeatures) - 1
  feats <- colnames(dataset$dataEntry[,2])
  # feats <- colnames(dataset$dataEntry)
  rows_data <- length(dataset$dataEntry$values[,2])
  df <- data.frame(matrix(0, ncol = 0, nrow = rows_data))
  for(i in feats){
    fe <- additionalInfo$independentFeatures[i][[i]]
    feval <- dataset$dataEntry$values[i][,1]
    df[fe] <- feval
  }

  mod <- unserialize(base64_dec(rawModel))
  covmodel <- mod$COVMODEL
  odemodel <- mod$ODEMODEL

  # predFeat <- additionalInfo$predictedFeatures[1][[1]]
  predFeat <- additionalInfo$predictedFeatures

  initial_concentration = rep(0, n_comp)
  for(i in 1:length(initial_concentration)){
    con = paste("y", i, sep="")
    initial_concentration[i] = df[[con]]
  }
  weight = df$weight
  gender <- df$gender
  t_inf <- df$infusion_time
  dose <- df$dose

  params<-c(covmodel(weight,gender), t_inf, dose)
  sample_time <- c(0, 5/60, 0.25, 0.5, 0.75, 1, 1.5, 2, 3, 4, 6, 8, 10, 12, 24, 36, 48, 72) # in hours
  solution <- ode(y = initial_concentration, times = sample_time, func = odemodel, parms = params)

  comp_names <- rep(0, n_comp + 1)
  comp_short <- rep(0, n_comp + 1)
  pred_names <- rep(0, n_comp + 1)
  for(i in 1:length(comp_names)){
    comp_names[i] <- additionalInfo$predictedFeatures[[i]]
    comp_short[i] <- as.integer(strsplit(additionalInfo$predictedFeatures[[i]], "_")[[1]])
    pred_names[i] <- names(additionalInfo$predictedFeatures)[i]
  }
  comp_all <- data.frame(cbind(comp_names, comp_short, pred_names))
  comp_all_s <- comp_all[order(comp_short),][,1]



  for(i in 1:dim(solution)[1]){
    prediction<- data.frame(t(solution[i,]))
    colnames(prediction)<- comp_all_s
    if(i==1){lh_preds<- list(unbox(prediction))
    }else{
      lh_preds[[i]]<- unbox(prediction)
    }
  }
  datpred <-list(predictions=lh_preds)

  return(datpred)
}
