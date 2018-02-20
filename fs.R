#################### Filter Based Feature Selection ############################
mRMR_selector <- function(dataframe, feature_count, label){
    dd <- mRMR.data(data=dataframe[,!names(dataframe) %in% c(label)])
    result <- mRMR.classic(data=dd, target_indices=length(dataframe)-1, feature_count=feature_count)

    return(unname(unlist(solutions(result))))
}

# Note that explicit factor variable name usage in formula expression should be replaced by as.simple.formula

cfs_selector <- function(dataframe, feature_count, label) {
    # note that cfs function on FSelector package use best search internally
	return(cfs(as.simple.formula(names(dataframe), label), dataframe))
}

relief_selector <- function(dataframe, feature_count, label){
    	importance <- relief(as.simple.formula(names(dataframe), label), dataframe)$attr_importance
	or <- names(dataframe)[order(importance)]
	return(or[or != label])
}

consistency_selector <- function(dataframe, feature_count, label){
    # note that consistency function on FSelector package use best search internally
    	
	return(consistency(as.simple.formula(names(dataframe), label), dataframe))
}

gfs_selector <- function(dataframe, feature_count) {
    # Generalized Fisher Score based feature selection

    
}
################################################################################


################### Wrapper Based Feature Selection ############################


backward_selector <- function(dataframe, feature_count, label){
    print(paste("Backward Selector Executed", model))
    rfFuncs$summary <- multiClassSummary
    rfe_ctrl <- rfeControl(functions=rfFuncs,
                           method="repeatedcv",
                           repeats=1,
                           number=3,

                           verbose=TRUE,
                           allowParallel=TRUE)
    
    result <- rfe(
                   dataframe[,!names(train) %in% c(label)],
                   dataframe[,label],
                   iters=2,
                   rfeControl=rfe_ctrl,
                   method=model)

    return(result)
}

forward_selector <- function(dataframe, feature_count, label){
    print(paste("Forward Selector Executed", model))
    evaluator<-function(subset){
        result_ <- train(train[,subset], train[, label],metric="Accuracy",method=model,trControl=validator)        

        return(mean(result_$results$Accuracy))

    }
    features <- names(dataframe)
    return(forward.search(features[features != label], evaluator))

}

genetic_selector <- function(dataframe, feature_count, label){
    print("Genetic")    
    # customize caret GA function.
    ga_ctrl <- gafsControl(functions=caretGA,
                           method="repeatedcv",
                           repeats=1,
                           number=5,
                           genParallel=TRUE,
                           verbose=TRUE,
                           allowParallel=TRUE)
    
    result <- gafs(
                   dataframe[,!names(dataframe) %in% label],
                   dataframe[,label],
                   iters=20,
                   popSize=30,
                   gafsControl=ga_ctrl,
                   method=model)
    print(result)
    return(result)
}

simulated_selector <- function(dataframe, feature_count, label) {
    print("Simulated Annealing") 
    sa_ctrl <- safsControl(functions=caretSA,
                           method="repeatedcv",
                           repeats=3,
                           number=5,

		  	   fitness_extern=multiClassSummary,
                           verbose=TRUE,
                           allowParallel=TRUE)
    
    result <- safs(
                   dataframe[,!names(train) %in% c(label)],
                   dataframe[,label],
                   iters=30,
                   safsControl=sa_ctrl,
                   method=model)

    return(result)     
}


################################################################################



