

#################### Filter Based Feature Selection Wrappers ############################
mRMR_selector <- function(dataframe, validator, feature_count, label){
    dd <- mRMR.data(data=dataframe[,!names(dataframe) %in% c(label)])
    result <- mRMR.classic(data=dd, target_indices=length(dataframe)-1, feature_count=feature_count)

    return(result@feature_names[unname(unlist(solutions(result)))])
}
MIM_selector <- function(dataframe, validator, feature_count, label){
    dd <- mRMR.data(data=dataframe[,!names(dataframe) %in% c(label)])
    result <- mRMR.classic(data=dd, target_indices=length(dataframe)-1, feature_count=feature_count)

    return(result@feature_names[unname(unlist(solutions(result)))])
}

JMI_selector <- function(dataframe, validator, feature_count, label){
    dd <- mRMR.data(data=dataframe[,!names(dataframe) %in% c(label)])
    result <- mRMR.classic(data=dd, target_indices=length(dataframe)-1, feature_count=feature_count)

    return(result@feature_names[unname(unlist(solutions(result)))])
}
# Note that explicit factor variable name usage in formula expression should be replaced by as.simple.formula

cfs_selector <- function(dataframe, validator, feature_count, label) {
    # note that cfs function on FSelector package use best search internally
	return(cfs(as.simple.formula(names(dataframe), label), dataframe))
}

relief_selector <- function(dataframe, validator, feature_count, label){
    	importance <- relief(as.simple.formula(names(dataframe), label), dataframe)$attr_importance
	or <- names(dataframe)[order(importance)]
	return(or[or != label])
}

consistency_selector <- function(dataframe, validator, feature_count, label){
    # note that consistency function on FSelector package use best search internally
    	
	return(consistency(as.simple.formula(names(dataframe), label), dataframe))
}

gfs_selector <- function(dataframe, feature_count) {
    # Generalized Fisher Score based feature selection

    
}
################################################################################


################### Wrapper Based Feature Selection ############################


backward_selector <- function(dataframe, validator, feature_count, label){
    print(paste("Backward Selector Executed", model))
    caretFuncs$summary <- multiClassSummary
    rfe_ctrl <- rfeControl(functions=caretFuncs,
                           method=options$cv,
                           repeats=options$cv_repeats,
                           number=options$cv_folds,
                           saveDetails=TRUE,
                           verbose=TRUE,
                           allowParallel=TRUE)
    
    result <- rfe(
                   dataframe[,!names(train) %in% c(label)],
                   dataframe[,label],
                   rfeControl=rfe_ctrl,
                   trControl=validator,
                   method=classification_models[[model]])

    return(result)
}

forward_selector <- function(dataframe, validator, feature_count, label){
    print(paste("Backward Selector Executed", model))
    caretFuncs$summary <- multiClassSummary
    rfe_ctrl <- rfeControl(functions=caretFuncs,
                           method=options$cv,
                           repeats=options$cv_repeats,
                           number=options$cv_folds,
                           saveDetails=TRUE,
                           verbose=TRUE,
                           allowParallel=TRUE)
    
    result <- rfe(
                   dataframe[,!names(train) %in% c(label)],
                   dataframe[,label],
                   rfeControl=rfe_ctrl,
                   trControl=validator,
                   method=classification_models[[model]])

    return(result)
}

genetic_selector <- function(dataframe, validator, feature_count, label){

    # customize caret GA function.
    caretGA$fitness_extern <- multiClassSummary
    ga_ctrl <- gafsControl(functions=caretGA,
                           method=options$cv,
                           repeats=options$cv_repeats,
                           number=options$cv_folds,
                           genParallel=TRUE,
                           verbose=TRUE,
                           allowParallel=TRUE)
    
    result <- gafs(
                   dataframe[,!names(dataframe) %in% label],
                   dataframe[,label],
                   iters=20,
                   popSize=30,
                   gafsControl=ga_ctrl,
                   trControl=validator,
                   method=classification_models[[model]])

    return(result)
}

simulated_selector <- function(dataframe, validator, feature_count, label) {
    caretSA$fitness_extern <- multiClassSummary
    sa_ctrl <- safsControl(functions=caretSA,
                           method=options$cv,
                           repeats=options$cv_repeats,
                           number=options$cv_folds,
                           verbose=TRUE,
                           allowParallel=TRUE)
    
    result <- safs(
                   dataframe[,!names(train) %in% c(label)],
                   dataframe[,label],
                   iters=30,
                   safsControl=sa_ctrl,
                   trControl=validator,
                   method=classification_models[[model]])

    return(result)     
}


################################################################################



