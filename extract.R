#!/usr/bin/env Rscript
# author : paganinist@gmail.com

set.seed(777) # For Reproducibility

dependencies <- c("MLmetrics", "kknn", "RRF", "getopt", "session", "reshape2", "ggplot2","ROCR", "rpart","DMwR", "mice", "plyr", "progress", "yaImpute", "log4r", "namespace", "import", "mboost", "doMC", "e1071", "caret", "mRMRe", "plotly", "R.matlab", "pROC", "devtools", "testthat", "roxygen2")
base_dependencies <- ("session")

# Install required dependencies automaticaaly
for(dependency in dependencies){

    if(!require(dependency, character.only=TRUE)){
        # work well on R version 3.2.4-1trusty
	# maually install gfortran
	# JDK8 required for FSelector and default-java symbolic link should be set on    
        install.packages(dependency, repos='http://cran.us.r-project.org')
        library(dependency, character.only=TRUE)
    }
}

as.simple.formula <- function(attributes, class) {
	return(as.formula(paste(class, paste(attributes, sep = "", collapse = " + "), sep = " ~ ")))
}

#python.load("visualize.py")

# Import Customized Local CRAN Library
load_all("./optparse")

# Libraries that included in R natively prompts error on install.packages function
for(base_dependency in base_dependencies){
	library(base_dependency, character.only=TRUE)
}

# Import Local Library
source("constants.R")
source("preprocessing.R")
source("fs.R")
source("arguments.R")
source("visualize.R")

classification_models <- list("RRF","rf", "xgbTree", "cforest", "adaboost", "kknn", "gamboost","glmboost","treebag","svmRadial","svmLinear")
regresion_models <- c() # not yet...
filter_selectors <- list("mRMR"=mRMR_selector, "relief"=relief_selector, "consistency"=consistency_selector,"cfs"=cfs_selector) 
wrapper_selectors <- list("backward"=backward_selector, "forward_selector"=forward_selector, "genetic_selector"=genetic_selector, "simulated_selector"=simulated_selector)

iofss <- c("cfs", "consistency") # internally optimized feature selectors


# Parallelization
registerDoMC(cores=4)

######### Matlab related variables & statements ########

# To Execute MATLAB Related Classifiers & Metric Evaluation,
# You need to require install MATLAB on host machine at least R2014a

if(FALSE){

    # Tested on MATLAB version 'R2014a'
    # ToolBox Dependencies
    # 1.FSLib_v4.2_2017


    matlab <- Matlab()
    try(matlab$startServer())
    
    isOpen <- open(matlab)
    print(isOpen)
    setVariable(matlab, train=train)

    print("setVariable executed")
    print(getVariable(matlab, 'train'))
}

########################################################
hello <- function(kk){
    print("j")

    return(kk)
}

option_list = list(


        # Dataset Options
	make_option(c("--train"), type="character", default=NULL, metavar="train", #action="callback", callback=function(x) x,
		    help="Data to be trained by ML model(should align feature to vertical)"),
	make_option(c("--test"), type="character", default=NULL, 
		    help="Data to be use as external validation set. Filename is Multiple test sets are allowed. "),
	make_option(c("--response"), type="character", default=NULL,
		    help="Column to be target of analysis. if header is TRUE, then it should be header name. otherwise, position of variable (starts with 1) is required. If -1 is entered, then last column is automatically selected. "), # ML models that requires multiple response variable like 'association rule learning' is not considered on current circumstances.
	make_option(c("--categorical"), type="character", default="list()", 
		    help="Feature names, or numbers to be considered as categorical variable"),
	make_option(c("--na"), type="character", default="omit", 
		    help="How missing values are handled, Possible choices are : omit, mice, knn, tree"),
	make_option(c("--header"), type="logical", default=FALSE,
		    help="Existence of header on train, test files"),

        # Parallelization Options
	make_option(c("--cores"), type="integer", default=2,
		    help="all cores are automatically occupied when -1 is entered"),


        # Analysis Options
	make_option(c("--feature_selectors"), type="character", default="list('mRMR')", 
		    help=paste("Possible Feature Selectors are as follows :",paste(names(wrapper_selectors), collapse=", "), paste(names(filter_selectors), collapse=", "))),
	make_option(c("--models"), type="character", default="list('kknn', 'treebag')", 
		    help=paste("Possible Models are as follows :",paste(names(wrapper_selectors), collapse=", "), paste(names(filter_selectors), collapse=", "))),

        # Validation Options
 	make_option(c("--metric"), type="character", default="roc", help=""),    
	make_option(c("--cv"), type="character", default="cv", help=""),
	make_option(c("--cv_folds"), type="character", default="10",
		    help="number of folds in k-fold-cross validation"), 
	make_option(c("--cv_repeats"), type="character", default="1",
		    help="number of repeats in repeated k-fold-cross validation. If --cv is not repeatedcv, then this option will be ignored."), 

        # Logging Options
	make_option(c("--experiment_name"), type="character", default="",
		    help="name that to be stored under log directory"),
	make_option(c("--log_level"), type="character", default="INFO",
		    help=paste(LOG_LEVELS, collapse=", ")),  
	make_option(c("--log_path"), type="character", default="./",
		    help=paste(LOG_LEVELS, collapse=", "))
		   )

optparser <- OptionParser(option_list=option_list)
options <- parse_args(optparser)

########################### Check Option Validity ###############################

options$categorical <- comma_seperated2R_string(options$categorical)
options$models <- comma_seperated2R_string(options$models)
options$feature_selectors <- comma_seperated2R_string(options$feature_selectors)

if(!is.null(options$test)){
    options$test <- comma_seperated2R_string(options$test, mode="c")
}

if(is.null(options$train) || is.null(options$response)){
	print_help(optparser)
        stop("Please Read Help")
}

if(options$experiment_name == "")
{
    options$experiment_name <- paste(tail(unlist(strsplit(options$train, "[/]")), n=1), format(Sys.time(), "%Y%m%d_%X"), sep="")
    print(paste("Because experiment_name option is empty, it is automatically set to : ", options$experiment_name))
}

#################################################################################

############################ Create Log Directories ##############################
ROOT_LOG_DIR <- options$log_path 

if(!file.exists(ROOT_LOG_DIR)){
	dir.create(file.path(ROOT_LOG_DIR))	
}

if(!file.exists(file.path(ROOT_LOG_DIR, options$experiment_name))){
	dir.create(file.path(ROOT_LOG_DIR, options$experiment_name))	
}

ROOT_LOG_DIR <- file.path(ROOT_LOG_DIR, options$experiment_name)

for(child_log_dir in CHILD_LOG_DIR){
	if(!file.exists(file.path(ROOT_LOG_DIR, child_log_dir))){
		dir.create(file.path(ROOT_LOG_DIR, child_log_dir))
	}
}


logger <- create.logger()
logfile(logger) <- file.path(ROOT_LOG_DIR, 'execution.log')
level(logger) <- options$log_level 
info(logger, options)



##################################################################################

train <- read.csv(options$train, header=options$header, na.strings=c("NA", ""))
train_size <- length(train)
tests<- list()
######################## MISSING VALUE HANDLING #################################

if(options$na == "omit"){
    train <- na.omit(train)
} else if(options$na=="mice"){
    train <- mice(train) 
} else if(options$na=="knn"){
    train <- knnImputation(train)
} else if(options$na=="tree"){
    #stop()
}else {

}



# Multiple Testset Handling
if(FALSE){

if(iterable(options$test)){
	temp_tests <- eval(parse(text=options$test))
	for(test in temp_tests)
	{
		tests[[test]] <- read.csv(test, header=options$header, na.strings=c("NA", ""))
	}
}else{
	tests[[options$test]] <- read.csv(options$test, header=options$header, na.strings=c("NA", ""))
}
}
# Check the variable number equivalance between training and test set.
for(test in names(tests)){
	if(length(tests[[test]]) != train_size){
		stop(paste(tests[[test]], "has differenent number of features compared to the training set"))
	}
}

# Convert label variable into factor type 
# Fix all variables into numeric type 


checkInclusion <- function(target, iterable_object){

	if(!(target %in% iterable_object)){
		return(NULL)
	}

	return(target)
}

categorical_variables <- list()
if(iterable(options$categorical)){

	categorical_variables <- eval(parse(text=options$categorical))

	for(categorical_variable in categorical_variables){
		train[, categorical_variable] <- as.factor(train[, categorical_variable])
	}
	train[!names(train) %in% categorical_variables] <- lapply(train[!names(train) %in% categorical_variables], function(x) as.numeric(as.character(x)))
	train <- as.data.frame(train)
	print(rapply(train, function(x) class(x)))
}


for(test in names(tests)){
	print(test)
	for(categorical_variable in categorical_variables){
		tests[[test]][[categorical_variable]] <- as.factor(tests[[test]][[categorical_variable]])
	}
	#tests[[test]][!names(tests[[test]]) %in% categorical_variables] <- lapply(tests[[test]][!names(tests[[test]]) %in% categorical_variables], function(x) as.numeric(as.character(x)))
	#tests[[test]] <- as.data.frame(tests[[test]])
}


# Parse Models From Option
if(iterable(options$models)){

	included_models <- eval(parse(text=options$models))
	classification_models <- rapply(as.list(classification_models), checkInclusion, iterable_object=included_models)

}

# Feature Selction/Reduction Inclusion check
if(iterable(options$feature_selectors)){
	included_feature_selectors <- eval(parse(text=options$feature_selectors))

	for(wrapper_selector in names(wrapper_selectors)){
		if(!wrapper_selector %in% included_feature_selectors){
			wrapper_selectors[[wrapper_selector]] <- NULL
		}
	}
	print(wrapper_selectors)
	for(filter_selector in names(filter_selectors)){
		if(!filter_selector %in% included_feature_selectors){
			filter_selectors[[filter_selector]] <- NULL
		}
	}
}
selectors <- c(wrapper_selectors, filter_selectors)

getEmptyPerformanceMatrix <- function(models, selectors){
    tframe <- matrix(ncol=length(selectors), nrow=length(models))
    rownames(tframe) <- models
    colnames(tframe) <- selectors
    return(tframe)
}

# By Reference, 
# [1] On Estimating Model Accuracy with Repeated K-Fold Cross-Validation
# Repeated CV does not affect on accurate model selection, but increase computational cost. 

# Question?
# 1. There is no description of stratification on validation sequence on paper.
# 2. Apply the Repeated CV

# Stratified Repeated K-Fold Cross Validation
#stratified_folds_indexes = createFolds(train, k=5, list=TRUE, returnTrain=TRUE)
validator = trainControl(
                        method=options$cv,
                        allowParallel=TRUE,
			classProbs=TRUE,
                        savePredictions=TRUE,
                        summaryFunction=multiClassSummary,
                        number=as.integer(options$cv_folds),
                       	repeats=options$cv_repeats,
                        )


results <- c()         # Store Evaluation Metric score of finalModels
filtered_indexes <- list()

#Automatical leveling
levels(train$V1) <- list(no="1", yes="2", middle="3")

selector_names <- names(selectors)
roc_matrix <- getEmptyPerformanceMatrix(classification_models, selector_names)
acc_matrix <- getEmptyPerformanceMatrix(classification_models, selector_names)
spe_matrix <- getEmptyPerformanceMatrix(classification_models, selector_names)
sen_matrix <- getEmptyPerformanceMatrix(classification_models, selector_names)
# Choose Classification Model 
for(model in classification_models)
{

    results[[model]] <- list()

    # Choose Filter Selector
print(names(selectors))
    for(filter_selector in c(names(selectors)))
    {
	print(filter_selector)
        info(logger, paste("Current Combination is : (", model, filter_selector, ")"))
	indexes <- c()

        # Check current filter is already evaluated for given dataset
    	if(filter_selector %in% names(filtered_indexes))
	{
	    indexes <- filtered_indexes[[filter_selector]]
	}
	else
	{
            indexes <- selectors[[filter_selector]](train, length(train) - 1, options$response)
	    filtered_indexes[[filter_selector]] <- indexes
	}

        results[[model]][[filter_selector]] <- list()
        tmp_metrics <- list()
        tmp_results <- list()      


	if(!filter_selector %in% names(wrapper_selectors)){
		print("hello?")
        	selected_train <- train[unlist(indexes)]
		# Choose optimal number of feature in filter selector
		for(num_feature in seq(5, as.integer(length(train) * 3/4), 5))
		{
		    # If filter selector does not use internal optimization, then 
		    if(!filter_selector %in% iofss)
		    {
			selected_train <- train[unlist(indexes[1:num_feature])]
		    }

		    info(logger, paste("Training model with", model, "and", num_feature, "features."))

		    result <-train(
				    as.simple.formula(names(selected_train), options$response),
				    data=train,
				    method=model,
				    trControl=validator)
        	    print(names(result$results))
		    max_tune_index <- which.max(result$results[ , "AUC"])
	  	    print(max_tune_index)
		    tmp_metrics <- c(tmp_metrics, result$results[max_tune_index, "AUC"])
		    tmp_results[[length(tmp_results) + 1]] <- result


		    if (filter_selector %in% iofss){
			# if filter selector uses internal optimized feature subset search algorithm,
			# ends loop directly.
			break    
		    }

		}
	}
	else{
		tmp_metrics <- c(1)
		tmp_results[[1]] <- indexes	
	}

        # Choose Max Index Using 
        max_index = which.max(tmp_metrics)

        max_result = tmp_results[[max_index]]
        max_data = names(max_result$trainingData)
        max_data = max_data[max_data != ".outcome"]
        print(max_result$results)
        print(names(max_result))


	max_tune_index <- which.max(max_result$results[ , "AUC"])
	print(max_tune_index)
        roc_matrix[model, filter_selector] <- max_result$results[max_tune_index, "AUC"]
        acc_matrix[model, filter_selector] <- max_result$results[max_tune_index, "Accuracy"]
        sen_matrix[model, filter_selector] <- max_result$results[max_tune_index, "Mean_Sensitivity"]
        spe_matrix[model, filter_selector] <- max_result$results[max_tune_index, "Mean_Specificity"]
        print(roc_matrix)

        pdf(file=file.path(ROOT_LOG_DIR, PLOT_DIR, paste(model, "_", filter_selector,".pdf",sep="")), width=8, height=8)
        plot(max_result)
        dev.off()

        roc_matrix.melted <- melt(roc_matrix)
        ggplot(data=roc_matrix.melted, aes(x=Var2, y=Var1, fill=value)) + geom_tile() + geom_text(aes(x=Var2, y=Var1, label=value), color="black", size=4)
        ggsave(file.path(ROOT_LOG_DIR, "roc_matrix.png"))

        write.table(roc_matrix, file.path(ROOT_LOG_DIR, METRIC_DIR, "roc"), sep=",")
        write.table(acc_matrix, file.path(ROOT_LOG_DIR, METRIC_DIR, "acc"), sep=",")
        write.table(sen_matrix, file.path(ROOT_LOG_DIR, METRIC_DIR, "sen"), sep=",")
        write.table(spe_matrix, file.path(ROOT_LOG_DIR, METRIC_DIR, "spe"), sep=",")

        results[[model]][[filter_selector]] <- list()
        #results[[model]][[filter_selector]][["Accuracy"]] <- max(tmp_metrics)
        results[[model]][[filter_selector]][["Features"]] <- max_data
        results[[model]][[filter_selector]][["FinalModel"]] <- max_result 

	write(max_data, file=file.path(ROOT_LOG_DIR, FEATURE_DIR, paste(model, filter_selector)))
	save.session(file=file.path(ROOT_LOG_DIR, SESSION_DIR, "feature_selector.RSession"))

        for(test in names(tests)){
            results[[model]][[filter_selector]][[test]] <- list()
            results[[model]][[filter_selector]][[test]][["Accuracy"]] <- confusionMatrix(table(unlist(predict(result, newdata=tests[[test]])), unlist(tests[[test]][options$response])))
            results[[model]][[filter_selector]][[test]][["ROC"]] <- auc(predict(result,newdata=tests[[test]]), as.numeric(unlist(tests[[test]][options$response])))

        }
    }

    if(FALSE){    
    print(paste("Start Wrapper Feature Selector Procedures with", paste(wrapper_selectors, collapse=" ")))
    for(wrapper_selector in names(wrapper_selectors))
    {

        selected_features <- wrapper_selectors[[wrapper_selector]](train, model, validator, options$response)
	print(selected_features)    
        print(paste("Training model with", model, "and", length(selected_features), "features."))

        result <-train(
                        train[,selected_features], 
                        train[,options$response],
                        metric="Accuracy",
                        method=model,
                        trControl=validator)

        results[[model]][[wrapper_selector]] <- list()
        results[[model]][[wrapper_selector]][["Accuracy"]] <- max(result$results$Accuracy)
        results[[model]][[wrapper_selector]][["Features"]] <- names(selected_features)
        results[[model]][[wrapper_selector]][["FinalModel"]] <- result
        print(paste(model,wrapper_selector, results[[model]][[wrapper_selector]][["Accuracy"]]))
	write(names(selected_features), file=file.path(ROOT_LOG_DIR, "feature", paste(model, wrapper_selector)))
	save.session(file=file.path(ROOT_LOG_DIR, "session", "feature_selector.RSession"))

    }
}
}

##################################### FINAL PERFORMANCE SUMMARY ################################################



################################################################################################################

