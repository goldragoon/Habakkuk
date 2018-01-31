#!/usr/bin/env Rscript
# author : paganinist@gmail.com

set.seed(777) # For Reproducibility



dependencies <- c("namespace", "import", "mboost", "doMC", "e1071", "caret", "mRMRe", "plotly", "R.matlab", "pROC", "FSelector", "optparse")
base_dependencies <- ("session")

# Install required dependencies automaticaaly
for(dependency in dependencies){
    if(!require(dependency, character.only=TRUE)){
        # work well on R version 3.2.4-1trusty
	# maually install gfortran
	# JDK8 required for FSelector and default-java symbolic link should be set on    
        install.packages(dependency)
        library(dependency, character.only=TRUE)
    }
}

# Libraries that included in R natively prompts error on install.packages function
for(base_dependency in base_dependencies){
	library(base_dependency, character.only=TRUE)
}

source("constants.R")
source("preprocessing.R")
source("fs.R")
source("arguments.R")
#source("explanatory_visualization.R")

# Check Why NULL
print(preprocessing:::gam)
print(preprocessing:::glm)


classification_models <- list("RRF","rf", "xgbTree", "cforest", "adaboost", "kknn", "gamboost","glmboost","treebag","svmRadial","svmLinear")
regresion_models <- c() # not yet...
filter_selectors <- list("mRMR"=mRMR_selector, "relief"=relief_selector, "consistency"=consistency_selector,"cfs"=cfs_selector) 
wrapper_selectors <- list("backward"=backward_selector, "forward_selector"=forward_selector, "genetic_selector"=genetic_selector, "simulated_selector"=simulated_selector)
iofss <- c("cfs", "consistency") # internally optimized feature selectors


#stop("Debugging Purpose Temporary Program Halt");

# Parallelization
registerDoMC(cores=16)

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


option_list = list(

	make_option(c("--train"), type="character", default=NULL, metavar="train",
		    help="Data to be trained by ML model(should align feature to vertical)"),
	make_option(c("--test"), type="character", default=NULL, 
		    help="Data to be use as external validation set. Filename is Multiple test sets are allowed. "),
	# ML models that requires multiple response variable like association rule learning is not considered on current circumstances.
	make_option(c("--response"), type="character", default=NULL,
		    help="Column to be target of analysis. if header is TRUE, then it should be header name. otherwise, position of variable (starts with 1) is required. If -1 is entered, then last column is automatically selected. "),
	make_option(c("--categorical"), type="character", default="list()", 
		    help="Feature names, or numbers to be considered as categorical variable"),

	make_option(c("--header"), type="logical", default=FALSE,
		    help="Existence of header on train, test files"),
	make_option(c("--experiment_name"), type="character", default="default",
		    help="name that to be stored under log directory"),

	make_option(c("--cores"), type="integer", default=2,
		    help="all cores are automatically occupied when -1 is entered"),
	make_option(c("--separated_session_logging"), type="logical", default=FALSE,
		    help=""),

	make_option(c("--feature_selectors"), type="character", default="list('mRMR')", 
		    help=paste("Possible Feature Selectors are as follows :",paste(names(wrapper_selectors), collapse=", "), paste(names(filter_selectors), collapse=", "))),
	make_option(c("--models"), type="character", default="list('kknn', 'treebag')", 
		    help=paste("Possible Models are as follows :",paste(names(wrapper_selectors), collapse=", "), paste(names(filter_selectors), collapse=", "))),
	make_option(c("--validation"), type="character", default="cv", help=""),
	make_option(c("--folds"), type="character", default="10",
		    help="number of folds in k-fold-cross validation"), 

	make_option(c("--type"), type="character", default="classification",
		    help="classification/regression")	    
		   )

optparser <- OptionParser(option_list=option_list)
options <- parse_args(optparser)

if(is.null(options$train) || is.null(options$test) || is.null(options$response)){
	print_help(optparser)
}

############################ Create Log Directories ##############################
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

##################################################################################

train <- read.csv(options$train, header=options$header, na.strings=c("NA", ""))

train_size <- length(train)

tests <- list()
# Multiple Testset Handling
if(iterable(options$test)){
	temp_tests <- eval(parse(text=options$test))
	for(test in temp_tests)
	{
		tests[[test]] <- read.csv(test, header=options$header, na.strings=c("NA", ""))
	}
}else{
	tests[[options$test]] <- read.csv(options$test, header=options$header, na.strings=c("NA", ""))
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
if(FALSE){
for(test in names(tests)){
	print(test)
	for(categorical_variable in categorical_variables){
		tests[[test]][[categorical_variable]] <- as.factor(tests[[test]][[categorical_variable]])
	}
	#tests[[test]][!names(tests[[test]]) %in% categorical_variables] <- lapply(tests[[test]][!names(tests[[test]]) %in% categorical_variables], function(x) as.numeric(as.character(x)))
	#tests[[test]] <- as.data.frame(tests[[test]])
}
}

# Models
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

# By Reference, 
# [1] On Estimating Model Accuracy with Repeated K-Fold Cross-Validation
# [2] 
# Repeated CV does not affect on accurate model selection, but increase computational cost. 

# Question?
# 1. There is no description of stratification on validation sequence on paper.
# 2. Apply the Repeated CV

# For reproducibility
print(NROW(train))
# Stratified Repeated K-Fold Cross Validation
#stratified_folds_indexes = createFolds(train, k=5, list=TRUE, returnTrain=TRUE)
validator = trainControl(
                        #index=stratified_folds_indexes,
                        method=options$validation,
                        verbose=TRUE,
			#repeats=1,
                        allowParallel=TRUE,
			classProbs=FALSE,
                        number=5)


results <- c()         # Store Evaluation Metric score of finalModels
filtered_indexes <- list()
print(classification_models)


# Classification Model 
for(model in classification_models)
{

    print(model)
    results[[model]] <- list()
    print(filter_selectors)

    for(filter_selector in names(filter_selectors))
    {

        print(paste(filter_selector, "Start!!!"))
	indexes <- c()
	print(options$response)

	print(rapply(train, function(x) class(x)))
    	if(filter_selector %in% names(filtered_indexes))
	{

		indexes <- filtered_indexes[[filter_selector]]
	}
	else
	{

        	indexes <- filter_selectors[[filter_selector]](train, 936, options$response)
		filtered_indexes[[filter_selector]] <- indexes
	}
	#print(indexes)
        results[[model]][[filter_selector]] <- list()
        tmp_accuracies <- c()
        tmp_results <- list()       
        selected_train <- NULL

	

        for(num_feature in seq(5, as.integer(length(train) * 3/4), 50))
        {
            # erroneous behavior when 

            if(filter_selector %in% iofss)
            {
                selected_train <- train[unlist(indexes)]
            }
            else{
                selected_train <- train[unlist(indexes[1:num_feature])]
	    	print("Selected!!")
		print(names(selected_train))
		print(options$response)
		
            }
            print(paste("Training model with", model, "and", num_feature, "features."))

            result <-train(
			    as.simple.formula(names(selected_train), options$response),
			    data=train,
                            method=model,
			    verbose=TRUE,
                            #ntree=100,#0, # for 'rf' model
                            trControl=validator)

            tmp_accuracies <- c(tmp_accuracies, max(result$results$Accuracy))
            tmp_results[[length(tmp_results) + 1]] <- result

            if (filter_selector %in% iofss){
                # if filter selector uses internal optimized feature subset search algorithm,
                # ends loop directly.
                break    
            }

        }

        max_index = which.max(tmp_accuracies)
        max_result = tmp_results[[max_index]]
        max_data = names(max_result$trainingData)
        max_data = max_data[max_data != ".outcome"]
        print("tmp_result")
        print(max_index)
        print(max_result)
	print("############################### MAX_DATA ####################################")
        print(max_data)
	print("#############################################################################")

        results[[model]][[filter_selector]] <- list()
        results[[model]][[filter_selector]][["Accuracy"]] <- max(tmp_accuracies)
        results[[model]][[filter_selector]][["Features"]] <- max_data
        results[[model]][[filter_selector]][["FinalModel"]] <- max_result 

	write(max_data, file=file.path(ROOT_LOG_DIR, "feature", paste(model, filter_selector)))
	save.session(file=file.path(ROOT_LOG_DIR, "session", "feature_selector.RSession"))

        for(test in names(tests)){
            results[[model]][[filter_selector]][[test]] <- list()
	    print("############################### TEST_DATA ####################################")
	    print(names(tests[[test]])) 
	    print("##############################################################################")
            results[[model]][[filter_selector]][[test]][["Accuracy"]] <- confusionMatrix(table(unlist(predict(result, newdata=tests[[test]])), unlist(tests[[test]][options$response])))
            results[[model]][[filter_selector]][[test]][["ROC"]] <- auc(predict(result,newdata=tests[[test]]), as.numeric(unlist(tests[[test]][options$response])))
            print(confusionMatrix(table(unlist(predict(result, newdata=tests[[test]])), unlist(tests[[test]][options$response]) )))	
            print(auc(predict(result,newdata=tests[[test]]), as.numeric(unlist(tests[[test]][options$response]))))		
        }


    }

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

######################## prediction #################################
#####################################################################
