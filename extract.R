#!/usr/bin/env Rscript
# author : paganinist@gmail.com

############################################################ Dependency Checker ###############################################################

options(error = quote({
  dump.frames(to.file=T, dumpto='last.dump')
  load('last.dump.rda')
  print(last.dump)
  q()
}))

# CRAN repository dependencies
dependencies <- c("gbm","plyr","caTools", "Matrix","gam","mgcv", "stringr", "mailR", "randomForest", "naivebayes", "mlrMBO", "MLmetrics", "kknn", "RRF", "getopt", "session", "reshape2", "ggplot2","ROCR", "rpart","DMwR", "mice", "plyr", "progress", "yaImpute", "log4r", "namespace", "import", "mboost", "doMC", "e1071", "caret", "plotly", "R.matlab", "pROC", "devtools", "testthat", "roxygen2", "mlrMBO")

# Native R dependencies
base_dependencies <- c("session") 

# Amended CRAN repository dependencies (located on current local directory)
amend_dependencies <- c("optparse", "ifs")

# Install required dependencies automaticaaly
for(dependency in dependencies){

    if(!require(dependency, character.only=TRUE)){
        # work well on R version 3.2.4-1trusty
	# maually install gfortran

        install.packages(dependency, repos='http://cran.us.r-project.org')
        library(dependency, character.only=TRUE)
    }
}

as.simple.formula <- function(attributes, class) {
	return(as.formula(paste(class, paste(attributes, sep = "", collapse = " + "), sep = " ~ ")))
}

get.data.frame.from.formula <- function(formula, data) {
    d = model.frame(formula, data, na.action = NULL)
    for(i in 1:dim(d)[2]) {
        if(is.factor(d[[i]]) || is.logical(d[[i]]) || is.character(d[[i]]))
            d[[i]] = factor(d[[i]])
    }
    return(d)
}

entropyHelper <- function(x, unit = "log") {
    return(entropy(table(x, useNA="always"), unit = unit))
}

###################### Import Customized Local CRAN Library ########################

for(amend_dependency in amend_dependencies){
    load_all(paste("./", amend_dependency, sep=""))
}

# 'optparse' Changes(Currently, wating for pull request)
# 1. Allow callback function to make_option parameter to sanitize input value seamlessly.

# 'ifs' Changes(Clone and create new repository) Origninally 'mRMRe' package
# 1. Allow high-order mutual and conditional information matrix approximation based on referenced paper.
# Note that high-order approximation is computationally heavy operation, that if and only if apply it
# when you computing resource is abundant enough. 

# 2. Add information theory based filter type feature selectors.

####################################################################################

# Libraries that included in R natively prompts error on install.packages function
for(base_dependency in base_dependencies){
	library(base_dependency, character.only=TRUE)
}

###### Import codes 

# Utils
source("combination.R")
source("discretize.R")

# Data Preprocessing
source("constants.R")
source("preprocessing.R")
source("arguments.R")
source("option_validity_checkers.R")

# Featuer Selection

source("fs.cfs.R")
source("fs.consistency.R")
source("fs.relief.R")
source("fs.bfs.R")
source("fs.R")

# Output
source("report.R")
source("visualize.R")


###############################################################################################################################################

classification_models <- list("NaiveBayes"="naive_bayes","RRF"="RRF","RRFboost"="RRF","Tree"="treebag", "Treeboost"="xgbTree", "Adaboost"="gbm", "KNN"="kknn", "Gam"="bam","Gamboost"="gamLoess","Glm"="glmnet","Glmboost"="LogitBoost","svmRadial"="svmRadial","svmLinear"="svmLinear")

regresion_models <- c() # not yet......
filter_selectors <- list("mRMR"=mRMR_selector, "MIM"=MIM_selector,"JMI"=JMI_selector,"MIFS"=mRMR_selector,"Relief"=relief_selector, "Consistency"=consistency_selector,"CFS"=cfs_selector) 
wrapper_selectors <- list("backward"=backward_selector, "forward"=forward_selector, "best"=backward_selector, "genetic"=genetic_selector, "simul_anneal"=simulated_selector)
evaluation_metrics <- list("Accuracy"="Accuracy", "Sensitivity"="Sensitivity", "Mean_Sensitivity"="Mean_Sensitivity", "Specificity"="Specificity", "Mean_Specificity"="Mean_Specificity", "prAUC"="prAUC", "prAUCSD"="prAUCSD", "roAUC"="AUC", "roAUCSD"="AUCSD", "Mean_F1"="Mean_F1","F1"="F1")
iofss <- c("CFS", "Consistency") # internally optimized feature selectors


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


        # Dataset Options
	make_option(c("--train"), type="character", default=NULL, metavar="train", #action="callback", callback=function(x) x,
		    help="Data to be trained by ML model(should align feature to vertical)"),
	make_option(c("--test"), type="character", default=NULL, 
		    help="Data to be use as external validation set. Filename is Multiple test sets are allowed. "),
	make_option(c("--response"), type="character", default=NULL,
		    help="Column to be target of analysis. if header is TRUE, then it should be header name. otherwise, position of variable (starts with 1) is required. If -1 is entered, then last column is automatically selected. "), # ML models that requires multiple response variable like 'association rule learning' is not considered on current circumstances.
	make_option(c("--categorical"), type="character", default="", 
		    help="Feature names, or numbers to be considered as categorical variable"),
	make_option(c("--na"), type="character", default="omit", 
		    help="How missing values are handled, Possible choices are : omit, mice, knn, tree"),
	make_option(c("--header"), type="logical", default=FALSE,
		    help="Existence of header on train, test files"),
	make_option(c("--exclude_feature"), type="character", default=FALSE,
		    help="Exclude features on analysis procedure listed in this option"),

        # Parallelization Options
	make_option(c("--cores"), type="integer", default=2,
		    help="all cores are automatically occupied when -1 is entered"),


        # Analysis Options
	make_option(c("--hy_op"), type="character", default="grid", 
		    help="Choose hyperparameter optimization algorithms to be applied."),
	make_option(c("--w_iters"), type="integer", default="10", 
		    help="Number of algorithm iterations in wrapper algorithm (Only applicable in backward, forward, genetic, simul_annel)"),
	make_option(c("--w_population"), type="integer", default="10", 
		    help="Number of initial feature subset population size (Only applicable in genetic)"),
	make_option(c("--seed"), type="integer", default=NULL, 
		    help="Existence of header on train, test files"),
	make_option(c("--feature_selectors"), type="character", default="mRMR", 
		    help=paste("Possible Feature Selectors are as follows :",paste(names(wrapper_selectors), collapse=", "), paste(names(filter_selectors), collapse=", "))),
	make_option(c("--models"), type="character", default="kknn,treebag", 
		    help=paste("Possible Models are as follows :",paste(names(wrapper_selectors), collapse=", "), paste(names(filter_selectors), collapse=", "))),

        # Validation Options
 	make_option(c("--metric"), type="character", default="roc", help=""),    
	make_option(c("--cv"), type="character", default="kfoldCV", help=""),
	make_option(c("--cv_folds"), type="integer", default="10",
		    help="number of folds in k-fold-cross validation"), 
	make_option(c("--cv_repeats"), type="integer", default="1",
		    help="number of repeats in repeated k-fold-cross validation. If --cv is not repeatedKFoldCV, then this option will be ignored."), 

        # Logging Options
	make_option(c("--experiment_name"), type="character", default="",
		    help="Name of directory that store current analysis procedure output."),
	make_option(c("--log_level"), type="character", default="INFO",
		    help=paste(LOG_LEVELS, collapse=", ")),  
	make_option(c("--log_path"), type="character", default="./",
		    help="Location of log directory(named with --experiment_name option)"),

        # Etc
	make_option(c("--mail"), type="character", default="",
		    help="Target mail address of analysis notification will be send.")
		   )

optparser <- OptionParser(option_list=option_list)
options <- parse_args(optparser)

########################### Check Option Validity ###############################

# Parallelization
registerDoMC(cores=options$cores)

options$categorical <- comma_seperated2R_string(options$categorical)

options$models <- comma_seperated2R_string(options$models)
options$feature_selectors <- comma_seperated2R_string(options$feature_selectors)

if(!is.null(options$test)){
    options$test <- comma_seperated2R_string(options$test, mode="c")
}
if(!is.null(options$mail)){
    options$mail <- comma_seperated2R_string(options$mail)
    options$mail <- eval(parse(text=options$mail))
}
if(options$help || is.null(options$train) || is.null(options$response)){
	print_help(optparser)
        stop("Please read the following instructions.")
}

# seed check
if(!is.null(options$seed)){
    set.seed(options$seed) # For Reproducibility
}

analysis_start_time <- Sys.time()
# experiment name check
if(options$experiment_name == "")
{
    options$experiment_name <- paste(tail(unlist(strsplit(options$train, "[/]")), n=1), format(analysis_start_time, "%Y%m%d_%H-%M-%S"), sep="")
    print(paste("Because experiment_name option is empty, it is automatically set to : ", options$experiment_name))
}


cv_methods <- list("kfoldCV"="cv", "bootstrap"="bootstrap", "LOOCV"="LOOCV", "repeatedkfoldCV"="repeatedCV")
# cv check 
if(options$cv == "" || !options$cv %in% names(cv_methods))
{
    stop(paste("Wrong cv option value is placed. Please choose one of the ", paste(names(cv_methods), collapse=", ")))
}
options$cv <- cv_methods[[options$cv]]


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
evaluated_options_test <- ""
for(child_log_dir in CHILD_LOG_DIR){

        # Prevent child dir duplication
	if(!file.exists(file.path(ROOT_LOG_DIR, child_log_dir))){

		dir.create(file.path(ROOT_LOG_DIR, child_log_dir))

                # Create dataset-wise sub-directory for storing informations
	        evaluated_options_test <- eval(parse(text=options$test))
                if(child_log_dir %in% c(METRIC_DIR))
                {

                    for(subdir in c("Training", evaluated_options_test))
                    {
         		dir.create(file.path(ROOT_LOG_DIR, child_log_dir, basename(subdir)))
                    }
                }
	}
}


logger <- create.logger()
logfile(logger) <- file.path(ROOT_LOG_DIR, 'execution.log')
level(logger) <- options$log_level 
info(logger, options)

######################## MISSING VALUE HANDLING #################################

if(options$na == "omit"){
    train <- na.omit(train)
} else if(options$na=="mice"){
    train <- mice(train) 
} else if(options$na=="knn"){
    train <- knnImputation(train)
} else {
    stop("Wrong Missing Value Option")
}
############################### Data set Handling ###################################################

train <- read.csv(options$train, header=options$header, na.strings=c("NA", ""))
train_size <- length(train)
tests <- list()
if(!is.null(options$test)){
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
}
############################ Categorical Value Handling ####################################

categorical_variables <- list()
if(iterable(options$categorical)){

	categorical_variables <- eval(parse(text=options$categorical))

	for(categorical_variable in categorical_variables){

                # Categorize training set variables.
		train[, categorical_variable] <- as.factor(train[, categorical_variable])
                levels(train[, categorical_variable]) <- sapply(sort(unique(train[, categorical_variable])), function(var) paste("Categorical",as.character(var),sep=""))

                # Categorize test set variables.
                for(test in names(tests)){
                    tests[[test]][, categorical_variable] <- as.factor(tests[[test]][, categorical_variable])
                    levels(tests[[test]][, categorical_variable]) <- sapply(sort(unique(tests[[test]][, categorical_variable])), function(var) paste("Categorical",as.character(var),sep=""))
                }
	}

        # Numerize training set variables.
	train[!names(train) %in% categorical_variables] <- lapply(train[!names(train) %in% categorical_variables], function(x) as.numeric(as.character(x)))
	train <- as.data.frame(train)

        # Numerize test set variables.
        for(test in names(tests)){
            tests[[test]][!names(tests[[test]]) %in% categorical_variables] <- lapply(tests[[test]][!names(tests[[test]]) %in% categorical_variables], function(x) as.numeric(as.character(x)))
            tests[[test]] <- as.data.frame(tests[[test]])
        }

        # Print whether the variable type is seamlessly processed.
	print(rapply(train, function(x) class(x)))
}

#############################################################################################

# Parse Models From Option
if(iterable(options$models)){

	included_models <- eval(parse(text=options$models))

	for(classification_model in names(classification_models)){
		if(!classification_model %in% included_models){
			classification_models[[classification_model]] <- NULL
		}
	}
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
selector_names <- names(selectors)

# By Reference, 
# [1] On Estimating Model Accuracy with Repeated K-Fold Cross-Validation
# Repeated CV does not affect on accurate model selection, but increase computational cost. 

# Stratified Repeated K-Fold Cross Validation
#stratified_folds_indexes = createFolds(train, k=5, list=TRUE, returnTrain=TRUE)
validator = trainControl(
                        method=options$cv,
                        allowParallel=TRUE,
			classProbs=TRUE,
                        savePredictions=TRUE,
                        summaryFunction=multiClassSummary,
                        number=options$cv_folds,
                       	repeats=options$cv_repeats,
                        )


results <- c()         # Store Evaluation Metric score of finalModels, may be replaced by (validation, test result variable located below)
filtered_indexes <- list() # Importance scores of current dataset calculated by filter type feature selector

validation_results <- list()
test_results <- list()

# Initialization of validation_results, and test_results
for(evaluation_metric in names(evaluation_metrics)){
    validation_results[[evaluation_metric]] <- getEmptyPerformanceMatrix(names(classification_models), selector_names)
    test_results[[evaluation_metric]] <- getEmptyPerformanceMatrix(names(classification_models), selector_names)
}

#analysis_combinations <- combination.named(classification_models, selectors) 
#print(analysis_combinations)

# Choose Classification Model 
for(model in names(classification_models))
{
    results[[model]] <- list()

    # Choose Filter Selector
    for(filter_selector in c(names(selectors)))
    {
        combination_start_time <- Sys.time()
	print(filter_selector)
        info(logger, paste("Start ML Model Analysis with Combination : (", model, filter_selector, ")"))
	indexes <- c()

        # Check current filter is already evaluated for given dataset
    	if(filter_selector %in% names(filtered_indexes))
	{
	    indexes <- filtered_indexes[[filter_selector]]
	}
	else
	{

            indexes <- selectors[[filter_selector]](train, validator, length(train) - 1, options$response)
	    filtered_indexes[[filter_selector]] <- indexes
	}
        print(indexes)
        results[[model]][[filter_selector]] <- list()
        tmp_metrics <- list()
        tmp_results <- list()      

	if(!filter_selector %in% names(wrapper_selectors)){
                # If feature selector is filter type.

        	selected_train <- train[indexes]
		# Choose optimal number of feature in filter selector
		for(num_feature in seq(2, as.integer(length(train) * 3/4), as.integer(length(train) / 10)))
		{
		    # If filter selector does not use internal optimization, then pass the condition below 
		    if(!filter_selector %in% iofss)
		    {
			selected_train <- train[indexes[1:num_feature]]
		    }

		    print(paste("Training model with", model, "and", num_feature, "features."))
		    info(logger, paste("Training model with", num_feature, "features."))
                    print(names(selected_train))

		    result <-train(
				    as.simple.formula(names(selected_train), options$response),
				    data=train,
				    method=classification_models[[model]],
				    trControl=validator)

		    # Saving maximum model in each cv
                    max_tune_index <- which.max(result$results[ , "AUC"])

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
                # If feature selector is wrapper type.
		tmp_metrics <- c(1)
		tmp_results[[1]] <- indexes	

                
	}

        # Choose Max Index Using Evaluation Metiric 
        max_index <- which.max(tmp_metrics)
        max_result <- tmp_results[[max_index]]

        # Choose Maximum Feature Subset on Give Combination
        max_data <- NULL
        if(!filter_selector %in% names(wrapper_selectors)){
            max_data <- names(max_result$trainingData)
            max_data <- max_data[max_data != ".outcome"]
        }
        else
        {
            max_data <- max_result$optVariables
        }

        # Choose Maximum ML Model Summary
        max_performance <- max_result$results
        if("external" %in% names(max_result))
        {
            # exceptional final model summary result saving error when genetic and simulated annealing wrapper selectors.
            max_performance <- max_result$external 
        }

        print(max_performance)
        print(names(max_result))
        print(names(max_result$trainingData))

	max_tune_index <- which.max(max_performance[ , "AUC"])
        

        ################### SAVING RESULT #########################################################
        print("Saving current combination results......")
        # Saving evaluation metrics matrices 
        for(evaluation_metric in names(evaluation_metrics)){
            if(evaluation_metrics[[evaluation_metric]] %in% names(max_performance))
            {
                validation_results[[evaluation_metric]][model, filter_selector] <- max_performance[max_tune_index, evaluation_metrics[[evaluation_metric]]]
                write.table(validation_results[[evaluation_metric]], file.path(ROOT_LOG_DIR, METRIC_DIR, "Training",paste(evaluation_metrics[[evaluation_metric]], "csv", sep=".")), sep=",")
            }
        }
        write.table(validation_results[["roAUC"]] + validation_results[["roAUCSD"]], file.path(ROOT_LOG_DIR, METRIC_DIR, "Training","AUC_95CIUPPER.csv"), sep=",")
        write.table(validation_results[["roAUC"]] - validation_results[["roAUCSD"]], file.path(ROOT_LOG_DIR, METRIC_DIR, "Training","AUC_95CILOWER.csv"), sep=",")

        # Saving Evaluation metric heatmap image
        roauc_matrix.melted <- melt(validation_results[["roAUC"]])
        ggplot(data=roauc_matrix.melted, aes(x=Var2, y=Var1, fill=value)) + geom_tile() + geom_text(aes(x=Var2, y=Var1, label=value), color="black", size=4)
        ggsave(file.path(ROOT_LOG_DIR, "roauc_matrix.png"))
        print(validation_results[["roAUC"]])

        if(FALSE){
            # Saving classifier plots
            pdf(file=file.path(ROOT_LOG_DIR, PLOT_DIR, paste(model, "_", filter_selector,".pdf",sep="")), width=8, height=8)
            plot(max_result)
            dev.off()
        }
        
        # Saving best performance feature subset
	write(max_data, file=file.path(ROOT_LOG_DIR, FEATURE_DIR, paste(model, filter_selector)))

        # Saving middle session in case of excepational situations
	save.session(file=file.path(ROOT_LOG_DIR, SESSION_DIR, "feature_selector.RSession"))

        # Redundant result saving code block.
        results[[model]][[filter_selector]] <- list()
        results[[model]][[filter_selector]][["Features"]] <- max_data
        results[[model]][[filter_selector]][["FinalModel"]] <- max_result 

        ################################################################################################
        combination_end_time <- Sys.time()
        info(logger, paste("Time elapsed in current combination: ", combination_end_time - combination_start_time, "seconds"))
    }

}

analysis_end_time <- Sys.time()
info(logger, paste("Time elapsed in whole combination: ", analysis_end_time - analysis_start_time, "seconds"))

# Saving test prediction evaluations 
for(test in evaluated_options_test){

    # Choose ML Model
    for(model in names(classification_models))
    {
        # Choose Feature Selector
        for(selector in c(names(selectors)))
        {

            pred_probs <- predict(results[[model]][[selector]][["FinalModel"]], newdata=tests[[test]], type="prob")
            pred_raw <- predict(results[[model]][[selector]][["FinalModel"]], newdata=tests[[test]])
            if("pred" %in% names(pred_raw))
                pred_raw <- unlist(pred_raw$pred)

            print(unlist(tests[[test]][options$response]))
            print(pred_raw)
            metrics <- confusionMatrix(unlist(tests[[test]][options$response]), pred_raw)
            metrics_byClass <- rapply(as.data.frame(metrics$byClass), mean)
            for(evaluation_metric in names(evaluation_metrics)){
                if(evaluation_metrics[[evaluation_metric]] %in% names(metrics$overall))
                {
                    test_results[[evaluation_metric]][model, selector] <- metrics$overall[evaluation_metrics[[evaluation_metric]]]
                    write.table(test_results[[evaluation_metric]], file.path(ROOT_LOG_DIR, METRIC_DIR, basename(test),paste(evaluation_metrics[[evaluation_metric]], "csv", sep=".")), sep=",")
                }
                else if(evaluation_metrics[[evaluation_metric]] %in% names(metrics_byClass))
                {
                    test_results[[evaluation_metric]][model, selector] <- metrics_byClass[evaluation_metrics[[evaluation_metric]]]
                    write.table(test_results[[evaluation_metric]], file.path(ROOT_LOG_DIR, METRIC_DIR, basename(test),paste(evaluation_metrics[[evaluation_metric]], "csv", sep=".")), sep=",")
                }
            }
        }
    }
}

# Send analysis result to mail address specified in 'mail' option list.

if(!is.null(options$mail) && options$mail[[1]]!=""){
    print(options$mail)

    # Compress result directory
    command <-paste("zip -r ", options$experiment_name, ".zip ", options$experiment_name, sep="")
    print(command)
    system(command)

    # Send Analysis Result to email
    # Due to securit issue, whole result file could not be send.
    send.mail(from="hyunbin.bii@gmail.com", 
              to=options$mail, 
              subject="Machine Learning Result Notification", 
              body=paste("Analysis started at", analysis_start_time, "ended at", analysis_end_time, "using dataset", options$train),
              attach.files=c(paste(options$experiment_name, "zip",sep=".")),
              smtp=list(host.name="smtp.gmail.com", port=465,user.name="hyunbin.bii@gmail.com", passwd="hyunbin.bii1323!#@#", ssl=TRUE), 
              authenticate=TRUE, 
              isend=TRUE)

}
