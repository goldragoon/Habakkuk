glm <- function(dataframe){
	# Assumptions
	# 1. Independence of each Variables
	# 2. Correct Distribution of the residuals
	# 3. Correct specification of the variance structure
	# 4. Linear Relationship between the response and the linear predictor
}

gam <- function(dataframe){

}

ns_preprocessing <- makeNamespace("preprocessing")
funcs <- list()
funcs[[as.character(substitute(glm))]]=glm
funcs[[as.character(substitute(gam))]]=gam

for(func in names(funcs))
{
	print(paste("Initializing", func))

	assign(func, funcs[[func]], env=ns_preprocessing)
}
