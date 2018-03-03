getEmptyPerformanceMatrix <- function(models, selectors){
    tframe <- matrix(ncol=length(selectors), nrow=length(models))
    rownames(tframe) <- models
    colnames(tframe) <- selectors
    return(tframe)
}
