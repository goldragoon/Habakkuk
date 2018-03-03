iterable <- function(expression){
	l <- nchar(expression)

	if(l == 0 || substring(expression, l, l) != ")") return(FALSE)

	target <- c("character", "list")
	evaluated = eval(parse(text=expression))

	if(class(evaluated) %in% target){
		return(TRUE)
	}
	else{
		return(FALSE)
	}
}

checkInclusion <- function(target, iterable_object){

	if(!(target %in% iterable_object)){
		return(NULL)
	}

	return(target)
}

comma_seperated2R_string <- function(comma_seperated, mode="list"){
    type_empty <- paste(rapply(strsplit(comma_seperated, "[,]"), trimws), collapse="','")
    return(paste(c(mode, "('", type_empty,"')"), collapse=""))
}
