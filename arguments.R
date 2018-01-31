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


