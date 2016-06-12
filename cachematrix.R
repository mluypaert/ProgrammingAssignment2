## Put comments here that give an overall description of what your
## functions do


## Function to create a cacheMatrix object, 
## used to cache the value of a matrix and it's inverse
makeCacheMatrix <- function(x = matrix()) {
	inversed_matrix <- NULL
	
	set <- function(y) {
		x <<- y
		inversed_matrix <<- NULL
	}
	get <- function() {
		return(x)
	}
	setInverse <- function(m){
		inversed_matrix <<- m
	}
	getInverse <- function(){
		return(inversed_matrix)
	}
	
	return(list(set = set, get = get, setInverse = setInverse, getInverse = getInverse))
}


## Function that retrieves and returns the inverse of a matrix in a cacheMatrix object if stored,
## or that calculates and stores the inverse of a matrix stored in a cacheMatrix object.
cacheSolve <- function(x, ...) {
		
		inversedMatrix <- x$getInverse()
		if( !is.null(inversedMatrix) ){
			message("returning cached data")
		}
		else{
			originalMatrix <- x$get()
			inversedMatrix <- solve(originalMatrix)
			x$setInverse(inversedMatrix)
		}
		
		return(inversedMatrix)
}
