## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
# makeCacheMatrix function set the inverse of the matrix
# and set the result in a variable, also we have a function
# to return the inverse matrix 
makeCacheMatrix <- function(x = matrix()) {

	m <- NULL 
	set <- function(y){
		x <<- y
		m <<- NULL
	}

	get <- function() x

	setInv <- function(solve) m <<- solve
	getInv <- function() m

	list(set = set, get= get, 
		setInv = setInv, getInv = getInv)
}


## Write a short comment describing this function
# We obtain the inverse matrix if that is uncalculated (null) 
# we solve it and set it to the object if not we return 
# the calculated matrix
cacheSolve <- function(x, ...) {
    m <- x$getInv()
    if(!is.null(m)){
      message("getting cached matrix")
      return(m)
    }
    
    data <- x$get()
    m <- solve(data, ...)
    x$setInv(m)
    m
}
