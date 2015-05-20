##This function creates a special "matrix" object that can cache its inverse.

## We use the "<<-" operator which can be used to assign a value to an object in an
##environment that is different from the current environment.


#The first function, makeCacheMatrix creates a special "matrix", which is really a list containing a function to

## 1.) set the value of the matrix
## 2.) get the value of the matrix
## 3.) set the value of the inverse
## 4.) get the value of the inverse


makeCacheMatrix <- function(x = matrix()) {
	x_inv <- NULL
	set <- function(y)
	{
		x <<- y
		x_inv <<- NULL
	}
	get <- function() x
	setinverse <- function(solve) x_inv <<- solve
	getinverse <- function() x_inv
	list(set = set, get = get, setinverse = setinverse, getinverse= getinverse)
}


## The following function calculates the inverse of the special "matrix" created with the above function. 
## However, it first checks to see if the inverse has already been calculated. 
## If so, it gets the inverse from the cache and skips the computation.
## Otherwise, it calculates the inverse of the data and sets the value of the inverse in the cache via the setinverse function.

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    x_inv <- x$getinverse()
    if(!is.null(x_inv))
    {
    	message("getting cached data")
    	return(x_inv)
    }
    data <- x$get()
    x_inv <- solve(data)
    x$setinverse(x_inv)
    x_inv
}
