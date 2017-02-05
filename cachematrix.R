## Matrix inversion is a computation that tends to be costly and it's helpful to cache the inverse matrix rather than calculate it again and again.  

## makeCacheMatrix creates a list that has a function which
## 1) sets the value of the matrix
## 2) gets the value of the matrix
## 3) sets the value of the inverse of the matrix
## 4) gets the value of the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
	# i stores the cached inverse matrix
	i <- NULL
	# sets the value of the matrix
	set <- function(y) {
		x <- y
		i <<- NULL
	}
	# gets the matrix
	get <- function() x
	
	# sets the inverse matrix
	setinverse <- function(inverse) i <<- inverse
	
	# gets the inverse matrix
	getinverse <- function() i
	# Returns the matrix with these new functions
	list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}


## This function cacheSolve returns the inverse matrix.  It first verifies if such an inverse matrix has already been computed.  If it has, cacheSolve gets the result and skips the computation.  Otherwise, the function does the computation and sets the value of the cache using the setinverse function.

cacheSolve <- function(x, ...) {
      i <- x$getinverse()
      
      # If inverse already computed, simply return it
      if(!is.null(i)) {
      	message("getting cached data")
      	return(i)
      }
      # Given the inverse is not yet computed, it is calculated here
      data <- x$get()
      i <- solve(data)
      # the inverse is cached
      x$setinverse(i)
      # the inverse is returned
      i
}
