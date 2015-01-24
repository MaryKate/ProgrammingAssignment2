## This assignment demonstrates Lexical Scoping in R.

## Objective: Cache the inverse of a matrix so that if it already exists, no neeed to recompute.

## makeCacheMatrix 
  ## - creates a special "matrix" object that can cache its inverse.
  ## - contains functions that sets & gets the matrix and sets & gets the inverse.
makeCacheMatrix <- function(x = matrix()) {
    
    m <- NULL  ##define the cache
    set <- function(y) {
      x <<- y ## assign the input matrix y to the variable x in the parent environment
      m <<- NULL ## re-initialize m in the parent environment to null
    }
    get <- function() x ## return the matrix x
    setinverse <- function(inverse) m <<- inverse ## set the cache m equal to the inverse of the matrix x
    getinverse <- function() m ## return the cached inverse of x
    list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)  
}

## cacheSolve computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), cacheSolve retrieves the inverse from the cache.
cacheSolve <- function(x, ...) {  
    m <- x$getinverse()
    if(!is.null(m)) {
      message("getting cached data") ## confirms that inverse already exists in cache and returns that.
      return(m)
    }
    data <- x$get()
    m <- solve(data, ...) ## otherwise, get the matrix and solve for the inverse.
    x$setinverse(m)
    m
}
