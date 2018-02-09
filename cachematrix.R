## Cousera Data Science Specialization by John Hopkins University
## R Programming course - Programming Assognment 2
## Invert a marix and cache its result
## so that subsequent requests for matrix inversion
## retrieve from cache rather than redoing the inverse calculation
## Refer for understanding of the code(similar code)- https://github.com/lgreski/datasciencectacontent/blob/master/markdown/rprog-breakingDownMakeVector.md

## Creates a cache for a matrix

makeCacheMatrix <- function(x = matrix()) {
        i <- NULL
        set <- function(y)      {
                x <<- y
                i <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) i <<- inverse
        getinverse <- function() i
        list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## Returns cached inverse for old matrix or Calculates the inverse for the new matrix

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        i <- x$getinverse()
        if(!is.null(i)) {
                message("getting cached data")
                return(i)
        }
        data <- x$get()
        i <- solve(data, ...)
        x$setinverse(i)
        i
}
