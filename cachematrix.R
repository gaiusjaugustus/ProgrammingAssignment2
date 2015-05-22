## Put comments here that give an overall description of what your
## functions do

## creates a matrix object that caches its inverse

makeCacheMatrix <- function(x = matrix()) {
        inverse <- NULL
        set <- function(y){
                x <<- y
                inverse <<- NULL
                
        }
        get <- function() x
        setinverse <- function(inverse) inverse <<-inverse
        getinverse <- function () inverse
        list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)

}


## computes the inverse of the matrix from above.  If the inverse has already,
## been calculated, it uses the value in the cache

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inverse <- x$getinverse()
        
        #if inverse was already calculated
        if(!is.null(inverse)){
                #skip computation
                message("...getting cached data")
                return(inverse)
        }
        
        #else calculate the inverse
        matrix.data <- x$get()
        inverse = solve(matrix.data, ...)
        
        #cache the inverse value
        x$setinverse(inverse)
        
        return(inverse)
}
