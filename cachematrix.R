## this function creates a list whose values are all functions
## that create a matrix (set), display a matrix (get),
## save the inverse of the matrix (setinverse) and return
## the inverse (getinverse)
makeCacheMatrix <- function(x = matrix()) {
        inverse <- NULL
        set <- function() {
                x <<- y
                inverse <<- NULL
        }
        get <- function() x
        setinverse <- function(inv)  inverse<<- inv
        getinverse <- function() inverse
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}

## this function:
## - returns a non-null, cached inverse
## - if the inverse has not been determined:
##     - determines if the matrix is 2D, square and 
##       det=0 ==> returns emtpy matrix
##     - othervise determines inverse and caches it

cacheSolve <- function(x, ...) {
        inv <- x$getinverse()
        if(!is.null(inv)) {
                message("Getting cached data")
                return(inv)
        }
        data <- x$get()
        dims <- dim(data)
        if(length(dims) != 2 ){
            x$setinverse(matrix())
            message("The matrix is not 2D!")
        }
        else if( dims[1] != dims[2] ){
            x$setinverse(matrix())
            message("The matrix is not square!")
        }
        else if( det(data) == 0 ){
            x$setinverse(matrix())            
            message("There is no inverse, det(x)=0")
        }
        else{
            inv <- solve(data)
            x$setinverse(inv)
            return(inv)
            ## Return a matrix that is the inverse of 'x'
        }
        invisible(matrix())
}
