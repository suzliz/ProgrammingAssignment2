## This function creates a vector that is a list containing a
## function to set or get the value of a vector and set or get
## the inverse of a matrix.


makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y){
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setinv <- function(solve) m <<- solve
        getinv <- function() m
        list(set=set, get=get, setinv = setinv, getinv = getinv)

}


## cacheSolve calculates the inverse of a matrix created with the
## makeCacheMatrix function. If first checks to see if the inverse
## has been calculated, If so it gets the inverse from the cache 
## (using getinv) ands skips the computation. If not, it calculates 
## the inverse and sets the value of the inverse (using setinv).

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getinv()
        if(!is.null(m)){
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data,...)
        x$setinv(m)
        m
        
}
