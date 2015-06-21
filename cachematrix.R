
## makeCacheMatrix creates a special "matrix" object that can cache its inverse, which is really a list containing a function to
## 1. set the value of the matrix
## 2. get the value of the matrix
## 3. set the value of the inverse of the matrix
## 4. get the value of the inverse of the matrix

makeCacheMatrix <- function(mtx = matrix()) {
        inverse <- NULL
        set <- function(y) {
                mtx <<- y
                inverse <<- NULL
        }
        get <- function() return(mtx);
        setInv <- function(inv) inverse <<- inv;
        getInv <- function() return(inverse);
        list(set = set, 
             get = get,
             setInv = setInv,
             getInv = getInv)

}


## cacheSolve computes the inverse of the special "matrix" returned by makeCacheMatrix. 
## If the inverse has already been calculated (and the matrix has not changed), cacheSolve will retrieve the 
## inverse from the cache and skips the computation. Otherwise, it calculates the inverse of the matrixa and 
## sets the value of the inverse in the cache via the setInverse function.

cacheSolve <- function(mtx, ...) {
        ## Return a matrix that is the inverse of 'x'
        inverse <- mtx$getInv()
        if(!is.null(inverse)) {
                message("getting cached inverse matrix")
                return(inverse)
        }
        data <- mtx$get()
        inverse <- solve(data, ...)
        x$setInv(inverse)
        return(inverse)
}
