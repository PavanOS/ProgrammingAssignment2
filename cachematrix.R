## These functions makeCacheMatrix & cacheSolve allow you to cache the expensive
## matrix inverse operation and lets you retreive cached inverse of the matrix 
## when you have to run the inverse repeatedly in a loop

## This function allows you to create list that contains 4 functions to
## set your matrix in cache
## get your matrix from cache
## setsolve - set the inverse of your matrix in cache
## grtsolve - get the inverse of matrix from cache
## usage : cm<- makeCacheMatrix (your_matrix), you need to first call this function to have the funtion pointer ready for the cacheSolve function to call one the above 4 functions.
makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setsolve <- function(solve) m <<- solve
        getsolve <- function() m
        list(set = set, get = get,
             setsolve = setsolve,
             getsolve = getsolve)
}


## usage : cacheSolve(cm), cm is the object returned by above makeCacheMatrix function
## It first calls the getSolve function on passed object to check of inverse is in cache, if found it returns the inverse
## If inverse not found in cache, it calls setSolve function on passed object to cache the inverse and returns it


cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getsolve()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setsolve(m)
        m
}
