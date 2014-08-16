
## makeCacheMatrix(x)

## Receives an n x n invertible matrix
## Caches functions for the matrix and its inverse

## The 'set' function sets the value of the matrix
## The 'get' function gets the value of the matrix
## The 'setInverse' uses 'solve' to set inverse of the matrix
## The 'getInverse' gets the inverse of the matrix

## NOTE: for this assignment all input matrices are assumed to be invertible
##       There is not error checking for invertibility.

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {                       ## Create set(y) function in next environment
        x <<- y                                ## It will be used to store the inverse on the stack
        m <<- NULL                             ## 'm' will be used as a semaphore to determine 
    }                                          ## whether the inverse has been calculated or not
    
    get <- function() x                        ## The get() function copies the orig 'x' on stack
    
    setInverse <- function(solve) m <<- solve  ## setInverse() uses solve(), stores inverse in 'm'
                                               ## and pushes it on the stack
    
    getInverse <- function() m                 ## getInverse() pops a copy of inverse off the stack
    list(set = set, get = get,                 ## A list of these functions is returnted to the 
         setInverse = setInverse,              ## calling environment
         getInverse = getInverse)
}

## Function cacheSolve(x, ...)

## Accepts the list (stack) of functions from makeCacheMatrix and the original input matrix
## It checks to see if the inverse has already been solved (using the solve function).
## If it has already been solved, it pulls the result from cache
## If it hasn't, it runs solve to find the inverse.
## The returned matrix is the inverse of 'x' that was originally given to makeCacheMatrix.

cacheSolve <- function(x, ...) {

    m <- x$getInverse()                 ## Pop the inverse of 'x' from the stack, it it exists
    if(!is.null(m)) {                   ## Check to see if the inverse was already solved, if so...
        message("getting cached data")  ## Notify that result was found in cache
        return(m)                       ## Return cached result
    }
    data <- x$get()                     ## If result is not in cache, pop a copy of x from the stack
    m <- solve(data, ...)               ## Solve for the inverse
    x$setInverse(m)                     ## Push a copy on the stack to cache for next iteration
    
    m                                   ## Return uncached inverse of 'x'
}


## Function testCacheSolve() to test makeCacheMatrix and cacheSolve
## Executes without arguments
##

testCacheSolve <- function() {
    
    testMatrix <- rbind(c(1,3,5),c(0,1,0),c(7:9))
    
    a <- makeCacheMatrix(testMatrix)
    message("After makeCacheMatrix, resulting list")
    
    print(a)
    
    for (i in 1:3) {
        b <- cacheSolve(a)
        
        message("Calculated inverse of x:")
        print(b)
        
    }
    
    message("2nd and 3rd Passes above should have been cached\n")
    
    print
    
    message("Verifying that result is truly inverse...")
    message("Should see identity matrix:")
    
    testMatrix %*% b
    
}

